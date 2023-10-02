module Regular.Dotty
  ( Dotty, leaf, item, solo, lots, runDotty
  , NodeId
  ) where

import Control.Monad.ST (runST)
import Data.Bifunctor   (second)
import Data.Foldable    (for_)
import Data.STRef
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as LTB

newtype NodeId = NodeId { _unNodeId :: Int}
  deriving(Eq,Ord,Show) via Int

data Node
  = Leaf Text
  | Item Text  Text
  | Solo Text  NodeId
  | Lots Text [NodeId]

data Graph = Graph
  { _grCount :: Int
  , _grNodes :: [(NodeId,Node)]
  }

newtype Dotty a = Dotty { _unDotty :: Graph -> (Graph, a) }

instance Functor Dotty where
  fmap f (Dotty u) = Dotty $ \x -> second f (u x)

instance Applicative Dotty where
  pure  x                   = Dotty $ \g -> (g, x)
  (<*>) (Dotty f) (Dotty x) = Dotty $ \g ->
    let (g' ,    f') = f g
        (g'',    x') = x g'
     in (g'', f' x') 

instance Monad Dotty where
  (Dotty x) >>= f = Dotty $ \g ->
    let (g',   x') = x g
        (Dotty y') = f x'
     in  y'    g'

add :: Node -> Dotty NodeId
add node = Dotty $ \case (Graph c n) -> (Graph (c+1) ((NodeId c,node):n), NodeId c)

leaf :: Text -> Dotty NodeId
leaf = add . Leaf

item :: Text -> Text -> Dotty NodeId
item name = add . Item name

solo :: Text -> NodeId -> Dotty NodeId
solo name = add . Solo name

lots :: Text -> [NodeId] -> Dotty NodeId
lots name = add . Lots name

showLTB :: (Show a) => a -> Builder
showLTB = LTB.fromString . show

runDotty :: Dotty NodeId -> LT.Text
runDotty (Dotty f) =
  let (Graph _ nodes, NodeId root) = f (Graph 0 [])
   in runST $ do
      out <- newSTRef @Builder mempty

      let write   x = modifySTRef out (<> x)
      let writeLn   = write . (<> "\n")
      let outId   (NodeId x) = "  node" <> showLTB x
      let outName            = LTB.fromLazyText 

      writeLn   "digraph G {"
      writeLn   "  node [ shape = Mrecord ];"
      writeLn   "  edge [ dir   = none    ];"
      writeLn $ "  root         = node" <> showLTB root <> ";"
      for_ nodes $ \case
        (nid, Leaf n  ) -> do
          writeLn $ mconcat [outId nid, " [ label = <{", outName n, "}> ];"]
        (nid, Item n x) -> do
          writeLn $ mconcat [outId nid, " [ label = <{", outName n, "}|{", LTB.fromLazyText x, "}> ];"]
        (nid, Solo n k) -> do
          writeLn $ mconcat [outId nid, " [ label = <{", outName n, "}|{1}> ];"]
          writeLn $ mconcat [outId nid, " -> ", outId k, ";"]
        (nid, Lots n ks) -> do
          writeLn $ mconcat
            [outId nid, " [ label = <{", outName n, "}|{", showLTB $ length ks, "}> ];"]
          for_ ks $ \k -> writeLn $ mconcat [outId nid, " -> ", outId k, ";"]
      writeLn $ "}"

      LTB.toLazyText <$> readSTRef out
