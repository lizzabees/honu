{-# LANGUAGE TemplateHaskell #-}
module Regular.Pretty
  ( dotty
  ) where

import Data.CharSet (CharSet)
import Data.CharSet qualified as CharSet (toAscList)
import Data.Functor.Foldable
import Data.Char (ord)
import Data.List (foldl')
import Data.Text.Lazy qualified as LT

import Regular.Expr (Bound(..), Expr(..), ExprF(..))
import Regular.Dotty

data SetElem a
  = Range a a
  | Lone  a
  deriving(Show)

succs :: (Enum a, Eq a) => a -> a -> Bool
succs y x = succ x == y

setElems :: (Enum a, Eq a) => [a] -> [SetElem a]
setElems = reverse . foldl' go []
  where
    go :: (Enum a, Eq a) => [SetElem a] -> a -> [SetElem a]
    go ((Range lo hi):acc) x | x `succs` hi = Range lo x:acc
    go ((Lone      c):acc) x | x `succs` c  = Range c  x:acc
    go                acc  x                = Lone     x:acc

prettySet :: Int -> Int -> CharSet -> String
prettySet minRng maxElems set =
  let
    subs = setElems $ CharSet.toAscList set
    toStr (Range lo hi) | (ord hi - ord lo) >= minRng = [lo,'-',hi]
    toStr (Range lo hi) = [lo ..  hi]
    toStr (Lone      c) = [c]
  in case splitAt (maxElems - 1) subs of
    (cs, []) -> mconcat ["[", concatMap toStr cs,"]"]
    (cs,  _) -> mconcat ["[", concatMap toStr cs, "...]"]

dotty :: Expr -> LT.Text
dotty = runDotty . cataA go
  where
    ltShow :: Show a => a -> LT.Text
    ltShow = LT.pack . show

    go :: ExprF (Dotty NodeId) -> Dotty NodeId
    go EpsF = leaf "ε"
    go NulF = leaf "∅"
    go AnyF = leaf "Any"

    go (OneF c) = item "Char"   $ LT.singleton c
    go (StrF s) = item "String" $ LT.pack s
    go (SetF s) = item "Set"    $ LT.pack $ prettySet 3 3 s
    
    go (AltF es) = sequence es >>= lots "Alt"
    go (AndF es) = sequence es >>= lots "And"
    go (CatF es) = sequence es >>= lots "Cat"

    go (ClsF e) = e >>= solo "Cls"
    go (CmpF e) = e >>= solo "Cmp"

    go (BndF e (AtLeast   n)) = e >>= solo ("At least " <> ltShow n)
    go (BndF e (AtMost    n)) = e >>= solo ("At most  " <> ltShow n)
    go (BndF e (Exactly   n)) = e >>= solo ("Exactly "  <> ltShow n)
    go (BndF e (Between l h)) = e >>= solo
      (mconcat ["Between ", ltShow l, " and ", ltShow h])
