module Config where

import Data.Text (Text)
import Data.List (foldl')
import Options.Applicative

import Regular.Expr (Pass, kleene, unbound)

data Config = Config
  { cfgPasses   :: [Pass]
  , cfgSimplify :: Bool
  , cfgExpr     :: Text
  }

passes :: String -> Either String [Pass]
passes = flip foldl' (Right []) $ curry $ \case
  (Left  e ,  _ ) -> Left e
  (Right ps, 'k') -> Right $ kleene :ps
  (Right ps, 'u') -> Right $ unbound:ps
  (_       ,  c ) -> Left  $ mconcat ["invalid pass '", [c], "'"]

config :: Parser Config
config = do
  cfgSimplify <- flag True False $ mconcat 
    [  short 'S'
    , long  "no-simplify"
    , help  "disable post-parsing AST simplification"
    ]
  cfgPasses <- option (eitherReader passes) $ mconcat
    [ short 'p'
    , long "passes"
    , help "list of ast optimization passes. k: kleene, b: desugar bounds"
    , showDefaultWith (const "uk")
    , metavar "PASS..."
    , value [unbound, kleene]
    ]
  cfgExpr <- argument str $ mconcat 
    [ metavar "REGEX"
    , help "regex to parse"
    ]
  pure Config {..}
