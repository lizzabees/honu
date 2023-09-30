module Main where

import Options.Applicative
import Text.Megaparsec (errorBundlePretty)

import Config
import Regular qualified (parse)
import Regular.Expr (simplify)

printAst :: Config -> IO ()
printAst (Config {..}) = 
  case Regular.parse "argument" cfgExpr of
    Left  errb -> putStr $ errorBundlePretty errb
    Right expr -> print  $ if cfgSimplify
      then simplify cfgPasses expr
      else expr

main :: IO ()
main = printAst =<< execParser opts
  where
    opts = info (config <**> helper) $ mconcat 
      [ fullDesc 
      , header   "honu - a language for turtles 🐢"
      , progDesc "print the regex ast"
      ]
