module Main where

import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.IO qualified as LTIO
import Options.Applicative
import Text.Megaparsec (errorBundlePretty)

import Config
import Regular (dotty, parse)
import Regular.Expr (simplify)

printAst :: Config -> IO ()
printAst (Config {..}) = 
  case parse "argument" cfgExpr of
    Left  errb -> putStr $ errorBundlePretty errb
    Right expr ->
      let
        expr' = if cfgSimplify
          then simplify cfgPasses expr
          else expr
        text  = case cfgOutput of
          Dot  -> dotty expr'
          Show -> LT.pack $ show expr'
      in LTIO.putStrLn text

main :: IO ()
main = printAst =<< execParser opts
  where
    opts = info (config <**> helper) $ mconcat 
      [ fullDesc 
      , header   "honu - a language for turtles 🐢"
      , progDesc "print the regex ast"
      ]
