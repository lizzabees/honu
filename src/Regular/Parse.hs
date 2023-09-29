module Regular.Parse
  ( Parser, parser, expr
  ) where

import Control.Monad (void)
import Data.Functor (($>))
import Data.CharSet (CharSet)
import Data.CharSet qualified as CharSet
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)

import Regular.Expr (Bound(..), Expr(..))

type Parser = Parsec Void Text

parser :: Parser Expr
parser = expr <* eof

escc :: Parser Char
escc  = char '\\' *> anySingle
        
cset :: Parser Expr
cset  = do
  void $ char '['
  neg <- option False $ char '^' $> True
  items <- many item
  void $ char ']'
  return $ case items of
    []  -> if neg then Any else Nul
    [s] -> if neg then And [Cmp $ Set s, Any] else Set s
    xs  -> let cs = Set $ foldr CharSet.union CharSet.empty xs
           in if neg then And [Cmp cs, Any] else cs

  where 
    regc :: Parser Char
    regc  = satisfy $ \case
      '-' -> False
      ']' -> False
      _   -> True

    setc :: Parser Char
    setc  = regc <|> escc

    item :: Parser CharSet
    item  = do
      lo <- setc
      hi <- optional $ char '-' *> setc
      case hi of
        Just hi' -> if lo > hi'
          then fail $ mconcat ["invalid char range '", [lo], "'-'", [hi'], "'"]
          else return $ CharSet.range lo hi'
        Nothing  -> return $ CharSet.singleton lo


bound :: Expr -> Parser Expr
bound e = do
  lo  <- optional decimal
  sep <- optional $ char ','
  hi  <- optional decimal
  void $ char '}'
  bnd <- case (lo, sep, hi) of
    (Just l , Nothing, Nothing) -> return $ Exactly l
    (Just l , Just  _, Nothing) -> return $ AtLeast l
    (Nothing, Just  _, Just  h) -> return $ AtMost  h
    (Just l , Just  _, Just  h) | l <= h -> return $ Between l h
    _                           -> fail "invalid bound"
  return $ Bnd e bnd

manyOr :: ([Expr] -> Expr) -> Parser [Expr] -> Parser Expr
manyOr ctor p = p >>= \case
  []  -> return Eps
  [e] -> return e
  es  -> return $ ctor es

quant :: Expr -> Parser Expr
quant e = option e $ oneOf @[] "*+?{" >>= \case
  '*' -> return $ Cls e
  '+' -> return $ Cat [e, Cls e]
  '?' -> return $ Alt [e, Eps  ]
  '{' -> bound e
  _   -> error "unreachable"

expr :: Parser Expr
expr = (Eps <$ eof) <|> alts
  where meta :: [Char]
        meta  = "|&!*+?.\\(){}[]"

        onec :: Parser Char
        onec  = noneOf meta

        litc :: Parser Expr
        litc  = One <$> (onec <|> escc)

        anyc :: Parser Expr
        anyc  = Any <$ char '.'

        nest :: Parser Expr
        nest  = between (char '(') (char ')') expr

        atom :: Parser Expr
        atom  = choice [litc, anyc, cset, nest] >>= quant

        cmpl :: Parser Expr
        cmpl  = do
          cmp <- option False $ char '!' $> True
          atm <- atom
          if cmp then return $ Cmp atm else return atm

        cats :: Parser Expr
        cats  = manyOr Cat $ many cmpl

        ands :: Parser Expr
        ands  = manyOr And $ sepBy cats (char '&')

        alts :: Parser Expr
        alts  = manyOr Alt $ sepBy ands (char '|')
