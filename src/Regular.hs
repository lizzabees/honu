{-# LANGUAGE TemplateHaskell #-}
module Regular where

import Prelude hiding (negate, product, sum)

import Control.Monad (void)
import Data.Functor (($>))
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.CharSet (CharSet)
import Data.CharSet qualified as CharSet
import Data.Semiring
import Data.Star
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)

data Expr
  = Eps
  | Nul
  | Any
  | One Char
  | Set CharSet
  | Alt [Expr]
  | And [Expr]
  | Cat [Expr]
  | Bnd Expr Bound
  | Cls Expr
  | Cmp Expr
  deriving(Eq, Ord, Show)

instance Semiring Expr where
  zero            = Nul
  one             = Eps

  plus Any   _ = Any
  plus _   Any = Any
  plus Nul   r = r
  plus l   Nul = l

  plus (Alt l) (Alt r) = Alt $ l ++ r
  plus (Alt l)  r      = Alt $ l ++ [r]
  plus  l      (Alt r) = Alt $ l:r
  plus  l       r      = Alt [l, r]

  times  Nul     _      = Nul
  times  _       Nul    = Nul
  times  Eps     r      = r
  times  l       Eps    = l
  times (Cat l) (Cat r) = Cat $ l ++ r
  times (Cat l)      r  = Cat $ l ++ [r]
  times  l      (Cat r) = Cat $ l:r
  times  l           r  = Cat [l,r]

instance Ring Expr where
  negate (Cmp e) = e
  negate      e  = Cmp e

instance Star Expr where
  star (Cls e) = Cls e
  star  Nul    = Eps
  star  Eps    = Eps
  star      e  = Cls e

  aplus e = Cat [e, Cls e]

newtype Cnj = Cnj { getCnj :: Expr }

instance Semiring Cnj where
  zero = Cnj Nul
  one  = Cnj Eps

  plus  (Cnj l) (Cnj r) = Cnj $ plus l r

  times (Cnj Nul)        _  = Cnj Nul
  times  _        (Cnj Nul) = Cnj Nul
  times (Cnj Any)  r        = r
  times  l        (Cnj Any) = l
  times (Cnj Eps)        r  = r
  times  l        (Cnj Eps) = l

  times (Cnj (And l)) (Cnj (And r)) = Cnj . And $ l ++ r
  times (Cnj (And l)) (Cnj      r ) = Cnj . And $ l ++ [r]
  times (Cnj      l ) (Cnj (And r)) = Cnj $ And $ l:r

  times (Cnj l) (Cnj r) | l == r = Cnj l
  times (Cnj l) (Cnj r)          = Cnj $ And [l,r]
  
deriving via Expr instance Ring Cnj
deriving via Expr instance Star Cnj

conjunction :: [Expr] -> Expr
conjunction = getCnj . product . map Cnj

data Bound
  = AtLeast Int
  | AtMost  Int
  | Between Int Int
  | Exactly Int
  deriving (Eq,Ord,Show)

makeBaseFunctor ''Expr

simplify :: Expr -> Expr
simplify = cata go
  where go :: ExprF Expr -> Expr
        go (AltF es) = sum         es
        go (AndF es) = conjunction es
        go (CatF es) = product     es
        go (CmpF e ) = negate      e
        go (ClsF e ) = star        e
        go e         = embed       e

-- $> simplify <$> runParser (expr <* eof) "cat" "((abc)123)def"

-- $> simplify <$> runParser (expr <* eof) "alt" "((a|b)|c)|d"

-- $> simplify <$> runParser (expr <* eof) "cnj" "((a&b)&c)&d"

-- $> simplify <$> runParser (expr <* eof) "cls" "(a*)*"

-- $> simplify <$> runParser (expr <* eof) "cmp" "!(!a)"


type Parser = Parsec Void Text

expr :: Parser Expr
expr = (Eps <$ eof) <|> alts
  where meta :: [Char]
        meta  = "|&!*+?.\\(){}[]"

        onec :: Parser Char
        onec  = noneOf meta

        escc :: Parser Char
        escc  = char '\\' *> anySingle
        
        litc :: Parser Expr
        litc  = One <$> (onec <|> escc)

        anyc :: Parser Expr
        anyc  = Any <$ char '.'

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

          where regc :: Parser Char
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

        nest :: Parser Expr
        nest  = between (char '(') (char ')') expr

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

        quant :: Expr -> Parser Expr
        quant e = option e $ oneOf @[] "*+?{" >>= \case
          '*' -> return $ Cls e
          '+' -> return $ Cat [e, Cls e]
          '?' -> return $ Alt [e, Eps  ]
          '{' -> bound e
          _   -> error "unreachable"

        atom :: Parser Expr
        atom  = choice [litc, anyc, cset, nest] >>= quant

        cmpl :: Parser Expr
        cmpl  = do
          cmp <- option False $ char '!' $> True
          atm <- atom
          if cmp then return $ Cmp atm else return atm

        manyOr :: ([Expr] -> Expr) -> Parser [Expr] -> Parser Expr
        manyOr ctor p = p >>= \case
          []  -> return Eps
          [e] -> return e
          es  -> return $ ctor es

        cats :: Parser Expr
        cats  = manyOr Cat $ many cmpl

        ands :: Parser Expr
        ands  = manyOr And $ sepBy cats (char '&')

        alts :: Parser Expr
        alts  = manyOr Alt $ sepBy ands (char '|')
