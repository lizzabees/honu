{-# LANGUAGE TemplateHaskell #-}
module Regular.Expr 
  ( Expr(..)
  , ExprF(..)
  , Bound(..)
  , Cnj(..)
  , conjunction
  , simplify
  ) where

import Prelude hiding (negate, product, sum)

import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.CharSet (CharSet)
import Data.Semiring
import Data.Star

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
