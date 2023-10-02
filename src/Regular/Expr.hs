{-# LANGUAGE TemplateHaskell #-}
module Regular.Expr 
  ( Expr(..)
  , ExprF(..)
  , Bound(..)
  , Cnj(..)
  , Pass
  , conjunction
  , kleene
  , nullable
  , simplify
  , unbound
  ) where

import Prelude hiding (negate, product, sum)

import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.CharSet (CharSet)
import Data.CharSet qualified as CharSet
import Data.List (foldl')
import Data.Semiring hiding ((+), (-), (*))
import Data.Star

data Expr
  = Eps
  | Nul
  | Any
  | One Char
  | Str String
  | Set CharSet
  | Alt [Expr]
  | And [Expr]
  | Cat [Expr]
  | Bnd Expr Bound
  | Cls Expr
  | Cmp Expr
  deriving(Eq, Ord, Show)

instance Semiring Expr where
  zero = Nul
  one  = Eps

  plus Any   _ = Any
  plus _   Any = Any
  plus Nul   r = r
  plus l   Nul = l

  plus (Set l) (Set r) = Set $ CharSet.union  l r
  plus (Set l) (One r) = Set $ CharSet.insert r l
  plus (One l) (Set r) = Set $ CharSet.insert l r
  plus (One l) (One r) = Set $ CharSet.fromList [l,r]

  plus (Alt l) (Alt r) = Alt $ l ++ r
  plus (Alt l)  r      = Alt $ l ++ [r]
  plus  l      (Alt r) = Alt $ l:r
  plus  l       r      = Alt [l, r]

  times  Nul     _      = Nul
  times  _       Nul    = Nul
  times  Eps     r      = r
  times  l       Eps    = l

  times (Str l) (Str r) = Str $ l ++ r
  times (Str l) (One r) = Str $ l ++ [r]
  times (One l) (Str r) = Str $ l:r
  times (One l) (One r) = Str $ [l,r]

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

type Pass = ExprF Expr -> ExprF Expr

unbound :: Pass
unbound = project . \case
  (BndF e (AtLeast   n)) -> atLeast e n
  (BndF e (AtMost    n)) -> atMost  e n
  (BndF e (Between l h)) -> between e l h
  (BndF e (Exactly   n)) -> exactly e n 
  e                      -> embed   e

  where
    atLeast :: Expr -> Int -> Expr
    atLeast e 0 = Cls e
    atLeast e n = Cat . reverse . (Cls e :) $ replicate n e
    
    atMost :: Expr -> Int -> Expr
    atMost _ 0 = Eps
    atMost e 1 = Alt [e, Eps]
    atMost e n = Cat . replicate n $ Alt [e, Eps]
    
    exactly :: Expr -> Int -> Expr
    exactly _ 0 = Eps
    exactly e 1 = e
    exactly e n = Cat $ replicate n e
    
    between :: Expr -> Int -> Int -> Expr
    between _ 0 0 = Eps
    between e 1 1 = e
    between e l h = Cat [exactly e l, atMost e (h - l)]

kleene :: Pass
kleene = project . \case
  AltF es -> sum         es
  AndF es -> conjunction es
  CatF es -> product     es
  CmpF e  -> negate      e
  ClsF e  -> star        e
  e       -> embed       e

simplify :: [Pass] -> Expr -> Expr
simplify = cata . foldl' (.) embed

nullable :: Expr -> Bool
nullable = cata nullable'

nullable' :: ExprF Bool -> Bool
nullable'  EpsF     = True
nullable'  NulF     = False
nullable'  AnyF     = False
nullable' (OneF  _) = False
nullable' (StrF  _) = False
nullable' (SetF  _) = False

nullable' (AltF es) = sum     es
nullable' (AndF es) = product es
nullable' (CatF es) = product es
nullable' (ClsF e ) = star    e
nullable' (CmpF e ) = not     e

nullable' (BndF _ (AtLeast   0)) = True
nullable' (BndF e (AtLeast   _)) = e
nullable' (BndF _ (AtMost    _)) = True
nullable' (BndF _ (Between 0 _)) = True
nullable' (BndF e (Between _ _)) = e
nullable' (BndF _ (Exactly   0)) = True
nullable' (BndF e (Exactly   _)) = e
