{-# OPTIONS_GHC -Wno-unused-imports #-}
module Regular
  ( module Regular.Expr
  , module Regular.Parse
  , module Regular.Pretty
  ) where

import Regular.Expr   (Expr)
import Regular.Parse  (parse)
import Regular.Pretty (dotty)
