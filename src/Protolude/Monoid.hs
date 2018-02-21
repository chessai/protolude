{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Protolude.Monoid
  ( Ap(..)
  , foldMapA
  , (++) 
  ) where

import Control.Applicative
import Data.Foldable
import Data.Monoid (Monoid(..))
import Prelude (($), (.))

infixr 5 ++
(++) :: Monoid m => m -> m -> m
(++) = mappend

newtype Ap f a = Ap { getAp :: f a }

instance (Applicative f, Monoid a) => Monoid (Ap f a) where
  mempty = Ap $ pure mempty
  mappend (Ap x) (Ap y) = Ap $ liftA2 mappend x y

foldMapA :: (Foldable t, Monoid m, Applicative f) => (a -> f m) -> t a -> f m
foldMapA f = getAp . foldMap (Ap . f)
