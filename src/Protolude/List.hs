{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Protolude.List (
  head,
  last,
  mhead,
  mlast,
  ordNub,
  sortOn,
  list,
  product,
  sum,
  zipConsecutives,
  zipConsecutivesWith
) where

import Control.Applicative (pure)
import Data.List (sortBy, tail, zip, zipWith)
import Data.Maybe (Maybe(..))
import Data.Ord (Ord, comparing)
import Data.Foldable (Foldable, foldl, foldr, foldl')
import Data.Monoid (Monoid(..))
import Data.Function ((.))
import Data.Functor (fmap)
import GHC.Num (Num, (+), (*))
import qualified Data.Set as Set

head :: (Foldable f) => f a -> Maybe a
head = foldr (\x _ -> pure x) Nothing

last :: (Foldable f) => f a -> Maybe a
last = foldl (\_ x -> pure x) Nothing

mhead :: (Foldable f, Monoid a) => f a -> a
mhead = foldr (\x _ -> x) mempty

mlast :: (Foldable f, Monoid a) => f a -> a
mlast = foldl (\_ x -> x) mempty

sortOn :: (Ord o) => (a -> o) -> [a] -> [a]
sortOn = sortBy . comparing

-- | /O(nlog n)/
ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ []     = []
    go s (x:xs) =
      if x `Set.member` s
      then go s xs
      else x : go (Set.insert x s) xs

list :: [b] -> (a -> b) -> [a] -> [b]
list def f xs = case xs of
  [] -> def
  _  -> fmap f xs

{-# INLINE product #-}
product :: (Foldable f, Num a) => f a -> a
product = foldl' (*) 1

{-# INLINE sum #-}
sum :: (Foldable f, Num a) => f a -> a
sum = foldl' (+) 0

-- diff consecutive elements:
-- > diffs = zipConsecutivesWith (flip (-))
--
-- determine if list is ascending (similar for descending and strict):
-- > isAscending = and . zipConsecutivesWith (<=)
--
-- fibs:
-- > fibs = 1 : 1 : zipConsecutivesWith (+) fibs
--
-- get the edges of a closed path defined by points (ps):
-- > edges ps = zipConsecutivesWith makeEdge (ps ++ take 1 ps)

zipConsecutives :: [a] -> [(a,a)]
zipConsecutives xs = zip xs (tail xs)

zipConsecutivesWith :: (a -> a -> b) -> [a] -> [b]
zipConsecutivesWith f xs = zipWith f xs (tail xs)
