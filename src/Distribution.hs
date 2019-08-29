{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Distribution where

import Control.Monad (ap)
import Data.List (genericLength)
import Data.Map (toList, fromListWith)

-- | Type synonym for probabilities.
type Prob = Rational

-- | Distribution data type.
newtype Dist a = D { runD :: [(a, Prob)] }

instance Applicative Dist where
  pure  = return
  (<*>) = ap

instance Functor Dist where
  fmap f dx = dx >>= return . f

instance Monad Dist where
  return x = D [(x, 1)]
  d >>= f  = D [(y, p * q) | (x, p) <- runD d, (y, q) <- runD (f x)]

-- | Construct a discrete distribution from a nonempty list of elements,
-- assigning the same probability to each element.
uniform :: [a] -> Dist a
uniform l = D [(x, 1 / genericLength l) | x <- l]

-- | Construct a distribution in which the first element has probability p
-- and the second 1−p.
choose :: Prob -> a -> a -> Dist a
choose p x y = D [(x, p), (y, 1 - p)]

-- | Recover the list representation of a distribution, reduced.
unpackD :: Ord a => Dist a -> [(a, Prob)]
unpackD = removeDups . removeZeroes . runD
  where
    removeZeroes = filter (\(_, p) -> p /= 0)
    removeDups   = toList . fromListWith (+)

-- | Remove duplicates and zeroes from a distribution.
reduction :: Ord a => Dist a -> Dist a
reduction = D . unpackD

-- | Sum the probabilities in the distribution.
weight :: Dist a -> Prob
weight (D l) = sum (map snd l)
