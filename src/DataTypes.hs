{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module DataTypes where

import Control.Monad (ap)
import Data.List (genericLength, unfoldr)
import Data.Map (toList,fromListWith)

type Prob = Rational

newtype Dist a = D { runD :: [(a, Prob)] }

instance Applicative Dist where
  pure  = return
  (<*>) = ap

instance Functor Dist where
  fmap f dx = dx >>= return . f

instance Monad Dist where
  return x = D [(x, 1)]
  d >>= f  = D [ (y, p * q) | (x, p) <- runD d, (y, q) <- runD (f x) ]

uniform :: [a] -> Dist a
uniform l    = D [(x, 1/genericLength l) | x <- l]

choose :: Prob -> a -> a -> Dist a
choose p x y = D [(x,p),(y, 1-p)]

-- | Recover list representation, reduced.
unpackD :: Ord a => Dist a -> [(a, Prob)]
unpackD = removeDups . removeZeroes . runD
  where
    removeZeroes = filter (\(x, p) -> p /= 0)
    removeDups   = toList . fromListWith (+)

reduction :: Ord a => Dist a -> Dist a
reduction = D . unpackD  -- Unpack and then repack.

--------------------------------------------------------------------------------

type Bit  =  Bool
type Bits =  [Bit]

class ToBits a where
  toBits :: a -> Bits

instance ToBits Int where
  toBits n = (n < 0) : unfoldr (
                        \m -> if m == 0
                                then Nothing
                                else Just (odd m, quot m 2)
                       ) n

instance ToBits () where
  toBits () = []

instance ToBits Bool where
  toBits b = [b]

instance ToBits Bits where
  toBits bits = bits
