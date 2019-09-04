{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeOperators #-}

module Language.Kuifje.Semantics where

import Prelude hiding (return, fmap, (>>=))
import Data.Map.Strict (fromListWith, toList, elems)

import Language.Kuifje.Distribution
import Language.Kuifje.Syntax

-- | Hyper-distribution type synonym.
type a ~~> b = Dist a -> Dist (Dist b)

-- | Bind with reduction applied to the input distribution.
(=>>) :: (Ord b) => Dist a -> (a -> Dist b) -> Dist b
m =>> f = reduction m >>= f

-- | Kleisli composition.
(==>) :: (Ord c) => (a ~> b) -> (b ~> c) -> (a ~> c)
f ==> g = \x -> f x >>= g

-- | For a given program, returns a function that calculates the
-- hyper-distribution for a given input distribution.
hysem :: (Ord s) => Kuifje s -> (s ~~> s)
hysem Skip          = return
hysem (Update f p)  = huplift f ==> hysem p
hysem (If c p q r)  = conditional c (hysem p) (hysem q) ==> hysem r
hysem (While c p q) = let wh = conditional c (hysem p ==> wh) (hysem q)
                      in wh
hysem (Observe f p) = hobsem f ==> hysem p

-- | Conditional semantics ('If' and 'While').
conditional :: (Ord s) => (s ~> Bool) -> (s ~~> s) -> (s ~~> s) -> (s ~~> s)
conditional c t e d
  = let d' = d =>> \s -> c s =>> \b -> return (b, s)
        w1 = sum [p | ((b, _), p) <- toList $ runD d', b]
        w2 = 1 - w1
        d1 = D $ fromListWith (+) [(s, p / w1) | ((b, s), p) <- toList $ runD d', b]
        d2 = D $ fromListWith (+) [(s, p / w2) | ((b, s), p) <- toList $ runD d', not b]
        h1 = t d1
        h2 = e d2
    in  if       null (runD d2)  then  h1
        else if  null (runD d1)  then  h2
                                 else  join (choose w1 h1 h2)

-- | Lifts a distribution to a hyper-distribution.
huplift :: (Ord s) => (s ~> s) -> (s ~~> s)
huplift f = return . (=>> f)

-- | 'Observe' semantics.
hobsem :: (Ord s, Ord o) => (s ~> o) -> (s ~~> s)
hobsem f = multiply . toPair . (=>> obsem f)
  where

    obsem :: (Ord o, Ord a) => (a ~> o) -> a ~> (o,a)
    obsem f' x = fmap (\w -> (w, x)) (f' x)

    toPair :: (Ord s, Ord o) => Dist (o, s) -> (Dist o, o -> Dist s)
    toPair dp = (d, f')
      where
        d     = fmap fst dp
        f' ws = let dpws = D $ fromListWith (+) [(s, p) | ((ws', s), p) <- toList $ runD dp, ws' == ws]
                in D $ fromListWith (+) [(s, p / weight dpws) | (s, p) <- toList $ runD dpws]

    multiply :: (Ord s) => (Dist o, o -> Dist s) -> Dist (Dist s)
    multiply (d, f') = fmap f' d

-- | Calculate Bayes Vulnerability for a distribution.
bayesVuln :: Ord a => Dist a -> Prob
bayesVuln = maximum . elems . runD . reduction

-- | Based on an entropy function for distributions, calculate the
-- average entropy for a hyper-distribution.
condEntropy :: (Dist a -> Rational) -> Dist (Dist a) -> Rational
condEntropy e m = average (fmap e m) where
  -- | Average a distribution of Rationals.
  average :: Dist Rational -> Rational
  average d = sum [r * p | (r, p) <- toList $ runD d]
