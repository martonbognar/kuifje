{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeOperators #-}

module Semantics where

import Control.Monad (join)

import DataTypes
import PrettyPrint
import Syntax

(=>>) :: Ord a => Dist a -> (a -> Dist b) -> Dist b
m =>> f = reduction m >>= f

weight :: Dist a -> Prob
weight (D l)  = sum (map snd l)

type a ~~> b = Dist a -> Dist (Dist b)

(==>) :: (a ~> b) -> (b ~> c) -> (a ~> c)
f ==> g = \x -> f x >>= g

hysem :: (Ord s) => PCL3 s -> (s ~~> s)
hysem Skip3            =  return
hysem (Update3 f p)    =  huplift f ==> (hysem p)
hysem (If3 c p q r)    =  conditional c (hysem p) (hysem q) ==> (hysem r)
hysem (While3 c p q)   =  let  while = conditional c ((hysem p) ==> while) (hysem q)
                          in   while
hysem (Observe3' f p)  =  hobsem f ==> (hysem p)

conditional :: Ord s => (s ~> Bool) -> (s ~~> s) -> (s ~~> s) -> (s ~~> s)
conditional c t e = \d ->
    let d'     = d =>> \s -> c s =>> \b -> return (b, s)
        w1     = sum [p | ((b,s),p) <- runD d', b]
        w2     = 1 - w1
        d1     = D [ (s, p / w1) | ((b,s),p) <- runD d', b]
        d2     = D [ (s, p / w2) | ((b,s),p) <- runD d', not b]
        h1     = t d1
        h2     = e d2
    in  if       null (runD d2)  then  h1
        else if  null (runD d1)  then  h2
                                 else  join (choose w1 h1 h2)

huplift :: Ord s => (s ~> s) -> (s ~~> s)
huplift f = return . (=>> f)

hobsem :: (Ord s, Ord o) => (s ~> o) -> (s ~~> s)
hobsem f = multiply . toPair . (=>> obsem f)
  where
    obsem :: Ord o => (a ~> o) -> (a ~> (o,a))
    obsem f = \x -> fmap (\w -> (w,x)) (f x)

    toPair :: (Ord s, Ord o) => Dist (o,s) -> (Dist o, o -> Dist s)
    toPair dp = (d,f)
       where
         d     =  fmap fst dp
         f ws  =  let  dpws  =  D [ (s, p) | ((ws',s), p) <- runD dp, ws' == ws]
                  in   D [ (s, p/weight dpws) | (s, p) <- runD dpws]

    multiply :: (Dist o, o -> Dist s) -> Dist (Dist s)
    multiply (d,f) = fmap f d

(<--->) :: PCL3 s -> PCL3 s -> PCL3 s
Skip3         <---> k  = k
Update3 f p   <---> k  = Update3 f (p <---> k)
While3 c p q  <---> k  = While3 c p (q <---> k)
If3 c p q r   <---> k  = If3 c p q (r <---> k)
Observe3' f p  <---> k  = Observe3' f (p <---> k)  -- added

example4 :: PCL3 (Bool,Bool)
example4 = observe3' (\(b1,b2) -> choose (1 / 2) b1 b2)

boolPairs :: Dist (Bool,Bool)
boolPairs = uniform [(b1,b2) | b1 <- bools, b2 <- bools]
  where bools = [True,False]

bv:: Ord a => Dist a -> Prob
bv = maximum . map snd . runD . reduction

condEntropy:: (Dist a -> Rational) -> Dist(Dist a) -> Rational
condEntropy e h = average (fmap e h) where
  average:: Dist Rational -> Rational -- Average a distribution of |Rational|'s.
  average d = sum [r * p | (r,p)<- runD d]
