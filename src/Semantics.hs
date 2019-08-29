{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeOperators #-}

module Semantics where

import Control.Monad (join)

import DataTypes
import Syntax

(=>>) :: Ord a => Dist a -> (a -> Dist b) -> Dist b
m =>> f = reduction m >>= f

weight :: Dist a -> Prob
weight (D l)  = sum (map snd l)

type a ~~> b = Dist a -> Dist (Dist b)

foldPCL3 :: (PCL3F s a -> a) -> (PCL3 s -> a)
foldPCL3 alg Skip3           =  alg Skip3F
foldPCL3 alg (Update3 f p)   =  alg (Update3F f ((foldPCL3 alg p)))
foldPCL3 alg (If3 c p q r)   =  alg (If3F c ((foldPCL3 alg p)) ((foldPCL3 alg q)) ((foldPCL3 alg r)))
foldPCL3 alg (While3 c p q)  =  alg (While3F c ((foldPCL3 alg p)) ((foldPCL3 alg q)))
foldPCL3 alg (Observe3 f p)  =  alg (Observe3F f (foldPCL3 alg p))
foldPCL3 alg (Observe3' f p) =  alg (Observe3F' f (foldPCL3 alg p))

hysem :: (Ord s) => PCL3 s -> (s ~~> s)
hysem = foldPCL3 algH

(==>) :: (a ~> b) -> (b ~> c) -> (a ~> c)
f ==> g = \x -> f x >>= g

algH :: (Ord s) => PCL3F s (s ~~> s) -> (s ~~> s)
algH Skip3F            =  return
algH (Update3F f p)    =  huplift f ==> p
algH (If3F c p q r)    =  conditional c p q ==> r
algH (While3F c p q)   =  let  while = conditional c (p ==> while) q
                          in   while
algH (Observe3F' f p)  =  hobsem f ==> p

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
Observe3 f p  <---> k  = Observe3 f (p <---> k)  -- added

--------------------------------------------------
skip3 :: PCL3 s
skip3 = Skip3

update3 :: (s ~> s) -> PCL3 s
update3 f  =  Update3 f skip3

while3 :: (s ~> Bool) -> PCL3 s -> PCL3 s
while3 c p  =  While3 c p skip3

cond3 :: (s ~> Bool) -> PCL3 s -> PCL3 s -> PCL3 s
cond3 c p q  =  If3 c p q skip3

observe3' :: (Ord o, ToBits o) => (s ~> o) -> PCL3 s
observe3' o = Observe3' o skip3

observe3 :: ToBits a => (s ~> a) -> PCL3 s
observe3 f = Observe3' (fmap toBits . f) skip3

example4 :: PCL3 (Bool,Bool)
example4 = observe3 (\(b1,b2) -> choose (1 / 2) b1 b2)

boolPairs :: Dist (Bool,Bool)
boolPairs = uniform [(b1,b2) | b1 <- bools, b2 <- bools]
  where bools = [True,False]

bv:: Ord a => Dist a -> Prob
bv = maximum . map snd . runD . reduction

condEntropy:: (Dist a -> Rational) -> Dist(Dist a) -> Rational
condEntropy e h = average (fmap e h) where
  average:: Dist Rational -> Rational -- Average a distribution of |Rational|'s.
  average d = sum [r * p | (r,p)<- runD d]
