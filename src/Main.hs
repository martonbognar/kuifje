{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad (join)
import Data.List (genericLength)

import DataTypes
import PrettyPrint
import Syntax

choose :: Prob -> a -> a -> Dist a
choose p x y = D [(x,p),(y, 1-p)]

reduction :: Ord a => Dist a -> Dist a
reduction = D . unpackD  -- Unpack and then repack.

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

type LArr a b  =  a -> Dist (Bits,b)

multiply :: (Dist Bits, Bits -> Dist s) -> Dist (Dist s)
multiply (d,f) = fmap f d

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

--------------------------------------------------
fraction :: Fractional a => a -> a -> a
fraction = (/)

skip3 :: PCL3 s
skip3 = Skip3

uniform :: [a] -> Dist a
uniform l    = D [(x, 1/genericLength l) | x <- l]

observe3 :: ToBits a => (s ~> a) -> PCL3 s
observe3 f = Observe3' (fmap toBits . f) skip3

example4 :: PCL3 (Bool,Bool)
example4 = observe3 (\(b1,b2) -> choose (fraction 1 2) b1 b2)

boolPairs :: Dist (Bool,Bool)
boolPairs = uniform [(b1,b2) | b1 <- bools, b2 <- bools]
  where bools = [True,False]

main :: IO ()
main = putStrLn "kuifje"
