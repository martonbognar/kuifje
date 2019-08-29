{-# LANGUAGE TemplateHaskell #-}

module SideChannel where

import Prelude hiding (exp)
import Control.Lens hiding (Profunctor)
import Data.Semigroup

import Distribution
import PrettyPrint ()
import Semantics
import Syntax

($=) :: (a -> b) -> a -> b
($=) = ($)

(.^) :: s -> ASetter s t a b -> b -> t
(.^) s x y = set x y s

data SE = SE {
  _base :: Integer,
  _exp :: Integer,
  _e :: Integer,
  _d :: Integer,
  _p :: Integer
  } deriving (Eq, Ord)
makeLenses ''SE

initSE :: Integer -> Integer -> SE
initSE base exp = SE { _base = base, _exp = exp, _e = 0, _d = 0, _p = 0 }

exponentiation :: [Integer] -> Kuifje SE
exponentiation ds =
  update (\s -> return (s.^e $= (s^.exp)))
  <> update (\s -> return (s.^p $= 1))
  <> while (\s -> return (s^.e /= 0)) (
    update (\s -> uniform [s.^d $= d' | d' <- ds])
    <> cond (\s -> return (s^.e `mod` s^.d /= 0)) (
      update (\s -> return (s.^p $= ((s^.p) * ((s^.base) ^ (s^.e `mod` s^.d)))))
      <> update (\s -> return (s.^e  $= (s^.e - (s^.e `mod` s^.d))))
    ) skip -- empty else branch
    <> update (\s -> return (s.^base $= ((s^.base)^(s^.d))))
    <> update (\s -> return (s.^e $= (s^.e `div` s^.d)))
  )

project :: Dist (Dist SE) -> Dist (Dist Integer)
project = fmap (fmap (\s -> s^.exp))

hyper2 :: Dist (Dist Integer)
hyper2 = project (hysem (exponentiation [2])
                        (uniform [initSE 6 exp | exp <- [0..15]]))

hyper235 :: Dist (Dist Integer)
hyper235 = project (hysem (exponentiation [2, 3, 5])
                          (uniform [initSE 6 exp | exp <- [0..15]]))

jail :: Ord a => Dist a -> Rational
jail d = let m = maximum (map snd (runD (reduction d)))
         in (1 * m - 5 * (1 - m)) `max` 0

run :: IO ()
run = do
  putStrLn "> hyper2"
  print hyper2
  putStrLn "> condEntropy bayesVuln hyper235"
  print $ condEntropy bayesVuln hyper235
  putStrLn "> condEntropy bayesVuln hyper2"
  print $ condEntropy bayesVuln hyper2
  putStrLn "> hyper235"
  print hyper235
  putStrLn "> condEntropy jail hyper235"
  print $ condEntropy jail hyper235
  putStrLn "> condEntropy jail hyper2"
  print $ condEntropy jail hyper2
