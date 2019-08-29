{-# LANGUAGE TemplateHaskell #-}

module SideChannel where

import Prelude hiding (exp)
import Control.Lens hiding (Profunctor, dimap)

import DataTypes
import PrettyPrint
import Semantics
import Syntax

($=) :: (a -> b) -> a -> b
($=) = ($)

(.^) s x y = set x y s

data SE = SE { _base :: Integer, _exp :: Integer, _e :: Integer, _d :: Integer, _p :: Integer } deriving (Eq, Ord)
makeLenses ''SE

initSE :: Integer -> Integer -> SE
initSE base exp = SE { _base = base, _exp = exp, _e = 0, _d = 0, _p = 0 }

exonentiation :: [Integer] -> PCL3 SE
exonentiation ds =
   update3 (\s -> return (s.^e  $= (s^.exp))) <--->
   update3 (\s -> return (s.^p  $= 1))  <--->
   while3 (\s -> return (s^.e /= 0))
           (  update3 (\s -> uniform [s.^d $= d' | d' <- ds]) <--->
              cond3 (\s -> return (s^.e `mod` s^.d /= 0))
                (  update3 (\s -> return (s.^p  $= ((s^.p) * ((s^.base) ^ (s^.e `mod` s^.d))))) <--->
                   update3 (\s -> return (s.^e  $= (s^.e - (s^.e `mod` s^.d)))))   -- Then branch
                skip3 <--->                                                        -- Else branch
              update3 (\s -> return (s.^base  $= ((s^.base)^(s^.d)))) <--->
              update3 (\s -> return (s.^e   $= (s^.e `div` s^.d)))
           )

project :: Dist (Dist SE) -> Dist (Dist Integer)
project = fmap (fmap (\s -> s^.exp))

hyper2 = project (hysem  (exonentiation [2])
                         (uniform [ initSE 6 exp | exp <- [0..15]]))

hyper235 = project (hysem  (exonentiation [2,3,5])
                           (uniform [ initSE 6 exp | exp <- [0..15]]))

jail:: Ord a => Dist a -> Rational
jail d =  let m = maximum (map snd (runD (reduction d))) in
          (1 * m - 5 * (1-m)) `max` 0

run :: IO ()
run = do
   putStrLn "> hyper2"
   print hyper2
   putStrLn "> condEntropy bv hyper235"
   print $ condEntropy bv hyper235
   putStrLn "> condEntropy bv hyper2"
   print $ condEntropy bv hyper2
   putStrLn "> hyper235"
   print hyper235
   putStrLn "> condEntropy jail hyper235"
   print $ condEntropy jail hyper235
   putStrLn "> condEntropy jail hyper2"
   print $ condEntropy jail hyper2
