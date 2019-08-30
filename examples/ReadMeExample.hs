{-# LANGUAGE TemplateHaskell #-}

module ReadMeExample where

import Control.Lens hiding (Profunctor)
import Data.Semigroup

import Language.Kuifje.Distribution
import Language.Kuifje.PrettyPrint ()
import Language.Kuifje.Semantics
import Language.Kuifje.Syntax

-- | Function synonym for nicer syntax.
(.^) :: s -> ASetter s t a b -> b -> t
(.^) s x y = set x y s

-- | State space for the program.
data SE = SE {
  _x :: Integer,
  _y :: Integer
  } deriving (Eq, Ord)
makeLenses ''SE

-- | Initialize the state by giving a value to x and setting y to 0.
initSE :: Integer -> SE
initSE x = SE { _x = x, _y = 0 }

program :: Kuifje SE
program
  = update (\s -> return (s.^y $ 0)) <>
    while (\s -> return (s^.x > 0)) (
      update (\s -> return (s.^y $ (s^.x + s^.y))) <>
      update (\s -> return (s.^x $ (s^.x - 1)))
    )

-- | Extract the meaningful variable from the state space.
project :: Dist (Dist SE) -> Dist (Dist Integer)
project = fmap (fmap (\s -> s^.y))

-- | Generate the hyper-distribution for an input of x : [5..8]
-- with uniform distribution.
hyper :: Dist (Dist Integer)
hyper = project $ hysem program (uniform [initSE x | x <- [5..8]])

run :: IO ()
run = do
  putStrLn "> hyper"
  print hyper
  putStrLn "> condEntropy bayesVuln hyper"
  print $ condEntropy bayesVuln hyper
