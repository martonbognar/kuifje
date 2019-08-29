module Monty where

import Data.List ((\\))
import Text.PrettyPrint.Boxes

import Distribution
import PrettyPrint
import Semantics
import Syntax

data Door = DoorA | DoorB | DoorC deriving (Eq, Show, Ord)

instance Boxable Door where
  toBox = text . show

hall :: Door -> Kuifje Door
hall chosenDoor =
  observe (\carDoor -> uniform ([DoorA, DoorB, DoorC] \\ [carDoor, chosenDoor]))

doors :: Dist Door
doors = uniform [DoorA, DoorB, DoorC]

monty :: Dist (Dist Door)
monty = hysem (hall DoorA) doors

run :: IO ()
run = do
  putStrLn "> monty"
  print monty
  putStrLn "> condEntropy bayesVuln monty"
  print $ condEntropy bayesVuln monty
