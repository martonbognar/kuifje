module Monty where

import Data.List ((\\))
import Text.PrettyPrint.Boxes

import DataTypes
import PrettyPrint
import Semantics
import Syntax

data Door = DoorA | DoorB | DoorC deriving (Eq, Show, Ord)

instance Boxable Door where
  toBox = text . show

hall :: Door -> PCL3 Door
hall chosenDoor =
  observe3' (\carDoor -> uniform ([DoorA,DoorB,DoorC] \\ [carDoor,chosenDoor]))

doors = uniform [DoorA,DoorB,DoorC]
monty = hysem (hall DoorA) doors

run :: IO ()
run = do
  putStrLn "> monty"
  print monty
  putStrLn "> condEntropy bv monty"
  print $ condEntropy bv monty
