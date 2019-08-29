{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}

module Syntax where

import DataTypes

type a ~> b = a -> Dist b

data PCL3 s
  = Skip3
  | Update3 (s ~> s) (PCL3 s)
  | If3 (s ~> Bool) (PCL3 s) (PCL3 s) (PCL3 s)
  | While3 (s ~> Bool) (PCL3 s) (PCL3 s)
  | forall o. (Ord o) => Observe3' (s ~> o) (PCL3 s)

data PCL3F s a
  =  Skip3F
  |  Update3F (s ~> s) a
  |  If3F (s ~> Bool) a a a
  |  While3F (s ~> Bool) a a
  |  forall o. (Ord o) => Observe3F' (s ~> o) a
