{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}

module Syntax where

import Distribution

type a ~> b = a -> Dist b

data Kuifje s
  = Skip
  | Update (s ~> s) (Kuifje s)
  | If (s ~> Bool) (Kuifje s) (Kuifje s) (Kuifje s)
  | While (s ~> Bool) (Kuifje s) (Kuifje s)
  | forall o. (Ord o) => Observe (s ~> o) (Kuifje s)

(<--->) :: Kuifje s -> Kuifje s -> Kuifje s
Skip         <---> k  = k
Update f p   <---> k  = Update f (p <---> k)
While c p q  <---> k  = While c p (q <---> k)
If c p q r   <---> k  = If c p q (r <---> k)
Observe f p  <---> k  = Observe f (p <---> k)  -- added

skip3 :: Kuifje s
skip3 = Skip

update3 :: (s ~> s) -> Kuifje s
update3 f  =  Update f skip3

while3 :: (s ~> Bool) -> Kuifje s -> Kuifje s
while3 c p  =  While c p skip3

cond3 :: (s ~> Bool) -> Kuifje s -> Kuifje s -> Kuifje s
cond3 c p q  =  If c p q skip3

observe3' :: (Ord o) => (s ~> o) -> Kuifje s
observe3' o = Observe o skip3
