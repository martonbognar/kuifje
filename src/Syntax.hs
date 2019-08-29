{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}

module Syntax where

import Data.Semigroup

import Distribution

-- | Kleisli arrow.
type a ~> b = a -> Dist b

-- | Syntax of the Kuifje language.
data Kuifje s
  = Skip
  | Update (s ~> s) (Kuifje s)
  | If (s ~> Bool) (Kuifje s) (Kuifje s) (Kuifje s)
  | While (s ~> Bool) (Kuifje s) (Kuifje s)
  | forall o. (Ord o) => Observe (s ~> o) (Kuifje s)

instance Semigroup (Kuifje s) where
  Skip        <> k = k
  Update f p  <> k = Update f (p <> k)
  While c p q <> k = While c p (q <> k)
  If c p q r  <> k = If c p q (r <> k)
  Observe f p <> k = Observe f (p <> k)

instance Monoid (Kuifje s) where
  mempty = Skip
  mappend = (<>)

-- Return a 'skip' instruction.
skip :: Kuifje s
skip = Skip

-- Return an 'update' instruction.
update :: (s ~> s) -> Kuifje s
update f = Update f skip

-- Return a 'while' instruction.
while :: (s ~> Bool) -> Kuifje s -> Kuifje s
while c p = While c p skip

-- Return an 'if' instruction.
cond :: (s ~> Bool) -> Kuifje s -> Kuifje s -> Kuifje s
cond c p q = If c p q skip

-- Return an 'observe' instruction.
observe :: (Ord o) => (s ~> o) -> Kuifje s
observe o = Observe o skip
