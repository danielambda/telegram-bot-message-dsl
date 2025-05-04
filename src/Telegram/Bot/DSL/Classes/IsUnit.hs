module Telegram.Bot.DSL.Classes.IsUnit (IsUnit(..)) where

import GHC.Generics (Generic, Rep, U1(U1), to)

class IsUnit u where
  unitValue :: u

  default unitValue :: (Generic u, GIsUnit (Rep u)) => u
  unitValue = to gunitValue

class GIsUnit u where
  gunitValue :: u a

instance GIsUnit U1 where
  gunitValue = U1

