module Telegram.Bot.DSL.Classes.IsUnit (IsUnit(..)) where

import GHC.Generics (Generic, Rep, U1(U1), to, D1, C1, M1 (..))

class IsUnit u where
  unitValue :: u

  default unitValue :: (Generic u, GIsUnit (Rep u)) => u
  unitValue = to gunitValue

instance IsUnit () where
  unitValue = ()

class GIsUnit u where
  gunitValue :: u a

instance GIsUnit U1 where
  gunitValue = U1

instance GIsUnit a => GIsUnit (D1 m0 (C1 m1 a)) where
  gunitValue = M1 (M1 gunitValue)

