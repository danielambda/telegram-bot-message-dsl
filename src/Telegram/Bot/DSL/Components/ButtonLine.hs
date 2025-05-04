module Telegram.Bot.DSL.Components.ButtonLine
  ( IsButtonLine(..)
  ) where

import Telegram.Bot.DSL.Components.Button (ButtonEntity, IsButton (..))
import GHC.TypeLits (Symbol)
import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy (..))
import Telegram.Bot.DSL.TaggedContext (TaggedContext)
import Telegram.Bot.API (InlineKeyboardButton)

type IsButtonLine :: [ButtonEntity] -> [(Symbol, Type)] -> Constraint
class IsButtonLine a ctx where
  getButtonLine :: Proxy a -> TaggedContext ctx -> [InlineKeyboardButton]

instance IsButtonLine '[] ctx where
  getButtonLine _ _ = []

instance (IsButton b ctx, IsButtonLine bl ctx)
      => IsButtonLine (b : bl) ctx where
  getButtonLine _ = do
    btn <- getButton (Proxy @b)
    bl <- getButtonLine (Proxy @bl)
    return $ btn ++ bl
