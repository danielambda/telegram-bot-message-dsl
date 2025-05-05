{-# LANGUAGE UndecidableInstances #-}

module Telegram.Bot.DSL.Conversions.AsTextLine (AsTextLine) where

import GHC.TypeError (TypeError, ErrorMessage(..))
import GHC.TypeLits (Symbol)

import Telegram.Bot.DSL.Components.TextLine (TextEntity, Txt)
import Telegram.Bot.DSL.Components.MessageLine (MTL)
import Telegram.Bot.DSL.Components.FmtText (ParseFmtTextLine, F)

type AsTextLine :: k -> [TextEntity]
type family AsTextLine a where
  AsTextLine (MTL a) = a
  AsTextLine (a :: [TextEntity]) = a
  AsTextLine (a :: TextEntity)   = '[a]
  AsTextLine (a :: Symbol)       = '[Txt a]
  AsTextLine (F a)               = ParseFmtTextLine a
  AsTextLine a = TypeError (Text "Cannot convert " :<>: ShowType a :<>: Text " to TextLine")

