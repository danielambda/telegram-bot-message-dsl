{-# LANGUAGE UndecidableInstances #-}

module Telegram.Bot.DSL.Conversions.AsMessageLine (AsMessageLine) where

import GHC.TypeError (ErrorMessage(..), TypeError)
import GHC.TypeLits (Symbol)

import Telegram.Bot.DSL.Components.TextLine (TextEntity, Txt)
import Telegram.Bot.DSL.Components.Button (ButtonEntity)
import Telegram.Bot.DSL.Components.MessageLine (MessageLine, MTL, MBL)
import Telegram.Bot.DSL.Components.FmtText (F, ParseFmtTextLine)

type AsMessageLine :: k -> MessageLine
type family AsMessageLine a where
  AsMessageLine (MTL a) = MTL a
  AsMessageLine (MBL a) = MBL a
  AsMessageLine (F a)         = MTL (ParseFmtTextLine a)
  AsMessageLine (a :: Symbol) = MTL '[Txt a]
  AsMessageLine (a :: TextEntity) = MTL '[a]
  AsMessageLine (a :: ButtonEntity) = MBL '[a]
  AsMessageLine a = TypeError (Text "Cannot convert " :<>: ShowType a :<>: Text " to MessageLine")
