{-# LANGUAGE UndecidableInstances #-}

module Telegram.Bot.DSL.Conversions.AsMessage (AsMessage) where

import GHC.TypeError (ErrorMessage(..), TypeError)
import GHC.TypeLits (Symbol)

import Telegram.Bot.DSL.Components.TextLine (TextEntity, Txt)
import Telegram.Bot.DSL.Components.Button (ButtonEntity)
import Telegram.Bot.DSL.Components.MessageLine (MTL, MBL)
import Telegram.Bot.DSL.Components.Message (MessageKind, Msg)
import Telegram.Bot.DSL.Components.FmtText (F, ParseFmtTextLine)

type AsMessage :: k -> MessageKind
type family AsMessage a where
  AsMessage (a :: MessageKind)  = a
  AsMessage (F a)               = Msg '[ParseFmtTextLine a] '[]
  AsMessage (a :: Symbol)       = Msg '[ '[Txt a]] '[]
  AsMessage (a :: TextEntity)   = Msg '[ '[a]]     '[]
  AsMessage (a :: ButtonEntity) = Msg '[]          '[ '[a]]
  AsMessage (MTL a)             = Msg '[a]         '[]
  AsMessage (MBL a)             = Msg '[]          '[a]
  AsMessage a = TypeError (Text "Cannot convert " :<>: ShowType a :<>: Text " to Message")

