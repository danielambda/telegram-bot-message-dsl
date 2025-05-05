module Telegram.Bot.DSL
  ( CallbackBtn, CallbackButtons, UnitCallbackBtn
  , (:|:)
  , renderMessage
  , module Telegram.Bot.DSL.Classes.IsCallbackData
  , module Telegram.Bot.DSL.Classes.IsUnit
  , module Telegram.Bot.DSL.Components.Button
  , module Telegram.Bot.DSL.Components.Message
  , module Telegram.Bot.DSL.Components.MessageLine
  , module Telegram.Bot.DSL.Components.TextLine
  , module Telegram.Bot.DSL.Conversions.AsTextLine
  , module Telegram.Bot.DSL.Conversions.AsMessageLine
  , module Telegram.Bot.DSL.Message
  , module Telegram.Bot.DSL.TaggedContext
  ) where

import Data.Proxy (Proxy (..))

import Telegram.Bot.DSL.Classes.IsCallbackData
import Telegram.Bot.DSL.Classes.IsUnit
import Telegram.Bot.DSL.Components.Button
import Telegram.Bot.DSL.Components.Message
import Telegram.Bot.DSL.Components.MessageLine
import Telegram.Bot.DSL.Components.TextLine
import Telegram.Bot.DSL.Conversions.AsTextLine
import Telegram.Bot.DSL.Conversions.AsMessageLine
import Telegram.Bot.DSL.Message
import Telegram.Bot.DSL.TaggedContext

type a :|: b = JoinMessageLines (AsMessageLine a) (AsMessageLine b)

renderMessage :: forall k {msg :: k} ctx. IsMessage (Proper msg) ctx
              => Proxy msg -> TaggedContext ctx -> Message
renderMessage _ = getMessage (Proxy @(Proper msg))

type CallbackBtn a = CallbackBtn' (AsTextLine a)
type UnitCallbackBtn a = UnitCallbackBtn' (AsTextLine a)
type CallbackButtons a = CallbackButtons' (AsTextLine a)
