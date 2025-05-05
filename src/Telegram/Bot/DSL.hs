module Telegram.Bot.DSL
  ( CallbackBtn, CallbackButtons, UnitCallbackBtn
  , (:|:), (:\), Proper
  , renderMessage
  , module Telegram.Bot.DSL.Classes.HasTaggedContext
  , module Telegram.Bot.DSL.Classes.IsCallbackData
  , module Telegram.Bot.DSL.Classes.IsUnit
  , module Telegram.Bot.DSL.Components.Button
  , module Telegram.Bot.DSL.Components.ButtonLine
  , module Telegram.Bot.DSL.Components.FmtText
  , module Telegram.Bot.DSL.Components.Message
  , module Telegram.Bot.DSL.Components.MessageLine
  , module Telegram.Bot.DSL.Components.TextLine
  , module Telegram.Bot.DSL.Conversions.AsMessage
  , module Telegram.Bot.DSL.Conversions.AsMessageLine
  , module Telegram.Bot.DSL.Conversions.AsTextLine
  , module Telegram.Bot.DSL.Message
  , module Telegram.Bot.DSL.Parsing
  , module Telegram.Bot.DSL.TaggedContext
  , module Telegram.Bot.DSL.Utils.RenameTag
  ) where

import Data.Proxy (Proxy (..))

import Telegram.Bot.DSL.Classes.HasTaggedContext
import Telegram.Bot.DSL.Classes.IsCallbackData
import Telegram.Bot.DSL.Classes.IsUnit
import Telegram.Bot.DSL.Components.Button
import Telegram.Bot.DSL.Components.ButtonLine
import Telegram.Bot.DSL.Components.FmtText
import Telegram.Bot.DSL.Components.Message
import Telegram.Bot.DSL.Components.MessageLine
import Telegram.Bot.DSL.Components.TextLine
import Telegram.Bot.DSL.Conversions.AsMessage
import Telegram.Bot.DSL.Conversions.AsMessageLine
import Telegram.Bot.DSL.Conversions.AsTextLine
import Telegram.Bot.DSL.Parsing
import Telegram.Bot.DSL.Message
import Telegram.Bot.DSL.TaggedContext
import Telegram.Bot.DSL.Utils.RenameTag

type Proper :: k -> ProperMessageKind
type Proper x = Proper' (AsMessage x)

infixl 9 :|:
type (:|:) :: k -> l -> MessageLine
type a :|: b = JoinMessageLines (AsMessageLine a) (AsMessageLine b)

infixl 0 :\
type (:\) :: k -> l -> MessageKind
type a :\ b = JoinMessages (AsMessage a) (AsMessage b)

renderMessage :: forall k {msg :: k} ctx. IsMessage (Proper msg) ctx
              => Proxy msg -> TaggedContext ctx -> Message
renderMessage _ = getMessage (Proxy @(Proper msg))

type CallbackBtn a = CallbackBtn' (AsTextLine a)
type UnitCallbackBtn a = UnitCallbackBtn' (AsTextLine a)
type CallbackButtons a = CallbackButtons' (AsTextLine a)
