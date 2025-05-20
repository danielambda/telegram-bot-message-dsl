module Telegram.Bot.DSL
  ( CallbackBtn, CallbackButtons, UnitCallbackBtn
  , (:|:), (:\), Proper
  , renderMessage
  , module Exports
  ) where

import Data.Proxy (Proxy (..))

import Telegram.Bot.DSL.Classes.HasTaggedContext as Exports
import Telegram.Bot.DSL.Classes.IsCallbackData as Exports
import Telegram.Bot.DSL.Classes.IsUnit as Exports
import Telegram.Bot.DSL.Components.Button as Exports
import Telegram.Bot.DSL.Components.ButtonLine as Exports
import Telegram.Bot.DSL.Components.FmtText as Exports
import Telegram.Bot.DSL.Components.Message as Exports
import Telegram.Bot.DSL.Components.MessageLine as Exports
import Telegram.Bot.DSL.Components.TextLine as Exports
import Telegram.Bot.DSL.Conversions.AsMessage as Exports
import Telegram.Bot.DSL.Conversions.AsMessageLine as Exports
import Telegram.Bot.DSL.Conversions.AsTextLine as Exports
import Telegram.Bot.DSL.Parsing as Exports
import Telegram.Bot.DSL.Message as Exports
import Telegram.Bot.DSL.TaggedContext as Exports
import Telegram.Bot.DSL.Utils.RenameTag as Exports

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
