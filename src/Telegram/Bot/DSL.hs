{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Telegram.Bot.DSL
  ( CallbackBtn, CallbackButtons, UnitCallbackBtn
  , AsTextLine
  , renderMessage
  , module Telegram.Bot.DSL.Components.Button
  , module Telegram.Bot.DSL.Components.Message
  , module Telegram.Bot.DSL.Components.MessageLine
  , module Telegram.Bot.DSL.Components.TextLine
  , module Telegram.Bot.DSL.Message
  , module Telegram.Bot.DSL.TaggedContext
  ) where

import GHC.Base (Symbol)
import GHC.TypeLits (TypeError, ErrorMessage(..))
import Data.Proxy (Proxy (..))

import Telegram.Bot.DSL.Components.Button
import Telegram.Bot.DSL.Components.Message
import Telegram.Bot.DSL.Components.MessageLine
import Telegram.Bot.DSL.Components.TextLine
import Telegram.Bot.DSL.Message
import Telegram.Bot.DSL.TaggedContext
import Telegram.Bot.DSL.Utils.ParseFmtTextLine (ParseFmtTextLine)

renderMessage :: forall k {msg :: k} ctx. IsMessage (Proper msg) ctx
              => Proxy msg -> TaggedContext ctx -> Message
renderMessage _ = getMessage (Proxy @(Proper msg))

type CallbackBtn a = CallbackBtn' (AsTextLine a)
type UnitCallbackBtn a = UnitCallbackBtn' (AsTextLine a)
type CallbackButtons a = CallbackButtons' (AsTextLine a)

type AsTextLine :: k -> [TextEntity]
type family AsTextLine a where
  AsTextLine (MTL a) = a
  AsTextLine (F a) = ParseFmtTextLine a
  AsTextLine (a :: [TextEntity]) = a
  AsTextLine (a :: TextEntity)   = '[a]
  AsTextLine (a :: Symbol)       = '[Txt a]
  AsTextLine a = TypeError (Text "Cannot convert " :<>: ShowType a :<>: Text " to TextLine")
