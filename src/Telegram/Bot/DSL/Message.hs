module Telegram.Bot.DSL.Message where

import Data.Text (Text)
import Telegram.Bot.API (SomeReplyMarkup, LinkPreviewOptions, ParseMode)

data Message = Message
  { messageText :: Text
  , messageParseMode :: Maybe ParseMode
  , messageLinkPreviewOptions :: Maybe LinkPreviewOptions
  , messageReplyMarkup :: Maybe SomeReplyMarkup
  }

textMessage :: Text -> Message
textMessage text = Message text Nothing Nothing Nothing

