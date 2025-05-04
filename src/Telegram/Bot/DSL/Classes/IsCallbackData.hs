module Telegram.Bot.DSL.Classes.IsCallbackData (IsCallbackData(..), ReadShow(..)) where

import qualified Data.Text as T (Text, pack, unpack)
import Text.Read (readMaybe)

class IsCallbackData a where
  toCallbackData :: a -> T.Text
  fromCallbackData :: T.Text -> Maybe a

newtype ReadShow a = ReadShow a
  deriving (Read, Show, Eq)

instance (Read a, Show a) => IsCallbackData (ReadShow a) where
  toCallbackData = T.pack . show
  fromCallbackData = readMaybe . T.unpack

instance IsCallbackData T.Text where
  toCallbackData = id
  fromCallbackData = pure

instance IsCallbackData String where
  toCallbackData = T.pack
  fromCallbackData = pure . T.unpack
