module Telegram.Bot.DSL.Utils.FromEitherError (FromEitherError) where

import GHC.TypeError (TypeError, ErrorMessage (..))

type FromEitherError :: Either ErrorMessage k -> k
type family FromEitherError a where
  FromEitherError ('Right a) = a
  FromEitherError ('Left err) = TypeError err
