{-# LANGUAGE UndecidableInstances #-}

module Telegram.Bot.DSL.Components.Button
  ( ButtonEntity(..)
  , IsButton(..)
  ) where

import Telegram.Bot.DSL.Components.TextLine (TextEntity, IsTextLine (..))
import Data.Kind (Type, Constraint)
import GHC.TypeLits (Symbol)
import Data.Proxy (Proxy (..))
import Telegram.Bot.DSL.TaggedContext
  (TaggedContext, TaggedContextHasEntry (..), type (++), appendTaggedContext)
import Telegram.Bot.API (InlineKeyboardButton (..), labeledInlineKeyboardButton)
import qualified Data.Text as T (Text)
import Telegram.Bot.DSL.Classes.IsUnit (IsUnit (..))
import Telegram.Bot.DSL.Classes.HasTaggedContext (HasTaggedContext (..))
import Telegram.Bot.DSL.Classes.IsCallbackData (IsCallbackData(..))

data ButtonEntity
  = CallbackBtn' [TextEntity] Type Symbol
  | UnitCallbackBtn' [TextEntity] Type
  | CallbackButtons' [TextEntity] Type Symbol

type IsButton :: ButtonEntity -> [(Symbol, Type)] -> Constraint
class IsButton a ctx where
  getButton :: Proxy a -> TaggedContext ctx -> [InlineKeyboardButton]

instance ( IsCallbackData a
         , TaggedContextHasEntry ctx s a
         , HasTaggedContext ctx0 a
         , IsTextLine l (ctx0 ++ ctx)
         )
      => IsButton (CallbackBtn' l a s) ctx where
  getButton _ = do
    a <- getTaggedContextEntry (Proxy @s)
    let ctx0 = getTaggedContext a
    label <- getTextLine (Proxy @l) . appendTaggedContext ctx0
    let callback = toCallbackData a
    return [callbackButton label callback]

instance ( IsCallbackData t
         , IsUnit t
         , IsTextLine tl ctx
         )
      => IsButton (UnitCallbackBtn' tl t) ctx where
  getButton _ = do
    label <- getTextLine (Proxy @tl)
    let callback = toCallbackData $ unitValue @t
    return [callbackButton label callback]

instance ( IsTextLine tl (ctx0 ++ ctx)
         , HasTaggedContext ctx0 a
         , IsCallbackData a
         , TaggedContextHasEntry ctx s [a]
         )
      => IsButton (CallbackButtons' tl a s) ctx where
  getButton _ = traverse f =<< getTaggedContextEntry (Proxy @s)
    where
      f :: a -> TaggedContext ctx -> InlineKeyboardButton
      f a = do
        let ctx0 = getTaggedContext a
        label <- getTextLine (Proxy @tl) . appendTaggedContext ctx0
        let callback = toCallbackData a
        return $ callbackButton label callback

callbackButton :: T.Text -> T.Text -> InlineKeyboardButton
callbackButton label callback = (labeledInlineKeyboardButton label)
  {inlineKeyboardButtonCallbackData = Just callback}

