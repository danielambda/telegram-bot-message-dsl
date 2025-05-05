{-# LANGUAGE UndecidableInstances #-}

module Telegram.Bot.DSL.Components.Button
  ( ButtonEntity(..), CallbackBtn', UnitCallbackBtn', CallbackButtons', Btn, Buttons
  , IsButton(..)
  , callbackButton
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

callbackButton :: IsCallbackData a => T.Text -> a -> InlineKeyboardButton
callbackButton label callback = (labeledInlineKeyboardButton label)
  {inlineKeyboardButtonCallbackData = Just (toCallbackData callback)}

data ButtonEntity
  = MkCallbackBtn' [TextEntity] Type Symbol
  | MkUnitCallbackBtn' [TextEntity] Type
  | MkCallbackButtons' [TextEntity] Type Symbol
  | MkBtn Symbol
  | MkButtons Symbol

type CallbackBtn' = MkCallbackBtn'
type UnitCallbackBtn' = MkUnitCallbackBtn'
type CallbackButtons' = MkCallbackButtons'
type Btn = MkBtn
type Buttons = MkButtons

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
    return [callbackButton label a]

instance ( IsCallbackData t
         , IsUnit t
         , IsTextLine tl ctx
         )
      => IsButton (UnitCallbackBtn' tl t) ctx where
  getButton _ = do
    label <- getTextLine (Proxy @tl)
    let callback = unitValue @t
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
        return $ callbackButton label a

instance TaggedContextHasEntry ctx s InlineKeyboardButton
      => IsButton (Btn s) ctx where
  getButton _ = (:[]) <$> getTaggedContextEntry (Proxy @s)

instance TaggedContextHasEntry ctx s [InlineKeyboardButton]
      => IsButton (Buttons s) ctx where
  getButton _ = getTaggedContextEntry (Proxy @s)

