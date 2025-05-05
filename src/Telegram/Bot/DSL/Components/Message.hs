{-# LANGUAGE UndecidableInstances #-}

module Telegram.Bot.DSL.Components.Message
  ( MessageKind(..), Msg
  , ProperMessageKind(..), PMsg
  , IsMessage(..)
  , Proper'
  , JoinMessages
  ) where

import DeFun.Core (type (@@), type (~>), App)
import DeFun.List (MapSym1)
import qualified Data.Text as T (unlines)
import Telegram.Bot.API (SomeReplyMarkup(..), InlineKeyboardMarkup (..))

import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Proxy (Proxy (..))
import GHC.TypeError (TypeError, ErrorMessage(..))
import GHC.TypeLits (Symbol)

import Telegram.Bot.DSL.Components.Button (ButtonEntity)
import Telegram.Bot.DSL.Components.ButtonLine (IsButtonLine(..))
import Telegram.Bot.DSL.Components.TextLine (TextEntity, IsTextLine(..))
import Telegram.Bot.DSL.Message (Message (..), textMessage)
import Telegram.Bot.DSL.TaggedContext (type (++), TaggedContext)

data ProperMessageKind = MkPMsg (NonEmpty [TextEntity]) [[ButtonEntity]]
data MessageKind = MkMsg [[TextEntity]] [[ButtonEntity]]

type Msg = MkMsg
type PMsg = MkPMsg

type Proper' :: MessageKind -> ProperMessageKind
type family Proper' msg where
  Proper' (Msg (tl:tls) bls) = PMsg (tl :| tls)
    (MapSym1 ProperBL @@ bls)
  Proper' (Msg '[] _) = TypeError (Text "Cannot have a message without text")

data ProperBL :: [ButtonEntity] ~> [ButtonEntity]
type instance App ProperBL a = a

type JoinMessages :: MessageKind -> MessageKind -> MessageKind
type family JoinMessages m1 m2 where
  JoinMessages (Msg tls1 '[]) (Msg tls2 bls) = Msg (tls1 ++ tls2) bls
  JoinMessages (Msg tls bls1) (Msg '[] bls2) = Msg tls (bls1 ++ bls2)
  JoinMessages (Msg _ (_:_))  (Msg (_:_) _)  = TypeError (Text "Cannot have text below buttons")

type IsMessage :: ProperMessageKind -> [(Symbol, Type)] -> Constraint
class IsMessage a ctx where
  getMessage :: Proxy a -> TaggedContext ctx -> Message

instance IsTextLine tl ctx
      => IsMessage (PMsg (tl :| '[]) '[]) ctx where
  getMessage _ ctx = let
    messageMarkup = SomeInlineKeyboardMarkup $ InlineKeyboardMarkup []
    msg = textMessage $ getTextLine (Proxy @tl) ctx
    in msg{messageReplyMarkup = Just messageMarkup}

instance (IsTextLine tl ctx, IsMessage (PMsg (tl1 :| tls) '[]) ctx)
      => IsMessage (PMsg (tl :| tl1 : tls) '[]) ctx where
  getMessage _ = do
    replyMessage <- getMessage (Proxy @(PMsg (tl1 :| tls) '[]))
    textLine <- getTextLine (Proxy @tl)
    let newText = T.unlines
            [ textLine
            , messageText replyMessage ]
    return $ replyMessage{messageText = newText}

instance (IsButtonLine bl ctx, IsMessage (PMsg tls bls) ctx)
      => IsMessage (PMsg tls (bl : bls)) ctx where
  getMessage _ = do
    replyMessage <- getMessage (Proxy @(PMsg tls bls))
    buttonLine <- getButtonLine (Proxy @bl)
    let initialButtons = concatMap f $ messageReplyMarkup replyMessage
    return $ replyMessage{messageReplyMarkup = Just $ SomeInlineKeyboardMarkup $
      InlineKeyboardMarkup (buttonLine : initialButtons)}
    where
      f (SomeInlineKeyboardMarkup (InlineKeyboardMarkup x)) = x
      f _ = []
