{-# LANGUAGE UndecidableInstances #-}

module Telegram.Bot.DSL.Components.Message
  ( (:\)
  , MessageKind(..)
  , ProperMessageKind(..)
  , IsMessage(..)
  , Proper
  ) where

import DeFun.Core (type (@@), type (~>), App)
import DeFun.List (Map, MapSym1)
import qualified Data.Text as T (unlines)
import Telegram.Bot.API (SomeReplyMarkup(..), InlineKeyboardMarkup (..))

import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Proxy (Proxy (..))
import GHC.TypeError (TypeError, ErrorMessage(..))
import GHC.TypeLits (Symbol)

import Telegram.Bot.DSL.Components.Button (ButtonEntity(..))
import Telegram.Bot.DSL.Components.ButtonLine (IsButtonLine (..))
import Telegram.Bot.DSL.Components.MessageLine (MessageLine(..))
import Telegram.Bot.DSL.Components.TextLine (TextEntity(..), IsTextLine (..))
import Telegram.Bot.DSL.Message (Message (..), textMessage)
import Telegram.Bot.DSL.TaggedContext (type (++), TaggedContext)

data ProperTL :: [TextEntity] ~> [TextEntity]
type instance App ProperTL '[] = TypeError (Text "Cannot have empty text line")
type instance App ProperTL (tl:tls) = tl:tls

data ProperBL :: [ButtonEntity] ~> [ButtonEntity]
type instance App ProperBL a = a

data ProperMessageKind = PMsg (NonEmpty [TextEntity]) [[ButtonEntity]]
data MessageKind = Msg [[TextEntity]] [[ButtonEntity]]

type Proper :: k -> ProperMessageKind
type Proper x = Proper' (AsMessage x)

type Proper' :: MessageKind -> ProperMessageKind
type family Proper' msg where
  Proper' (Msg (tl:tls) bls) = PMsg
    (ProperTL @@ tl :| Map ProperTL tls)
    (MapSym1 ProperBL @@ bls)
  Proper' (Msg '[] _) = TypeError (Text "Cannot have a message without text")

infixl 0 :\
type (:\) :: k -> l -> MessageKind
type a :\ b = JoinMessages (AsMessage a) (AsMessage b)

type AsMessage :: k -> MessageKind
type family AsMessage a where
  AsMessage (a :: MessageKind)  = a
  AsMessage (a :: Symbol)       = Msg '[ '[Txt a]] '[]
  AsMessage (a :: TextEntity)   = Msg '[ '[a]]     '[]
  AsMessage (a :: ButtonEntity) = Msg '[]          '[ '[a]]
  AsMessage (MTL a)             = Msg '[a]         '[]
  AsMessage (MBL a)             = Msg '[]          '[a]
  AsMessage a = TypeError (Text "Cannot convert " :<>: ShowType a :<>: Text " to Message")

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
