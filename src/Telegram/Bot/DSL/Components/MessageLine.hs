{-# LANGUAGE UndecidableInstances #-}

module Telegram.Bot.DSL.Components.MessageLine
  ( MessageLine(..)
  , (:|:)
  , AsMessageLine
  ) where

import Telegram.Bot.DSL.Components.Button (ButtonEntity(..))
import Telegram.Bot.DSL.Components.TextLine (TextEntity(..), FmtKind(..))
import GHC.TypeLits (Symbol)
import GHC.TypeError (TypeError, ErrorMessage(..))
import Telegram.Bot.DSL.TaggedContext (type (++))
import Telegram.Bot.DSL.Utils.ParseFmtTextLine (ParseFmtTextLine)

data MessageLine
  = MTL [TextEntity]
  | MBL [ButtonEntity]

infixr 9 :|:
type (:|:) :: k -> l -> MessageLine
type a :|: b = JoinMessageLines (AsMessageLine a) (AsMessageLine b)

type AsMessageLine :: k -> MessageLine
type family AsMessageLine a where
  AsMessageLine (F a) = MTL (ParseFmtTextLine a)
  AsMessageLine (MTL a) = MTL a
  AsMessageLine (MBL a) = MBL a
  AsMessageLine (a :: Symbol) = MTL '[Txt a]
  AsMessageLine (a :: TextEntity) = MTL '[a]
  AsMessageLine (a :: ButtonEntity) = MBL '[a]
  AsMessageLine a = TypeError (Text "Cannot convert " :<>: ShowType a :<>: Text " to MessageLine")

type JoinMessageLines :: MessageLine -> MessageLine -> MessageLine
type family JoinMessageLines a b where
  JoinMessageLines (MTL a) (MTL b) = MTL (a ++ b)
  JoinMessageLines (MBL a) (MBL b) = MBL (a ++ b)
  JoinMessageLines (MTL a) (MBL b) = JoinMessageLinesError a b
  JoinMessageLines (MBL a) (MTL b) = JoinMessageLinesError a b

type JoinMessageLinesError a b = TypeError
  (Text "Cannot have " :<>: ShowType a :<>: Text " in the same line with " :<>: ShowType b)

