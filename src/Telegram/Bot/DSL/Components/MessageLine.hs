{-# LANGUAGE UndecidableInstances #-}

module Telegram.Bot.DSL.Components.MessageLine
  ( MessageLine(..), MTL, MBL
  , JoinMessageLines
  ) where

import Telegram.Bot.DSL.Components.Button (ButtonEntity(..))
import Telegram.Bot.DSL.Components.TextLine (TextEntity(..))
import GHC.TypeError (TypeError, ErrorMessage(..))
import Telegram.Bot.DSL.TaggedContext (type (++))

data MessageLine
  = MkMTL [TextEntity]
  | MkMBL [ButtonEntity]

type MTL = 'MkMTL
type MBL = 'MkMBL

type JoinMessageLines :: MessageLine -> MessageLine -> MessageLine
type family JoinMessageLines a b where
  JoinMessageLines (MTL a) (MTL b) = MTL (a ++ b)
  JoinMessageLines (MBL a) (MBL b) = MBL (a ++ b)
  JoinMessageLines (MTL a) (MBL b) = JoinMessageLinesError a b
  JoinMessageLines (MBL a) (MTL b) = JoinMessageLinesError a b

type JoinMessageLinesError a b = TypeError
  (Text "Cannot have " :<>: ShowType a :<>: Text " in the same line with " :<>: ShowType b)

