{-# LANGUAGE UndecidableInstances #-}

module Telegram.Bot.DSL.Utils.RenameTag (RenameTag) where

import GHC.TypeLits (Symbol, TypeError, ErrorMessage(..))

import Telegram.Bot.DSL.Components.Message (MessageKind(..))

type RenameTag :: Symbol -> Symbol -> MessageKind -> MessageKind
type family RenameTag x y msg where
  RenameTag x x msg = TypeError (Text "Not implemented") -- msg
  -- RenameTag x y (Msg tls bls) = Msg
  --   (MapSym1 (RenameTLTag x y) @@ tls)
  --   (MapSym1 (RenameBLTag x y) @@ bls)

-- type RenameTLTag x y = MapSym1 (RenameTextEntityTag x y)
-- data RenameTextEntityTag :: Symbol -> Symbol -> TextEntity ~> TextEntity
-- type instance App (RenameTextEntityTag x y) (Var a) = If (x == a) (Var y) (Var a)
-- type instance App (RenameTextEntityTag _ _) (Txt a) = Txt a
--
-- type RenameBLTag x y = MapSym1 (RenameButtonTag x y)
-- data RenameButtonTag :: Symbol -> Symbol -> ButtonEntity ~> ButtonEntity
-- type instance App (RenameButtonTag x y) (CallbackBtn' a b c) =
--   CallbackBtn' (RenameTLTag x y @@ a) b c
