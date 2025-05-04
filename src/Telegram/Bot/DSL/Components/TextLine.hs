{-# LANGUAGE UndecidableInstances #-}

module Telegram.Bot.DSL.Components.TextLine (TextEntity(..) , IsTextLine(..)) where

import qualified Data.Text as T (Text, pack)

import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy (..))
import GHC.Base (Symbol)
import GHC.TypeLits (KnownSymbol, symbolVal)

import Telegram.Bot.DSL.TaggedContext (TaggedContext (..), TaggedContextHasEntry (..))

data TextEntity
  = Txt Symbol
  | Var Symbol
  | VarShow Symbol

type IsTextLine :: [TextEntity] -> [(Symbol, Type)] -> Constraint
class IsTextLine a ctx where
  getTextLine :: Proxy a -> TaggedContext ctx -> T.Text

instance KnownSymbol s
      => IsTextLine (Txt s : '[]) ctx where
  getTextLine _ _ = T.pack $ symbolVal $ Proxy @s

instance TaggedContextHasEntry ctx a T.Text
      => IsTextLine (Var a : '[]) ctx where
  getTextLine _ = getTaggedContextEntry (Proxy @a)

instance (Show show, TaggedContextHasEntry ctx a show)
      => IsTextLine (VarShow a : '[]) ctx where
  getTextLine _ = T.pack . show . getTaggedContextEntry (Proxy @a)

instance (KnownSymbol s, IsTextLine (l : ls) ctx)
      => IsTextLine (Txt s : l : ls) ctx where
  getTextLine _ tlData
    =  T.pack (symbolVal $ Proxy @s)
    <> getTextLine (Proxy @(l:ls)) tlData

instance (IsTextLine (l : ls) ctx, TaggedContextHasEntry ctx a T.Text)
      => IsTextLine (Var a : l : ls) ctx where
  getTextLine _ = do
    var <- getTaggedContextEntry (Proxy @a)
    textLine <- getTextLine (Proxy @(l:ls))
    return $ var <> textLine

