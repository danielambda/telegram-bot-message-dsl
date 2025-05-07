{-# LANGUAGE UndecidableInstances #-}

module Telegram.Bot.DSL.Components.TextLine
  ( TextEntity(..), Txt, Var, VarShow
  , VarUnlines, VarUnlinesMapShow
  , VarUnwords, VarUnwordsMapShow
  , IsTextLine(..)
  ) where

import qualified Data.Text as T (Text, pack, unlines, unwords)

import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy (..))
import GHC.Base (Symbol)
import GHC.TypeLits (KnownSymbol, symbolVal)

import Telegram.Bot.DSL.TaggedContext (TaggedContext (..), TaggedContextHasEntry (..))
import Data.Foldable (Foldable(toList))

data TextEntity
  = MkTxt Symbol
  | MkVar Symbol
  | MkVarShow Symbol
  | MkVarUnlines Symbol
  | MkVarUnlinesMapShow Symbol
  | MkVarUnwords Symbol
  | MkVarUnwordsMapShow Symbol

type Txt = 'MkTxt
type Var = 'MkVar
type VarShow = 'MkVarShow
type VarUnlines = 'MkVarUnlines
type VarUnlinesMapShow = 'MkVarUnlinesMapShow
type VarUnwords = 'MkVarUnwords
type VarUnwordsMapShow = 'MkVarUnwordsMapShow

type IsTextLine :: [TextEntity] -> [(Symbol, Type)] -> Constraint
class IsTextLine a ctx where
  getTextLine :: Proxy a -> TaggedContext ctx -> T.Text

instance KnownSymbol s
      => IsTextLine '[Txt s] ctx where
  getTextLine _ _ = T.pack $ symbolVal $ Proxy @s

instance TaggedContextHasEntry ctx a T.Text
      => IsTextLine '[Var a] ctx where
  getTextLine _ = getTaggedContextEntry (Proxy @a)

instance (Show show, TaggedContextHasEntry ctx a show)
      => IsTextLine '[VarShow a] ctx where
  getTextLine _ = T.pack . show . getTaggedContextEntry (Proxy @a)

instance (Foldable f, TaggedContextHasEntry ctx a (f T.Text))
      => IsTextLine '[VarUnlines a] ctx where
  getTextLine _ = T.unlines . toList . getTaggedContextEntry (Proxy @a)

instance (Functor f, Foldable f, Show show, TaggedContextHasEntry ctx a (f show))
      => IsTextLine '[VarUnlinesMapShow a] ctx where
  getTextLine _ = T.unlines . toList . fmap (T.pack . show) . getTaggedContextEntry (Proxy @a)

instance (Foldable f, TaggedContextHasEntry ctx a (f T.Text))
      => IsTextLine '[VarUnwords a] ctx where
  getTextLine _ = T.unwords . toList . getTaggedContextEntry (Proxy @a)

instance (Functor f, Foldable f, Show show, TaggedContextHasEntry ctx a (f show))
      => IsTextLine '[VarUnwordsMapShow a] ctx where
  getTextLine _ = T.unwords . toList . fmap (T.pack . show) . getTaggedContextEntry (Proxy @a)

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

instance (IsTextLine (l : ls) ctx, TaggedContextHasEntry ctx a show, Show show)
      => IsTextLine (VarShow a : l : ls) ctx where
  getTextLine _ = do
    var <- T.pack . show <$> getTaggedContextEntry (Proxy @a)
    textLine <- getTextLine (Proxy @(l:ls))
    return $ var <> textLine

