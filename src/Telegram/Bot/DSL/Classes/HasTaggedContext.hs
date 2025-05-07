{-# LANGUAGE UndecidableInstances #-}

module Telegram.Bot.DSL.Classes.HasTaggedContext
  ( HasTaggedContext(..)
  , HasFields(..)
  , RecordFields
  ) where

import Telegram.Bot.DSL.TaggedContext (TaggedContext (..), type (++), Tagged (..))

import GHC.Generics (Generic(..), K1, M1, type (:*:), D, C, S, Meta(..))
import GHC.Records (HasField (..))
import Data.Kind (Type)
import GHC.TypeLits (Symbol)

class HasTaggedContext ctx a | a -> ctx where
  getTaggedContext :: a -> TaggedContext ctx

instance {-# OVERLAPPABLE #-}
         (HasFields fields a, fields ~ RecordFields a)
      => HasTaggedContext fields a where
  getTaggedContext = getFields

class HasFields fields a where
  getFields :: a -> TaggedContext fields

instance {-# OVERLAPPABLE #-}
         HasFields '[] a where
  getFields _ = EmptyTaggedContext

instance {-# OVERLAPS #-}
         (HasField name a ty, HasFields fields a)
      => HasFields ('(name, ty) : fields) a where
  getFields a = Tagged @name (getField @name a) :. getFields a

type RecordFields a = RecordFields' (Rep a)
type RecordFields' :: (Type -> Type) -> [(Symbol, Type)]
type family RecordFields' a where
  RecordFields' (M1 S (MetaSel (Just name) _ _ _) (K1 _ a)) = '[ '(name, a)]
  RecordFields' (M1 D (MetaData dataName _ _ _) x) = RecordFields' x
  RecordFields' (M1 C (MetaCons consName _ _) x)   = RecordFields' x
  RecordFields' (x :*: y) = RecordFields' x ++ RecordFields' y
  RecordFields' _ = '[]
