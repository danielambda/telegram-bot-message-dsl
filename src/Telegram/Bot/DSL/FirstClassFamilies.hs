{-
  This file contains code derived from first-class-families package
  Original source: https://github.com/Lysxia/first-class-families
 -}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Telegram.Bot.DSL.FirstClassFamilies 
  ( Exp, Eval, Map
  , type (++), type (==)
  ) where

import Data.Kind (Type)

type Exp a = a -> Type

type Eval :: Exp a -> a
type family Eval e

data Map :: (a -> Exp b) -> [a] -> Exp [b]
type instance Eval (Map _ '[]) = '[]
type instance Eval (Map f (x :  xs)) = Eval (f x) :  Eval (Map f xs)
-- type instance Eval (Map f (x :| xs)) = Eval (f x) :| Eval (Map f xs)

infixr 5 ++
type (++) :: [k] -> [k] -> [k]
type family as ++ bs where
  '[]      ++ bs = bs
  (a : as) ++ bs = a : (as ++ bs)

type (==) :: k -> k -> Bool
type family x == y where
  x == x = True
  _ == _ = False

