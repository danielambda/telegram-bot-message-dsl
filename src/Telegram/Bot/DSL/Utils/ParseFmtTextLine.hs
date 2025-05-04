{-# LANGUAGE UndecidableInstances #-}

module Telegram.Bot.DSL.Utils.ParseFmtTextLine (ParseFmtTextLine) where

import DeFun.Core (type (~>), App)
import DeFun.List (Map)

import GHC.TypeError (TypeError, ErrorMessage(..))
import GHC.TypeLits (Symbol)

import Telegram.Bot.DSL.Components.TextLine (TextEntity(..))
import Telegram.Bot.DSL.Parsing (GetParseResult, Run, InterpolatedParser, InterpolatedPart(..), Words)

type ParseFmtTextLine :: Symbol -> [TextEntity]
type ParseFmtTextLine s = Map ParseFmtTextLine' (GetParseResult (Run InterpolatedParser s))

data ParseFmtTextLine' :: InterpolatedPart ~> TextEntity
type instance App ParseFmtTextLine' (OutBrackets s) = Txt s
type instance App ParseFmtTextLine' (InBrackets s) = CanonicalizeInBrackets (Words s)

type family CanonicalizeInBrackets words where
  CanonicalizeInBrackets '[] = TypeError ('Text "Empty {} are not allowed")
  CanonicalizeInBrackets '[single] = Var single
  CanonicalizeInBrackets '["show", arg] = VarShow arg
  CanonicalizeInBrackets a = TypeError ('Text "Unknown function " :<>: 'ShowType a)
