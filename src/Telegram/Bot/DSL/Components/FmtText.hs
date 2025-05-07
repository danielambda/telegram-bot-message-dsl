{-# LANGUAGE UndecidableInstances #-}

module Telegram.Bot.DSL.Components.FmtText (ParseFmtTextLine, FmtTextLine(..), F) where

import DeFun.Core (type (~>), App)
import DeFun.List (Map)

import GHC.TypeError (TypeError, ErrorMessage(..))
import GHC.TypeLits (Symbol)

import Telegram.Bot.DSL.Components.TextLine
  ( TextEntity, Txt, Var, VarShow
  , VarUnlines, VarUnwordsMapShow
  , VarUnwords, VarUnlinesMapShow
  )
import Telegram.Bot.DSL.Parsing (GetParseResult, Run, InterpolatedParser, InterpolatedPart(..), Words)

newtype FmtTextLine = MkF Symbol
type F = MkF

type ParseFmtTextLine :: Symbol -> [TextEntity]
type ParseFmtTextLine s = Map ParseFmtTextLine' (GetParseResult (Run InterpolatedParser s))

data ParseFmtTextLine' :: InterpolatedPart ~> TextEntity
type instance App ParseFmtTextLine' (OutBrackets s) = Txt s
type instance App ParseFmtTextLine' (InBrackets s) = ParseInBrackets (Words s)

type ParseInBrackets :: [Symbol] -> TextEntity
type family ParseInBrackets words where
  ParseInBrackets '[] = TypeError ('Text "Empty {} are not allowed")
  ParseInBrackets '[single] = Var single
  ParseInBrackets '["show", arg] = VarShow arg
  ParseInBrackets '["unlines", arg] = VarUnlines arg
  ParseInBrackets '["unlinesMapShow", arg] = VarUnlinesMapShow arg
  ParseInBrackets '["unwords", arg] = VarUnwords arg
  ParseInBrackets '["unwordsMapShow", arg] = VarUnwordsMapShow arg
  ParseInBrackets a = TypeError ('Text "Unknown function " :<>: 'ShowType a)
