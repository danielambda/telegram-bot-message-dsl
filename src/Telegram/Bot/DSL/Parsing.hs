{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Telegram.Bot.DSL.Parsing
  ( MainParser
  , RunInput
  , Run
  , Trim
  ) where

import DeFun.Core (type (@@), App, type (~>), Con1)
import GHC.TypeLits
  (Symbol, Nat, UnconsSymbol, AppendSymbol, ConsSymbol, type (+))
import Data.Type.Bool (If, type (&&))
import Data.Type.Equality (type (==))
import DeFun.Bool (Not)
import DeFun.Function (ConstSym, ConstSym1, IdSym)

type a :<>: b = AppendSymbol a b

data InterpolatedPart
  = OutBrackets Symbol
  | InBrackets Symbol

type MainParser = 'Parser MainParser'
data MainParser' :: Input ~> Either ParseError (Input, [InterpolatedPart])
type instance App MainParser' inp = FailIfInputRemaining
  (RunInput (Many FSymbolEntryRawP) inp)

type family FailIfInputRemaining p where
  FailIfInputRemaining ('Right '( 'Input i "", parsed)) =
    'Right '( 'Input i "", parsed)
  FailIfInputRemaining ('Right '( 'Input i ss, parsed)) =
    'Left ('ParseError i ("Could not parse \"" :<>: ss :<>: "\""))
  FailIfInputRemaining x = x

type Many :: Parser a -> Parser [a]
type Many v = 'Parser (Many' v)
data Many' :: Parser a -> Input ~> Either ParseError (Input, [a])
type instance App (Many' p) inp =
  RunInput (ConsSym :<$>: p :<*>: Many p :<|>: Pure '[]) inp

type ManyCh :: Parser Char -> Parser Symbol
type ManyCh v = 'Parser (ManyCh' v)
data ManyCh' :: Parser Char -> Input ~> Either ParseError (Input, Symbol)
type instance App (ManyCh' p) inp =
  RunInput (ConsSymbolSym :<$>: p :<*>: ManyCh p :<|>: Pure "") inp

type SomeCh :: Parser Char -> Parser Symbol
type SomeCh v = 'Parser (SomeCh' v)
data SomeCh' :: Parser Char -> Input ~> Either ParseError (Input, Symbol)
type instance App (SomeCh' p) inp =
  RunInput (ConsSymbolSym :<$>: p :<*>: (SomeCh p :<|>: Pure "")) inp

data ConsSym :: a ~> [a] ~> [a]
type instance App ConsSym a = ConsSym1 a
data ConsSym1 :: a -> [a] ~> [a]
type instance App (ConsSym1 a) as = a ': as

type LiteralP :: Parser Symbol
type LiteralP
  =     SomeCh (ParseIf (NotEqSym '{' `And` NotEqSym '}'))
  :<|>: "{" :<$: CharP '{' :<*: CharP '{'
  :<|>: "}" :<$: CharP '}' :<*: CharP '}'

type FSymbolEntryRawP
  =     Con1 InBrackets :<$>: (CharP '{' :*>: InBracketsP :<*: CharP '}')
  :<|>: Con1 OutBrackets :<$>: LiteralP

type InBracketsP = SpanP (NotEqSym '}' `And` NotEqSym '{')

data AppendSymbolSym :: Symbol ~> Symbol ~> Symbol
type instance App AppendSymbolSym sym = AppendSymbolSym1 sym

data AppendSymbolSym1 :: Symbol -> Symbol ~> Symbol
type instance App (AppendSymbolSym1 a) b = AppendSymbol a b

data PrependSymbolSym1 :: Symbol -> Symbol ~> Symbol
type instance App (PrependSymbolSym1 a) b = AppendSymbol b a

data Input = Input Nat Symbol
data ParseError = ParseError Nat Symbol
newtype Parser a = Parser (Input ~> Either ParseError (Input, a))

type Run :: Parser a -> Symbol -> Either ParseError (Input, a)
type Run p s = RunInput p ('Input 0 s)

type family RunInput p i where
  RunInput ('Parser p) i = p @@ i

type InputUncons :: Input -> Maybe (Char, Input)
type family InputUncons i where
  InputUncons ('Input i s) = InputUnconsHelp i (UnconsSymbol s)

type family InputUnconsHelp i a where
  InputUnconsHelp _ 'Nothing = 'Nothing
  InputUnconsHelp i ('Just '(ch, sym)) = 'Just '(ch, 'Input (i + 1) sym)

type family InputLoc i where
  InputLoc ('Input loc _) = loc

infixl 4 :<$>:
type f :<$>: a = 'Parser (Fmap' f a)
type Fmap' :: (a ~> b) -> Parser a -> Input ~> Either ParseError (Input, b)
data Fmap' f p i
type instance App (Fmap' f ('Parser p)) i = MapParseResult f (p @@ i)

type MapParseResult :: (a ~> b) -> Either ParseError (Input, a) -> Either ParseError (Input, b)
type family MapParseResult f a where
  MapParseResult _ ('Left a) = 'Left a
  MapParseResult f ('Right '(i, a)) = 'Right '(i, f @@ a)

type BindEither :: (a ~> Either l b) -> Either l a -> Either l b
type family BindEither f a where
  BindEither _ ('Left a) = 'Left a
  BindEither f ('Right a) = f @@ a

type AltParseResult :: Either l r -> Either l r -> Either l r
type family AltParseResult a b where
  AltParseResult ('Left _) b = b
  AltParseResult a _ = a

infixl 4 :<*>:
type (:<*>:) :: Parser (a ~> b) -> Parser a -> Parser b
type f :<*>: a = 'Parser (Ap' f a)

data Ap' :: Parser (a ~> b) -> Parser a -> Input ~> Either ParseError (Input, b)
type instance App (Ap' ('Parser pf) pa) i =
  BindEither (Ap'' pa) (pf @@ i)

data Ap'' :: Parser a -> (Input, a ~> b) ~> Either ParseError (Input, b)
type instance App (Ap'' ('Parser pa)) '(i, f) =
  MapParseResult f (pa @@ i)

infixl 4 :*>:
type (:*>:) :: Parser a -> Parser b -> Parser b
type a1 :*>: a2 = ConstSym1 IdSym :<$>: a1 :<*>: a2

infixl 4 :<$:
type (:<$:) :: a -> Parser b -> Parser a
type f :<$: a = ConstSym1 f :<$>: a

infixl 4 :<*:
type (:<*:) :: Parser a -> Parser b -> Parser a
type a1 :<*: a2 = ConstSym :<$>: a1 :<*>: a2

type Pure :: a -> Parser a
type Pure a = 'Parser (Pure' a)

data Pure' :: a -> Input ~> Either ParseError (Input, a)
type instance App (Pure' a) i = 'Right '(i, a)

infixl 3 :<|>:
type a :<|>: b = 'Parser (Alt' a b)

data Alt' :: Parser a -> Parser a -> Input ~> Either ParseError (Input, a)
type instance App (Alt' ('Parser pa) ('Parser pb)) i = AltParseResult (pa @@ i) (pb @@ i)

type CharP :: Char -> Parser Char
type CharP ch = 'Parser (CharP' ch)

data CharP' :: Char -> Input ~> Either ParseError (Input, Char)
type instance App (CharP' ch) i = CharP'' ch (InputLoc i) (InputUncons i)

type CharP'' :: Char -> Nat -> Maybe (Char, Input) -> Either ParseError (Input, Char)
type family CharP'' ch n i where
  CharP'' ch n 'Nothing =
    'Left ('ParseError n ("Expected '" :<>: ConsSymbol ch "', but reached end of string"))
  CharP'' ch n ('Just '(x, inp)) = If (x == ch)
    ('Right '(inp, ch))
    ('Left ('ParseError n ("Expected '" :<>: ConsSymbol ch "', but found '" :<>: ConsSymbol x "'")))

data CharPSym :: Char ~> Parser Char
type instance App CharPSym ch = CharP ch

type Foldr :: (Char ~> b ~> b) -> b -> Symbol -> b
type Foldr k z sym = Foldr' k z (UnconsSymbol sym)

type Foldr' :: (Char ~> b ~> b) -> b -> Maybe (Char, Symbol) -> b
type family Foldr' k z msym where
  Foldr' _ z 'Nothing = z
  Foldr' k z ('Just '(y, ys)) = k @@ y @@ Foldr' k z (UnconsSymbol ys)

data TraverseConsF :: (Char ~> Parser Char) -> Char ~> b ~> b
type instance App (TraverseConsF f) ch = TraverseConsFSym1 f ch

data TraverseConsFSym1 :: (Char ~> Parser Char) -> Char -> Parser Symbol ~> Parser Symbol
type instance App (TraverseConsFSym1 f ch) psym =
  ConsSymbolSym :<$>: (f @@ ch) :<*>: psym

data ConsSymbolSym :: Char ~> Symbol ~> Symbol
type instance App ConsSymbolSym ch = ConsSymbolSym1 ch

data ConsSymbolSym1 :: Char -> Symbol ~> Symbol
type instance App (ConsSymbolSym1 ch) sym = ConsSymbol ch sym

type SpanP :: (Char ~> Bool) -> Parser Symbol
type SpanP p = ManyCh (ParseIf p)

data And :: (Char ~> Bool) -> (Char ~> Bool) -> Char ~> Bool
type instance App (And a b) ch = a @@ ch && b @@ ch

type ParseIf :: (Char ~> Bool) -> Parser Char
type ParseIf p = 'Parser (ParseIf' p)

data ParseIf' :: (Char ~> Bool) -> Input ~> Either ParseError (Input, Char)
type instance App (ParseIf' p) inp =
  ParseIf'' (InputLoc inp) p (InputUncons inp)

type ParseIf'' :: Nat -> (Char ~> Bool) -> Maybe (Char, Input) -> Either ParseError (Input, a)
type family ParseIf'' n p msym where
  ParseIf'' n _ 'Nothing = 'Left ('ParseError n "Reached end of string")
  ParseIf'' n p ('Just '(y, ys)) = If (p @@ y)
    ('Right '(ys, y))
    ('Left ('ParseError n (ConsSymbol '\'' (ConsSymbol y "does not match given predicate"))))

data IsSpace :: Char ~> Bool
type instance App IsSpace ch = ch == ' '

data NotEqSym :: a -> a ~> Bool
type instance App (NotEqSym a) b = Not (a == b)

data FailIfEmptyP' :: Parser a -> Input ~> Either ParseError (Input, a)
type instance App (FailIfEmptyP' ('Parser p)) ('Input n sym) = If (sym == "")
  ('Left ('ParseError n "Expected not empty"))
  (p @@ 'Input n sym)

type DropWhile :: (Char ~> Bool) -> Symbol -> Symbol
type DropWhile p sym = DropWhile' p (UnconsSymbol sym)

type DropWhile' :: (Char ~> Bool) -> Maybe (Char, Symbol) -> Symbol
type family DropWhile' p msym where
  DropWhile' _ 'Nothing = ""
  DropWhile' p ('Just '(x, xs)) =
    If (p @@ x) (DropWhile p xs) (ConsSymbol x xs)

type DropWhileEnd :: (Char ~> Bool) -> Symbol -> Symbol
type DropWhileEnd p sym = Foldr (DropWhileEnd' p) "" sym

data DropWhileEnd' :: (Char ~> Bool) -> Char ~> Symbol ~> Symbol
type instance App (DropWhileEnd' p) ch = DropWhileEnd'' p ch

data DropWhileEnd'' :: (Char ~> Bool) -> Char -> Symbol ~> Symbol
type instance App (DropWhileEnd'' p ch) sym =
  If (p @@ ch && sym == "") "" (ConsSymbol ch sym)

type TrimStart sym = DropWhile IsSpace sym
type TrimEnd sym = DropWhileEnd IsSpace sym
type Trim sym = TrimEnd (TrimStart sym)

type Words :: Symbol -> [Symbol]
type Words s = Words' (DropWhile IsSpace s)
type family Words' s' where
  Words' "" = '[]
  Words' s' = Words'' (Break IsSpace s')
type family Words'' s'' where Words'' '(w, s'') = w : Words s''

type Break :: (Char ~> Bool) -> Symbol -> (Symbol, Symbol)
type Break p s = Break' p (UnconsSymbol s)
type family Break' p msym where
  Break' _ 'Nothing = '("", "")
  Break' p ('Just '(x, xs)) = If (p @@ x) '("", ConsSymbol x xs) (Break'' x (Break p xs))
type family Break'' x yszs where Break'' x '(ys, zs) = '(ConsSymbol x ys, zs)
