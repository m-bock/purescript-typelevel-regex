module Type.Char
  ( Char'
  , UnsafeMkChar
  , class IsEmptyOrFail
  , class SymIsChar
  , symIsChar
  )
  where

import Prelude

import Prim.Symbol as Sym
import Prim.TypeError (class Fail, Beside, Doc, Text)

foreign import data Char' :: Type

foreign import data UnsafeMkChar :: Symbol -> Char'

type CharError s = Text "Expected a single character, got: \"" <> Text s <> Text "\""

infixr 6 type Beside as <>

--------------------------------------------------------------------------------

class SymIsChar (s :: Symbol) (char :: Char') | s -> char

symIsChar :: forall @sym @char. SymIsChar sym char => Unit
symIsChar = unit

instance symIsCharEmpty ::
  ( Fail (CharError "")
  ) =>
  SymIsChar "" char

else instance symIsCharNonEmpty ::
  ( Sym.Cons head tail sym
  , IsEmptyOrFail (CharError sym) tail
  ) =>
  SymIsChar sym (UnsafeMkChar head)

--------------------------------------------------------------------------------

class IsEmptyOrFail (err :: Doc) (s :: Symbol)

instance isEmptyOrFailEmpty :: IsEmptyOrFail err ""

else instance isEmptyOrFailError ::
  ( Fail err
  ) =>
  IsEmptyOrFail err s
