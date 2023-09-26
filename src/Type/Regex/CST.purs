module Type.Regex.CST where

import Type.Char (Char')

--------------------------------------------------------------------------------
--- Types
--------------------------------------------------------------------------------

--- Regex

foreign import data Regex :: Type

-- | Empty Regex
foreign import data Nil :: Regex

-- | `.`
foreign import data Wildcard :: Regex

-- | `[a-z123]`
foreign import data RegexCharClass :: CharClass -> Boolean -> Regex

foreign import data Lit :: Char' -> Regex

-- | `\\a`
foreign import data Quote :: Char' -> Regex

-- | `$`
foreign import data EndOfStr :: Regex

-- | `^`
foreign import data StartOfStr :: Regex

-- | `?`
foreign import data Optional :: Regex -> Regex

-- | `+`
foreign import data OneOrMore :: Regex -> Regex

-- | `*`
foreign import data Many :: Regex -> Regex

-- | `(` .. `)`
foreign import data Group :: Regex -> Regex

-- | `ab`
foreign import data Cat :: Regex -> Regex -> Regex

-- | `|`
foreign import data Alt :: Regex -> Regex -> Regex

infixr 6 type Cat as ~

--- CharClass

foreign import data CharClass :: Type

foreign import data CharClassNil :: CharClass

foreign import data CharClassLit :: Char' -> CharClass -> CharClass

foreign import data CharClassRange :: Char' -> Char' -> CharClass -> CharClass
