module Type.Regex.RegexRep where

import Type.Char (Char')

foreign import data Regex :: Type

---

foreign import data Nil :: Regex

foreign import data EndOfStr :: Regex

foreign import data StartOfStr :: Regex

foreign import data Wildcard :: Regex

foreign import data Lit :: Char' -> Regex

foreign import data NotLits :: Symbol -> Regex

foreign import data Lits :: Symbol -> Regex

foreign import data Many :: Regex -> Regex

foreign import data Cat :: Regex -> Regex -> Regex

foreign import data Alt :: Regex -> Regex -> Regex

---

infixr 6 type Cat as ~
