module Type.Regex.Types where


foreign import data Regex :: Type

-- | Empty Regex
foreign import data Nil :: Regex

-- | `*`
foreign import data Wildcard :: Regex

-- | `[a-z123]`
foreign import data CharClass :: Symbol -> Regex


-- | `[^a-z123]`
foreign import data NegativeCharClass :: Symbol -> Regex


foreign import data Lit :: Symbol -> Regex

-- | `$`
foreign import data EndOfStr :: Regex

-- | `^`
foreign import data StartOfStr :: Regex

-- | `(` .. `)`
foreign import data Group :: Regex -> Regex

foreign import data Cat :: Regex -> Regex -> Regex

-- | `{n}`
foreign import data ManyMin :: Int -> Regex -> Regex

-- | `{n, m}`
foreign import data ManyMinMax :: Int -> Int -> Regex -> Regex


infixr 6 type Cat as ~

