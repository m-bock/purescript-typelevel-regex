module Regex where

import Prelude

import Prim.Boolean (False, True)
import Prim.Symbol as Sym
import Type.Data.Boolean (class And)
import Type.Proxy (Proxy(..))

foreign import data Regex :: Type

---

foreign import data Any :: Regex

-- foreign import data Chars :: Symbol -> Regex

-- foreign import data NotChars :: Symbol -> Regex

foreign import data Lit :: Symbol -> Regex

foreign import data Nil :: Regex

--

foreign import data Cat :: Regex -> Regex -> Regex

infixr 6 type Cat as ~

--

foreign import data Optional :: Regex -> Regex

-- foreign import data Many :: Regex -> Regex

-- foreign import data NTimes :: Int -> Int -> Regex -> Regex

-- foreign import data OneOrMore :: Regex -> Regex

--

-- type Optional = NTimes 0 1

-- type ZeroOrMore = NTimes 0

------------------------------------------------------------------------

-- type RegexURL = "https?://[abcdef]\\.[abc](/[abc]*)*"

-- type RegexURL' = Lit "http"
--   ~ NTimes 0 1 (Lit "s")
--   ~ Lit "://"
--   ~ Chars "abcdef"
--   ~ Lit "."
--   ~ Chars "abc"
--   ~ Many (NTimes 0 1 (Lit "/") ~ NTimes 0 1 (Chars "abc"))

------------------------------------------------------------------------

class
  Reverse (regexIn :: Regex) (r :: Regex) (regexOut :: Regex)
  | regexIn r -> regexOut

instance Reverse Nil a a

instance
  ( Reverse tail (head ~ r) regexOut
  ) =>
  Reverse (head ~ tail) r regexOut

reverse :: forall sym sym'. Reverse sym Nil sym' => Proxy sym -> Proxy sym'
reverse _ = Proxy

-- xxx :: Proxy ?a
-- xxx = reverse (Proxy :: _ (Lit "a" ~ Lit "b" ~ Lit "c" ~ Nil))

-- "abc" "cba"

------------------------------------------------------------------------

class
  CompileRegex' (sym :: Symbol) (regex :: Regex)
  | sym -> regex

instance CompileRegex' "" Nil

else instance
  ( Sym.Cons head tail sym'
  , CompileRegex head tail Nil regex'
  , Reverse regex' Nil regex
  , Sym.Append sym EOF sym'
  ) =>
  CompileRegex' sym regex

------------------------------------------------------------------------

type EOF = "$"

class
  CompileRegex (head :: Symbol) (tail :: Symbol) (regexIn :: Regex) (regex :: Regex)
  | head tail regexIn -> regex

compileRegex :: forall @sym regex. CompileRegex' sym regex => Proxy regex
compileRegex = Proxy

instance CompileRegex EOF "" r r

else instance
  ( CompileRegex head' tail' (Any ~ rin) regex
  , Sym.Cons head' tail' tail
  ) =>
  CompileRegex "." tail rin regex

else instance
  ( CompileRegex head'' tail'' (Lit head' ~ rin) regex
  , Sym.Cons head' tail' tail
  , Sym.Cons head'' tail'' tail'
  ) =>
  CompileRegex "\\" tail rin regex

else instance
  ( CompileRegex head' tail' ((Optional (Lit lit)) ~ rin) regex
  , Sym.Cons head' tail' tail
  ) =>
  CompileRegex "?" tail (Lit lit ~ rin) regex

else instance
  ( Sym.Cons head' tail' tail
  , CompileRegex head' tail' (Lit head ~ rin) regex
  ) =>
  CompileRegex head tail rin regex

------------------------------------------------------------------------

class Match' (regex :: Symbol) (sym :: Symbol)

instance
  ( CompileRegex' regex regex'
  , Init sym head tail
  , Match regex' head tail True EOF
  ) =>
  Match' regex sym

match :: forall @regex @sym. Match' regex sym => String
match = ""

------------------------------------------------------------------------

class
  Init (sym :: Symbol) (head :: Symbol) (tail :: Symbol)
  | sym -> head tail

instance (Sym.Cons head tail sym', Sym.Append sym EOF sym') => Init sym head tail

------------------------------------------------------------------------

class
  Match (regex :: Regex) (head :: Symbol) (tail :: Symbol) (success :: Boolean) (rest :: Symbol)
  | regex head tail -> success rest

match_ :: forall regex head tail success rest. Match regex head tail success rest => Proxy regex -> Proxy head -> Proxy tail -> Proxy success -> Proxy rest -> Unit
match_ _ _ _ _ _ = unit

-- xxxx = match_
--   (Proxy :: _ (Optional (Lit "a") ~ Nil))
--   (Proxy :: _ "$")
--   (Proxy :: _ "")
--   (Proxy :: _ ?a)
--   (Proxy :: _ ?b)

instance
  ( MatchLit head tail lit success rest
  ) =>
  Match (Lit lit) head tail success rest

instance
  ( Match r head tail success' rest'
  , MatchOptRes success' r head tail rest' success rest
  ) =>
  Match (Optional r) head tail success rest

---

class
  MatchOptRes
    (matchSuccess :: Boolean)
    (regex :: Regex)
    (head :: Symbol)
    (tail :: Symbol)
    (rest2 :: Symbol)
    (success :: Boolean)
    (rest :: Symbol)
  | matchSuccess regex head tail rest2 -> success rest

instance
  MatchOptRes True regex head_ tail_ rest True rest

instance
  ( Sym.Cons head tail rest
  ) =>
  MatchOptRes False regex head tail rest2_ True rest

---

instance Match Any head tail True tail

instance
  Match Nil EOF "" True EOF

else instance
  Match Nil head tail False tail

instance
  ( Match r1 head tail success1 rest
  , Sym.Cons head' tail' rest
  , Match r2 head' tail' success2 rest'
  , And success1 success2 success
  ) =>
  Match (Cat r1 r2) head tail success rest'

------------------------------------------------------------------------

class
  MatchLit (head :: Symbol) (tail :: Symbol) (lit :: Symbol) (success :: Boolean) (rest :: Symbol)
  | head tail lit -> success rest

instance MatchLit lit tail lit True tail

else instance MatchLit head tail lit False tail

------------------------------------------------------------------------

-- y :: Proxy ?a
-- y = compileRegex @"aab"

x :: String
x = match @"ab?aa" @"abaa"