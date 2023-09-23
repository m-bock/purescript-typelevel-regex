module Regex where

import Prelude

import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Boolean (False, True)
import Prim.Int as Int
import Prim.Symbol as Sym
import Type.Proxy (Proxy(..))
import Type.Regex.AsciiTable (class AsciiCode)

foreign import data Regex :: Type

---

foreign import data Any :: Regex

foreign import data CharClass :: Symbol -> Regex

-- foreign import data NotChars :: Symbol -> Regex

foreign import data Lit :: Symbol -> Regex

foreign import data Nil :: Regex

foreign import data End :: Regex

foreign import data Start :: Regex

foreign import data Group :: Regex -> Regex

--

foreign import data Cat :: Regex -> Regex -> Regex

infixr 6 type Cat as ~

--

foreign import data Optional :: Regex -> Regex

foreign import data Many :: Regex -> Regex

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
  , CompileRegex head tail Nil 0 EOF regex'
  , Reverse regex' Nil regex
  , Sym.Append sym EOF sym'
  ) =>
  CompileRegex' sym regex

------------------------------------------------------------------------

type EOF = "☒"

class
  CompileRegex
    (head :: Symbol)
    (tail :: Symbol)
    (regexIn :: Regex)
    (depth :: Int)
    (rest :: Symbol)
    (regex :: Regex)
  | head tail regexIn depth -> rest regex

compileRegex :: forall @sym regex. CompileRegex' sym regex => Proxy regex
compileRegex = Proxy

instance CompileRegex EOF "" r 0 EOF r

else instance CompileRegex ")" tail r depth tail r

else instance
  ( Sym.Cons head' tail' tail
  , Int.Add depth 1 depth'
  , CompileRegex head' tail' Nil depth' rest regex'
  , Reverse regex' Nil regex''
  , Sym.Cons head'' tail'' rest
  , CompileRegex head'' tail'' (Group regex'' ~ rin) depth rest2 regex
  ) =>
  CompileRegex "(" tail rin depth rest2 regex

else instance
  ( CompileRegex head' tail' (Any ~ rin) depth rest regex
  , Sym.Cons head' tail' tail
  ) =>
  CompileRegex "." tail rin depth rest regex

else instance
  ( CompileRegex head' tail' (Start ~ rin) depth rest regex
  , Sym.Cons head' tail' tail
  ) =>
  CompileRegex "^" tail rin depth rest regex

else instance
  CompileRegex "$" EOF rin depth EOF (End ~ rin)

else instance
  ( CompileRegex head'' tail'' (Lit head' ~ rin) depth rest regex
  , Sym.Cons head' tail' tail
  , Sym.Cons head'' tail'' tail'
  ) =>
  CompileRegex "\\" tail rin depth rest regex

else instance
  ( CompileRegex head' tail' (Optional r ~ rin) depth rest regex
  , Sym.Cons head' tail' tail
  ) =>
  CompileRegex "?" tail (r ~ rin) depth rest regex

else instance
  ( CompileRegex head' tail' (Many r ~ r ~ rin) depth rest regex
  , Sym.Cons head' tail' tail
  ) =>
  CompileRegex "+" tail (r ~ rin) depth rest regex


else instance
  ( CompileRegex head' tail' (Many r ~ rin) depth rest regex
  , Sym.Cons head' tail' tail
  ) =>
  CompileRegex "*" tail (r ~ rin) depth rest regex

else instance

  ( Sym.Cons head' tail' tail
  , ParseCharClass head' tail' "" rest sym
  , Sym.Cons head'' tail'' rest
  , CompileRegex head'' tail'' (CharClass sym ~ rin) depth rest' regex
  ) =>
  CompileRegex "[" tail rin depth rest' regex

else instance
  ( Sym.Cons head' tail' tail
  , CompileRegex head' tail' (Lit head ~ rin) depth rest regex
  ) =>
  CompileRegex head tail rin depth rest regex

------------------------------------------------------------------------

class
  ParseCharClass
    (head :: Symbol)
    (tail :: Symbol)
    (symIn :: Symbol)
    (rest :: Symbol)
    (sym :: Symbol)
  | head tail symIn -> rest sym

instance
  ParseCharClass "]" tail symIn tail symIn

else instance
  ( Sym.Cons head' tail' tail
  , Sym.Append head' symIn symIn'
  , Sym.Cons head'' tail'' tail'
  , ParseCharClass head'' tail'' symIn' rest sym
  ) =>
  ParseCharClass "\\" tail symIn rest sym

else instance
  ( Sym.Cons symInHead symInTail symIn
  , AsciiCode start symInHead
  , Sym.Cons head' tail' tail
  , AsciiCode end head'
  , GetCharRange start end "" chars
  , Sym.Append chars symInTail symIn'
  , Sym.Cons head'' tail'' tail'
  , ParseCharClass head'' tail'' symIn' rest sym

  ) =>
  ParseCharClass "-" tail symIn rest sym

else instance
  ( Sym.Cons head' tail' tail
  , ParseCharClass head' tail' symIn' rest sym
  , Sym.Append head symIn symIn'
  ) =>
  ParseCharClass head tail symIn rest sym

------------------------------------------------------------------------

class
  Contains (head :: Symbol) (tail :: Symbol) (sym :: Symbol) (result :: Boolean)
  | head tail sym -> result

instance Contains EOF "" sym False

else instance Contains sym tail sym True

else instance
  ( Sym.Cons head' tail' tail
  , Contains head' tail' sym result
  ) =>
  Contains head tail sym result

------------------------------------------------------------------------

class Match' (regex :: Symbol) (sym :: Symbol)

instance
  ( CompileRegex' regex regex'
  , Init sym head tail
  , Scan regex' head tail True
  ) =>
  Match' regex sym

match :: forall @regex @sym. Match' regex sym => IsSymbol sym => String
match = reflectSymbol (Proxy :: _ sym)

------------------------------------------------------------------------

class NotMatch' (regex :: Symbol) (sym :: Symbol)

instance
  ( CompileRegex' regex regex'
  , Init sym head tail
  , Scan regex' head tail False
  ) =>
  NotMatch' regex sym

notMatch :: forall @regex @sym. NotMatch' regex sym => IsSymbol sym => String
notMatch = reflectSymbol (Proxy :: _ sym)

------------------------------------------------------------------------

class
  Scan (regex :: Regex) (head :: Symbol) (tail :: Symbol) (result :: Boolean)
  | regex head tail -> result

instance
  ( Match r head tail success rest
  ) =>
  Scan (Start ~ r) head tail success

else instance
  ( Match regex head tail success rest
  , ScanResult success regex head tail result
  ) =>
  Scan regex head tail result

class
  ScanResult
    (matchResult :: Boolean)
    (regex :: Regex)
    (head :: Symbol)
    (tail :: Symbol)
    (result :: Boolean)
  | matchResult regex head tail -> result

instance ScanResult True regex head tail True

instance
  ( Sym.Cons head' tail' tail
  , Scan regex head' tail' success
  ) =>
  ScanResult False regex head tail success

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

else instance
  ( Sym.Cons EOF tail rest
  ) =>
  Match End EOF tail True rest

-- else instance
--   Match r EOF tail False tail

else instance
  Match End head tail False tail

else instance
  ( Sym.Cons head tail rest
  ) =>
  Match Nil head tail True rest

else instance
  ( Match r head tail success' rest'
  , MatchOptRes success' r head tail rest' success rest
  ) =>
  Match (Optional r) head tail success rest

else instance
  ( Match r head tail success' rest'
  , MatchManyRes success' r head tail rest' success rest
  ) =>
  Match (Many r) head tail success rest

else instance
  ( Match r head tail success rest
  ) =>
  Match (Group r) head tail success rest

else instance Match Any head tail True tail

else instance
  ( Init chars charsHead charsTail
  , Contains charsHead charsTail head success
  ) =>
  Match (CharClass chars) head tail success tail

else instance
  ( Match r1 head tail success1 rest
  , MatchCatResult success1 r2 rest success rest'
  ) =>
  Match (Cat r1 r2) head tail success rest'

class
  MatchCatResult (res :: Boolean) (regex :: Regex) (restIn :: Symbol) (success :: Boolean) (rest :: Symbol)
  | res regex restIn -> success rest

instance
  ( Sym.Cons head tail r
  , Match regex head tail success rest
  ) =>
  MatchCatResult True regex r success rest

instance MatchCatResult False regex r False r

------------------------------------------------------------------------

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

------------------------------------------------------------------------

class
  MatchManyRes
    (matchSuccess :: Boolean)
    (regex :: Regex)
    (head :: Symbol)
    (tail :: Symbol)
    (rest2 :: Symbol)
    (success :: Boolean)
    (rest :: Symbol)
  | matchSuccess regex head tail rest2 -> success rest

instance
  ( Sym.Cons head' tail' rest
  , Match (Many regex) head' tail' success' rest'
  ) =>
  MatchManyRes True regex head_ tail_ rest success' rest'

instance
  ( Sym.Cons head tail rest
  ) =>
  MatchManyRes False regex head tail rest2_ True rest

------------------------------------------------------------------------

class
  MatchLit (head :: Symbol) (tail :: Symbol) (lit :: Symbol) (success :: Boolean) (rest :: Symbol)
  | head tail lit -> success rest

instance MatchLit lit tail lit True tail

else instance MatchLit head tail lit False tail

------------------------------------------------------------------------

class
  GetCharRange (start :: Int) (end :: Int) (charsIn :: Symbol) (chars :: Symbol)
  | start end charsIn -> chars

instance
  ( AsciiCode start char
  , Sym.Append char charsIn chars
  ) =>
  GetCharRange start start charsIn chars

else instance
  ( AsciiCode start char
  , GetCharRange start' end charsIn' chars
  , Sym.Append char charsIn charsIn'
  , Int.Add start 1 start'
  ) =>
  GetCharRange start end charsIn chars

------------------------------------------------------------------------

-- y :: Proxy ?a
-- y = compileRegex @"(ab)*"

x3 :: String
x3 = match
  @"^https?://([a-z]+\\.)?[a-z]+\\.[a-z]+(/[a-z_]+)*$"
  @"http://www.hello.com/a/bb_bb"

x :: String
x = match
  @"^(/abc)*$"
  @"/abc/abc/abc/abc/abc"

x2 :: String
x2 = notMatch
  @"^(ab)$"
  @"b"
