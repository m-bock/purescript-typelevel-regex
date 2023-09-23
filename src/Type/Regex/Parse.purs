module Type.Regex.Parse where

import Prelude

import Prim.Boolean (False, True)
import Prim.Int as Int
import Prim.Symbol as Sym
import Prim.TypeError (class Fail, Doc, Text)
import Type.Char (class SymIsChar)
import Type.Proxy (Proxy(..))
import Type.Regex.Ast (type (~))
import Type.Regex.Ast as Ast


--------------------------------------------------------------------------------
--- Errors
--------------------------------------------------------------------------------

type ErrorMissingClose = Text "Parenthesis mismatch: Missing ')'"
type ErrorMissingOpen = Text "Parenthesis mismatch: Missing '('"
type ErrorIllegalQuantification = Text "Nothing to repeat"
type ErrorInvalidRange = Text "Invalid character range"
type UnexpectedEndOfCharClass = Text "Unexpected end of character class"

--------------------------------------------------------------------------------
--- ParseRegex
--------------------------------------------------------------------------------

class
  ParseRegex (spec :: Symbol) (regex :: Ast.Regex)
  | spec -> regex

instance parseRegexInst ::
  ( ParseRegexAtDepth spec 0 "" regex
  ) =>
  ParseRegex spec regex

parseRegex :: forall @spec regex. ParseRegex spec regex => Proxy regex
parseRegex = Proxy

--- ParseRegexAtDepth

class
  ParseRegexAtDepth (spec :: Symbol) (depth :: Int) (rest :: Symbol) (regex :: Ast.Regex)
  | spec depth -> rest regex

instance parseRegexAtDepth ::
  ( ParseRegexGo spec Ast.Nil depth rest regex
  , ReverseRegex regex regex'
  ) =>
  ParseRegexAtDepth spec depth rest regex'

--- ParseRegexGo

class
  ParseRegexGo
    (sym :: Symbol)
    (regexFrom :: Ast.Regex)
    (depth :: Int)
    (rest :: Symbol)
    (regexTo :: Ast.Regex)
  | sym regexFrom depth -> rest regexTo

instance parseRegexGoEnd :: ParseRegexGo "" regex 0 "" regex

else instance parseRegexGoEndError ::
  Fail ErrorMissingClose =>
  ParseRegexGo "" regex depth rest regex

else instance parseRegexGoCons ::
  ( Sym.Cons head tail sym
  , ParseRegexMatch head tail regexFrom depth rest regexTo
  ) =>
  ParseRegexGo sym regexFrom depth rest regexTo

--- ParseRegexMatch

class
  ParseRegexMatch
    (head :: Symbol)
    (tail :: Symbol)
    (regexFrom :: Ast.Regex)
    (depth :: Int)
    (rest :: Symbol)
    (regex :: Ast.Regex)
  | head tail regexFrom depth -> rest regex

instance parseRegexMatchGroupGroupCloseError ::
  ( Fail ErrorMissingOpen
  ) =>
  ParseRegexMatch ")" tail regexFrom 0 rest regexTo

else instance parseRegexMatchGroupClose ::
  ParseRegexMatch ")" tail regex depth tail regex

else instance parseRegexMatchGroupStart ::
  ( Increment depth depthNext
  , ParseRegexAtDepth tail depthNext rest' regexTo'
  , ParseRegexGo rest' (Ast.Group regexTo' ~ regexFrom) depth rest regexTo
  ) =>
  ParseRegexMatch "(" tail regexFrom depth rest regexTo

else instance parseRegexMatchWildcard ::
  ( ParseRegexGo tail (Ast.Wildcard ~ regexFrom) depth rest regexTo
  ) =>
  ParseRegexMatch "." tail regexFrom depth rest regexTo

else instance parseRegexMatchStartOfStr ::
  ( ParseRegexGo tail (Ast.StartOfStr ~ regexFrom) depth rest regexTo
  ) =>
  ParseRegexMatch "^" tail regexFrom depth rest regexTo

else instance parseRegexMatchEndOfStr ::
  ( ParseRegexGo tail (Ast.EndOfStr ~ regexFrom) depth rest regexTo
  ) =>
  ParseRegexMatch "$" tail regexFrom depth rest regexTo

else instance parseRegexMatchOptional ::
  ( ParseRegexGo tail (Ast.Optional regexHead ~ regexTail) depth rest regexTo
  , IsQuantifiable regexHead
  ) =>
  ParseRegexMatch "?" tail (regexHead ~ regexTail) depth rest regexTo

else instance parseRegexMatchOneOrMore ::
  ( ParseRegexGo tail (Ast.OneOrMore regexHead ~ regexTail) depth rest regexTo
  , IsQuantifiable regexHead
  ) =>
  ParseRegexMatch "+" tail (regexHead ~ regexTail) depth rest regexTo

else instance parseRegexMatchMany ::
  ( ParseRegexGo tail (Ast.Many regexHead ~ regexTail) depth rest regexTo
  , IsQuantifiable regexHead
  ) =>
  ParseRegexMatch "*" tail (regexHead ~ regexTail) depth rest regexTo

else instance parseRegexMatchAlt ::
  ( ParseRegexGo tail regexTail depth rest' regexTo
  , ParseRegexGo rest' (Ast.Alt regexHead regexTo ~ regexTail) depth rest regexTo
  ) =>
  ParseRegexMatch "|" tail (regexHead ~ regexTail) depth rest regexTo

else instance parseRegexMatchQuote ::
  ( ParseRegexGo tail' (Ast.Lit char ~ regexFrom) depth rest regexTo
  , Sym.Cons head' tail' tail
  , SymIsChar head' char
  ) =>
  ParseRegexMatch "\\" tail regexFrom depth rest regexTo

else instance parseRegexMatchLit ::
  ( ParseRegexGo tail (Ast.Lit char ~ regexFrom) depth rest regexTo
  , SymIsChar head char
  ) =>
  ParseRegexMatch head tail regexFrom depth rest regexTo

--------------------------------------------------------------------------------
---  ParseCharClass
--------------------------------------------------------------------------------

class ParseCharacterClass (sym :: Symbol) (rest :: Symbol) (chars :: Ast.CharClass) (positive :: Boolean)

instance parseCharacterClassInst ::
  ( ConsOrFail (Text "Expecting '['") "[" tail sym
  , ParseCharacterClassGo tail Ast.CharClassNil rest charClass
  ) =>
  ParseCharacterClass sym rest charClass positive

parseCharacterClass
  :: forall @sym @rest @chars @positive
   . ParseCharacterClass sym rest chars positive
  => Unit
parseCharacterClass = unit

--- ParseCharacterClassNegate

class ParseCharacterClassNegate (head :: Symbol) (tail :: Symbol) (rest :: Symbol) (chars :: Ast.CharClass) (positive :: Boolean)

instance parseCharacterClassNegateMatch ::
  ( ParseCharacterClassGo tail Ast.CharClassNil rest charClass
  ) =>
  ParseCharacterClassNegate "^" tail rest charClass False

else instance parseCharacterClassNegateNoMatch ::
  ( ParseCharacterClassGo sym Ast.CharClassNil rest charClass
  , Sym.Cons head tail sym
  ) =>
  ParseCharacterClassNegate head tail rest charClass True

--- ParseCharacterClassGo

class
  ParseCharacterClassGo
    (sym :: Symbol)
    (charsIn :: Ast.CharClass)
    (rest :: Symbol)
    (chars :: Ast.CharClass)

instance parseCharacterClassGoError ::
  ( Fail UnexpectedEndOfCharClass
  ) =>
  ParseCharacterClassGo "" charsClassFrom rest charsClassTo

else instance parseCharacterClassGoCons ::
  ( Sym.Cons head tail sym
  , ParseCharacterClassMatch head tail charsClassFrom rest charsClassTo
  ) =>
  ParseCharacterClassGo sym charsClassFrom rest charsClassTo

--- ParseCharacterClassMatch

class
  ParseCharacterClassMatch
    (head :: Symbol)
    (tail :: Symbol)
    (charClassFrom :: Ast.CharClass)
    (rest :: Symbol)
    (charClassTo :: Ast.CharClass)
  | head tail charClassFrom -> rest charClassTo

instance parseCharacterClassMatchClose ::
  ParseCharacterClassMatch "]" tail charClass tail charClass

else instance parseCharacterClassMatchQuote ::
  ( Sym.Cons head' tail' tail
  , SymIsChar head' char
  , ParseCharacterClassGo tail' (Ast.CharClassLit char charClassFrom) rest charClassTo
  ) =>
  ParseCharacterClassMatch "\\" tail charClassFrom rest charClassTo

else instance parseCharacterClassMatchLit ::
  ( SymIsChar head char
  , ParseCharacterClassGo tail (Ast.CharClassLit char charClassFrom) rest charClassTo
  ) =>
  ParseCharacterClassMatch head tail charClassFrom rest charClassTo

else instance parseCharacterClassMatchRange ::
  ( ConsOrFail (Text "TODO") charEnd' tail' tail
  , SymIsChar charEnd' charEnd
  , ParseCharacterClassGo tail' (Ast.CharClassRange charStart charEnd charClassFrom) rest charClassTo
  ) =>
  ParseCharacterClassMatch "-" tail (Ast.CharClassLit charStart charClassFrom) rest charClassTo

--------------------------------------------------------------------------------
--- Increment
--------------------------------------------------------------------------------

class Increment (n :: Int) (n' :: Int) | n -> n'

instance increment ::
  ( Int.Add n 1 n'
  ) =>
  Increment n n'

--------------------------------------------------------------------------------
--- ConsOrFail
--------------------------------------------------------------------------------

class
  ConsOrFail (doc :: Doc) (head :: Symbol) (tail :: Symbol) (sym :: Symbol)
  | sym -> head tail

instance consOrFailEmpty ::
  ( Fail doc
  ) =>
  ConsOrFail doc head tail ""

else instance consOrFailNonEmpty ::
  ( Sym.Cons head tail sym
  ) =>
  ConsOrFail doc head tail sym

--------------------------------------------------------------------------------
--- IsQuantifiable
--------------------------------------------------------------------------------

class IsQuantifiable (regex :: Ast.Regex)

instance isQuantifiableLit ::
  IsQuantifiable (Ast.Lit s)

else instance isQuantifiableCharClass ::
  IsQuantifiable (Ast.RegexCharClass positive s)

-- else instance isQuantifiableNegativeCharClass ::
--   IsQuantifiable (NegativeCharClass s)

else instance isQuantifiableGroup ::
  IsQuantifiable (Ast.Group r)

else instance isQuantifiableFail ::
  ( Fail ErrorIllegalQuantification
  ) =>
  IsQuantifiable r

--------------------------------------------------------------------------------
--- ReverseRegex
--------------------------------------------------------------------------------

class
  ReverseRegex (regexFrom :: Ast.Regex) (regexOut :: Ast.Regex)
  | regexFrom -> regexOut

instance reverseRegex ::
  ( ReverseRegexGo regex Ast.Nil regexOut
  ) =>
  ReverseRegex regex regexOut

--- ReverseRegexGo

class
  ReverseRegexGo (regex :: Ast.Regex) (regexFrom :: Ast.Regex) (regexTo :: Ast.Regex)
  | regex regexFrom -> regexTo

instance reverseRegexGoNil ::
  ReverseRegexGo Ast.Nil a a

instance reverseRegexGoCons ::
  ( ReverseRegexGo tail (head ~ regexFrom) regexTo
  ) =>
  ReverseRegexGo (head ~ tail) regexFrom regexTo
