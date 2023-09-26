module Type.Regex.Parse where

import Prelude

import Prim.Boolean (False, True)
import Prim.Int as Int
import Prim.Symbol as Sym
import Prim.TypeError (class Fail, Beside, Doc, Text)
import Type.Char (class SymIsChar)
import Type.Proxy (Proxy(..))
import Type.Regex.CST (type (~))
import Type.Regex.CST as CST

--------------------------------------------------------------------------------
--- Errors
--------------------------------------------------------------------------------

type MkError (err :: Doc) = Beside (Text "Regex Parse Error: ") err

type ErrorMissingClose = MkError (Text "Parenthesis mismatch: Missing ')'")

type ErrorMissingOpen = MkError (Text "Parenthesis mismatch: Missing '('")

type ErrorIllegalQuantification = MkError (Text "Nothing to repeat")

type ErrorInvalidRange = MkError (Text "Invalid character range")

type UnexpectedEndOfCharClass = MkError (Text "Unexpected end of character class")

type ErrorUnexpectedEnd = (Text "Regex Parse Error: Unexpected end")

--------------------------------------------------------------------------------
--- ParseRegex
--------------------------------------------------------------------------------

class
  ParseRegex (spec :: Symbol) (regex :: CST.Regex)
  | spec -> regex

instance parseRegexInst ::
  ( ParseRegexAtDepth spec 0 "" regex
  ) =>
  ParseRegex spec regex

parseRegex :: forall @spec regex. ParseRegex spec regex => Proxy regex
parseRegex = Proxy

--- ParseRegexAtDepth

class
  ParseRegexAtDepth (spec :: Symbol) (depth :: Int) (rest :: Symbol) (regex :: CST.Regex)
  | spec depth -> rest regex

instance parseRegexAtDepth ::
  ( ParseRegexGo spec CST.Nil depth rest regex
  , ReverseRegex regex regex'
  ) =>
  ParseRegexAtDepth spec depth rest regex'

--- ParseRegexGo

class
  ParseRegexGo
    (sym :: Symbol)
    (regexFrom :: CST.Regex)
    (depth :: Int)
    (rest :: Symbol)
    (regexTo :: CST.Regex)
  | sym regexFrom depth -> rest regexTo

instance parseRegexGoEnd :: ParseRegexGo "" regex 0 "" regex

else instance parseRegexGoEndError ::
  ( Fail ErrorMissingClose
  ) =>
  ParseRegexGo "" regex depth "" regex

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
    (regexFrom :: CST.Regex)
    (depth :: Int)
    (rest :: Symbol)
    (regex :: CST.Regex)
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
  , ParseRegexGo rest' (CST.Group regexTo' ~ regexFrom) depth rest regexTo
  ) =>
  ParseRegexMatch "(" tail regexFrom depth rest regexTo

else instance parseRegexMatchWildcard ::
  ( ParseRegexGo tail (CST.Wildcard ~ regexFrom) depth rest regexTo
  ) =>
  ParseRegexMatch "." tail regexFrom depth rest regexTo

else instance parseRegexMatchStartOfStr ::
  ( ParseRegexGo tail (CST.StartOfStr ~ regexFrom) depth rest regexTo
  ) =>
  ParseRegexMatch "^" tail regexFrom depth rest regexTo

else instance parseRegexMatchEndOfStr ::
  ( ParseRegexGo tail (CST.EndOfStr ~ regexFrom) depth rest regexTo
  ) =>
  ParseRegexMatch "$" tail regexFrom depth rest regexTo

else instance parseRegexMatchOptional ::
  ( ParseRegexGo tail (CST.Optional regexHead ~ regexTail) depth rest regexTo
  , IsQuantifiable regexHead
  ) =>
  ParseRegexMatch "?" tail (regexHead ~ regexTail) depth rest regexTo

else instance
  ( Fail ErrorIllegalQuantification
  ) =>
  ParseRegexMatch "?" tail regex depth rest regexTo

else instance parseRegexMatchOneOrMore ::
  ( ParseRegexGo tail (CST.OneOrMore regexHead ~ regexTail) depth rest regexTo
  , IsQuantifiable regexHead
  ) =>
  ParseRegexMatch "+" tail (regexHead ~ regexTail) depth rest regexTo

else instance
  ( Fail ErrorIllegalQuantification
  ) =>
  ParseRegexMatch "?" tail regex depth rest regexTo

else instance parseRegexMatchMany ::
  ( ParseRegexGo tail (CST.Many regexHead ~ regexTail) depth rest regexTo
  , IsQuantifiable regexHead
  ) =>
  ParseRegexMatch "*" tail (regexHead ~ regexTail) depth rest regexTo

else instance
  ( Fail ErrorIllegalQuantification
  ) =>
  ParseRegexMatch "?" tail regex depth rest regexTo

else instance parseRegexMatchAlt ::
  ( ParseRegexAtDepth tail depth rest regexTo
  , Decrement depth depth'
  , ReverseRegex regex regex'
  ) =>
  ParseRegexMatch "|" tail regex depth rest (CST.Alt regex' regexTo ~ CST.Nil)

else instance parseRegexMatchCharClass ::
  ( Sym.Cons "[" tail sym
  , ParseCharacterClass sym rest charClass positive
  , ParseRegexGo rest (CST.RegexCharClass charClass positive ~ regexFrom) depth rest' regexTo
  ) =>
  ParseRegexMatch "[" tail regexFrom depth rest' regexTo

else instance parseRegexMatchQuote ::
  ( ParseRegexGo tail' (CST.Quote char ~ regexFrom) depth rest regexTo
  , Sym.Cons head' tail' tail
  , SymIsChar head' char
  ) =>
  ParseRegexMatch "\\" tail regexFrom depth rest regexTo

else instance parseRegexMatchLit ::
  ( ParseRegexGo tail (CST.Lit char ~ regexFrom) depth rest regexTo
  , SymIsChar head char
  ) =>
  ParseRegexMatch head tail regexFrom depth rest regexTo

--------------------------------------------------------------------------------
---  ParseCharClass
--------------------------------------------------------------------------------

class
  ParseCharacterClass
    (sym :: Symbol)
    (rest :: Symbol)
    (chars :: CST.CharClass)
    (positive :: Boolean)
  | sym -> rest chars positive

instance parseCharacterClassInst ::
  ( ConsOrFail (Text "Regex Parse Error: Expecting '['") "[" tail sym
  , ParseCharacterClassGo tail CST.CharClassNil rest positive charClass
  ) =>
  ParseCharacterClass sym rest charClass positive

parseCharacterClass
  :: forall @sym @rest @chars @positive
   . ParseCharacterClass sym rest chars positive
  => Unit
parseCharacterClass = unit

--- ParseCharacterClassGo

class
  ParseCharacterClassGo
    (sym :: Symbol)
    (charsIn :: CST.CharClass)
    (rest :: Symbol)
    (positive :: Boolean)
    (chars :: CST.CharClass)
  | sym charsIn -> rest positive chars

instance parseCharacterClassGoError ::
  ( Fail UnexpectedEndOfCharClass
  ) =>
  ParseCharacterClassGo "" charsClassFrom rest positive charsClassTo

else instance parseCharacterClassGoCons ::
  ( Sym.Cons head tail sym
  , ParseCharacterClassMatch head tail charsClassFrom rest positive charsClassTo
  ) =>
  ParseCharacterClassGo sym charsClassFrom rest positive charsClassTo

--- ParseCharacterClassMatch

class
  ParseCharacterClassMatch
    (head :: Symbol)
    (tail :: Symbol)
    (charClassFrom :: CST.CharClass)
    (rest :: Symbol)
    (positive :: Boolean)
    (charClassTo :: CST.CharClass)
  | head tail charClassFrom -> rest positive charClassTo

instance parseCharacterClassMatchClose ::
  ParseCharacterClassMatch "]" tail charClass tail True charClass

else instance parseCharacterClassMatchNegate ::
  ( ParseCharacterClassGo tail CST.CharClassNil tail' positive charClass'
  ) =>
  ParseCharacterClassMatch "^" tail CST.CharClassNil tail' False charClass'

else instance parseCharacterClassMatchQuote ::
  ( Sym.Cons head' tail' tail
  , SymIsChar head' char
  , ParseCharacterClassGo tail' (CST.CharClassLit char charClassFrom) rest positive charClassTo
  ) =>
  ParseCharacterClassMatch "\\" tail charClassFrom rest positive charClassTo

else instance parseCharacterClassMatchRange ::
  ( ConsOrFail ErrorUnexpectedEnd charEnd' tail' tail
  , SymIsChar charEnd' charEnd
  , ParseCharacterClassGo tail' (CST.CharClassRange charStart charEnd charClassFrom) rest positive charClassTo
  ) =>
  ParseCharacterClassMatch "-" tail (CST.CharClassLit charStart charClassFrom) rest positive charClassTo

else instance parseCharacterClassMatchLit ::
  ( SymIsChar head char
  , ParseCharacterClassGo tail (CST.CharClassLit char charClassFrom) rest positive charClassTo
  ) =>
  ParseCharacterClassMatch head tail charClassFrom rest positive charClassTo

--------------------------------------------------------------------------------
--- Increment
--------------------------------------------------------------------------------

class Increment (n :: Int) (n' :: Int) | n -> n'

instance increment ::
  ( Int.Add n 1 n'
  ) =>
  Increment n n'

--------------------------------------------------------------------------------
--- Decrement
--------------------------------------------------------------------------------

class Decrement (n :: Int) (n' :: Int) | n -> n'

instance decrement ::
  ( Int.Add n' 1 n
  ) =>
  Decrement n n'

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

class IsQuantifiable (regex :: CST.Regex)

instance isQuantifiableLit ::
  IsQuantifiable (CST.Lit s)

else instance isQuantifiableCharClass ::
  IsQuantifiable (CST.RegexCharClass positive s)

else instance isQuantifiableGroup ::
  IsQuantifiable (CST.Group r)

else instance isQuantifiableFail ::
  ( Fail ErrorIllegalQuantification
  ) =>
  IsQuantifiable r

--------------------------------------------------------------------------------
--- ReverseRegex
--------------------------------------------------------------------------------

class
  ReverseRegex (regexFrom :: CST.Regex) (regexOut :: CST.Regex)
  | regexFrom -> regexOut

instance reverseRegex ::
  ( ReverseRegexGo regex CST.Nil regexOut
  ) =>
  ReverseRegex regex regexOut

--- ReverseRegexGo

class
  ReverseRegexGo (regex :: CST.Regex) (regexFrom :: CST.Regex) (regexTo :: CST.Regex)
  | regex regexFrom -> regexTo

instance reverseRegexGoNil ::
  ReverseRegexGo CST.Nil a a

else instance reverseRegexGoCons ::
  ( ReverseRegexGo tail (head ~ regexFrom) regexTo
  ) =>
  ReverseRegexGo (head ~ tail) regexFrom regexTo
