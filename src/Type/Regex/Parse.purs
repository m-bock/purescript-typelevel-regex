module Type.Regex.Parse where

import Prim.Int as Int
import Prim.Ordering (LT, Ordering)
import Prim.Symbol as Sym
import Prim.TypeError (class Fail, Text)
import Type.Proxy (Proxy(..))
import Type.Regex.Types (type (~), Group, Nil, Regex)
import Type.Regex.Types as R
import TypelevelRegex.AsciiTable (class AsciiCode)

--------------------------------------------------------------------------------
--- Errors
--------------------------------------------------------------------------------

type ErrorMissingClose = "Parenthesis mismatch: Missing ')'"
type ErrorMissingOpen = "Parenthesis mismatch: Missing '('"
type ErrorIllegalQuantification = "Nothing to repeat"
type ErrorInvalidRange = "Invalid character range"

type UnexpectedEndOfCharClass = "Unexpected end of character class"

--------------------------------------------------------------------------------
--- ParseRegex
--------------------------------------------------------------------------------

class
  ParseRegex (spec :: Symbol) (regex :: Regex)
  | spec -> regex

instance
  ( ParseRegexDepth spec 0 "" regex
  ) =>
  ParseRegex spec regex

parseRegex :: forall @spec regex. ParseRegex spec regex => Proxy regex
parseRegex = Proxy

--- Depth

class
  ParseRegexDepth (spec :: Symbol) (depth :: Int) (rest :: Symbol) (regex :: Regex)
  | spec depth -> rest regex

instance
  ( ParseRegexGo spec Nil depth rest regex
  , ReverseRegex regex regex'
  ) =>
  ParseRegexDepth spec depth rest regex'

--- Go

class
  ParseRegexGo
    (sym :: Symbol)
    (regexFrom :: Regex)
    (depth :: Int)
    (rest :: Symbol)
    (regexTo :: Regex)
  | sym regexFrom depth -> rest regexTo

instance parseRegexGoEnd :: ParseRegexGo "" regex 0 "" regex

else instance parseRegexGoEndError ::
  Fail (Text ErrorMissingClose) =>
  ParseRegexGo "" regex depth rest regex

else instance parseRegexGoCons ::
  ( Sym.Cons head tail sym
  , ParseRegexMatch head tail regexFrom depth rest regexTo
  ) =>
  ParseRegexGo sym regexFrom depth rest regexTo

--- Match

class
  ParseRegexMatch
    (head :: Symbol)
    (tail :: Symbol)
    (regexFrom :: Regex)
    (depth :: Int)
    (rest :: Symbol)
    (regex :: Regex)
  | head tail regexFrom depth -> rest regex

instance parseRegexMatchGroupGroupCloseError ::
  ( Fail (Text ErrorMissingOpen)
  ) =>
  ParseRegexMatch ")" tail regexFrom 0 rest regexTo

else instance parseRegexMatchGroupClose ::
  ParseRegexMatch ")" tail regex depth tail regex

else instance parseRegexMatchGroupStart ::
  ( Int.Add depth 1 depthNext
  , ParseRegexDepth tail depthNext rest' regexTo'
  , ParseRegexGo rest' (Group regexTo' ~ regexFrom) depth rest regexTo
  ) =>
  ParseRegexMatch "(" tail regexFrom depth rest regexTo

else instance parseRegexMatchWildcard ::
  ( ParseRegexGo tail (R.Wildcard ~ regexFrom) depth rest regexTo
  ) =>
  ParseRegexMatch "." tail regexFrom depth rest regexTo

else instance parseRegexMatchStartOfStr ::
  ( ParseRegexGo tail (R.StartOfStr ~ regexFrom) depth rest regexTo
  ) =>
  ParseRegexMatch "^" tail regexFrom depth rest regexTo

else instance parseRegexMatchEndOfStr ::
  ( ParseRegexGo tail (R.EndOfStr ~ regexFrom) depth rest regexTo
  ) =>
  ParseRegexMatch "$" tail regexFrom depth rest regexTo

else instance parseRegexMatchOptional ::
  ( ParseRegexGo tail (R.ManyMinMax 0 1 regexHead ~ regexTail) depth rest regexTo
  , IsQuantifiable regexHead
  ) =>
  ParseRegexMatch "?" tail (regexHead ~ regexTail) depth rest regexTo

else instance parseRegexMatchOneOrMore ::
  ( ParseRegexGo tail (R.ManyMin 1 regexHead ~ regexTail) depth rest regexTo
  , IsQuantifiable regexHead
  ) =>
  ParseRegexMatch "+" tail (regexHead ~ regexTail) depth rest regexTo

else instance parseRegexMatchMany ::
  ( ParseRegexGo tail (R.ManyMin 0 regexHead ~ regexTail) depth rest regexTo
  , IsQuantifiable regexHead
  ) =>
  ParseRegexMatch "*" tail (regexHead ~ regexTail) depth rest regexTo

else instance parseRegexMatchQuote ::
  ( ParseRegexGo tail' (R.Lit head' ~ regexFrom) depth rest regexTo
  , Sym.Cons head' tail' tail
  ) =>
  ParseRegexMatch "\\" tail regexFrom depth rest regexTo

else instance parseRegexMatchLit ::
  ( ParseRegexGo tail (R.Lit head ~ regexFrom) depth rest regexTo
  ) =>
  ParseRegexMatch head tail regexFrom depth rest regexTo

--------------------------------------------------------------------------------
--- ParseCharacterClass
--------------------------------------------------------------------------------

foreign import data CharClass :: Type

foreign import data CharClassNil :: CharClass

foreign import data CharClassLit :: Symbol -> CharClass -> CharClass

foreign import data CharClassRange :: Symbol -> Symbol -> CharClass -> CharClass

class ParseCharacterClass (sym :: Symbol) (rest :: Symbol) (chars :: Symbol)

instance
  ( ParseCharacterClassGo sym CharClassNil rest charClass
  , ResolveCharacterClass charClass chars
  ) =>
  ParseCharacterClass sym rest chars

--- Go

class
  ParseCharacterClassGo
    (sym :: Symbol)
    (charsIn :: CharClass)
    (rest :: Symbol)
    (chars :: CharClass)

instance
  ( Fail (Text UnexpectedEndOfCharClass)
  ) =>
  ParseCharacterClassGo "" charsClassFrom rest charsClassTo

else instance
  ( Sym.Cons head tail sym
  , ParseCharacterClassMatch head tail charsClassFrom rest charsClassTo
  ) =>
  ParseCharacterClassGo sym charsClassFrom rest charsClassTo

--- Match

class
  ParseCharacterClassMatch
    (head :: Symbol)
    (tail :: Symbol)
    (charsIn :: CharClass)
    (rest :: Symbol)
    (chars :: CharClass)

instance parseCharacterClassMatchClose ::
  ParseCharacterClassMatch "]" tail charsIn tail charsIn

-- instance parseCharacterClassMatchQuote ::
--   (
--     Sym.Cons prevChar
--   ) =>
--   ParseCharacterClassMatch "\\" tail charsIn rest chars

--------------------------------------------------------------------------------
--- ResolveCharacterClass
--------------------------------------------------------------------------------

class
  ResolveCharacterClass (charClass :: CharClass) (chars :: Symbol)
  | charClass -> chars

instance resolveCharacterClassNil ::
  ResolveCharacterClass CharClassNil ""

instance resolveCharacterClassLit ::
  ( Sym.Cons lit chars' chars
  , ResolveCharacterClass charClass chars'
  ) =>
  ResolveCharacterClass (CharClassLit lit charClass) chars

instance resolveCharacterClassRange ::
  ( Sym.Append chars chars' chars''
  , ResolveCharacterClass charClass chars'
  , GetCharRange start end chars
  ) =>
  ResolveCharacterClass (CharClassRange start end charClass) chars''

--------------------------------------------------------------------------------
--- GetCharRange
--------------------------------------------------------------------------------

class
  GetCharRange (start :: Symbol) (end :: Symbol) (chars :: Symbol)
  | start end -> chars

instance
  ( GetCharRangeGuard ord start end chars
  , AsciiCode start charStart
  , AsciiCode end charEnd
  , Int.Compare start end ord
  ) =>
  GetCharRange charStart charEnd chars

--- Guard

class
  GetCharRangeGuard (ord :: Ordering) (start :: Int) (end :: Int) (chars :: Symbol)
  | ord start end -> chars

instance getCharRangeGuardSucceed ::
  ( GetCharRangeGo start end "" chars
  ) =>
  GetCharRangeGuard LT start end chars

else instance getCharRangeGuardFail ::
  ( Fail (Text ErrorInvalidRange)
  ) =>
  GetCharRangeGuard ord start end chars

--- Go

class
  GetCharRangeGo (start :: Int) (end :: Int) (charsFrom :: Symbol) (charsTo :: Symbol)
  | start end charsFrom -> charsTo

instance getCharRangeGoLast ::
  ( AsciiCode end char
  , Sym.Append char charsFrom charsTo
  ) =>
  GetCharRangeGo end end charsFrom charsTo

else instance getCharRangeGoNext ::
  ( AsciiCode start char
  , GetCharRangeGo start' end charsFrom' charsTo
  , Sym.Append char charsFrom charsFrom'
  , Int.Add start 1 start'
  ) =>
  GetCharRangeGo start end charsFrom charsTo

--------------------------------------------------------------------------------
--- IsQuantifiable
--------------------------------------------------------------------------------

class IsQuantifiable (regex :: Regex)

instance IsQuantifiable (R.Lit s)
else instance IsQuantifiable (R.CharClass s)
else instance IsQuantifiable (R.NegativeCharClass s)
else instance IsQuantifiable (R.Group r)
else instance (Fail (Text ErrorIllegalQuantification)) => IsQuantifiable r

--------------------------------------------------------------------------------
--- ReverseRegex
--------------------------------------------------------------------------------

class
  ReverseRegex (regexFrom :: Regex) (regexOut :: Regex)
  | regexFrom -> regexOut

instance
  ( ReverseRegexGo regex Nil regexOut
  ) =>
  ReverseRegex regex regexOut

--- Go

class
  ReverseRegexGo (regex :: Regex) (regexFrom :: Regex) (regexTo :: Regex)
  | regex regexFrom -> regexTo

instance ReverseRegexGo Nil a a

instance
  ( ReverseRegexGo tail (head ~ regexFrom) regexTo
  ) =>
  ReverseRegexGo (head ~ tail) regexFrom regexTo

