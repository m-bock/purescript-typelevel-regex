module Type.Regex.Parse where

import Prim.Int as Int
import Prim.Symbol as Sym
import Prim.TypeError (class Fail, Text)
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Type.Regex.Types (type (~), Group, Nil, Regex)
import Type.Regex.Types as R

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

--------------------------------------------------------------------------------
--- ParseRegex
--------------------------------------------------------------------------------

class
  ParseRegexDepth (spec :: Symbol) (depth :: Int) (rest :: Symbol) (regex :: Regex)
  | spec depth -> rest regex

instance
  ( ParseRegexGo spec Nil depth rest regex
  , ReverseRegex regex regex'
  ) =>
  ParseRegexDepth spec depth rest regex'

--------------------------------------------------------------------------------
--- ParseRegexGo
--------------------------------------------------------------------------------

class
  ParseRegexGo
    (spec :: Symbol)
    (regexFrom :: Regex)
    (depth :: Int)
    (rest :: Symbol)
    (regexTo :: Regex)
  | spec regexFrom depth -> rest regexTo

instance parseRegexGoEnd :: ParseRegexGo "" regex 0 "" regex

else instance parseRegexGoEndError ::
  Fail (Text "Parenthesis mismatch: Missing ')'") =>
  ParseRegexGo "" regex depth rest regex

else instance parseRegexGoCons ::
  ( Sym.Cons head tail spec
  , ParseRegexMatch head tail regexFrom depth rest regexTo
  ) =>
  ParseRegexGo spec regexFrom depth rest regexTo

--------------------------------------------------------------------------------
--- ParseRegexMatch
--------------------------------------------------------------------------------

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
  ( Fail (Text "Parenthesis mismatch: Missing '('")
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
--- IsQuantifiable
--------------------------------------------------------------------------------

class IsQuantifiable (regex :: Regex)

instance IsQuantifiable (R.Lit s)
else instance IsQuantifiable (R.CharClass s)
else instance IsQuantifiable (R.NegativeCharClass s)
else instance IsQuantifiable (R.Group r)
else instance (Fail (Text "Nothing to repeat")) => IsQuantifiable r

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

--------------------------------------------------------------------------------
--- ReverseRegexGo
--------------------------------------------------------------------------------

class
  ReverseRegexGo (regex :: Regex) (regexFrom :: Regex) (regexTo :: Regex)
  | regex regexFrom -> regexTo

instance ReverseRegexGo Nil a a

instance
  ( ReverseRegexGo tail (head ~ regexFrom) regexTo
  ) =>
  ReverseRegexGo (head ~ tail) regexFrom regexTo
