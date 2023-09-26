module Type.Regex.Compile where

import Prim.Boolean (False, True)
import Prim.Int as Int
import Prim.Ordering (GT, Ordering)
import Prim.Symbol as Sym
import Prim.TypeError (class Fail, Beside, Doc, Text)
import Type.Char (UnsafeMkChar)
import Type.Regex.AsciiTable (class AsciiCode)
import Type.Regex.CST as CST
import Type.Regex.RegexRep as R

type MkError (err :: Doc) = Beside (Text "Regex Compile Error: ") err

type ErrorUnexpected = MkError (Text "Unexpected error. Please report this as a bug.")

type ErrorRange = MkError (Text "Range error")

--------------------------------------------------------------------------------
--- CompileRegex
--------------------------------------------------------------------------------

class
  CompileRegex (cst :: CST.Regex) (regex :: R.Regex)
  | cst -> regex

instance compileRegexNil ::
  CompileRegex CST.Nil R.Nil

else instance compileRegexWildcard ::
  CompileRegex CST.Wildcard R.Wildcard

else instance compileRegexCharClass ::
  ( CompileCharClass charClass positive regex
  ) =>
  CompileRegex (CST.RegexCharClass charClass positive) regex

else instance compileRegexLit ::
  CompileRegex (CST.Lit char) (R.Lit char)

else instance compileRegexQuote ::
  CompileRegex (CST.Quote char) (R.Lit char)

else instance compileRegexEndOfStr ::
  CompileRegex CST.EndOfStr R.EndOfStr

else instance compileRegexStartOfStr ::
  CompileRegex CST.StartOfStr R.StartOfStr

else instance compileRegexOptional ::
  ( CompileRegex cst regex
  ) =>
  CompileRegex (CST.Optional cst) (R.Alt regex R.Nil)

else instance compileRegexOneOrMore ::
  ( CompileRegex cst regex
  ) =>
  CompileRegex (CST.OneOrMore cst) (R.Cat regex (R.Many regex))

else instance compileRegexMany ::
  ( CompileRegex cst regex
  ) =>
  CompileRegex (CST.Many cst) (R.Many regex)

else instance compileRegexGroup ::
  ( CompileRegex cst regex
  ) =>
  CompileRegex (CST.Group cst) regex

else instance compileRegexCat' ::
  ( CompileRegex cst regex
  ) =>
  CompileRegex (CST.Cat cst CST.Nil) regex

else instance compileRegexCat'' ::
  ( CompileRegex cst regex
  ) =>
  CompileRegex (CST.Cat CST.Nil cst) regex

else instance compileRegexCat ::
  ( CompileRegex cst1 regex1
  , CompileRegex cst2 regex2
  ) =>
  CompileRegex (CST.Cat cst1 cst2) (R.Cat regex1 regex2)

else instance compileRegexAlt ::
  ( CompileRegex cst1 regex1
  , CompileRegex cst2 regex2
  ) =>
  CompileRegex (CST.Alt cst1 cst2) (R.Alt regex1 regex2)

else instance compileRegexErrUnexpected ::
  ( Fail ErrorUnexpected
  ) =>
  CompileRegex cst regex

--------------------------------------------------------------------------------
--- CompileCharClass
--------------------------------------------------------------------------------

class
  CompileCharClass (charClass :: CST.CharClass) (positive :: Boolean) (regex :: R.Regex)
  | charClass positive -> regex

instance compileCharClassPositive ::
  ( CompileCharClassGo charClass "" chars
  ) =>
  CompileCharClass charClass True (R.Lits chars)

else instance compileCharClassNegative ::
  ( CompileCharClassGo charClass "" chars
  ) =>
  CompileCharClass charClass False (R.NotLits chars)

--- CompileCharClassGo

class
  CompileCharClassGo
    (charClass :: CST.CharClass)
    (charsFrom :: Symbol)
    (charsTo :: Symbol)
  | charClass charsFrom -> charsTo

instance compileCharClassGoNil ::
  CompileCharClassGo CST.CharClassNil chars chars

else instance compileCharClassGoLit ::
  ( CompileCharClassGo charClass chars' charsTo
  , Sym.Append chars char chars'
  ) =>
  CompileCharClassGo (CST.CharClassLit (UnsafeMkChar char) charClass) chars charsTo

else instance compileCharClassGoRange ::
  ( AsciiCode from charFrom
  , AsciiCode to charTo
  , GetCharRange from to chars'
  , Sym.Append chars chars' chars''
  , CompileCharClassGo charClass chars'' charsTo
  ) =>
  CompileCharClassGo
    (CST.CharClassRange (UnsafeMkChar charFrom) (UnsafeMkChar charTo) charClass)
    chars
    charsTo

------------------------------------------------------------------------
--- GetCharRange
------------------------------------------------------------------------

class
  GetCharRange (start :: Int) (end :: Int) (chars :: Symbol)
  | start end -> chars

instance
  ( GetCharRangeGuard start end chars
  ) =>
  GetCharRange start end chars

--- GetCharRangeGuard

class
  GetCharRangeGuard (start :: Int) (end :: Int) (chars :: Symbol)
  | start end -> chars

instance getCharRangeGuard ::
  ( Int.Compare start end result
  , GetCharRangeGuardResult result start end chars
  ) =>
  GetCharRangeGuard start end chars

--- GetCharRangeGuardResult

class
  GetCharRangeGuardResult
    (result :: Ordering)
    (start :: Int)
    (end :: Int)
    (chars :: Symbol)
  | result start end -> chars

instance getCharRangeGuardResultFail ::
  Fail ErrorRange =>
  GetCharRangeGuardResult GT start end charsIn

else instance getCharRangeGuardResultOk ::
  ( GetCharRangeGo start end "" chars
  ) =>
  GetCharRangeGuardResult result start end chars

--- GetCharRangeGo

class
  GetCharRangeGo
    (start :: Int)
    (end :: Int)
    (charsIn :: Symbol)
    (chars :: Symbol)
  | start end charsIn -> chars

instance getCharRangeGoLast ::
  ( AsciiCode start char
  , Sym.Append char charsIn chars
  ) =>
  GetCharRangeGo start start charsIn chars

else instance getCharRangeGoNext ::
  ( AsciiCode start char
  , GetCharRangeGo start' end charsIn' chars
  , Sym.Append char charsIn charsIn'
  , Int.Add start 1 start'
  ) =>
  GetCharRangeGo start end charsIn chars
