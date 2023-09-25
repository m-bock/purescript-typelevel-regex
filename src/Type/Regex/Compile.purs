module Type.Regex.Compile where

import Prim.Boolean (False, True)
import Prim.Symbol as Sym
import Prim.TypeError (class Fail, Beside, Doc, Text)
import Type.Char (UnsafeMkChar)
import Type.Regex.Ast as Ast
import Type.Regex.RegexRep as R

type MkError (err :: Doc) = Beside (Text "Regex Compile Error: ") err

type ErrorUnexpected = MkError (Text "Unexpected error. Please report this as a bug.")

--------------------------------------------------------------------------------
--- CompileRegex
--------------------------------------------------------------------------------

class
  CompileRegex (ast :: Ast.Regex) (regex :: R.Regex)
  | ast -> regex

instance compileRegexNil ::
  CompileRegex Ast.Nil R.Nil

-- instance compileRegexWildcard ::
--   CompileRegex Ast.Wildcard R.Wildcard

else instance compileRegexCharClass ::
  ( CompileCharClass charClass positive regex
  ) =>
  CompileRegex (Ast.RegexCharClass charClass positive) regex

else instance compileRegexLit ::
  CompileRegex (Ast.Lit char) (R.Lit char)

else instance compileRegexEndOfStr ::
  CompileRegex Ast.EndOfStr R.EndOfStr

else instance compileRegexStartOfStr ::
  CompileRegex Ast.StartOfStr R.StartOfStr

else instance compileRegexOptional ::
  ( CompileRegex ast regex
  ) =>
  CompileRegex (Ast.Optional ast) (R.Alt regex R.Nil)

else instance compileRegexOneOrMore ::
  ( CompileRegex ast regex
  ) =>
  CompileRegex (Ast.OneOrMore ast) (R.Cat regex (R.Many regex))

else instance compileRegexMany ::
  ( CompileRegex ast regex
  ) =>
  CompileRegex (Ast.Many ast) (R.Many regex)

else instance compileRegexGroup ::
  ( CompileRegex ast regex
  ) =>
  CompileRegex (Ast.Group ast) regex

else instance compileRegexCat' ::
  ( CompileRegex ast regex
  ) =>
  CompileRegex (Ast.Cat ast Ast.Nil) regex

else instance compileRegexCat'' ::
  ( CompileRegex ast regex
  ) =>
  CompileRegex (Ast.Cat Ast.Nil ast) regex

else instance compileRegexCat ::
  ( CompileRegex ast1 regex1
  , CompileRegex ast2 regex2
  ) =>
  CompileRegex (Ast.Cat ast1 ast2) (R.Cat regex1 regex2)

else instance compileRegexAlt ::
  ( CompileRegex ast1 regex1
  , CompileRegex ast2 regex2
  ) =>
  CompileRegex (Ast.Alt ast1 ast2) (R.Alt regex1 regex2)

else instance compileRegexErrUnexpected ::
  ( Fail ErrorUnexpected
  ) =>
  CompileRegex ast regex

--------------------------------------------------------------------------------
--- CompileCharClass
--------------------------------------------------------------------------------

class
  CompileCharClass (charClass :: Ast.CharClass) (positive :: Boolean) (regex :: R.Regex)
  | charClass positive -> regex

instance compileCharClassPositive ::
  ( CompileCharClassPositiveGo charClass R.Never regex
  ) =>
  CompileCharClass charClass True regex

else instance compileCharClassNegative ::
  ( CompileCharClassNegativeGo charClass "" chars
  ) =>
  CompileCharClass charClass False (R.NotLit chars)

--- CompileCharClassPositiveGo

class
  CompileCharClassPositiveGo
    (charClass :: Ast.CharClass)
    (regexFrom :: R.Regex)
    (regexTo :: R.Regex)
  | charClass regexFrom -> regexTo

instance compileCharClassPositiveGoNil ::
  CompileCharClassPositiveGo Ast.CharClassNil regex regex

else instance compileCharClassPositiveGoLit ::
  ( CompileCharClassPositiveGo charClass (R.Alt (R.Lit char) regexFrom) regexTo
  ) =>
  CompileCharClassPositiveGo (Ast.CharClassLit char charClass) regexFrom regexTo

---

--- CompileCharClassNegativeGo

class
  CompileCharClassNegativeGo
    (charClass :: Ast.CharClass)
    (charsFrom :: Symbol)
    (charsTo :: Symbol)
  | charClass charsFrom -> charsTo

instance compileCharClassNegativeGoNil ::
  CompileCharClassNegativeGo Ast.CharClassNil chars chars

else instance compileCharClassNegativeGoLit ::
  ( CompileCharClassNegativeGo charClass chars' charsTo
  , Sym.Append chars char chars'
  ) =>
  CompileCharClassNegativeGo (Ast.CharClassLit (UnsafeMkChar char) charClass) chars charsTo
