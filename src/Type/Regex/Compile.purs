module Type.Regex.Compile where

import Prim.Boolean (True)
import Type.Regex.Ast as Ast
import Type.Regex.RegexRep as R

--------------------------------------------------------------------------------
--- CompileRegex
--------------------------------------------------------------------------------

class
  CompileRegex (ast :: Ast.Regex) (regex :: R.Regex)
  | ast -> regex

instance compileRegexNil ::
  CompileRegex Ast.Nil R.Nil

instance compileRegexWildcard ::
  CompileRegex Ast.Wildcard R.Wildcard

instance compileRegexCharClass ::
  ( CompileCharClass charClass positive regex
  ) =>
  CompileRegex (Ast.RegexCharClass charClass positive) regex

instance compileRegexLit ::
  CompileRegex (Ast.Lit char) (R.Lit char True)

instance compileRegexEndOfStr ::
  CompileRegex Ast.EndOfStr R.EndOfStr

instance compileRegexStartOfStr ::
  CompileRegex Ast.StartOfStr R.StartOfStr

instance compileRegexOptional ::
  ( CompileRegex ast regex
  ) =>
  CompileRegex (Ast.Optional ast) (R.Alt regex R.Nil)

instance compileRegexOneOrMore ::
  ( CompileRegex ast regex
  ) =>
  CompileRegex (Ast.OneOrMore ast) (R.Cat regex (R.Many regex))

instance compileRegexMany ::
  ( CompileRegex ast regex
  ) =>
  CompileRegex (Ast.Many ast) (R.Many regex)

instance compileRegexGroup ::
  ( CompileRegex ast regex
  ) =>
  CompileRegex (Ast.Group ast) (R.Group regex)

instance compileRegexCat ::
  ( CompileRegex ast1 regex1
  , CompileRegex ast2 regex2
  ) =>
  CompileRegex (Ast.Cat ast1 ast2) (R.Cat regex1 regex2)

instance compileRegexAlt ::
  ( CompileRegex ast1 regex1
  , CompileRegex ast2 regex2
  ) =>
  CompileRegex (Ast.Alt ast1 ast2) (R.Alt regex1 regex2)

--------------------------------------------------------------------------------
--- CompileCharClass
--------------------------------------------------------------------------------

class
  CompileCharClass (charClass :: Ast.CharClass) (positive :: Boolean) (regex :: R.Regex)
  | charClass positive -> regex

instance compileCharClass ::
  ( CompileCharClassGo charClass positive R.Nil regex
  ) =>
  CompileCharClass charClass positive regex

--- CompileCharClassGo

class
  CompileCharClassGo (charClass :: Ast.CharClass) (positive :: Boolean) (regexFrom :: R.Regex) (regexTo :: R.Regex)
  | charClass positive regexFrom -> regexTo

instance compileCharClassGoNil ::
  CompileCharClassGo Ast.CharClassNil positive regex regex

else instance compileCharClassGoLit ::
  ( CompileCharClassGo charClass positive (R.Lit char positive R.~ regexFrom) regexTo
  ) =>
  CompileCharClassGo (Ast.CharClassLit char charClass) positive regexFrom regexTo
