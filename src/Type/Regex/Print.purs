module Type.Regex.Print where

import Prelude

import Prim.Symbol as Sym
import Prim.TypeError (class Fail, Text)
import Type.Char (UnsafeMkChar)
import Type.Regex.Ast as Ast

class
  PrintRegex (ast :: Ast.Regex) (sym :: Symbol)
  | ast -> sym

instance PrintRegex Ast.Nil ""

else instance PrintRegex (Ast.Lit (UnsafeMkChar char)) char

else instance
  ( PrintRegex ast1 sym1
  , PrintRegex ast2 sym2
  , Sym.Append sym1 sym2 sym
  ) =>
  PrintRegex (Ast.Cat ast1 ast2) sym

else instance
  ( PrintRegex ast1 sym1
  , PrintRegex ast2 sym2
  , Append3 sym1 "|" sym2 sym
  ) =>
  PrintRegex (Ast.Alt ast1 ast2) sym

else instance
  ( PrintRegex ast sym
  , Append3 "(" sym ")" sym'
  ) =>
  PrintRegex (Ast.Group ast) sym'

else instance
  ( PrintRegex ast sym
  , Sym.Append sym "*" sym'
  ) =>
  PrintRegex (Ast.Many ast) sym'

else instance
  ( PrintRegex ast sym
  , Sym.Append sym "?" sym'
  ) =>
  PrintRegex (Ast.Optional ast) sym'

else instance
  ( PrintRegex ast sym
  , Sym.Append sym "+" sym'
  ) =>
  PrintRegex (Ast.OneOrMore ast) sym'



else instance (Fail (Text "Regex Print error")) => PrintRegex ast sym

---

class
  Append3 (sym1 :: Symbol) (sym2 :: Symbol) (sym3 :: Symbol) (sym :: Symbol)
  | sym1 sym2 sym3 -> sym

instance
  ( Sym.Append sym1 sym2 symMid
  , Sym.Append symMid sym3 sym
  ) =>
  Append3 sym1 sym2 sym3 sym