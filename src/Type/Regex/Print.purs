module Type.Regex.Print where

import Prim.Boolean (False, True)
import Prim.Symbol as Sym
import Prim.TypeError (class Fail, Text)
import Type.Char (UnsafeMkChar)
import Type.Regex.CST as CST

class
  PrintRegex (cst :: CST.Regex) (sym :: Symbol)
  | cst -> sym

instance PrintRegex CST.Nil ""

else instance PrintRegex CST.Wildcard "*"

else instance
  ( PrintCharClass charClass sym
  , Append3 "[" sym "]" sym'
  ) =>
  PrintRegex (CST.RegexCharClass charClass True) sym'

else instance
  ( PrintCharClass charClass sym
  , Append3 "[^" sym "]" sym'
  ) =>
  PrintRegex (CST.RegexCharClass charClass False) sym'

else instance PrintRegex (CST.Lit (UnsafeMkChar char)) char

else instance
  ( Sym.Append "\\" char sym
  ) =>
  PrintRegex (CST.Quote (UnsafeMkChar char)) sym

else instance PrintRegex CST.EndOfStr "$"

else instance PrintRegex CST.StartOfStr "^"

else instance
  ( PrintRegex cst1 sym1
  , PrintRegex cst2 sym2
  , Sym.Append sym1 sym2 sym
  ) =>
  PrintRegex (CST.Cat cst1 cst2) sym

else instance
  ( PrintRegex cst1 sym1
  , PrintRegex cst2 sym2
  , Append3 sym1 "|" sym2 sym
  ) =>
  PrintRegex (CST.Alt cst1 cst2) sym

else instance
  ( PrintRegex cst sym
  , Append3 "(" sym ")" sym'
  ) =>
  PrintRegex (CST.Group cst) sym'

else instance
  ( PrintRegex cst sym
  , Sym.Append sym "*" sym'
  ) =>
  PrintRegex (CST.Many cst) sym'

else instance
  ( PrintRegex cst sym
  , Sym.Append sym "?" sym'
  ) =>
  PrintRegex (CST.Optional cst) sym'

else instance
  ( PrintRegex cst sym
  , Sym.Append sym "+" sym'
  ) =>
  PrintRegex (CST.OneOrMore cst) sym'

else instance (Fail (Text "Regex Print error")) => PrintRegex cst sym

--------------------------------------------------------------------------------
--- PrintCharClass
--------------------------------------------------------------------------------

class
  PrintCharClass (charClass :: CST.CharClass) (sym :: Symbol)
  | charClass -> sym

instance printCharClassNil ::
  PrintCharClass CST.CharClassNil ""

else instance printCharClassLit ::
  ( Sym.Append sym char sym'
  , PrintCharClass charClass sym
  ) =>
  PrintCharClass
    (CST.CharClassLit (UnsafeMkChar char) charClass)
    sym'

else instance printCharClassRange ::
  ( Append3 charFrom "-" charTo sym2
  , Sym.Append sym1 sym2 sym
  , PrintCharClass charClass sym1
  ) =>
  PrintCharClass
    (CST.CharClassRange (UnsafeMkChar charFrom) (UnsafeMkChar charTo) charClass)
    sym

else instance (Fail (Text "Regex PrintCharClass error")) => PrintCharClass charClass sym

--------------------------------------------------------------------------------
--- Append3
--------------------------------------------------------------------------------

class
  Append3 (sym1 :: Symbol) (sym2 :: Symbol) (sym3 :: Symbol) (sym :: Symbol)
  | sym1 sym2 sym3 -> sym

instance
  ( Sym.Append sym1 sym2 symMid
  , Sym.Append symMid sym3 sym
  ) =>
  Append3 sym1 sym2 sym3 sym