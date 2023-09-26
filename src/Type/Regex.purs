module Type.Regex where

import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Ordering (EQ, Ordering)
import Prim.Symbol as Sym
import Prim.TypeError (class Fail, Text)
import Type.Proxy (Proxy(..))
import Type.Regex.CST as CST
import Type.Regex.Compile (class CompileRegex)
import Type.Regex.Match (class MatchRegex)
import Type.Regex.Parse (class ParseRegex)
import Type.Regex.Print (class PrintRegex)

class
  TestRegex (regex :: Symbol) (str :: Symbol)

instance
  ( ParseRegex' regexStr regexCST
  , CompileRegex regexCST regex
  , MatchRegex regex str
  ) =>
  TestRegex regexStr str

guard :: forall @regex @str. TestRegex regex str => IsSymbol str => String
guard = reflectSymbol (Proxy :: Proxy str)

reflectRegex
  :: forall @spec cst regex
   . ParseRegex spec cst
  => CompileRegex cst regex
  => IsSymbol spec
  => String
reflectRegex = reflectSymbol (Proxy :: Proxy spec)

---

class ParseRegex' (spec :: Symbol) (cst :: CST.Regex) | spec -> cst

instance
  ( ParseRegex spec cst
  , PrintRegex cst spec'
  , Sym.Compare spec spec' ord
  , ParseRegex'Result ord
  ) =>
  ParseRegex' spec cst

class ParseRegex'Result (result :: Ordering)

instance ParseRegex'Result EQ

else instance
  ( Fail (Text "Unexpected regex parse error. Please report a bug.")
  ) =>
  ParseRegex'Result ord
