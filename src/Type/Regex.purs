module Type.Regex where

import Prelude

import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Boolean (True)
import Type.Proxy (Proxy(..))
import Type.Regex.Compile (class CompileRegex)
import Type.Regex.Match (class ScanRegex)
import Type.Regex.Parse (class ParseRegex)

class
  TestRegex (regex :: Symbol) (str :: Symbol)

instance
  ( ParseRegex regexStr regexAst
  , CompileRegex regexAst regex
  , ScanRegex regex str True
  , IsSymbol str
  ) =>
  TestRegex regexStr str





stringGuard :: forall @regex @str. TestRegex regex str => IsSymbol str => String
stringGuard = reflectSymbol (Proxy :: Proxy str)