module Type.Regex where

import Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))
import Type.Regex.Compile (class CompileRegex)
import Type.Regex.Match (class ScanRegex)
import Type.Regex.Parse (class ParseRegex)

class
  TestRegex (regex :: Symbol) (str :: Symbol)

instance
  ( ParseRegex regexStr regexAst
  , CompileRegex regexAst regex
  , ScanRegex regex str
  ) =>
  TestRegex regexStr str

guard :: forall @regex @str. TestRegex regex str => IsSymbol str => String
guard = reflectSymbol (Proxy :: Proxy str)

compileRegex :: forall @spec ast regex. 
  ParseRegex spec ast => CompileRegex ast regex => Proxy regex
compileRegex = Proxy