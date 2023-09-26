module Type.Regex where

import Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))
import Type.Regex.Compile (class CompileRegex)
import Type.Regex.Match (class MatchRegex)
import Type.Regex.Parse (class ParseRegex)
import Type.Regex.Print (class PrintRegex)

class
  TestRegex (regex :: Symbol) (str :: Symbol)

instance
  ( ParseRegex regexStr regexAst
  , CompileRegex regexAst regex
  , MatchRegex regex str
  ) =>
  TestRegex regexStr str

guard :: forall @regex @str. TestRegex regex str => IsSymbol str => String
guard = reflectSymbol (Proxy :: Proxy str)

compileRegex
  :: forall @spec ast regex
   . ParseRegex spec ast
  => CompileRegex ast regex
  => Proxy regex
compileRegex = Proxy

parse_
  :: forall @spec spec2 ast
   . ParseRegex spec ast
  => PrintRegex ast spec2
  => Proxy spec2
parse_ = Proxy
