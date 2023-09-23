module Type.Regex where

import Prelude

import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Boolean (True)
import Type.Proxy (Proxy(..))
import Type.Regex.Compile (class CompileRegex)
import Type.Regex.Match (class ScanRegex)
import Type.Regex.Parse (class ParseRegex)

class
  C (regex :: Symbol) (str :: Symbol)
  where
  f :: Proxy regex -> Proxy str -> String

instance
  ( ParseRegex regexStr regexAst
  , CompileRegex regexAst regex
  , ScanRegex regex str True
  , IsSymbol str
  ) =>
  C regexStr str
  where
  f _ _ = reflectSymbol (Proxy :: Proxy str)

g :: forall @regex @str. C regex str => String
g = f (Proxy :: Proxy regex) (Proxy :: Proxy str)