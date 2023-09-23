module Type.Regex.Match where

import Prelude

import Prim.Boolean (True)
import Prim.Symbol as Sym
import Type.Char (UnsafeMkChar)
import Type.Regex.RegexRep (type (~), Regex)
import Type.Regex.RegexRep as R

class ScanRegex (regex :: Regex) (str :: Symbol) (matches :: Boolean)

instance
  ( RegexAttemptGo regex str matches
  ) =>
  ScanRegex regex str matches

--- RegexAttemptGo

class
  RegexAttemptGo (regex :: Regex) (str :: Symbol) (matches :: Boolean)
  | regex str -> matches

instance RegexAttemptGo R.Nil "" True

else instance
  ( Sym.Cons head tail str
  , RegexAttemptMatch regex head tail matches
  ) =>
  RegexAttemptGo regex str matches

--- RegexAttemptMatch

class
  RegexAttemptMatch (regex :: Regex) (head :: Symbol) (tail :: Symbol) (matches :: Boolean)
  | regex head tail -> matches

instance
  ( RegexAttemptGo regex tail matches
  ) =>
  RegexAttemptMatch (R.Lit (UnsafeMkChar head) True ~ regex) head tail matches

-- instance () => RegexAttempt (R.Cat regex1 regex2)  "" True

