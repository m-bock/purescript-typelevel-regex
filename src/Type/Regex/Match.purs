module Type.Regex.Match where


import Prim.Boolean (False, True)
import Prim.Symbol as Sym
import Prim.TypeError (class Fail, Text)
import Type.Char (UnsafeMkChar)
import Type.Regex.RegexRep (type (~), Regex)
import Type.Regex.RegexRep as R

class ScanRegex (regex :: Regex) (str :: Symbol)

instance
  ( RegexAttemptGo regex str matches
  , ScanRegexResult matches
  ) =>
  ScanRegex regex str

--- ScanRegexResult

class ScanRegexResult (matches :: Boolean)

instance ScanRegexResult True
instance
  ( Fail (Text "Regex failed to match")
  ) =>
  ScanRegexResult False

--- RegexAttemptGo

class
  RegexAttemptGo (regex :: Regex) (str :: Symbol) (matches :: Boolean)
  | regex str -> matches

instance RegexAttemptGo R.Nil "" True

else instance RegexAttemptGo regex "" False

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

else instance
  RegexAttemptMatch regex head tail False

-- instance () => RegexAttempt (R.Cat regex1 regex2)  "" True

