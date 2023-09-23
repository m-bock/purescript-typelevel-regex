module Type.Regex.Match where

import Prim.Boolean (False, True)
import Prim.Symbol as Sym
import Prim.TypeError (class Fail, Text)
import Type.Char (UnsafeMkChar)
import Type.Regex.RegexRep (type (~), Regex)
import Type.Regex.RegexRep as R

class ScanRegex (regex :: Regex) (str :: Symbol)

instance
  ( RegexAttemptGo regex str "" matches
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
  RegexAttemptGo (regex :: Regex) (str :: Symbol) (rest :: Symbol) (matches :: Boolean)
  | regex str -> rest matches

instance RegexAttemptGo R.Nil "" "" True

else instance RegexAttemptGo regex "" "" False

else instance
  ( Sym.Cons head tail str
  , RegexAttemptMatch regex head tail rest matches
  ) =>
  RegexAttemptGo regex str rest matches

--- RegexAttemptMatch

class
  RegexAttemptMatch (regex :: Regex) (head :: Symbol) (tail :: Symbol) (rest :: Symbol) (matches :: Boolean)
  | regex head tail -> rest matches

instance
  RegexAttemptMatch (R.Lit (UnsafeMkChar head) True) head tail tail True

else instance
  ( RegexAttemptMatch regex1 head tail rest matches
  , RegexAttemptMatchCatResult regex2 matches rest rest' matches'
  ) =>
  RegexAttemptMatch (R.Cat regex1 regex2) head tail rest' matches'

else instance
  RegexAttemptMatch regex head tail tail False

--- RegexAttemptMatchCatResult

class
  RegexAttemptMatchCatResult (regex :: Regex) (matched :: Boolean) (restIn :: Symbol) (restOut :: Symbol) (matches :: Boolean)
  | regex matched restIn -> restOut matches

instance
  ( RegexAttemptGo regex restIn restOut matches
  ) =>
  RegexAttemptMatchCatResult regex True restIn restOut matches

else instance
  RegexAttemptMatchCatResult regex False rest rest False
