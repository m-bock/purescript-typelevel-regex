module Type.Regex.Match where

import Prim.Boolean (False, True)
import Prim.Symbol as Sym
import Prim.TypeError (class Fail, Above, Beside, Text)
import Type.Char (UnsafeMkChar)
import Type.Data.Boolean (class And, class Not, class Or)
import Type.Regex.RegexRep (type (~), Regex)
import Type.Regex.RegexRep as R

class ScanRegex (regex :: Regex) (str :: Symbol)

instance
  ( RegexAttemptGo regex str rest matches
  , ScanRegexResult rest matches
  ) =>
  ScanRegex (R.StartOfStr ~ regex) str

-- instance
--   ( RegexAttemptGo regex str rest matches
--   , ScanRegexResult rest matches
--   ) =>
--   ScanRegex regex str

--- ScanRegexResult

class ScanRegexResult (rest :: Symbol) (matches :: Boolean)

instance ScanRegexResult rest True

else instance
  ( Fail
      ( Above
          (Text "Regex failed to match.")
          (Beside (Text "Unmateched rest is: ") (Text rest))
      )
  ) =>
  ScanRegexResult rest matches

--- RegexAttemptGo

class
  RegexAttemptGo (regex :: Regex) (str :: Symbol) (rest :: Symbol) (matches :: Boolean)
  | regex str -> rest matches

instance regexAttemptGoRegexEnd ::
  RegexAttemptGo R.Nil str str True

else instance regexAttemptGoStringEnd ::
  ( IsFinalRegex regex True
  ) =>
  RegexAttemptGo regex "" "" True

else instance regexAttemptGoStringContinue ::
  ( Sym.Cons head tail str
  , RegexAttemptMatch regex head tail rest matches
  ) =>
  RegexAttemptGo regex str rest matches

--- IsFinalRegex

class
  IsFinalRegex (regex :: Regex) (bool :: Boolean)
  | regex -> bool

instance IsFinalRegex R.EndOfStr True

else instance IsFinalRegex R.Nil True

else instance
  ( IsFinalRegex regex1 bool1
  , IsFinalRegex regex2 bool2
  , Or bool1 bool2 bool
  ) =>
  IsFinalRegex (R.Alt regex1 regex2) bool

else instance
  ( IsFinalRegex regex1 bool1
  , IsFinalRegex regex2 bool2
  , And bool1 bool2 bool
  ) =>
  IsFinalRegex (R.Cat regex1 regex2) bool

else instance
  IsFinalRegex (R.Many regex) True

else instance
  IsFinalRegex regex False

--- RegexAttemptMatch

class
  RegexAttemptMatch (regex :: Regex) (head :: Symbol) (tail :: Symbol) (rest :: Symbol) (matches :: Boolean)
  | regex head tail -> rest matches

instance
  RegexAttemptMatch (R.Lit (UnsafeMkChar head)) head tail tail True

else instance
  ( Contains chars head matches
  , Not matches matches'
  ) =>
  RegexAttemptMatch (R.NotLits chars) head tail tail matches'

else instance
  ( Contains chars head matches
  ) =>
  RegexAttemptMatch (R.Lits chars) head tail tail matches

else instance
  ( Sym.Cons head tail str
  ) =>
  RegexAttemptMatch R.Nil head tail str True

else instance
  ( Sym.Cons head tail str
  , RegexAttemptGo regex1 str rest matches
  , RegexAttemptMatchCatResult regex2 matches rest rest' matches'
  ) =>
  RegexAttemptMatch (R.Cat regex1 regex2) head tail rest' matches'

else instance
  ( Sym.Cons head tail str
  , RegexAttemptGo regex1 str restLeft matches

  , RegexAttemptMatchAltResult regex2 matches restLeft str rest' matches'
  ) =>
  RegexAttemptMatch (R.Alt regex1 regex2) head tail rest' matches'

else instance
  ( Sym.Cons head tail sym
  , RegexAttemptGo regex sym restIn matches
  , RegexManyResult matches regex sym restIn rest' matches'
  ) =>
  RegexAttemptMatch (R.Many regex) head tail rest' matches'

else instance
  ( Sym.Cons head tail rest
  ) =>
  RegexAttemptMatch R.Never head tail rest False

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

--- RegexAttemptMatchAltResult

class
  RegexAttemptMatchAltResult (regex :: Regex) (matched :: Boolean) (restInLeft :: Symbol) (restInRight :: Symbol) (restOut :: Symbol) (matches :: Boolean)
  | regex matched restInLeft restInRight -> restOut matches

instance
  RegexAttemptMatchAltResult regex True rest restInRight rest True

else instance
  ( RegexAttemptGo regex restIn restOut matches
  ) =>
  RegexAttemptMatchAltResult regex False restInLeft restIn restOut matches

--- RegexManyResult

class
  RegexManyResult
    (matched :: Boolean)
    (regex :: Regex)
    (backtrace :: Symbol)
    (restIn :: Symbol)
    (restOut :: Symbol)
    (matches :: Boolean)
  | matched regex backtrace restIn -> restOut matches

instance regexManyResultContinue ::
  ( RegexAttemptGo (R.Many regex) restIn restOut matches
  ) =>
  RegexManyResult True regex backtrace restIn restOut matches

else instance regexManyResultStop ::
  RegexManyResult False regex backtrace restIn backtrace True

------------------------------------------------------------------------
-- Contains
------------------------------------------------------------------------

class
  Contains (chars :: Symbol) (char :: Symbol) (result :: Boolean)
  | chars char -> result

instance Contains "" char False

else instance
  ( Sym.Cons head tail sym
  , ContainsMatch head tail char result
  ) =>
  Contains sym char result

--- ContainsMatch

class
  ContainsMatch (head :: Symbol) (tail :: Symbol) (char :: Symbol) (result :: Boolean)
  | head tail char -> result

instance ContainsMatch char tail char True

else instance
  ( Contains tail char result
  ) =>
  ContainsMatch head tail char result

