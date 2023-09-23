module Type.Regex.Match where

import Prelude

import Type.Regex.RegexRep (Regex)

class ScanRegex (regex :: Regex) (str :: Symbol) (matches :: Boolean)

-- class RegexMatch (regex :: Regex) (str :: Symbol) (matches :: Bool) 