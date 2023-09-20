module Test.Type.Regex.ParseSpec where

import Prelude

import Type.Proxy (Proxy(..))
import Type.Regex.Parse (parseRegex)
import Type.Regex.Types (Nil)


test1 :: Proxy Nil
test1 = parseRegex @""


testx :: Proxy ?a
testx = parseRegex @"(a())" 