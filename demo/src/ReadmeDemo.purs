{-
# purescript-typelevel-regex

Apply regular expressions to type level strings.

## Features

- Nice error messages
- No runtime overhead

## Examples

Regexes in the following examples are simplified.
In reality would be a bit more complex.
-}

module ReadmeDemo where

import Type.Proxy (Proxy(..))
import Type.Regex (compileRegex)
import Type.Regex as Regex
import Type.Regex.Parse (parseRegex)

{-
The following will only compile if the the string matches the regex.
`guard` is similar to `test` which is exists for many regex libraries.
The difference is that instead of returning a boolean,
it reflects the type level input string if the regex matches.

-}

type RegexURL = "^https?://([a-z]+\\.)?[a-z]+\\.[a-z]+(/[a-z_]+)*$"

simple :: String
simple = Regex.guard @"hello|world|abc" @"abc"

-- p :: Proxy ?a
-- p = parseRegex @"(a|b)"

-- c :: Proxy ?a
-- c = compileRegex @"(a|(cd))"

-- email :: String
-- email = Regex.guard @"[a-z]@[a-z]\\.(com|org)" @"joe@doe.com"

{-

## Supported regex features

|                    |                     |
| ------------------ | ------------------- |
| Character literals | `a`, `b`, `c`, ...  |
| Concatenation      | `abc`, `hello`, ... |
|                    |                     |

-}
