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

import Prelude

import Type.Proxy (Proxy(..))
import Type.Regex (compileRegex, parse_)
import Type.Regex as Regex
import Type.Regex.Parse (parseCharacterClass, parseRegex)

{-
The following will only compile if the the string matches the regex.
`guard` is similar to `test` which is exists for many regex libraries.
The difference is that instead of returning a boolean,
it reflects the type level input string if the regex matches.

-}

type RegexURL = "^https?://([a-z]+\\.)?[a-z]+\\.[a-z]+(/[a-z_]+)*$"

sample :: String
sample = Regex.guard
  @"^https?://[abc]+\\.(app|com|org)(/[abc]+)*(\\?([abc]+))?$"
  @"https://abc.org/aba/aa?aaaa"

sample2 :: String
sample2 = Regex.guard @"^[^ab]$" @"c"

{-

## Supported regex features

|                    |                          |
| ------------------ | ------------------------ |
| Character literals | `a`, `b`, `c`, ...       |
| Concatenation      | `abc`, `hello`, ...      |
| Groups             | `(abc)`, `(hello)`, ...  |
| Alternatives       | `a|b|c`, `(foo|bar)`     |
| Match Many         | `(foo)*`                 |

-}
