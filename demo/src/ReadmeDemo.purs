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
import Type.Regex (compileRegex, parse_)
import Type.Regex as Regex
import Type.Regex.Parse (parseRegex)

{-
The following will only compile if the the string matches the regex.
`guard` is similar to `test` which is exists for many regex libraries.
The difference is that instead of returning a boolean,
it reflects the type level input string if the regex matches.

-}

type RegexURL = "^https?://([a-z]+\\.)?[a-z]+\\.[a-z]+(/[a-z_]+)*$"

-- sample :: String
-- sample = Regex.guard @"http(s|)://(a|b|c)(a|b|c)* (de|com|org)(/(a|b|c)(a|b|c)*)*(?q|)" @"https://aaac org/aba/aa?q"

sample2 :: String
sample2 = Regex.guard @"a$" @"a"

-- http(s|):(a|b)

-- http(s|):(a|b)(a|b)

-- p :: Proxy ?a
-- p = parseRegex @"(a|)"

-- p2 :: Proxy ?a
-- p2 = parse_ @"http*"

--"hello(abc|y|(y|zz)j)k"

-- "foo(b|(c|a))bar"

-- c :: Proxy ?a
-- c = compileRegex @"$"

-- email :: String
-- email = Regex.guard @"[a-z]@[a-z]\\.(com|org)" @"joe@doe.com"

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
