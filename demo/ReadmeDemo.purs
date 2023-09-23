{-
# purescript-typelevel-regex

Apply regular expressions to type level strings.

## Features

- Nice error messages
- No runtime overhead

## Examples

Regexes int he following examples are simplified for readability.
In reality would be a bit more complex.
-}

module ReadmeDemo where

import Type.Regex as Regex

{-
The following will only compile if the the string matches the regex.
`guard` is similar to `test` which is exists for many regex libraries.
The difference is that instead of returning a boolean,
it reflects the type level input string if the regex matches.

-}

simple :: String
simple = Regex.guard @"hello" @"hello"

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
