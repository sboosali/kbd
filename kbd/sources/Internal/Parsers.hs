--------------------------------------------------

{-| Extend the @parsers@ package with some utilities.

-}

module Internal.Parsers where

--------------------------------------------------

import qualified "parsers"  Text.Parser.Combinators as P
import qualified "parsers"  Text.Parser.Char        as P
--import qualified "parsers"  Text.Parser.Token       as P

import           "parsers"  Text.Parser.Char        (CharParsing)
--import           "parsers"  Text.Parser.Token       (TokenParsing)

--import qualified "charset" Data.CharSet as CharSet

--------------------------------------------------

import "spiros"     Prelude.Spiros

--------------------------------------------------

{-| "Seal" a parser, by:

* requiring a trailing EOF (end-of-file), via 'P.eof'; and
* accepting leading-or-trailing whitespace (by skipping it), via 'P.spaces'.

A "sealed" parser can't be composed with others (as easily).
Instead, this convenience function prepares a parser to be the
"top-level non-terminal" of the implied grammar,
and thus to be given a particular 'CharParsing' instance's @runParser@.

-}
sealParser
  :: CharParsing p
  => p a
  -> p a
sealParser p
  = (P.spaces *> p <* P.spaces) <* P.eof

--------------------------------------------------

-- | Repeats a parser, separated by whitespace. (see 'P.sepBy').
sepByWhitespace
  :: CharParsing p
  => p a -> p [a]
sepByWhitespace p
  = p `P.sepBy` pWhitespace

-- | Skips any white-space trailing the given parser.
skipTrailingWhitespace
  :: CharParsing p
  => p a -> p a
skipTrailingWhitespace p
  = p <* P.spaces

-- | Skips *one* or more white-space characters. 
pWhitespace :: CharParsing p => p ()
pWhitespace = P.space *> P.spaces

--------------------------------------------------