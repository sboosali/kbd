--------------------------------------------------

{-|

-}

module Internal.KeyboardInput.Utilities where

--------------------------------------------------

import qualified "parsers"  Text.Parser.Combinators as P
import qualified "parsers"  Text.Parser.Char        as P
--import qualified "parsers"  Text.Parser.Token       as P

import           "parsers"  Text.Parser.Char        (CharParsing)
--import           "parsers"  Text.Parser.Token       (TokenParsing)

--import qualified "charset" Data.CharSet as CharSet

--------------------------------------------------

import           "spiros"     Prelude.Spiros

--------------------------------------------------

-- | (trivial)
char2string :: Char -> String
char2string = (:[])

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
  = p <* pWhitespace

-- | Skips *one* or more white-space characters. 
pWhitespace :: CharParsing p => p ()
pWhitespace = P.space *> P.spaces

--------------------------------------------------




--------------------------------------------------