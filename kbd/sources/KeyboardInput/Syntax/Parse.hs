{-# LANGUAGE RecordWildCards       #-}

--------------------------------------------------

{-|

-}
module KeyboardInput.Syntax.Parse where

--------------------------------------------------

import Internal.KeyboardInput.Utilities
import Internal.KeyboardInput.Milliseconds
import Internal.KeyboardInput.Tokens

import KeyboardInput.Syntax.Types

import KeyboardInput.Syntax.Config

--------------------------------------------------

import qualified "trifecta" Text.Trifecta           as T

import qualified "parsers"  Text.Parser.Combinators as P
import qualified "parsers"  Text.Parser.Char        as P
import qualified "parsers"  Text.Parser.Token       as P
-- import qualified "parsers"  Text.Parser.LookAhead   as P
-- import qualified "parsers"  Text.Parser.Permutation as P

import           "parsers"  Text.Parser.Char        (CharParsing)
import           "parsers"  Text.Parser.Token       (TokenParsing)

import qualified "charset" Data.CharSet as CharSet

--------------------------------------------------

import qualified "base" Data.Char as Char

--------------------------------------------------

import "spiros" Prelude.Spiros

--------------------------------------------------

{-| Parse an arbitrary key sequence.

@
parseKeySequence = 'parseKeySequenceWith' 'defaultConfig'
@

==

Modifiers are grouped only by hyphenation:

>>> parseKeySequence "C-x b"
Right (KeySequence [KeyChord { modifiers = [Modifier "C"], key = Key "x" }, KeyChord { modifiers = [], key = Key "b" } ])

==

The keys and modifiers may be arbitrary characters or strings:

>>> parseKeySequence "X-Y-z"
Right (KeySequence [KeyChord { modifiers = [Modifier "X",Modifier "Y"], key = Key "z" } ])
>>> parseKeySequence "o O <olive> OIL"
Right (KeySequence [KeyChord { modifiers = [], key = Key "o" },KeyChord { modifiers = [], key = Key "O" },KeyChord { modifiers = [], key = Key "<olive>" },KeyChord { modifiers = [], key = Key " OIL" }])

-}
parseKeySequence :: String -> Either String KeySequence
parseKeySequence = parseKeySequenceWith defaultConfig

--------------------------------------------------

{-| 

-}
parseKeySequenceWith :: Config -> String -> Either String KeySequence
parseKeySequenceWith Config{..} = todo

--------------------------------------------------

{-| 

-}
-- parseActionSequence :: String -> Either String ActionSequence
-- parseActionSequence = _


--------------------------------------------------

{-| 

-}



--------------------------------------------------

pActionSequence = ActionSequence
 <$> sepByWhitespace pAction

--------------------------------------------------

pAction
   = PressAction  <$> pKeyChord
 <|> DelayAction  <$> pDelay
 <|> InsertAction <$> pInsertion

  -- NOTE:
  -- pKeyChord must precede pDelay, since a single (unitlees) digit represents the corresponding key (e.g. the @1 / !@ key), and a digit is a prefix-string of a number (which is several digits).

--------------------------------------------------

-- | Parse a duration: a natural or a real; with units-of-time (milliseconds @"...ms"@ or seconds @"...s"@ only).
--
-- e.g. @"250ms"@ or @"0.250s"@. or @"3s"@.
--
-- NOTE: one millisecond is the smallest resolution. 
-- for example, @"0.000001s"@ is the same as  @"0.001s"@ a.k.a @"1ms"@, since we round up.
-- why this truncation? because in the context of user-input-automation, most delays below a few milliseconds are either: (1) irrelevant, e.g. when an application that needs a delay between keyboard shortcuts events, such short delays aren't long enough; or (2) impossible. e.g. the original motivation for *this* package was the @genovation-control-pad@ package (at <https://hackage.haskell.org/package/genovation-control-pad>); the namesake device is a keypad that can be programmed to send delays between its events; the minimum delay, and the smallest increment, is @4ms@. 
-- 
-- 
pDelay :: forall p. TokenParsing p => p Milliseconds
pDelay = pMilliseconds

 where
 pMilliseconds = (toMilliseconds &flip)
   <$> pNumber
   <*> pTimeUnit

 pNumber :: p Double
 pNumber = P.naturalOrDouble 
   <&> either fromInteger id

  -- NOTE:
  -- naturalOrDouble :: TokenParsing m => m (Either Integer Double)

--------------------------------------------------

pTimeUnit :: CharParsing p => p TimeUnit
pTimeUnit
    = MillisecondsUnit <$ "ms"
  <|> SecondsUnit      <$ "s"

--------------------------------------------------

-- | Parse a literal string.
--
-- e.g. @"\"- sboo\""@
--
pInsertion :: TokenParsing p => p ()
pInsertion = P.stringLiteral

-- pInsertion :: CharParsing p => p ()
-- pInsertion =  
--  P.between "\"" pPhrase "\""

--------------------------------------------------

-- | 
pKeySequence :: CharParsing p => p KeySequence
pKeySequence
  = KeySequence <$> go

  where
  go = sepByWhitespace pKeyChord

--------------------------------------------------

-- | 
pKeyChord :: CharParsing p => p KeyChord
pKeyChord = KeyChord
 <$> pModifier `P.sepBy` (P.char '-') --TODO parameter or signature.
 <*> pKey

--------------------------------------------------

-- | 
pModifier :: CharParsing p => p Modifier
pModifier
  = toModifier <$> go --TODO annotate

  where
  go = P.upper

  toModifier = char2string > Modifier

--------------------------------------------------

-- | 
pKey :: CharParsing p => p Key
pKey
  = Key <$> go

  where
  go = pKeyThreeLetterAbbreviation
   <|> pKeyThreeLetterAbbreviation
   <|> pKeySingleCharacter

--------------------------------------------------

-- | e.g. @"RET"@
pKeyThreeLetterAbbreviation :: CharParsing p => p String
pKeyThreeLetterAbbreviation =
 go <$> P.upper <*> P.upper <*> P.upper
 where
 go x y z = [x,y,z]

--------------------------------------------------

-- | e.g. @"<return>"@
pKeyBracketedString :: CharParsing p => p String
pKeyBracketedString
 = P.between (P.string "<") pWord (P.string ">")

--------------------------------------------------

-- | e.g. @"return"@ or @"kp-return"@.
pWord :: CharParsing p => p String
pWord
 = char2string <$> go

 where
 go =
   P.oneOfSet alphanumericOrSeparator

 alphanumericOrSeparator = 
  alphanumerics CharSet.\\ separators

 alphanumerics =
  CharSet.build Char.isAlphaNum

 separators =
  CharSet.fromList ("-_/+*")

--------------------------------------------------

-- | e.g. @"x"@ or @"X"@, or @"1"@.
pKeySingleCharacter :: CharParsing p => p String
pKeySingleCharacter
 = char2string <$> go

 where
 go =
   P.oneOfSet alphanumerics

 alphanumerics =
  CharSet.build Char.isAlphaNum

--------------------------------------------------

--keysequence :: [Keychord] -> Keysequence
--keychord :: [Modifier] -> Key -> Keychord

-- sepBy :: Alternative m => m a -> m sep -> m [a]

-- oneOfSet :: CharParsing m => CharSet -> m Char

-- spaces :: CharParsing m => m () 
-- space :: CharParsing m => m Char

-- stringLiteral :: forall m s. (TokenParsing m, IsString s) => m s
-- This token parser parses a literal string. Returns the literal string value. This parsers deals correctly with escape sequences and gaps.

-- naturalOrDouble :: TokenParsing m => m (Either Integer Double)
-- This token parser parses either natural or a float. Returns the value of the number. This parsers deals with any overlap in the grammar rules for naturals and floats. 

--------------------------------------------------