{-# LANGUAGE RecordWildCards       #-}

--------------------------------------------------

{-|

>>> pActionSequence

-}
module KeyboardInput.Syntax.Parse where

--------------------------------------------------

import Internal.Trifecta
import Internal.KeyboardInput.Utilities
import Internal.KeyboardInput.Milliseconds
--import Internal.KeyboardInput.Tokens

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

-- | Generalization of 'parseKeySequence'.
-- 
-- Generalizes the output from being in the @('Either' _ ...)@ monad
-- to any @('MonadThrow' m) => m ...@.
--
-- (See 'parseKeySequence' for documention).
parseKeySequenceM
  :: (MonadThrow m)
  => String -> m KeySequence
parseKeySequenceM
  = parseKeySequence
  > either throwL return

-- | Convenience function.
--
-- (See 'parseKeySequence' for documention).
parseKeySequenceIO :: String -> IO ()
parseKeySequenceIO
  = T.parseTest pEOFKeySequence

--------------------------------------------------

{-| 

-}
parseKeySequenceWith :: Config -> String -> Either String KeySequence
parseKeySequenceWith Config{..}
  = T.parseString pEOFKeySequence mempty
  > result2either

--------------------------------------------------

{-| 

-}
-- parseActionSequence :: String -> Either String ActionSequence
-- parseActionSequence = _


{-| Parse an arbitrary key-or-action sequence. See 'parseKeySequence'.

@
parseActionSequence = 'parseActionSequenceWith' 'defau+ltConfig'
@

==

>>> parseActionSequence "C-x b 100ms \"*shell*\" RET"
Right (ActionSequence [PressAction (KeyChord { modifiers = [Modifier "C"], key = Key "x" }),PressAction (KeyChord { modifiers = [], key = Key "b" }),DelayAction (Milliseconds 100),InsertAction ",PressAction (KeyChord { modifiers = [], key = Key "RET" })])

-}
parseActionSequence :: String -> Either String ActionSequence
parseActionSequence = parseActionSequenceWith defaultConfig

--------------------------------------------------

-- | Generalization of 'parseActionSequence'.
-- 
-- Generalizes the output from being in the @('Either' _ ...)@ monad
-- to any @('MonadThrow' m) => m ...@.
--
-- (See 'parseActionSequence' for documention).
parseActionSequenceM
  :: (MonadThrow m)
  => String -> m ActionSequence
parseActionSequenceM
  = parseActionSequence
  > either throwL return

-- | Convenience function.
--
-- (See 'parseActionSequence' for documention).
parseActionSequenceIO :: String -> IO ()
parseActionSequenceIO
  = T.parseTest pEOFActionSequence

-- parseActionSequenceIO :: String -> String -> IO ()
-- parseActionSequenceIO title input
--     = T.parseTest pActionSequence title input
--   <&> result2either

--------------------------------------------------

{-| 

-}
parseActionSequenceWith :: Config -> String -> Either String ActionSequence
parseActionSequenceWith Config{..}
  = T.parseString pEOFActionSequence mempty
  > result2either

--------------------------------------------------

{-| 

-}
pEOFKeySequence :: forall p. TokenParsing p => p KeySequence
pEOFKeySequence = p
  where
  p = pKeySequence <* P.eof

--------------------------------------------------


{-| 

-}
pEOFActionSequence :: forall p. TokenParsing p => p ActionSequence
pEOFActionSequence = p
  where
  p = pActionSequence <* P.eof

--------------------------------------------------

pActionSequence :: forall p. TokenParsing p => p ActionSequence
pActionSequence = ActionSequence
 <$> some pAction

-- pActionSequence = ActionSequence
--  <$> (pAction `P.sepBy` P.space)

--------------------------------------------------

pAction :: forall p. TokenParsing p => p Action
pAction
   = empty
 <|> P.token (P.try (DelayAction  <$> pDelay))
 <|>          P.try (InsertAction <$> pInsertion)
 <|> P.token (P.try (PressAction  <$> pKeyChord))

  -- NOTE:
  -- pKeyChord must precede pDelay, since a single (unitlees) digit represents the corresponding key (e.g. the @1 / !@ key), and a digit is a prefix-string of a number (which is several digits).



-- pAction
--    = empty
--  <|> P.try (DelayAction  <$> pDelay)
--  <|> P.try (InsertAction <$> pInsertion)
--  <|> P.try (PressAction  <$> pKeyChord)

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
    = MillisecondsUnit <$ P.string "ms"
  <|> SecondsUnit      <$ P.string "s"

--------------------------------------------------

-- | Parse a literal string.
--
-- e.g. @"\"- sboo\""@
--

pInsertion :: TokenParsing p => p String
pInsertion =
  P.stringLiteral
  
-- pInsertion :: TokenParsing p => p String
-- pInsertion = P.stringLiteral

-- pInsertion :: CharParsing p => p ()
-- pInsertion =  
--  P.between "\"" "\"" pPhrase

-- pInsertion :: CharParsing p => p String
-- pInsertion =
--   pPhrase `P.surroundedBy` (P.char '"')

--------------------------------------------------

--
-- e.g. @"- sboo"@
--
pPhrase :: CharParsing p => p String
pPhrase = go

 where
 go =
   some pNonQuote

 pNonQuote = --TODO escaping
  P.noneOfSet (CharSet.singleton '"')

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
 <$> P.try (pModifier `P.endBy` (P.char '-')) --TODO parameter or signature.
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
  go = P.try pKeyBracketedString
   <|> P.try pKeyThreeLetterAbbreviation
   <|> P.try pKeySingleCharacter

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
 = P.between (P.string "<") (P.string ">") pBracketableWord

--------------------------------------------------

-- | e.g. @"return"@ or @"kp-return"@.
pBracketableWord :: CharParsing p => p String
pBracketableWord
 = go

 where
 go =
   some pAlphanumericOrSeparator

 pAlphanumericOrSeparator =
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

-- 
--
-- throwN
--   :: (MonadThrow m)
--   => Name -> String -> m a
--
-- throwL
--   :: (MonadThrow m, HasCallStack)
--   => String -> m a
--

--

--------------------------------------------------