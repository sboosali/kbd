{-# LANGUAGE RecordWildCards       #-}

--------------------------------------------------

{-|

TODO

@
>>> pActionSequence
@

-}
module KeyboardInput.Syntax.Action.Parse where

--------------------------------------------------

import Prelude.KeyboardInput

--------------------------------------------------

import Internal.Parsers
import Internal.Trifecta

--import Internal.KeyboardInput.Utilities
import Internal.KeyboardInput.Milliseconds

--------------------------------------------------

--import KeyboardInput.Syntax.Key.Types
import KeyboardInput.Syntax.Action.Types

import KeyboardInput.Syntax.Key.Parse

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

--import qualified "base" Data.Char as Char

--------------------------------------------------

{-| Parse an arbitrary key-or-action sequence. See 'parseKeySequence'.

@
parseActionSequence = 'parseActionSequenceWith' 'defau+ltConfig'
@

==

>>> parseActionSequenceM "C-x b 100ms \"*shell*\" RET"
ActionSequence [PressAction (KeyChord { modifiers = [Modifier "C"], key = Key "x" }),PressAction (KeyChord { modifiers = [], key = Key "b" }),DelayAction (Milliseconds 100),InsertAction ",PressAction (KeyChord { modifiers = [], key = Key "RET" })]

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

--------------------------------------------------

-- | Convenience function.
--
-- (See 'parseActionSequence' for documention).

parseActionSequenceIO :: String -> IO ()
parseActionSequenceIO
  = T.parseTest pSealedActionSequence

-- parseActionSequenceIO :: String -> String -> IO ()
-- parseActionSequenceIO title input
--     = T.parseTest pActionSequence title input
--   <&> result2either

--------------------------------------------------

{-| 

-}

parseActionSequenceWith :: Config -> String -> Either String ActionSequence
parseActionSequenceWith Config{..}
  = T.parseString pSealedActionSequence mempty
  > result2either

--------------------------------------------------

{-| 

-}

pSealedActionSequence :: forall p. TokenParsing p => p ActionSequence
pSealedActionSequence
  = sealParser pActionSequence

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
 P.<?> "Action"

  -- NOTE:
  -- pKeyChord must precede pDelay, since a single (unitlees) digit represents the corresponding key (e.g. the @1 / !@ key), and a digit is a prefix-string of a number (which is several digits).

-- pAction
--    = empty
--  <|> P.try (DelayAction  <$> pDelay)
--  <|> P.try (InsertAction <$> pInsertion)
--  <|> P.try (PressAction  <$> pKeyChord)

--------------------------------------------------

{-| Parse a duration.

This duration:

* is either a natural number or a real number;
* must include a unit-of-time.

These units-of-time are either:

* milliseconds @"...ms"@; or
* seconds @"...s"@.

e.g. @"250ms"@ or @"0.250s"@. or @"3s"@.

NOTE: one millisecond is the smallest resolution. 
for example, @"0.000001s"@ is the same as  @"0.001s"@ a.k.a @"1ms"@, since we round up.
why this truncation? because in the context of user-input-automation, most delays below a few milliseconds are either: (1) irrelevant, e.g. when an application that needs a delay between keyboard shortcuts events, such short delays aren't long enough; or (2) impossible. e.g. the original motivation for *this* package was the @genovation-control-pad@ package (at <https://hackage.haskell.org/package/genovation-control-pad>); the namesake device is a keypad that can be programmed to send delays between its events; the minimum delay, and the smallest increment, is @4ms@. 

-}

pDelay :: forall p. TokenParsing p => p Milliseconds
pDelay = pMilliseconds
 P.<?> "Duration"

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

-- | Parse a unit-of-time abbreviation.
--
--

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
pInsertion
  = P.stringLiteral
     P.<?> "Double-Quoted String"

-- pInsertion :: TokenParsing p => p String
-- pInsertion = P.stringLiteral

-- pInsertion :: CharParsing p => p ()
-- pInsertion =  
--  P.between "\"" "\"" pPhrase

-- pInsertion :: CharParsing p => p String
-- pInsertion =
--   pPhrase `P.surroundedBy` (P.char '"')

--------------------------------------------------

-- |
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



--------------------------------------------------
