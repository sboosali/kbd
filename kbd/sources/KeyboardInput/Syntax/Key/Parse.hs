{-# LANGUAGE RecordWildCards       #-}

--------------------------------------------------

{-|

TODO

@
>>> pKeySequence
@

-}
module KeyboardInput.Syntax.Key.Parse where

--------------------------------------------------

import Prelude.KeyboardInput

--------------------------------------------------

import Internal.Parsers
import Internal.Trifecta

import Internal.KeyboardInput.Utilities

--------------------------------------------------

import KeyboardInput.Syntax.Key.Types

import KeyboardInput.Syntax.Config

--------------------------------------------------

import qualified "trifecta" Text.Trifecta           as T

import qualified "parsers"  Text.Parser.Combinators as P
import qualified "parsers"  Text.Parser.Char        as P

import           "parsers"  Text.Parser.Char        (CharParsing)
import           "parsers"  Text.Parser.Token       (TokenParsing)

import qualified "charset" Data.CharSet as CharSet

--------------------------------------------------

import qualified "base" Data.Char as Char

--------------------------------------------------

{-| Parse an arbitrary key sequence.

@
parseKeySequence = 'parseKeySequenceWith' 'defaultConfig'
@

==

Modifiers are grouped only by hyphenation:

>>> parseKeySequenceM "C-x b"
KeySequence [KeyChord { modifiers = [Modifier "C"], key = Key "x" }, KeyChord { modifiers = [], key = Key "b" } ]

==

The keys and modifiers may be arbitrary characters or strings:

>>> parseKeySequenceM "X-Y-z"
KeySequence [KeyChord { modifiers = [Modifier "X",Modifier "Y"], key = Key "z" } ]

>>> parseKeySequenceM "o O <olive> OIL"
KeySequence [KeyChord { modifiers = [], key = Key "o" },KeyChord { modifiers = [], key = Key "O" },KeyChord { modifiers = [], key = Key "<olive>" },KeyChord { modifiers = [], key = Key " OIL" }]

-}
parseKeySequence :: String -> Either String KeySequence
parseKeySequence
  = parseKeySequenceWith defaultConfig

--------------------------------------------------

{-| 

-}
parseKeySequenceWith :: Config -> String -> Either String KeySequence
parseKeySequenceWith Config{..}
  = T.parseString pSealedKeySequence mempty
  > result2either

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
  = parseKeySequenceWithM defaultConfig

-- | Generalization of 'parseKeySequenceWith'.
-- 
-- (See 'parseKeySequence' for documention).
parseKeySequenceWithM
  :: (MonadThrow m)
  => Config
  -> String
  -> m KeySequence
parseKeySequenceWithM config
  = parseKeySequenceWith config
  > either throwL return

-- | Convenience function.
--
-- (See 'parseKeySequence' for documention).
parseKeySequenceIO :: String -> IO ()
parseKeySequenceIO
  = T.parseTest pSealedKeySequence --TODO Config

--------------------------------------------------

{-| a "sealed" 'pKeySequence', via 'sealParser'.

-}
pSealedKeySequence :: forall p. TokenParsing p => p KeySequence
pSealedKeySequence
  = sealParser pKeySequence

--------------------------------------------------

-- | a whitespace-separated list of 'pKeyChord'.
pKeySequence :: CharParsing p => p KeySequence
pKeySequence
  = KeySequence <$> go

  where
  go = sepByWhitespace pKeyChord

--------------------------------------------------

-- | a 'pKey', optionially preceded by a hyphen-separated list of 'pModifier'.
pKeyChord :: CharParsing p => p KeyChord
pKeyChord = KeyChord
 <$> P.try (pModifier `P.endBy` (P.char '-')) --TODO parameter or signature.
 <*> pKey

--------------------------------------------------

-- | a single, upper-case letter.
--
-- via 'P.upper'.
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

-- | e.g. @\"RET\"@
pKeyThreeLetterAbbreviation :: CharParsing p => p String
pKeyThreeLetterAbbreviation =
 go <$> P.upper <*> P.upper <*> P.upper
 where
 go x y z = [x,y,z]

--------------------------------------------------

-- | e.g. @"\<return\>"@
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

-- | e.g. @"x"@ or @\"X\"@, or @"1"@.
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