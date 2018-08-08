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
import qualified "parsers"  Text.Parser.Token       as P

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

non-modifier keys may be uppercase:

>>> parseKeySequenceM "C-A"
KeySequence [KeyChord {modifiers = [Modifier "C"], key = Key "A"}]

which, for consumers\/interpreters of these keybindings, should be equivalent to:

>>> parseKeySequenceM "C-S-a"
KeySequence [KeyChord {modifiers = [Modifier "C",Modifier "S"], key = Key "a"}]

==

The keys and modifiers may be arbitrary characters or strings:

>>> parseKeySequenceM "X-Y-z"
KeySequence [KeyChord { modifiers = [Modifier "X",Modifier "Y"], key = Key "z" } ]

>>> parseKeySequenceM "o O <olive> OIL"
KeySequence [KeyChord { modifiers = [], key = Key "o" },KeyChord { modifiers = [], key = Key "O" },KeyChord { modifiers = [], key = Key "olive" },KeyChord { modifiers = [], key = Key "OIL" }]

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
pKeySequence :: TokenParsing p => p KeySequence
pKeySequence
  = KeySequence <$> go

  where
  go = some pKeyChord

--------------------------------------------------

-- | a 'pKey', optionially preceded by a hyphen-separated list of 'pModifier'.
pKeyChord :: forall p. TokenParsing p => p KeyChord
pKeyChord = P.token go

 where
 go = KeyChord
  <$> pOptionalModifiers
  <*> pKey
  P.<?> "Key-Chord"

 pOptionalModifiers :: p [Modifier]
 pOptionalModifiers
   = many pModifier

 -- pOptionalModifiers
 --   = pModifier `P.endBy` pSeparator

 -- <$> P.try (pModifier `P.endBy` (P.char '-')) --TODO the hyphen: parameter or signature.

--------------------------------------------------

-- | a single, upper-case letter.
--
-- via 'P.upper'.
pModifier :: forall p. CharParsing p => p Modifier
pModifier
  = P.try (go <* pSeparator)
 
  where
  go
    = toModifier
     <$> P.upper
     P.<?> "Modifier"

  pSeparator :: p Char
  pSeparator
   = (P.char '-')

  toModifier
    = char2string > Modifier

  --   = toModifier <$> P.try go --TODO annotate
  --    P.<?> "Modifier"
  -- go = P.upper
  --   <* P.notFollowedBy (P.alphaNum)


--------------------------------------------------

{-|

e.g.:

* @x@
* @X@
* @9@
* @RET@
* @<return>@
* @'\n'@

-}
pKey :: TokenParsing p => p Key
pKey
   = Key <$> go
   --  NOTE `pKey` gets no label (i.e. `P.<?>`).
   -- why? to expose each of its alternatives's (i.e. `<|>`) labels.

  where
  go = P.try pKeyBracketedString
   <|> P.try pKeyThreeLetterAbbreviation
   <|> P.try pKeyLiteralCharacter
   <|> P.try pKeyModifier
   <|> P.try pKeySingleCharacter

--------------------------------------------------

-- | e.g. @"\<return\>"@
pKeyBracketedString :: TokenParsing p => p String
pKeyBracketedString
  = P.token p
    P.<?> "key name (e.g. <return>)"

  where
  p = P.between (P.string "<") (P.string ">") pBracketableWord

--------------------------------------------------

-- | e.g. @\"RET\"@
pKeyThreeLetterAbbreviation :: TokenParsing p => p String
pKeyThreeLetterAbbreviation
  = P.token p
    P.<?> "key abbreviation (e.g. RET)"

  where
  p = go <$> P.upper <*> P.upper <*> P.upper
  go x y z = [x,y,z]

--------------------------------------------------

-- | e.g. @"x"@ or @\"X\"@, or @"1"@.
pKeySingleCharacter :: TokenParsing p => p String
pKeySingleCharacter
  = P.token p
    P.<?> "key alphanumeric-character (e.g. x or X or 1)"

  where
  p =
    char2string <$> go

  go =
    P.oneOfSet alphanumerics

  alphanumerics =
    CharSet.build Char.isAlphaNum

--------------------------------------------------

-- | e.g. @"'x'"@ or @"'\\t'"@.
pKeyLiteralCharacter :: TokenParsing p => p String
pKeyLiteralCharacter
  = p
    P.<?> "printable character-literal (e.g. '\\n')"

  where
  p = char2string <$> P.charLiteral

--------------------------------------------------

{- | a direction letter; followed (immediately) by a single, upper-case letter.

e.g.

* @dC@ means: "press down the Control key"; @"d"@ for "down".
* @uC@ means: "release the Control key"; @"u"@ for "up".

-}
pKeyModifier :: forall p. TokenParsing p => p String
pKeyModifier
  = P.try p
    P.<?> "pressed-or-released modifier key (e.g. dC or uC; meaning \"control down\" or \"control up\", respectively)"

  where
  p = go <$> pDirection <*> pKeyModifier_

  go x y = [x,y]

  pDirection
      = P.char 'd'
    <|> P.char 'u'

  pKeyModifier_ = P.upper

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
  CharSet.fromList ("-_/|;:!@#$%^&*+") --TODO steal some syntax for richer token?

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