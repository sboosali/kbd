{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}

{-# LANGUAGE DeriveAnyClass        #-}

--------------------------------------------------

{-|

-}
module KeyboardInput.Syntax.Parse where

--------------------------------------------------

import KeyboardInput.Syntax.Types

--------------------------------------------------

import qualified "trifecta" Text.Trifecta           as T

import qualified "parsers"  Text.Parser.Combinators as P
import qualified "parsers"  Text.Parser.Char        as P
import qualified "parsers"  Text.Parser.Token       as P
import qualified "parsers"  Text.Parser.LookAhead   as P
import qualified "parsers"  Text.Parser.Permutation as P

--------------------------------------------------

import "spiros" Prelude.Spiros

--------------------------------------------------

{-| Parse an arbitrary key sequence.

Modifiers are grouped only by hyphenation:

>>> parseKeySequence "C-x b"
Right (KeySequence [KeyChord { modifiers = [Modifier "C"], key = Key "x" }, KeyChord { modifiers = [], key = Key "b" } ])

The keys and modifiers may be arbitrary characters or strings:

>>> parseKeySequence "X-Y-z"
Right (KeySequence [KeyChord { modifiers = [Modifier "X",Modifier "Y"], key = Key "z" } ])
>>> parseKeySequence "o O <olive> OIL"
Right (KeySequence [KeyChord { modifiers = [], key = Key "o" },KeyChord { modifiers = [], key = Key "O" },KeyChord { modifiers = [], key = Key "<olive>" },KeyChord { modifiers = [], key = Key " OIL" }])

-}
parseKeySequence :: String -> Either String KeySequence
parseKeySequence = _

--------------------------------------------------

{-| 

-}



--------------------------------------------------

{-| 

-}
-- parseActionSequence :: String -> Either String ActionSequence
-- parseActionSequence = _


--------------------------------------------------

{-| 

-}


--------------------------------------------------
--keysequence :: [Keychord] -> Keysequence
--keychord :: [Modifier] -> Key -> Keychord