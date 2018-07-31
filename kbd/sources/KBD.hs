--------------------------------------------------

{-|

Usage: import this unqualified (or qualified). it defines a subset of this package's API, for simplicity. it has a short (non-hierarchical) name mirroring the package name, for convenience.

e.g.:

@
import "KBD"

emacs_switch_buffer :: Either String 'KeySequence'
emacs_switch_buffer 
 = 'kbd'' "C-x b"

global_transfer_text_between_windows :: Either String 'ActionSequence'
global_transfer_text_between_windows
 = 'kbd' "C-c A-\<tab\> 100ms C-v \"\n\n\" 100ms A-\<tab\>"
@


Doctests:

@
>>> kbd "C-c A-\<tab\> 100ms C-v \"\\n\\n\" 100ms A-\<tab\>"
ActionSequence [PressAction (KeyChord {modifiers = [Modifier \"C\"], key = Key "c"}),PressAction (KeyChord {modifiers = [Modifier \"A\"], key = Key "tab"}),DelayAction (Milliseconds 100),PressAction (KeyChord {modifiers = [Modifier \"C\"], key = Key "v"}),InsertAction "\n\n",DelayAction (Milliseconds 100),PressAction (KeyChord {modifiers = [Modifier \"A\"], key = Key "tab"})]
@

i.e., with the output pretty-printed:

@
ActionSequence
 [ 'PressAction' ('KeyChord' { 'modifiers' = ['Modifier' \"C\"]
                         , 'key'       = 'Key' "c"
                         })

 , 'PressAction' ('KeyChord' { 'modifiers' = ['Modifier' \"A\"]
                         , 'key'       = 'Key' "tab"
                         })

 , 'DelayAction' 100

 , 'PressAction' ('KeyChord' { 'modifiers' = ['Modifier' \"C\"]
                         , 'key'       = 'Key' "v"
                         })

 , 'InsertAction' "\n\n"

 , 'DelayAction' 100

 , 'PressAction' ('KeyChord' { 'modifiers' = ['Modifier' \"A\"]
                         , 'key'       = 'Key' "tab"
                         })
 ]
@


Documentation:

See "KeyboardInput.Syntax" for documention (several pages worth).


-}

module KBD
 ( module KeyboardInput.Syntax.Types
 , module KBD
 ) where

--------------------------------------------------

import Prelude.KeyboardInput
--import Internal.KeyboardInput.Utilities

import KeyboardInput.Syntax.Types
import KeyboardInput.Syntax.Parse
-- import KeyboardInput.Syntax.Create
-- import KeyboardInput.Syntax.Config
-- import KeyboardInput.Syntax

--------------------------------------------------

kbd
  :: (MonadThrow m)
  => String -> m ActionSequence
kbd
  = parseActionSequenceM

--------------------------------------------------

kbd'
  :: (MonadThrow m)
  => String -> m KeySequence
kbd'
  = parseKeySequenceM

--------------------------------------------------