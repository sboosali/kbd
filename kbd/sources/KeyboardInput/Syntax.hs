--------------------------------------------------

{-|





e.g. keysequences:

@

C-x b

M-%
M-S-5

M-!
M-S-1

@



modifiers. the possible prefixes.

universal modifiers:

@

Control
"C-"

Alt
"A-"

Shift
"S-"

@



"virtual" modifiers:

@

Meta
"M-"

Hyper
"H-"

@


keyboard-specific modifiers:

@

the Command key (Apple)
"D-"

the Win key (Windows)
"W-"

@



e.g. numpad keys:

@
--
<kp-0>
…
<kp-9>
--
<kp-next>
<kp-down>
<kp-end>
<kp-home>
<kp-left>
<kp-next>
--
<kp-add>
<kp-decimal>
<kp-delete>
<kp-divide>
<kp-down>
<kp-end>
@


e.g. keys that have capitalized aliases:

@
ESC a.k.a. <escape>
TAB a.k.a. <tab>
DEL a.k.a. <backspace>
RET a.k.a. <return>
@


e.g. other keys that are specially-named and\/or aliased:

@
<home>
<end>
<insert>
<delete>
    
<prior>
<next>
    
<right>
<left>
<up>
<down>
    
<lwindow>
<rwindow>
<menu>
    
<f1>
…
<f12>
…
<f20>
@


-}

module KeyboardInput.Syntax 
 ( module KeyboardInput.Syntax.Types
 , module KeyboardInput.Syntax.Parse
 , module KeyboardInput.Syntax.Create
 , module KeyboardInput.Syntax.Config
 -- , module KeyboardInput.Syntax
 ) where

--------------------------------------------------

import KeyboardInput.Syntax.Types
import KeyboardInput.Syntax.Parse
import KeyboardInput.Syntax.Create
import KeyboardInput.Syntax.Config
-- import KeyboardInput.Syntax

--------------------

import "spiros" Prelude.Spiros hiding (Text)
--import "spiros" Prelude.Spiros hiding (Text)

--TODO different subsets of re-exports (like one without text).
--TODO signatures for both Text-versus-String-versus-Bytestring and Lazy-versus-Strict.

--------------------

--import "text" Data.Text (Text)

--------------------

--------------------------------------------------