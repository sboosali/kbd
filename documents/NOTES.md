





==================================================
Keys Of A Standard Keyboard

----------------------------------------

ESC (a.k.a <escape>)

<f1>...<f12>

<print>        (labeled "PrtScr SysRq")
<scroll_lock>  (labeled "Scroll Lock")
<pause>        (labeled "Pause Break")

RET (a.k.a <return>)
TAB (a.k.a <tab>)
DEL (a.k.a <backspace>)
??? (a.k.a <delete>)


----------------------------------------

==================================================
Emacs: KeySequence Syntax

http://ergoemacs.org/emacs/gnu_emacs_keybinding.html

----------------------------------------





----------------------------------------

==================================================
Emacs: List Of Known Keys

http://ergoemacs.org/emacs/keystroke_rep.html

----------------------------------------
[modifiers]

Control
"C-"

Meta
"M-"

Alt
"A-"

----------------------------------------
TODO [normal keys]

Escape
"ESC"

----------------------------------------
["dead" keys]

> Microsoft Natural Ergonomic Keyboard 4000, French version.
> The ^ and ¨ (to the right of P) are dead keys.

--------------------

e.g.

    <dead-acute>          Prefix Command
    <dead-tilde>          A Ã
    <dead-grave>          A À
    <dead-circumflex>     1 ¹
    <dead-circum>         1 ¹
    <dead-asciicircum>    1 ¹
    <dead-acute>          A Á
    <dead-diaeresis>      A Ä

-----------------------



----------------------------------------
[Number Pad Keys]

--------------------

e.g.

    <C-S-kp-1>  <C-S-end>
    <C-S-kp-2>  <C-S-down>
    <C-S-kp-3>  <C-S-next>
    …
    <M-kp-next> <M-next>
    <S-kp-down> <S-down>
    <S-kp-end>  <S-end>
    <S-kp-home> <S-home>
    <S-kp-left> <S-left>
    <S-kp-next> <S-next>
    …
    <kp-0>    0
    <kp-1>    1
    <kp-2>    2
    …
    <kp-add>      +
    <kp-decimal>  .
    <kp-delete>   C-d
    <kp-divide>   /
    <kp-down>     <down>
    <kp-end>      <end>

--------------------


----------------------------------------
["special" keys]

--------------------

e.g.

    <escape>	ESC
    
    TAB		forward-button
    <backtab>	backward-button
    <backspace>	DEL
    <return>	RET
    <tab>		TAB
    
    <home>		move-beginning-of-line
    <end>		move-end-of-line
    <insert>	overwrite-mode
    <delete>	C-d
    
    <prior>		scroll-down
    <next>		scroll-up
    
    <lwindow>	ignore
    <rwindow>	ignore
    <menu>		execute-extended-command
    
    <right>		forward-char
    <left>		backward-char
    <up>		previous-line
    <down>		next-line
    
    <f3>		kmacro-start-macro-or-insert-counter
    <f4>		kmacro-end-or-call-macro
    <f10>		menu-bar-open
    <f16>		clipboard-kill-ring-save
    <f18>		clipboard-yank
    <f20>		clipboard-kill-region

> Note: Syntax such as {RET, TAB, DEL, ESC, …}, are actual ASCII characters. While {<return>, <tab>, <delete>, <escape>, …} are special keys.

> Note the {<F16>, <F18>, <F20>}. Some keyboards have function keys up to 24 of them.

--------------------

----------------------------------------
[Mouse (pseudo-)Keys]

--------------------

e.g.

    <mouse-1>       mouse-set-point
    <mouse-2>       help-follow-mouse
    <mouse-3>       mouse-save-then-kill
    <wheel-down>                    mwheel-scroll
    <wheel-up>      mwheel-scroll
    <mouse-movement>                ignore
    <S-down-mouse-1>                mouse-appearance-menu
    <S-mouse-3>     kmacro-end-call-mouse
    <S-wheel-down>  mwheel-scroll
    <S-wheel-up>    mwheel-scroll
    <double-mouse-1>                mouse-set-point
    <triple-mouse-1>                mouse-set-point
    <down-mouse-1>  mouse-drag-region
    <drag-mouse-1>  mouse-set-region
    <drag-n-drop>   w32-drag-n-drop

--------------------


----------------------------------------
[other keys]


----------------------------------------

==================================================
== `deriving` templates

-- nullary type (default)
deriving (Show,Read,Eq,Ord,Lift,Generic,NFData,Hashable)

-- enum-like
deriving (Show,Read,Eq,Ord,Enum,Bounded,Lift,Generic,NFData,Hashable)

-- unary type
deriving (Show,Read,Eq,Ord,Functor,Foldable,Traversable,Generic,NFData,Hashable)

-- string-like
newtype STRINGTYPE = STRINGTYPE String
 deriving (Show,Read,Eq,Ord,Lift,Generic,NFData,Hashable,Semigroup,Monoid,IsString)

-- list-like
newtype LISTTYPE   = LISTTYPE [ITEMTYPE]
 deriving (Show,Read,Eq,Ord,Lift,Generic,NFData,Hashable,Semigroup,Monoid,IsList)

==================================================

