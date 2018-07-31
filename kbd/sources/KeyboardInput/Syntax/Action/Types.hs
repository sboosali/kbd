--------------------------------------------------

{-|



-}

module KeyboardInput.Syntax.Action.Types where

--------------------------------------------------

import Prelude.KeyboardInput

----------------------------------------

import KeyboardInput.Syntax.Key.Types

import Internal.KeyboardInput.Milliseconds

--------------------------------------------------

--------------------------------------------------

{-|

-}
newtype ActionSequence = ActionSequence

  [Action]

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid,NFData,Hashable)
  -- deriving anyclass (IsList)

instance IsList ActionSequence where
  type Item ActionSequence = Action
  fromList = coerce
  toList   = coerce

--------------------------------------------------

{-|

-}
data Action

  = PressAction  KeyChord
  | InsertAction String
  | DelayAction  Milliseconds

  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------