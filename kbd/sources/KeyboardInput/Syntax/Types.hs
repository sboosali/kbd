{-# LANGUAGE DuplicateRecordFields #-}

--------------------------------------------------

{-|



-}
module KeyboardInput.Syntax.Types where

import Internal.KeyboardInput.Milliseconds

--------------------------------------------------

import "spiros" Prelude.Spiros

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

{-|

-}
data KeyBinding a = KeyBinding
 { sequence :: KeySequence
 , action   :: a
 }
 deriving stock    (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Lift,Generic,Generic1)
 deriving anyclass (NFData,Hashable)

--------------------------------------------------

{-|

-}
newtype KeySequence = KeySequence

  [KeyChord]

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid,NFData,Hashable)
  -- deriving anyclass (IsList)

instance IsList KeySequence where
  type Item KeySequence = KeyChord
  fromList = coerce
  toList   = coerce

--------------------------------------------------

{-|

-}
data KeyChord = KeyChord
  { modifiers :: [Modifier] -- Set
  , key       :: Key
  }
  deriving          (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

{-|

-}
newtype Modifier = Modifier

  String

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid,NFData,Hashable)
  -- deriving anyclass (IsString)

instance IsString Modifier where
  fromString = coerce

--------------------------------------------------

{-|

-}
newtype Key = Key

  String

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid,NFData,Hashable)
  -- deriving anyclass (IsString)

instance IsString Key where
  fromString = coerce

--------------------------------------------------