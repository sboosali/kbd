{-# LANGUAGE DuplicateRecordFields #-}

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}

--------------------------------------------------

{-|



-}
module KeyboardInput.Syntax.Types where

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
newtype Milliseconds = Milliseconds

  Natural

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Num,Real,Enum,Ix,Bits,Eq,Ord)
  deriving newtype  (NFData,Hashable)

-- | Addition.
instance Semigroup Milliseconds where
  (<>) = coerce ((+) :: Natural -> Natural -> Natural)

-- | Zero.
instance Monoid Milliseconds where
  mempty = Milliseconds 0

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