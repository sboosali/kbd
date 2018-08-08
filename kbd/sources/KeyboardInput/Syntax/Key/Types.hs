{-# LANGUAGE DuplicateRecordFields #-}

--------------------------------------------------

{-|



-}

module KeyboardInput.Syntax.Key.Types where

--------------------------------------------------

--------------------------------------------------

--------------------------------------------------

import Prelude.KeyboardInput

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

-- {-|

-- -}

-- newtype Key = Key

--   String

--   deriving stock    (Show,Read,Lift,Generic)
--   deriving newtype  (Eq,Ord,Semigroup,Monoid,NFData,Hashable)
--   -- deriving anyclass (IsString)

-- instance IsString Key where
--   fromString = coerce

--------------------------------------------------

{-|

-}

data Key

  = CharacterKey   Char    -- ^ e.g. @a@ or @A@

  | QuotedKey      Char    -- ^ e.g. @'\t'@

  | NamedKey       String  -- ^ e.g. @<tab>@

  | AbbreviatedKey String  -- ^ e.g. @TAB@

  | ModifierKey    String String -- ^ e.g. @dC@ or TODO @C_down@

  | ASCIIKey       Int     -- ^ e.g. @'\009'@ TODO

  | ScanCodeKey    Int     -- ^ e.g. @0x0D@ TODO

  deriving stock    (Eq,Ord,Show,Read,Lift,Generic)
  deriving anyclass (NFData,Hashable)
  -- deriving anyclass (IsString)

--instance IsString KeyChar where  fromString = coerce

--------------------------------------------------