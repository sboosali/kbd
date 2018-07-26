{-# LANGUAGE RecordWildCards       #-}

--------------------------------------------------

{-|

-}

module KeyboardInput.Syntax.Config where

--------------------------------------------------

import KeyboardInput.Syntax.Types

--------------------------------------------------

--------------------------------------------------

import "spiros" Prelude.Spiros

--------------------------------------------------

{-|

-}

data Config = Config
  { modifierSeparator           :: Char
  , requireCapitalizedModifiers :: Bool
  , acceptedModifiers           :: Maybe [Char]
  
  }
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | 'defaultConfig'
instance Default Config where
  def = defaultConfig

{-|

* Hyphen-separated modifiers;
* uppercase-*or*-lowercase modifiers;
* arbitrary characters as modifiers (no whitelist);

-}

defaultConfig :: Config
defaultConfig = Config{..}
  where
  modifierSeparator           = '-'
  requireCapitalizedModifiers = False
  acceptedModifiers           = Nothing --TODO this reads weirdly. maybe "modifiersWhitelist"?

--------------------------------------------------