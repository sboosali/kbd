--------------------------------------------------

{-|

-}

module Internal.KeyboardInput.Tokens where

--------------------------------------------------

import qualified "containers" Data.Set       as Set

import           "spiros"     Prelude.Spiros

--------------------------------------------------

data KeyToken

  = AbbreviatedKey     String -- ^ e.g. @(kbd "RET")@
  | BracketedKey       String -- ^ e.g. @(kbd "<return>")@
  | SingleCharacterKey Char   -- ^ e.g. @(kbd "x")@

  deriving stock (Show,Read,Eq,Ord,Lift,Generic)

  -- deriving stock    (Show,Read,Lift,Generic)
  -- deriving newtype  (Num,Real,Enum,Ix,Bits,Eq,Ord)
  -- deriving newtype  (NFData,Hashable)

--------------------------------------------------

{-| How to tokenize some key.

For example, say we have a key type.
e.g. an enum @(ReturnKey :: Key)@, or a code @(13 :: Key)@.
In the code below, we configure the enter-key to have multiple aliases
in several syntactic categories.

@
-- -XRecordWildCards
-- -XOverloadedLists
tokensOfReturnKey :: TokensOfKey
tokensOfReturnKey = TokensOfKey{..}
  where
  abbreviations   = [ "RET" ]
  synonyms        = [ "<return>", "<enter>" ]
  characters      = [ '\n', '\r' ]
@

-}

data TokensOfKey = TokensOfKey

  { abbreviations   :: Set String
  , synonyms        :: Set String
  , characters      :: Set Char
  }

  deriving stock (Show,Read,Eq,Ord,Generic)

--------------------------------------------------

matchToken :: TokensOfKey -> KeyToken -> Bool
matchToken TokensOfKey{..} = go

 where
 go = \case
   AbbreviatedKey     s -> s `Set.member` abbreviations
   BracketedKey       s -> s `Set.member` synonyms
   SingleCharacterKey c -> c `Set.member` characters

--------------------------------------------------