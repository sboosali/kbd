--------------------------------------------------

{-| Extend the @trifecta@ package with some utilities.

-}

module Internal.Trifecta where

--------------------------------------------------

import qualified "trifecta"       Text.Trifecta.Result         as T

--import qualified "ansi-wl-pprint" Text.PrettyPrint.ANSI.Leijen as P

--------------------------------------------------

import           "spiros"     Prelude.Spiros

--------------------------------------------------

result2either :: T.Result a -> Either String a
result2either = T.foldResult (show > Left) Right

--------------------------------------------------



--------------------------------------------------



--------------------------------------------------
{-

P.whiteSpace

P.stringLiteral'
parses a single-quoted string-literal.

-}
--------------------------------------------------