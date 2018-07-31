----------------------------------------

module Prelude.KeyboardInput
 ( module X
 ) where

----------------------------------------

import "spiros" Prelude.Spiros           as X hiding (Text)
--import "spiros" Prelude.Spiros.Exception as X

--TODO different subsets of re-exports (like one without text).
--TODO signatures for both Text-versus-String-versus-Bytestring and Lazy-versus-Strict.

----------------------------------------

import "text" Data.Text as X (Text)

----------------------------------------



----------------------------------------