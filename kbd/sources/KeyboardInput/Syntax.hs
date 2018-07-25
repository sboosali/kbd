{-# language GeneralizedNewtypeDeriving #-}

--------------------------------------------------

{-|

-}

module KeyboardInput.Syntax 
 ( module KeyboardInput.Syntax.Types
 , module KeyboardInput.Syntax.Parse
 , module KeyboardInput.Syntax.Create
 -- , module KeyboardInput.Syntax
 ) where

--------------------------------------------------

import KeyboardInput.Syntax.Types
import KeyboardInput.Syntax.Parse
import KeyboardInput.Syntax.Create

--------------------

import "spiros" Prelude.Spiros hiding (Text)

--TODO different subsets of re-exports (like one without text).
--TODO signatures for both Text-versus-String-versus-Bytestring and Lazy-versus-Strict.

--------------------

import "text" Data.Text (Text)

--------------------

--------------------------------------------------