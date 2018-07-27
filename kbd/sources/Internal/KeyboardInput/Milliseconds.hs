--------------------------------------------------

{-|

-}
module Internal.KeyboardInput.Milliseconds where

--------------------------------------------------

import "spiros" Prelude.Spiros

--------------------------------------------------

{-|

-}
newtype Milliseconds = Milliseconds

  Natural

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Eq,Ord)
  deriving newtype  (Num,Integral,Real,Enum)
  deriving newtype  (NFData,Hashable)

-- | Addition.
instance Semigroup Milliseconds where
  (<>) = coerce ((+) :: Natural -> Natural -> Natural)

-- | Zero.
instance Monoid Milliseconds where
  mempty = Milliseconds 0

--------------------------------------------------

data TimeUnit

  = MillisecondsUnit
  | SecondsUnit

  deriving stock (Show,Enum,Bounded,Eq,Ord,Lift,Generic)

  -- deriving stock    (Show,Read,Lift,Generic)
  -- deriving newtype  (Num,Real,Enum,Ix,Bits,Eq,Ord)
  -- deriving newtype  (NFData,Hashable)

--------------------------------------------------

toMilliseconds
  :: forall i. (RealFrac i)
  => TimeUnit -> i -> Milliseconds

toMilliseconds unit
 = convert unit
 > roundUp
 where

 convert = \case
  MillisecondsUnit -> id
  SecondsUnit     -> (*1000) 

 roundUp :: i -> Milliseconds
 roundUp = ceiling

--------------------------------------------------
-- NOTES

-- class (Real a, Fractional a) => RealFrac a where 
--  ...
--  ceiling :: Integral b => a -> b

-- instance RealFrac Double where 
--  ...
--  ceiling :: Integral b => Double -> b

--------------------------------------------------