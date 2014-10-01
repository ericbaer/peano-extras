-----------------------------------------------------------------------------
--
-- Module      :  Data.Peano.Extras
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Minor GHC-only supplements to "Data.Peano".
--
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Peano.Extras (
    -- ** The usual synonyms
    One, Two, Three, Four, Five, Six, Seven, Eight, Nine,
    -- ** Class for naturals
    Nat(..)
) where
 
import Data.Peano
import Data.Typeable

type One   = Succ Zero
type Two   = Succ One
type Three = Succ Two
type Four  = Succ Three
type Five  = Succ Four
type Six   = Succ Five
type Seven = Succ Six
type Eight = Succ Seven
type Nine  = Succ Eight

-- | A class to allow us to examine type-level Peano numbers at the value level.
class Nat (p :: Peano) where
    nat :: (Integral n) => proxy p -> n
    demotePeano :: proxy p -> Peano

-- | The base case
instance Nat Zero where
    nat _ = 0
    demotePeano _ = Zero

-- | The inductive step
instance (Nat p) => Nat (Succ p) where
    nat _ = succ $ nat (Proxy :: Proxy p)
    demotePeano _ = Succ $ demotePeano (Proxy :: Proxy p) 

