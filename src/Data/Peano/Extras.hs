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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
module Data.Peano.Extras where
 
import Data.Peano
import Data.Typeable

-----------------------------------------------------------------------------
-- * Synonyms
-----------------------------------------------------------------------------

type Z = 'Zero
type S = 'Succ

-- ** Synonyms for addition

type Plus1 n   = S n
type Plus2 n   = S (S n)
type Plus3 n   = S (S (S n))
type Plus4 n   = S (S (S (S n)))
type Plus5 n   = S (S (S (S (S n))))
type Plus6 n   = S (S (S (S (S (S n)))))
type Plus7 n   = S (S (S (S (S (S (S n))))))
type Plus8 n   = S (S (S (S (S (S (S (S n)))))))
type Plus9 n   = S (S (S (S (S (S (S (S (S n))))))))
type Plus10 n  = S (S (S (S (S (S (S (S (S (S n)))))))))
type Plus11 n  = S (S (S (S (S (S (S (S (S (S (S n))))))))))
type Plus12 n  = S (S (S (S (S (S (S (S (S (S (S (S n)))))))))))
type Plus13 n  = S (S (S (S (S (S (S (S (S (S (S (S (S n))))))))))))
type Plus14 n  = S (S (S (S (S (S (S (S (S (S (S (S (S (S n)))))))))))))
type Plus15 n  = S (S (S (S (S (S (S (S (S (S (S (S (S (S (S n))))))))))))))
type Plus16 n  = S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S n)))))))))))))))
type Plus100 n = Plus10 (Plus10 (Plus10 (Plus10 (Plus10 (Plus10 (Plus10
    (Plus10 (Plus10 (Plus10 n)))))))))
type Plus1000 n = Plus100 (Plus100 (Plus100 (Plus100  (Plus100 (Plus100
    (Plus100 (Plus100 (Plus100 (Plus100 n)))))))))

-- ** Synonyms named as words
type One      = Plus1 Z
type Two      = Plus2 Z
type Three    = Plus3 Z
type Four     = Plus4 Z
type Five     = Plus5 Z
type Six      = Plus6 Z
type Seven    = Plus7 Z
type Eight    = Plus8 Z
type Nine     = Plus9 Z
type Ten      = Plus10 Z
type Eleven   = Plus11 Z
type Twelve   = Plus12 Z
type Thirteen = Plus13 Z
type Fourteen = Plus14 Z
type Fifteen  = Plus15 Z
type Sixteen  = Plus16 Z
type Hundred  = Plus100 Z
type Thousand = Plus1000 Z

-- ** Synonyms named as numbers
type P1    = Plus1 Z
type P2    = Plus2 Z
type P3    = Plus3 Z
type P4    = Plus4 Z
type P5    = Plus5 Z
type P6    = Plus6 Z
type P7    = Plus7 Z
type P8    = Plus8 Z
type P9    = Plus9 Z
type P10   = Plus10 Z
type P11   = Plus11 Z
type P12   = Plus12 Z
type P13   = Plus13 Z
type P14   = Plus14 Z
type P15   = Plus15 Z
type P16   = Plus16 Z
type P100  = Plus100 Z
type P1000 = Plus1000 Z

-----------------------------------------------------------------------------
-- * Typeclasses
-----------------------------------------------------------------------------

-- | A class to allow us to examine type-level Peano numbers at the value level.
class Nat (p :: Peano) where
    nat :: proxy p -> Int
    demotePeano :: proxy p -> Peano

-- | The base case
instance Nat Zero where
    nat _ = 0
    {-# INLINE nat #-}
    demotePeano _ = Zero
    {-# INLINE demotePeano #-}

-- | The inductive step
instance (Nat p) => Nat (Succ p) where
    nat _ = succ $ nat (Proxy :: Proxy p)
    {-# INLINE nat #-}
    demotePeano _ = Succ $ demotePeano (Proxy :: Proxy p) 
    {-# INLINE demotePeano #-}


data WithInfinity a = Infinite | Finite a

class Dec (a :: WithInfinity Peano) (b :: WithInfinity Peano) | a -> b, b -> a
instance Dec Infinite Infinite
instance Dec (Finite (Succ n)) (Finite n)

-- | One of the advantages of typeclasses and functional dependencies over
--   type families: inverse functions are easy to define
type Inc a b = Dec b a
