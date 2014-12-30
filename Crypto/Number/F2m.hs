{-# LANGUAGE CPP #-}
#ifndef MIN_VERSION_integer_gmp
#define MIN_VERSION_integer_gmp(a,b,c) 0
#endif
-- |
-- Module      : Crypto.Number.F2m
-- License     : BSD-style
-- Maintainer  : Danny Navarro <j@dannynavarro.net>
-- Stability   : experimental
-- Portability : Good
--
-- This module provides basic arithmetic operations over F₂m. Performance is
-- not optimal and it doesn't provide protection against timing
-- attacks. The 'm' parameter is implicitly derived from the irreducible
-- polynomial where applicable.
module Crypto.Number.F2m
    ( addF2m
    , mulF2m
    , squareF2m
    , modF2m
    , invF2m
    , divF2m
    ) where

import Control.Applicative ((<$>))
import Data.Bits ((.&.),(.|.),xor,shift,testBit)
import Crypto.Number.Basic

-- | Addition over F₂m. This is just a synonym of  'xor'.
addF2m :: Integer -> Integer -> Integer
addF2m = xor
{-# INLINE addF2m #-}

-- | Binary polynomial reduction modulo using long division algorithm.
modF2m :: Integer  -- ^ Irreducible binary polynomial
       -> Integer -> Integer
modF2m fx = go
  where
    lfx = log2 fx
    go n | s == 0  = n `xor` fx
         | s < 0 = n
         | otherwise = go $ n `xor` shift fx s
      where
        s = log2 n - lfx
{-# INLINE modF2m #-}

-- | Multiplication over F₂m.
mulF2m :: Integer  -- ^ Irreducible binary polynomial
       -> Integer -> Integer -> Integer
mulF2m fx n1 n2 = modF2m fx
                $ go (if n2 `mod` 2 == 1 then n1 else 0) (log2 n2)
  where
    go n s | s == 0  = n
           | otherwise = if testBit n2 s
                            then go (n `xor` shift n1 s) (s - 1)
                            else go n (s - 1)
{-# INLINABLE mulF2m #-}

-- | Squaring over F₂m.
-- TODO: This is still slower than @mulF2m@.

-- Multiplication table? C?
squareF2m :: Integer  -- ^ Irreducible binary polynomial
          -> Integer -> Integer
squareF2m fx = modF2m fx . square
{-# INLINE squareF2m #-}

square :: Integer -> Integer
square n1 = go n1 ln1
  where
    ln1 = log2 n1
    go n s | s == 0 = n
           | otherwise = go (x .|. y) (s - 1)
      where
        x = shift (shift n (2 * (s - ln1) - 1)) (2 * (ln1 - s) + 2)
        y = n .&. (shift 1 (2 * (ln1 - s) + 1) - 1)
{-# INLINE square #-}

-- | Inversion over  F₂m using extended Euclidean algorithm.
invF2m :: Integer -- ^ Irreducible binary polynomial
       -> Integer -> Maybe Integer
invF2m _  0 = Nothing
invF2m fx n = go n fx 1 0
    where
      go u v g1 g2
          | u == 1 = Just $ modF2m fx g1
          | otherwise = if j < 0
                           then go u  (v  `xor` shift  u (-j))
                                   g1 (g2 `xor` shift g1 (-j))
                           else go (u  `xor` shift v  j) v
                                   (g1 `xor` shift g2 j) g2
        where
          j = log2 u - log2 v
{-# INLINABLE invF2m #-}

-- | Division over F₂m. If the dividend doesn't have an inverse it returns
-- 'Nothing'.
divF2m :: Integer  -- ^ Irreducible binary polynomial
       -> Integer  -- ^ Dividend
       -> Integer  -- ^ Quotient
       -> Maybe Integer
divF2m fx n1 n2 = mulF2m fx n1 <$> invF2m fx n2
{-# INLINE divF2m #-}
