{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module      : Crypto.Number.ModArithmetic
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good

module Crypto.Number.ModArithmetic
    ( exponantiation_rtl_binary
    , exponantiation
    , inverse
    , inverseCoprimes
    ) where

import Control.Exception (throw, Exception)
import Crypto.Number.Basic (gcde_binary)
import Data.Bits
import Data.Typeable

-- | Raised when two numbers are supposed to be coprimes but are not.
data CoprimesAssertionError = CoprimesAssertionError
    deriving (Show,Typeable)

instance Exception CoprimesAssertionError

-- note on exponantiation: 0^0 is treated as 1 for mimicking the standard library;
-- the mathematic debate is still open on whether or not this is true, but pratically
-- in computer science it shouldn't be useful for anything anyway.

-- | exponantiation_rtl_binary computes modular exponantiation as b^e mod m
-- using the right-to-left binary exponentiation algorithm (HAC 14.79)
exponantiation_rtl_binary :: Integer -> Integer -> Integer -> Integer
exponantiation_rtl_binary 0 0 m = 1 `mod` m
exponantiation_rtl_binary b e m = loop e b 1
    where sq x          = (x * x) `mod` m
          loop !0 _  !a = a `mod` m
          loop !i !s !a = loop (i `shiftR` 1) (sq s) (if odd i then a * s else a)

-- | exponantiation computes modular exponantiation as b^e mod m
-- using repetitive squaring.
exponantiation :: Integer -> Integer -> Integer -> Integer
exponantiation b e m
    | b == 1    = b
    | e == 0    = 1
    | e == 1    = b `mod` m
    | even e    = let p = (exponantiation b (e `div` 2) m) `mod` m
                   in (p^(2::Integer)) `mod` m
    | otherwise = (b * exponantiation b (e-1) m) `mod` m

-- | inverse computes the modular inverse as in g^(-1) mod m
inverse :: Integer -> Integer -> Maybe Integer
inverse g m = if d > 1 then Nothing else Just (x `mod` m)
    where (x,_,d) = gcde_binary g m

-- | Compute the modular inverse of 2 coprime numbers.
-- This is equivalent to inverse except that the result
-- is known to exists.
--
-- if the numbers are not defined as coprime, this function
-- will raise a CoprimesAssertionError.
inverseCoprimes :: Integer -> Integer -> Integer
inverseCoprimes g m =
    case inverse g m of
        Nothing -> throw CoprimesAssertionError
        Just i  -> i
