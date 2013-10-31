{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module      : Crypto.Number.ModArithmetic
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good

module Crypto.Number.ModArithmetic
    (
    -- * exponentiation
      expSafe
    , expFast
    , exponentiation_rtl_binary
    , exponentiation
    -- * deprecated name for exponentiation
    , exponantiation_rtl_binary
    , exponantiation
    -- * inverse computing
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

-- | Compute the modular exponentiation of base^exponant using
-- algorithms design to avoid side channels and timing measurement
--
-- When used with integer-simple, this function is not different
-- from expFast, and thus provide the same unstudied and dubious
-- timing and side channels claims.
expSafe :: Integer -- ^ base
        -> Integer -- ^ exponant
        -> Integer -- ^ modulo
        -> Integer -- ^ result
expSafe = exponentiation

-- | Compute the modular exponentiation of base^exponant using
-- the fastest algorithm without any consideration for
-- hiding parameters.
--
-- Use this function when all the parameters are public,
-- otherwise 'expSafe' should be prefered.
expFast :: Integer -- ^ base
        -> Integer -- ^ exponant
        -> Integer -- ^ modulo
        -> Integer -- ^ result
expFast = exponentiation

-- note on exponentiation: 0^0 is treated as 1 for mimicking the standard library;
-- the mathematic debate is still open on whether or not this is true, but pratically
-- in computer science it shouldn't be useful for anything anyway.

-- | exponentiation_rtl_binary computes modular exponentiation as b^e mod m
-- using the right-to-left binary exponentiation algorithm (HAC 14.79)
exponentiation_rtl_binary :: Integer -> Integer -> Integer -> Integer
exponentiation_rtl_binary 0 0 m = 1 `mod` m
exponentiation_rtl_binary b e m = loop e b 1
    where sq x          = (x * x) `mod` m
          loop !0 _  !a = a `mod` m
          loop !i !s !a = loop (i `shiftR` 1) (sq s) (if odd i then a * s else a)

-- | exponentiation computes modular exponentiation as b^e mod m
-- using repetitive squaring.
exponentiation :: Integer -> Integer -> Integer -> Integer
exponentiation b e m
    | b == 1    = b
    | e == 0    = 1
    | e == 1    = b `mod` m
    | even e    = let p = (exponentiation b (e `div` 2) m) `mod` m
                   in (p^(2::Integer)) `mod` m
    | otherwise = (b * exponentiation b (e-1) m) `mod` m

--{-# DEPRECATED exponantiation_rtl_binary "typo in API name it's called exponentiation_rtl_binary #-}
exponantiation_rtl_binary :: Integer -> Integer -> Integer -> Integer
exponantiation_rtl_binary = exponentiation_rtl_binary

--{-# DEPRECATED exponentiation "typo in API name it's called exponentiation #-}
exponantiation :: Integer -> Integer -> Integer -> Integer
exponantiation = exponentiation

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
