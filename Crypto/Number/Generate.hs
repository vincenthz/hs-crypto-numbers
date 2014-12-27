-- |
-- Module      : Crypto.Number.Generate
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good

module Crypto.Number.Generate
    ( generateMax
    , generateBetween
    , generateOfSize
    ) where

import Crypto.Number.Basic
import Crypto.Number.Serialize
import Crypto.Random.API
import qualified Data.ByteString as B
import Data.Bits ((.|.), (.&.), shiftL)

-- | generate a positive integer x, s.t. 0 <= x < m, uniformly at random
generateMax :: CPRG g => g -> Integer -> (Integer, g)
generateMax rng m
    | m < 1 = error "generateMax: m must be >= 1"
    | m == 1 = (0,rng)
    | otherwise =
        let (tentativeResult, rng') =
                withRandomBytes rng (lengthBytes m) $ \bs ->
                    let lengthBits = (log2 (m-1) + 1)
                        mask = if lengthBits `mod` 8 == 0
                               then 0xff
                               else (1 `shiftL` (lengthBits `mod` 8)) - 1 in
                    os2ip $ snd $ B.mapAccumL (\acc w -> (0xff, w .&. acc))
                                      mask bs in
        if tentativeResult < m
            then (tentativeResult, rng')
            else generateMax rng' m

-- | generate a number between the inclusive bound [low,high] uniformly at random.
generateBetween :: CPRG g => g -> Integer -> Integer -> (Integer, g)
generateBetween rng low high = (low + v, rng')
    where (v, rng') = generateMax rng (high - low + 1)

-- | generate a positive integer of a specific size in bits.
-- the number of bits need to be multiple of 8. It will always returns
-- an integer that is close to 2^(1+bits/8) by setting the 2 highest bits to 1.
generateOfSize :: CPRG g => g -> Int -> (Integer, g)
generateOfSize rng bits = withRandomBytes rng (bits `div` 8) $ \bs ->
    os2ip $ snd $ B.mapAccumL (\acc w -> (0, w .|. acc)) 0xc0 bs
