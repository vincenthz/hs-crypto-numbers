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

import Crypto.Number.Serialize
import Crypto.Random.API
import qualified Data.ByteString as B
import Data.Bits ((.|.))

-- | generate a positive integer between 0 and m.
-- using as many bytes as necessary to the same size as m, that are converted to integer.
generateMax :: CPRG g => g -> Integer -> (Integer, g)
generateMax rng m = withRandomBytes rng (lengthBytes m) $ \bs ->
    os2ip bs `mod` m

-- | generate a number between the inclusive bound [low,high].
generateBetween :: CPRG g => g -> Integer -> Integer -> (Integer, g)
generateBetween rng low high = (low + v, rng')
    where (v, rng') = generateMax rng (high - low + 1)

-- | generate a positive integer of a specific size in bits.
-- the number of bits need to be multiple of 8. It will always returns
-- an integer that is close to 2^(1+bits/8) by setting the 2 highest bits to 1.
generateOfSize :: CPRG g => g -> Int -> (Integer, g)
generateOfSize rng bits = withRandomBytes rng (bits `div` 8) $ \bs ->
    os2ip $ snd $ B.mapAccumL (\acc w -> (0, w .|. acc)) 0xc0 bs
