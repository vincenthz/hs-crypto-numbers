module Crypto.Number.Serialize
    ( i2osp
    , os2ip
    , i2ospOf
    , lengthBytes
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Bits

-- | os2ip converts a byte string into a positive integer
os2ip :: ByteString -> Integer
os2ip = B.foldl' (\a b -> (256 * a) .|. (fromIntegral b)) 0

-- | i2osp converts a positive integer into a byte string
i2osp :: Integer -> ByteString
i2osp m
    | m < 0     = error "i2osp: cannot convert a negative integer to a bytestring"
    | otherwise = B.reverse $ B.unfoldr divMod256 m
    where divMod256 0 = Nothing
          divMod256 n = Just (fromIntegral a,b) where (b,a) = n `divMod` 256


-- | just like i2osp, but take an extra parameter for size.
-- if the number is too big to fit in @len bytes, nothing is returned
-- otherwise the number is padded with 0 to fit the @len required.
i2ospOf :: Int -> Integer -> Maybe ByteString
i2ospOf len m
    | lenbytes < len  = Just $ B.replicate (len - lenbytes) 0 `B.append` bytes 
    | lenbytes == len = Just bytes
    | otherwise       = Nothing
    where
        lenbytes = B.length bytes
        bytes    = i2osp m

-- | returns the number of bytes to store an integer with i2osp
lengthBytes :: Integer -> Int
lengthBytes n
    | n < 256   = 1
    | otherwise = 1 + lengthBytes (n `div` 256)
