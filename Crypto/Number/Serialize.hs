module Crypto.Number.Serialize
    ( i2osp
    , os2ip
    , i2ospOf
    , i2ospOf_
    , lengthBytes
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Data.Bits
import Foreign.Storable
import Foreign.Ptr

divMod256 :: Integer -> (Integer, Integer)
divMod256 n = (n `shiftR` 8, n .&. 0xff)

-- | os2ip converts a byte string into a positive integer
os2ip :: ByteString -> Integer
os2ip = B.foldl' (\a b -> (256 * a) .|. (fromIntegral b)) 0

-- | i2osp converts a positive integer into a byte string
i2osp :: Integer -> ByteString
i2osp m
    | m < 0     = error "i2osp: cannot convert a negative integer to a bytestring"
    | otherwise = B.reverse $ B.unfoldr fdivMod256 m
    where fdivMod256 0 = Nothing
          fdivMod256 n = Just (fromIntegral a,b) where (b,a) = divMod256 n


-- | just like i2osp, but take an extra parameter for size.
-- if the number is too big to fit in @len bytes, nothing is returned
-- otherwise the number is padded with 0 to fit the @len required.
--
-- FIXME: use unsafeCreate to fill the bytestring
i2ospOf :: Int -> Integer -> Maybe ByteString
i2ospOf len m
    | lenbytes < len  = Just $ B.replicate (len - lenbytes) 0 `B.append` bytes
    | lenbytes == len = Just bytes
    | otherwise       = Nothing
    where
        lenbytes = B.length bytes
        bytes    = i2osp m

-- | just like i2ospOf except that it doesn't expect a failure.
-- for example if you just took a modulo of the number that represent
-- the size (example the RSA modulo n).
i2ospOf_ :: Int -> Integer -> ByteString
i2ospOf_ len m = B.unsafeCreate len fillPtr
    where fillPtr srcPtr = loop m (srcPtr `plusPtr` (len-1))
            where loop n ptr = do
                      let (nn,a) = divMod256 n
                      poke ptr (fromIntegral a)
                      if ptr == srcPtr
                          then return ()
                          else (if nn == 0 then fillerLoop else loop nn) (ptr `plusPtr` (-1))
                  fillerLoop ptr = do
                      poke ptr 0
                      if ptr == srcPtr
                          then return ()
                          else fillerLoop (ptr `plusPtr` (-1))

-- | returns the number of bytes to store an integer with i2osp
lengthBytes :: Integer -> Int
lengthBytes n
    | n < 256   = 1
    | otherwise = 1 + lengthBytes (n `shiftR` 8)
