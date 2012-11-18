module Main where

import Criterion.Main

import Crypto.Number.Serialize
import Crypto.Number.Generate
import qualified Data.ByteString as B
import Crypto.Number.ModArithmetic

primes = [3, 5, 7, 29, 31, 211, 2309, 2311, 30029, 200560490131, 304250263527209]
carmichaelNumbers = [41041, 62745, 63973, 75361, 101101, 126217, 172081, 188461, 278545, 340561]

main = defaultMain
    [ bgroup "serialization bs->i"
        [ bench "8"    $ nf os2ip b8
        , bench "32"   $ nf os2ip b32
        , bench "64"   $ nf os2ip b64
        , bench "256"  $ nf os2ip b256
        , bench "1024" $ nf os2ip b1024
        ]
    , bgroup "serialization i->bs"
        [ bench "10"     $ nf i2osp (2^10)
        , bench "100"    $ nf i2osp (2^100)
        , bench "1000"   $ nf i2osp (2^1000)
        , bench "10000"  $ nf i2osp (2^10000)
        , bench "100000" $ nf i2osp (2^100000)
        ]
    , bgroup "exponentiation"
        [ bench "2^1234 mod 2^999" $ nf (exponantiation 2 1234) (2^999)
        , bench "130^5432 mod 100^9990" $ nf (exponantiation 130 5432) (100^9999)
        , bench "2^1234 mod 2^999" $ nf (exponantiation_rtl_binary 2 1234) (2^999)
        , bench "130^5432 mod 100^9990" $ nf (exponantiation_rtl_binary 130 5432) (100^9999)
        ]
    ]
    where b8    = B.replicate 8 0xf7
          b32   = B.replicate 32 0xf7
          b64   = B.replicate 64 0x7f
          b256  = B.replicate 256 0x7f
          b1024 = B.replicate 1024 0x7f
