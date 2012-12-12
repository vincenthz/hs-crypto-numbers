module Main where

import Criterion.Main

import Crypto.Number.Serialize
import Crypto.Number.Generate
import qualified Data.ByteString as B
import Crypto.Number.ModArithmetic

primes = [3, 5, 7, 29, 31, 211, 2309, 2311, 30029, 200560490131, 304250263527209]
carmichaelNumbers = [41041, 62745, 63973, 75361, 101101, 126217, 172081, 188461, 278545, 340561]

lg1, lg2 :: Integer
lg1 = 21389083291083903845902381390285907190274907230982112390820985903825329874812973821790321904790217490217409721904832974210974921740972109481490128430982190472109874802174907490271904124908210958093285098309582093850918902581290859012850829105809128590218590281905812905810928590128509128940821903829018390849839578967358920127598901248259797158249684571948075896458741905823982671490352896791052386357019528367902
lg2 = 21392813098390824190840192812389082390812940821904891028439028490128904829104891208940835932882910839218309812093118249089871209347472901874902407219740921840928149087284397490128903843789289014374839281492038091283923091809734832974180398210938901284839274091749021709

main = defaultMain
    [ bgroup "std ops"
        [ bench "mod" $ nf (mod lg1) lg2
        , bench "rem" $ nf (rem lg1) lg2
        , bench "div" $ nf (div lg1) lg2
        , bench "quot" $ nf (quot lg1) lg2
        , bench "divmod" $ nf (divMod lg1) lg2
        , bench "quotRem" $ nf (quotRem lg1) lg2
        ]
    , bgroup "serialization bs->i"
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
