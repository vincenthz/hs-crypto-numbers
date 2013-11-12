{-# LANGUAGE OverloadedStrings #-}

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import Test.QuickCheck
import Test.HUnit ((@?=))
--import Test.QuickCheck.Test

import Control.Applicative ((<$>))

import qualified Data.ByteString as B
import Data.ByteString.Char8 () -- orphan IsString instance

import Crypto.Number.ModArithmetic
import Crypto.Number.Basic
import Crypto.Number.Generate
import Crypto.Number.Prime
import Crypto.Number.Serialize
import Crypto.Number.F2m

import RNG

prop_gcde_binary_valid :: (Positive Integer, Positive Integer) -> Bool
prop_gcde_binary_valid (Positive a, Positive b) =
    and [v==v', a*x' + b*y' == v', a*x + b*y == v, gcd a b == v]
    where (x,y,v)    = gcde_binary a b
          (x',y',v') = gcde a b

prop_modexp_rtl_valid :: (NonNegative Integer,
                          NonNegative Integer,
                          Positive Integer)
                      -> Bool
prop_modexp_rtl_valid (NonNegative a, NonNegative b, Positive m) =
    exponantiation_rtl_binary a b m == ((a ^ b) `mod` m)

prop_modinv_valid :: (Positive Integer, Positive Integer) -> Bool
prop_modinv_valid (Positive a, Positive m)
    | m > 1           = case inverse a m of
                             Just ainv -> (ainv * a) `mod` m == 1
                             Nothing   -> True
    | otherwise       = True

prop_sqrti_valid :: Positive Integer -> Bool
prop_sqrti_valid (Positive i) = l*l <= i && i <= u*u where (l, u) = sqrti i

{-
prop_generate_prime_valid :: Seed -> Bool
prop_generate_prime_valid i =
    -- because of the next naive test, we can't generate easily number above 32 bits
    -- otherwise it slows down the tests to uselessness. when AKS or ECPP is implemented
    -- we can revisit the number here
    primalityTestNaive $ withRNG i (\g -> generatePrime g 32)
-}

prop_miller_rabin_valid :: (Seed, PositiveSmall) -> Bool
prop_miller_rabin_valid (seed, PositiveSmall i)
    | i <= 3    = True
    | otherwise =
        -- miller rabin only returns with certitude that the integer is composite.
        let b = withRNG seed (\g -> isProbablyPrime g i)
         in (b == False && primalityTestNaive i == False) || b == True

prop_generate_valid :: (Seed, Positive Integer) -> Bool
prop_generate_valid (seed, Positive h) =
    let v = withRNG seed (\g -> generateMax g h)
     in (v >= 0 && v < h)

prop_invF2m_valid :: Fx -> PositiveLarge -> Bool
prop_invF2m_valid (Fx fx) (PositiveLarge a) = maybe True ((1 ==) . mulF2m fx a) (invF2m fx a)

prop_squareF2m_valid :: Fx -> PositiveLarge -> Bool
prop_squareF2m_valid (Fx fx) (PositiveLarge a) = mulF2m fx a a == squareF2m fx a

withAleasInteger :: Rng -> Seed -> (Rng -> (a,Rng)) -> a
withAleasInteger g (Seed i) f = fst $ f $ reseed (i2osp $ fromIntegral i) g

withRNG :: Seed -> (Rng -> (a,Rng)) -> a
withRNG seed f = withAleasInteger rng seed f

newtype PositiveSmall = PositiveSmall Integer
                      deriving (Show,Eq)

instance Arbitrary PositiveSmall where
    arbitrary = PositiveSmall . fromIntegral <$> (resize (2^(20 :: Int)) (arbitrary :: Gen Int))

newtype PositiveLarge = PositiveLarge Integer
                      deriving (Show,Eq)

instance Arbitrary PositiveLarge where
    arbitrary = PositiveLarge <$> sized (\n -> choose (1, fromIntegral n^(100::Int)))

newtype Fx = Fx Integer deriving (Show,Eq)

instance Arbitrary Fx where
    arbitrary = elements $ map Fx
              [ 283  -- [8,4,3,1,0] Rijndael
                -- SEC2 polynomials
              , 11692013098647223345629478661730264157247460344009  -- [163,7,6,3,0]
              , 13803492693581127574869511724554050904902217944359662576256527028453377 -- [233,74,0]
              , 883423532389192164791648750371459257913741948437809479060803169365786625 --  [239,36,0]
              , 883423532389192164791649115746868590639471499359017658131558014629445633 -- [239,158,0]
              , 15541351137805832567355695254588151253139254712417116170014499277911234281641667989665  -- [283,12,7,5,0]
              , 1322111937580497197903830616065542079656809365928562438569297590548811582472622691650378420879430724437687334722581078999041 -- [409,87,0]
              , 7729075046034516689390703781863974688597854659412869997314470502903038284579120849072387533163845155924927232063004354354730157322085975311485817346934161497393961629647909  -- [571,10,5,2,0]
              ]

data Range = Range Integer Integer
           deriving (Show,Eq)

instance Arbitrary Range where
    arbitrary = do (Positive x) <- arbitrary :: Gen (Positive Int)
                   (Positive r) <- arbitrary :: Gen (Positive Int)
                   return $ Range (fromIntegral x) (fromIntegral r)

newtype Seed = Seed Integer
             deriving (Eq)

instance Show Seed where
    show s = "Seed " ++ show s

instance Arbitrary Seed where
    arbitrary = arbitrary >>= \(Positive i) -> return (Seed i)

serializationKATTests :: [Test]
serializationKATTests = concatMap f vectors
    where f (v, bs) = [ testCase ("i2osp " ++ show v) (i2osp v  @?= bs)
                      , testCase ("os2ip " ++ show v) (os2ip bs @?= v)
                      ]
          vectors =
            [ (0x10000, "\SOH\NUL\NUL")
            , (0x1234, "\DC24")
            , (0xf123456, "\SI\DC24V")
            , (0xf21908421feabd21490, "\SI!\144\132!\254\171\210\DC4\144")
            , (0x7521908421feabd21490, "u!\144\132!\254\171\210\DC4\144")
            ]

serializationOfKATTests :: [Test]
serializationOfKATTests = concatMap f vectors
    where f (elen, v, bs) = [ testCase ("i2osp " ++ show v) (i2ospOf elen v @?= Just bs)
                            , testCase ("os2ip " ++ show v) (os2ip bs @?= v)
                            ]
          vectors =
            [ (5, 0x10000, "\NUL\NUL\SOH\NUL\NUL")
            , (3, 0x1234, "\NUL\DC24")
            , (8, 0xf123456, "\NUL\NUL\NUL\NUL\SI\DC24V")
            , (10, 0xf21908421feabd21490, "\SI!\144\132!\254\171\210\DC4\144")
            , (12, 0x7521908421feabd21490, "\NUL\NULu!\144\132!\254\171\210\DC4\144")
            ]

main :: IO ()
main = defaultMain
    [ testGroup "serialization"
        [ testProperty "unbinary.binary==id" (\(Positive i) -> os2ip (i2osp i) == i)
        , testProperty "length integer" (\(Positive i) -> B.length (i2osp i) == lengthBytes i)
        , testGroup "KAT" serializationKATTests
        , testGroup "KAT2" serializationOfKATTests
        ]
    , testGroup "gcde binary"
        [ testProperty "gcde" prop_gcde_binary_valid
        ]
    , testGroup "exponantiation"
        [ testProperty "right-to-left" prop_modexp_rtl_valid
        ]
    , testGroup "inverse"
        [ testProperty "inverse" prop_modinv_valid
        ]
    , testGroup "sqrt integer"
        [ testProperty "sqrt" prop_sqrti_valid
        ]
    , testGroup "generation"
        [ testProperty "max" prop_generate_valid
        --, testProperty "between" (\seed (Range l h) -> let generated = withRNG seed (\rng -> generateBetween rng l (l+h))
        --                                                in (generated > l && generated < h))
        ]
    , testGroup "primality test"
        [ testProperty "miller-rabin" prop_miller_rabin_valid
        ]
    , testGroup "F2m"
        [ testProperty "invF2m" prop_invF2m_valid
        , testProperty "squareF2m" prop_squareF2m_valid
        ]
    ]
