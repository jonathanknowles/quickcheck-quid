{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.QuickCheck.QuidSpec
    where

import Control.Monad
    ( forM_, replicateM )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromMaybe )
import Data.Ord
    ( Down (..) )
import Data.Ratio
    ( (%) )
import Data.Set
    ( Set )
import Internal.Test.QuickCheck.Quid
    ( Chunked (..)
    , Frequency (..)
    , Prefix (..)
    , Quid
    , Size (..)
    , UppercaseLatin (..)
    , arbitraryNatural
    , arbitraryQuid
    , frequencies
    , quidFromNatural
    , quidToNatural
    , shrinkNatural
    , shrinkQuid
    )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, it, parallel )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , checkCoverage
    , choose
    , conjoin
    , counterexample
    , cover
    , forAll
    , forAllBlind
    , frequency
    , label
    , property
    , shrinkMapBy
    , shrinkNothing
    , sized
    , withMaxSuccess
    , (===)
    )
import Test.QuickCheck.Classes.Hspec
    ( testLawsMany )
import Text.Pretty.Simple
    ( pShow )
import Text.Printf
    ( printf )

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy as TL
import qualified Test.QuickCheck.Classes as Laws

spec :: Spec
spec = do

    parallel $ describe "Lawfulness of type class instances" $ do
        testLawsMany @(Sized Quid)
            [ Laws.eqLaws
            , Laws.ordLaws
            ]
        testLawsMany @(Sized (UppercaseLatin Quid))
            [ Laws.showLaws
            , Laws.showReadLaws
            ]
        testLawsMany @(Prefix "test-prefix:" Int)
            [ Laws.showLaws
            , Laws.showReadLaws
            ]

    parallel $ describe "Uniformity" $ do
        it "prop_arbitraryQuid_uniform" $
            property prop_arbitraryQuid_uniform

    parallel $ describe "Uniqueness" $ do
        it "prop_arbitraryQuid_unique" $
            property prop_arbitraryQuid_unique

    parallel $ describe "Shrinkability" $ do
        it "prop_shrinkQuid_lessThan" $
            property prop_shrinkQuid_lessThan
        it "prop_shrinkQuid_minimalElement" $
            property prop_shrinkQuid_minimalElement
        it "prop_shrinkQuid_minimalSet" $
            property prop_shrinkQuid_minimalSet
        it "prop_shrinkQuid_ordered" $
            property prop_shrinkQuid_ordered
        it "prop_shrinkQuid_unique" $
            property prop_shrinkQuid_unique

    parallel $ describe "Unit tests" $ do
        unitTests_show_uppercaseLatin_quidFromNatural

--------------------------------------------------------------------------------
-- Uniformity
--------------------------------------------------------------------------------

newtype SizeExponent = SizeExponent {unSizeExponent :: Int}
    deriving (Eq, Ord, Show)

instance Arbitrary SizeExponent where
    arbitrary = SizeExponent <$> frequency
        [ (1, pure 0)
        , (1, pure 256)
        , (16, choose (1, 255))
        ]
    shrink = shrinkMapBy SizeExponent unSizeExponent shrink

prop_arbitraryQuid_uniform :: SizeExponent -> Property
prop_arbitraryQuid_uniform (SizeExponent sizeExponent) =
    forAllBlind arbitraryQuids prop
  where
    arbitraryQuids :: Gen [Quid]
    arbitraryQuids = replicateM arbitraryQuidCount (arbitraryQuid sizeExponent)

    arbitraryQuidCount :: Int
    arbitraryQuidCount = bucketCount * 256

    bucketCount :: Int
    bucketCount = fromIntegral $ min 256 size

    bucketSize :: Natural
    bucketSize = max 1 (size `div` fromIntegral bucketCount)

    size :: Natural
    size = 2 ^ fromIntegral sizeExponent

    prop :: [Quid] -> Property
    prop quids =
        label labelSizeBucketCount $
        counterexample (unwords . words $ labelSizeBucketCount) $
        conjoin
            [ occupiedBucketCount == bucketCount
            , occupiedBucketFrequencyGreatestToSmallestRatio >= 1
            , occupiedBucketFrequencyGreatestToSmallestRatio <= 2
            ]
      where
        labelSizeBucketCount :: String
        labelSizeBucketCount = mconcat
            [ "(size exponent = "
            , printf "% 4d" sizeExponent
            , ", arbitrary quid count = "
            , printf "% 6d" arbitraryQuidCount
            , ", bucket count = "
            , printf "% 4d" bucketCount
            , ", occupied bucket count = "
            , printf "% 4d" occupiedBucketCount
            , ")"
            ]

        occupiedBuckets :: [Natural]
        occupiedBuckets = quidToBucket <$> quids

        occupiedBucketFrequencies :: [(Natural, Frequency)]
        occupiedBucketFrequencies = frequencies occupiedBuckets

        occupiedBucketCount :: Int
        occupiedBucketCount = length occupiedBucketFrequencies

        quidToBucket :: Quid -> Natural
        quidToBucket = (`div` fromIntegral bucketSize) . quidToNatural

        occupiedBucketFrequencyGreatestToSmallestRatio = (%)
            (unFrequency bucketFrequencyGreatest)
            (unFrequency bucketFrequencySmallest)
        bucketFrequencyGreatest = snd $ head occupiedBucketFrequencies
        bucketFrequencySmallest = snd $ last occupiedBucketFrequencies

--------------------------------------------------------------------------------
-- Uniqueness
--------------------------------------------------------------------------------

prop_arbitraryQuid_unique :: Property
prop_arbitraryQuid_unique =
    withMaxSuccess 1 $
    forAllBlind arbitraryFixedWidthQuids $ \uids ->
        Set.size (Set.fromList uids) === L.length uids
  where
    arbitraryFixedWidthQuids :: Gen [Quid]
    arbitraryFixedWidthQuids = fmap unWidth256 <$>
        replicateM 1_000_000 (arbitrary @(Width256 Quid))

--------------------------------------------------------------------------------
-- Shrinkability
--------------------------------------------------------------------------------

prop_shrinkQuid_lessThan :: Width256 Quid -> Property
prop_shrinkQuid_lessThan (Width256 q) =
    property $ all (< q) (shrinkQuid q)

prop_shrinkQuid_minimalElement :: Sized Quid -> Property
prop_shrinkQuid_minimalElement (Sized q) =
    checkCoverage $
    cover 10 (q /= minimalQuid) "q /= minimalQuid" $
    case shrinkQuid q of
        s : _ -> s === minimalQuid
        _     -> q === minimalQuid
  where
    minimalQuid = quidFromNatural 0

prop_shrinkQuid_minimalSet :: [Width256 Quid] -> Property
prop_shrinkQuid_minimalSet qs =
    label (show $ bucket expectedSize) $
    counterexample (show expectedSize) $
    counterexample (show minimalSet) $
    conjoin
        [ Set.toList minimalSet `L.isPrefixOf` allQuids
        , Set.size minimalSet == expectedSize
        ]
  where
    allQuids :: [Quid]
    allQuids = quidFromNatural <$> [0 ..]

    bucket :: Int -> (Int, Int)
    bucket size = (lo, hi)
      where
        lo = expectedSize `div` 10 * 10
        hi = lo + 9

    expectedSize :: Int
    expectedSize = L.length qs

    minimalSet :: Set Quid
    minimalSet = Set.map unWidth256 $ fromMaybe
        (error "Cannot shrink to minimal set")
        (shrinkWhile ((>= expectedSize) . Set.size) shrink (Set.fromList qs))

prop_shrinkQuid_ordered :: Width256 Quid -> Property
prop_shrinkQuid_ordered (Width256 q) =
    L.sort shrunkValues === shrunkValues
  where
    shrunkValues = shrinkQuid q

prop_shrinkQuid_unique :: Width256 Quid -> Property
prop_shrinkQuid_unique (Width256 q) =
    Set.size (Set.fromList shrunkValues) === L.length shrunkValues
  where
    shrunkValues = shrinkQuid q

--------------------------------------------------------------------------------
-- Unit tests
--------------------------------------------------------------------------------

unitTests_show_uppercaseLatin_quidFromNatural :: Spec
unitTests_show_uppercaseLatin_quidFromNatural = unitTests
    "unitTests_show_uppercaseLatin_quidFromNatural"
    (show . UppercaseLatin . quidFromNatural)
    (mkTest <$> tests)
  where
    mkTest (params, result) = UnitTestData {params, result}
    tests =
        [ ( 0, "A")
        , ( 1, "B")
        , (24, "Y")
        , (25, "Z")

        , (26 +  0, "AA")
        , (26 +  1, "AB")
        , (26 + 24, "AY")
        , (26 + 25, "AZ")

        , (2 * 26 +  0, "BA")
        , (2 * 26 +  1, "BB")
        , (2 * 26 + 24, "BY")
        , (2 * 26 + 25, "BZ")

        , (26 * 26 +  0, "ZA")
        , (26 * 26 +  1, "ZB")
        , (26 * 26 + 24, "ZY")
        , (26 * 26 + 25, "ZZ")

        , (26                                          , "AA")
        , (26 + 26^2                                   , "AAA")
        , (26 + 26^2 + 26^3                            , "AAAA")
        , (26 + 26^2 + 26^3 + 26^4                     , "AAAAA")
        , (26 + 26^2 + 26^3 + 26^4 + 26^5              , "AAAAAA")
        , (26 + 26^2 + 26^3 + 26^4 + 26^5 + 26^6       , "AAAAAAA")
        , (26 + 26^2 + 26^3 + 26^4 + 26^5 + 26^6 + 26^7, "AAAAAAAA")

        , (26                                                  - 1, "Z")
        , (26 + 26^2                                           - 1, "ZZ")
        , (26 + 26^2 + 26^3                                    - 1, "ZZZ")
        , (26 + 26^2 + 26^3 + 26^4                             - 1, "ZZZZ")
        , (26 + 26^2 + 26^3 + 26^4 + 26^5                      - 1, "ZZZZZ")
        , (26 + 26^2 + 26^3 + 26^4 + 26^5 + 26^6               - 1, "ZZZZZZ")
        , (26 + 26^2 + 26^3 + 26^4 + 26^5 + 26^6 + 26^7        - 1, "ZZZZZZZ")
        , (26 + 26^2 + 26^3 + 26^4 + 26^5 + 26^6 + 26^7 + 26^8 - 1, "ZZZZZZZZ")
        ]

--------------------------------------------------------------------------------
-- Unit test support
--------------------------------------------------------------------------------

data UnitTestData params result = UnitTestData
    { params :: params
    , result :: result
    }
    deriving (Eq, Show)

unitTests
    :: (Eq result, Show result)
    => String
    -> (params -> result)
    -> [UnitTestData params result]
    -> Spec
unitTests title f unitTestData =
    describe title $
    forM_ (zip testNumbers unitTestData) $
        \(testNumber :: Int, test) -> do
            let subtitle = "Unit test #" <> show testNumber
            it subtitle $
                let resultExpected = result test in
                let resultActual = f (params test) in
                property $ Pretty resultExpected === Pretty resultActual
  where
    testNumbers :: [Int]
    testNumbers = [1 ..]

--------------------------------------------------------------------------------
-- Pretty-printing
--------------------------------------------------------------------------------

-- | A combinator that causes the output of `show` to be pretty-printed.
--
newtype Pretty a = Pretty { unPretty :: a }
    deriving Eq

instance Show a => Show (Pretty a) where
    show (Pretty a) = TL.unpack ("\n" <> pShow a <> "\n")

--------------------------------------------------------------------------------
-- Shrinking
--------------------------------------------------------------------------------

shrinkWhile :: (a -> Bool) -> (a -> [a]) -> a -> Maybe a
shrinkWhile condition shrink = loop
  where
    loop a
        | condition a =
            case L.find condition (shrink a) of
                Nothing -> Just a
                Just a' -> loop a'
        | otherwise =
            Nothing

--------------------------------------------------------------------------------
-- Test types
--------------------------------------------------------------------------------

newtype TestId = TestId Quid
    deriving (Eq, Ord)
    deriving Arbitrary via (Size 256 Quid)
    deriving Show via (Prefix "test-id:" (Chunked 4 "-" (UppercaseLatin Quid)))

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (Prefix p a) where
    arbitrary = Prefix <$> arbitrary
    shrink = shrinkMapBy Prefix unPrefix shrink

newtype Sized a = Sized { unSized :: a }
    deriving newtype (Eq, Ord, Read, Show)

newtype Width256 a = Width256 { unWidth256 :: a }
    deriving newtype (Eq, Ord, Read, Show)

instance Arbitrary (Sized Quid) where
    arbitrary = Sized <$> sized arbitraryQuid
    shrink = shrinkMapBy Sized unSized shrinkQuid

instance Arbitrary (Sized (UppercaseLatin Quid)) where
    arbitrary = Sized . UppercaseLatin <$> sized arbitraryQuid
    shrink = shrinkMapBy
        (Sized . UppercaseLatin)
        (unUppercaseLatin . unSized)
        shrinkQuid

instance Arbitrary (Width256 Quid) where
    arbitrary = Width256 <$> arbitraryQuid 256
    shrink = shrinkMapBy Width256 unWidth256 shrinkQuid
