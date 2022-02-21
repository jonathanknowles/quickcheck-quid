{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.QuickCheck.QuidSpec
    where

import Control.Arrow
    ( (&&&) )
import Control.Monad
    ( replicateM )
import Data.Maybe
    ( fromMaybe )
import Data.Ord
    ( Down (..) )
import Data.Ratio
    ( (%) )
import Data.Set
    ( Set )
import Internal.Test.QuickCheck.Quid
    ( Quid, arbitraryQuid, quidFromNatural, quidToNatural, shrinkQuid )
import Internal.Test.QuickCheck.Quid.Combinators.Prefix
    ( Prefix (..) )
import Internal.Test.QuickCheck.Quid.Combinators.Size
    ( Size (..) )
import Internal.Test.QuickCheck.Quid.Representations.Latin
    ( Latin (..) )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, it, parallel )
import Test.QuickCheck
    ( Arbitrary (..)
    , Fixed (..)
    , Gen
    , Property
    , checkCoverage
    , choose
    , conjoin
    , counterexample
    , cover
    , forAllBlind
    , frequency
    , label
    , property
    , resize
    , shrinkMapBy
    , withMaxSuccess
    , (===)
    )
import Test.QuickCheck.Classes.Hspec
    ( testLawsMany )
import Text.Printf
    ( printf )

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Test.QuickCheck.Classes as Laws

spec :: Spec
spec = do

    parallel $ describe "Lawfulness of type class instances" $ do
        testLawsMany @Quid
            [ Laws.eqLaws
            , Laws.ordLaws
            ]
        testLawsMany @TestId
            [ Laws.eqLaws
            , Laws.ordLaws
            , Laws.showLaws
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
    arbitraryQuids = replicateM arbitraryQuidCount $
        resize sizeExponent arbitraryQuid

    arbitraryQuidCount :: Int
    arbitraryQuidCount = bucketCount * 256

    bucketCount :: Int
    bucketCount = fromIntegral $ min 256 size

    bucketSize :: Natural
    bucketSize = max 1 (size `div` fromIntegral bucketCount)

    size :: Natural
    size = 2 ^ fromIntegral @Int @Natural sizeExponent

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
    forAllBlind arbitraryFixedSizeQuids $ \uids ->
        Set.size (Set.fromList uids) === L.length uids
  where
    arbitraryFixedSizeQuids :: Gen [Quid]
    arbitraryFixedSizeQuids = fmap (unSize . getFixed) <$>
        replicateM 1_000_000 (arbitrary @(Fixed (Size 256 Quid)))

--------------------------------------------------------------------------------
-- Shrinkability
--------------------------------------------------------------------------------

prop_shrinkQuid_lessThan :: Size 256 Quid -> Property
prop_shrinkQuid_lessThan (Size q) =
    property $ all (< q) (shrinkQuid q)

prop_shrinkQuid_minimalElement :: Quid -> Property
prop_shrinkQuid_minimalElement q =
    checkCoverage $
    cover 10 (q /= minimalQuid) "q /= minimalQuid" $
    case shrinkQuid q of
        s : _ -> s === minimalQuid
        _     -> q === minimalQuid
  where
    minimalQuid = quidFromNatural 0

prop_shrinkQuid_minimalSet :: [Size 256 Quid] -> Property
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
        lo = size `div` 10 * 10
        hi = lo + 9

    expectedSize :: Int
    expectedSize = L.length qs

    minimalSet :: Set Quid
    minimalSet = Set.map unSize $ fromMaybe
        (error "Cannot shrink to minimal set")
        (shrinkWhile ((>= expectedSize) . Set.size) shrink (Set.fromList qs))

prop_shrinkQuid_ordered :: Size 256 Quid -> Property
prop_shrinkQuid_ordered (Size q) =
    L.sort shrunkValues === shrunkValues
  where
    shrunkValues = shrinkQuid q

prop_shrinkQuid_unique :: Size 256 Quid -> Property
prop_shrinkQuid_unique (Size q) =
    Set.size (Set.fromList shrunkValues) === L.length shrunkValues
  where
    shrunkValues = shrinkQuid q

--------------------------------------------------------------------------------
-- Shrinking
--------------------------------------------------------------------------------

shrinkWhile :: (a -> Bool) -> (a -> [a]) -> a -> Maybe a
shrinkWhile condition shrinkFn = loop
  where
    loop a
        | condition a =
            case L.find condition (shrinkFn a) of
                Nothing -> Just a
                Just a' -> loop a'
        | otherwise =
            Nothing

--------------------------------------------------------------------------------
-- Frequencies
--------------------------------------------------------------------------------

newtype Frequency = Frequency {unFrequency :: Natural}
    deriving (Eq, Ord, Show)

instance Semigroup Frequency where
    Frequency f1 <> Frequency f2 = Frequency (f1 + f2)

instance Monoid Frequency where
    mempty = Frequency 1

frequencies :: (Foldable f, Ord k) => f k -> [(k, Frequency)]
frequencies
    = L.sortOn ((Down . snd) &&& fst)
    . Map.toList
    . L.foldr (flip (Map.insertWith (<>)) mempty) Map.empty

--------------------------------------------------------------------------------
-- Test types
--------------------------------------------------------------------------------

newtype TestId = TestId Quid
    deriving (Eq, Ord)
    deriving Arbitrary via (Size 256 Quid)
    deriving (Read, Show) via (Prefix "test-id:" (Latin Quid))
