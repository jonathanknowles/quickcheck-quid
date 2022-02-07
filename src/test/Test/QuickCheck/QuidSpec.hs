{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
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
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, it, parallel )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , checkCoverage
    , conjoin
    , counterexample
    , cover
    , forAll
    , forAllBlind
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
import Test.QuickCheck.Natural
    ( arbitraryNatural, shrinkNatural )
import Test.QuickCheck.Quid
    ( Quid
    , arbitraryQuid
    , quidFromNatural
    , shrinkQuid
    )
import Text.Pretty.Simple
    ( pShow )

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy as TL
import qualified Test.QuickCheck.Classes as Laws

-- TODO
-- Relationships between strings of A and naturals
-- Relationships between strings of Z and naturals

spec :: Spec
spec = do

    parallel $ describe "Lawfulness of type class instances" $ do
        testLawsMany @(Sized Quid)
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

    parallel $ describe "Unit tests" $ do
        unitTests_show_quidFromNatural

--------------------------------------------------------------------------------
-- Uniformity
--------------------------------------------------------------------------------

prop_arbitraryQuid_uniform :: Property
prop_arbitraryQuid_uniform =
    withMaxSuccess 1 $
    forAll generateFrequencies $ \(m, (c1, f1), (c2, f2)) ->
        conjoin
            [ all (`Map.member` m) ['A' .. 'Z']
            , f1 % f2 > 90 % 100
            , f2 % f1 > 90 % 100
            ]
  where
    generateFrequencies :: Gen (Map Char Int, (Char, Int), (Char, Int))
    generateFrequencies = do
        quids <- fmap unSized <$> replicateM 1_000_000 (arbitrary @(Sized Quid))
        let charFrequencies = charFrequenciesOfQuids quids
        pure
            ( charFrequencies
            , smallestCharFrequency charFrequencies
            , greatestCharFrequency charFrequencies
            )

    greatestCharFrequency :: Map Char Int -> (Char, Int)
    greatestCharFrequency = head . L.sortOn (Down . snd) . Map.toList

    smallestCharFrequency :: Map Char Int -> (Char, Int)
    smallestCharFrequency = head . L.sortOn snd . Map.toList

    charFrequenciesOfQuid :: Quid -> Map Char Int
    charFrequenciesOfQuid q = foldr
        (\c m -> Map.insertWith (+) c 1 m)
        Map.empty
        (filter (/= '-') (show q))

    charFrequenciesOfQuids :: [Quid] -> Map Char Int
    charFrequenciesOfQuids qs =
        Map.unionsWith (+) (charFrequenciesOfQuid <$> qs)

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
    arbitraryFixedWidthQuids = fmap unWidth32 <$>
        replicateM 1_000_000 (arbitrary @(Width32 Quid))

--------------------------------------------------------------------------------
-- Shrinkability
--------------------------------------------------------------------------------

prop_shrinkQuid_lessThan :: Width32 Quid -> Property
prop_shrinkQuid_lessThan (Width32 q) =
    property $ all (< q) (shrinkQuid q)

prop_shrinkQuid_minimalElement :: Sized Quid -> Property
prop_shrinkQuid_minimalElement (Sized q) =
    checkCoverage $
    cover 10 (q /= minimalQuid) "q /= minimalQuid" $
    case shrinkQuid q of
        s : _ -> s === minimalQuid
        _     -> q === minimalQuid
  where
    minimalQuid = read "A"

prop_shrinkQuid_minimalSet :: [Width32 Quid] -> Property
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
    minimalSet = Set.map unWidth32 $ fromMaybe
        (error "Cannot shrink to minimal set")
        (shrinkWhile ((>= expectedSize) . Set.size) shrink (Set.fromList qs))

prop_shrinkQuid_ordered :: Width32 Quid -> Property
prop_shrinkQuid_ordered (Width32 q) =
    L.sort shrunkValues === shrunkValues
  where
    shrunkValues = shrinkQuid q

prop_shrinkQuid_unique :: Width32 Quid -> Property
prop_shrinkQuid_unique (Width32 q) =
    Set.size (Set.fromList shrunkValues) === L.length shrunkValues
  where
    shrunkValues = shrinkQuid q

--------------------------------------------------------------------------------
-- Unit tests
--------------------------------------------------------------------------------

unitTests_show_quidFromNatural :: Spec
unitTests_show_quidFromNatural = unitTests
    "unitTests_show_quidFromNatural"
    (show . quidFromNatural)
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
        , (26 + 26^2 + 26^3 + 26^4                     , "AAAA-A")
        , (26 + 26^2 + 26^3 + 26^4 + 26^5              , "AAAA-AA")
        , (26 + 26^2 + 26^3 + 26^4 + 26^5 + 26^6       , "AAAA-AAA")
        , (26 + 26^2 + 26^3 + 26^4 + 26^5 + 26^6 + 26^7, "AAAA-AAAA")

        , (26                                                  - 1, "Z")
        , (26 + 26^2                                           - 1, "ZZ")
        , (26 + 26^2 + 26^3                                    - 1, "ZZZ")
        , (26 + 26^2 + 26^3 + 26^4                             - 1, "ZZZZ")
        , (26 + 26^2 + 26^3 + 26^4 + 26^5                      - 1, "ZZZZ-Z")
        , (26 + 26^2 + 26^3 + 26^4 + 26^5 + 26^6               - 1, "ZZZZ-ZZ")
        , (26 + 26^2 + 26^3 + 26^4 + 26^5 + 26^6 + 26^7        - 1, "ZZZZ-ZZZ")
        , (26 + 26^2 + 26^3 + 26^4 + 26^5 + 26^6 + 26^7 + 26^8 - 1, "ZZZZ-ZZZZ")
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
-- Arbitrary instances
--------------------------------------------------------------------------------

newtype Sized a = Sized { unSized :: a }
    deriving newtype (Eq, Ord, Read, Show)

newtype Width32 a = Width32 { unWidth32 :: a }
    deriving newtype (Eq, Ord, Read, Show)

instance Arbitrary (Sized Quid) where
    arbitrary = Sized <$> sized arbitraryQuid
    shrink = shrinkMapBy Sized unSized shrinkQuid

instance Arbitrary (Width32 Quid) where
    arbitrary = Width32 <$> arbitraryQuid 32
    shrink = shrinkMapBy Width32 unWidth32 shrinkQuid
