{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.QuickCheck.Quid.Representations.LatinSpec
    where

import Control.Monad
    ( forM_ )
import Internal.Test.QuickCheck.Quid
    ( Quid, arbitraryQuid, quidFromNatural, shrinkQuid )
import Internal.Test.QuickCheck.Quid.Representations.Latin
    ( Latin (..) )
import Test.Hspec
    ( Spec, describe, it, parallel )
import Test.QuickCheck
    ( Arbitrary (..), property, sized, shrinkMapBy, (===) )
import Test.QuickCheck.Classes.Hspec
    ( testLawsMany )
import Text.Pretty.Simple
    ( pShow )

import qualified Data.Text.Lazy as TL
import qualified Test.QuickCheck.Classes as Laws

spec :: Spec
spec = do

    parallel $ describe "Lawfulness of type class instances" $ do
        testLawsMany @(Latin Quid)
            [ Laws.showLaws
            , Laws.showReadLaws
            ]

    parallel $ describe "Unit tests" $ do
        unitTests_show_latin_quidFromNatural

--------------------------------------------------------------------------------
-- Unit tests
--------------------------------------------------------------------------------

unitTests_show_latin_quidFromNatural :: Spec
unitTests_show_latin_quidFromNatural = unitTests
    "unitTests_show_latin_quidFromNatural"
    (show . Latin . quidFromNatural)
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
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary (Latin Quid) where
    arbitrary = Latin <$> arbitraryQuid
    shrink = shrinkMapBy Latin unLatin shrinkQuid
