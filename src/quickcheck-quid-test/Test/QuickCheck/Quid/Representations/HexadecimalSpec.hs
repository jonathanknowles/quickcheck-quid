{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.QuickCheck.Quid.Representations.HexadecimalSpec
    where

import Internal.Test.QuickCheck.Quid
    ( Quid, arbitraryQuid, quidFromNatural, shrinkQuid )
import Internal.Test.QuickCheck.Quid.Representations.Hexadecimal
    ( Hexadecimal (..), HexadecimalString )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, it, parallel )
import Test.Hspec.Unit
    ( UnitTestData (..), unitTests )
import Test.QuickCheck
    ( Arbitrary (..), Property, property, shrinkMapBy, (===) )
import Test.QuickCheck.Classes.Hspec
    ( testLawsMany )

import Prelude hiding
    ( (^) )

import qualified Prelude
import qualified Test.QuickCheck.Classes as Laws

spec :: Spec
spec = do

    parallel $ describe "Lawfulness of type class instances" $ do
        testLawsMany @(Hexadecimal Quid)
            [ Laws.showLaws
            , Laws.showReadLaws
            ]

    parallel $ describe "Round-trip tests" $ do
        it "Roundtrip between Hexadecimal strings and quids" $
            property prop_roundTrip_HexadecimalString_Quid

    parallel $ describe "Unit tests" $ do
        unitTests_show_hexadecimal_quidFromNatural

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

prop_roundTrip_HexadecimalString_Quid :: HexadecimalString -> Property
prop_roundTrip_HexadecimalString_Quid hexadecimalString =
    show (read @(Hexadecimal Quid) expectedOutput) === expectedOutput
  where
    expectedOutput :: String
    expectedOutput = show hexadecimalString

--------------------------------------------------------------------------------
-- Unit tests
--------------------------------------------------------------------------------

unitTests_show_hexadecimal_quidFromNatural :: Spec
unitTests_show_hexadecimal_quidFromNatural = unitTests
    "unitTests_show_hexadecimal_quidFromNatural"
    (show . Hexadecimal . quidFromNatural)
    (mkTest <$> tests)
  where
    mkTest :: (Natural, String) -> UnitTestData Natural String
    mkTest (params, result) = UnitTestData {params, result}

    (^) :: Natural -> Natural -> Natural
    (^) = (Prelude.^)

    tests =
        [ ( 0, "0")
        , ( 1, "1")
        , (14, "E")
        , (15, "F")

        , (16 +  0, "00")
        , (16 +  1, "01")
        , (16 + 14, "0E")
        , (16 + 15, "0F")

        , (2 * 16 +  0, "10")
        , (2 * 16 +  1, "11")
        , (2 * 16 + 14, "1E")
        , (2 * 16 + 15, "1F")

        , (16 * 16 +  0, "F0")
        , (16 * 16 +  1, "F1")
        , (16 * 16 + 14, "FE")
        , (16 * 16 + 15, "FF")

        , (16                                          , "00")
        , (16 + 16^2                                   , "000")
        , (16 + 16^2 + 16^3                            , "0000")
        , (16 + 16^2 + 16^3 + 16^4                     , "00000")
        , (16 + 16^2 + 16^3 + 16^4 + 16^5              , "000000")
        , (16 + 16^2 + 16^3 + 16^4 + 16^5 + 16^6       , "0000000")
        , (16 + 16^2 + 16^3 + 16^4 + 16^5 + 16^6 + 16^7, "00000000")

        , (16                                                  - 1, "F")
        , (16 + 16^2                                           - 1, "FF")
        , (16 + 16^2 + 16^3                                    - 1, "FFF")
        , (16 + 16^2 + 16^3 + 16^4                             - 1, "FFFF")
        , (16 + 16^2 + 16^3 + 16^4 + 16^5                      - 1, "FFFFF")
        , (16 + 16^2 + 16^3 + 16^4 + 16^5 + 16^6               - 1, "FFFFFF")
        , (16 + 16^2 + 16^3 + 16^4 + 16^5 + 16^6 + 16^7        - 1, "FFFFFFF")
        , (16 + 16^2 + 16^3 + 16^4 + 16^5 + 16^6 + 16^7 + 16^8 - 1, "FFFFFFFF")
        ]

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary (Hexadecimal Quid) where
    arbitrary = Hexadecimal <$> arbitraryQuid
    shrink = shrinkMapBy Hexadecimal unHexadecimal shrinkQuid
