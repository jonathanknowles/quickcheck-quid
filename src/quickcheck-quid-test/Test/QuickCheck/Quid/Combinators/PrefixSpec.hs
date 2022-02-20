{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.QuickCheck.Quid.Combinators.PrefixSpec
    where

import Internal.Test.QuickCheck.Quid.Combinators.Prefix
    ( Prefix (..) )
import Test.Hspec
    ( Spec, describe, it, parallel )
import Test.QuickCheck
    ( Arbitrary (..), shrinkMapBy )
import Test.QuickCheck.Classes.Hspec
    ( testLawsMany )

import qualified Data.Text.Lazy as TL
import qualified Test.QuickCheck.Classes as Laws

spec :: Spec
spec = do

    parallel $ describe "Lawfulness of type class instances" $ do
        testLawsMany @(Prefix "A" Int)
            [ Laws.showLaws
            , Laws.showReadLaws
            ]
        testLawsMany @(Prefix ":" Int)
            [ Laws.showLaws
            , Laws.showReadLaws
            ]
        testLawsMany @(Prefix "1" Int)
            [ Laws.showLaws
            , Laws.showReadLaws
            ]
        testLawsMany @(Prefix "test-prefix:" Int)
            [ Laws.showLaws
            , Laws.showReadLaws
            ]

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (Prefix p a) where
    arbitrary = Prefix <$> arbitrary
    shrink = shrinkMapBy Prefix unPrefix shrink
