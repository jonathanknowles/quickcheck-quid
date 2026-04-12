{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.QuickCheck.Quid.Combinators.PrefixSpec
    where

import Internal.Test.QuickCheck.Quid.Combinators.Prefix
    ( Prefix (..) )
import Test.Hspec
    ( Spec, describe, parallel )
import Test.QuickCheck
    ( Arbitrary (..), shrinkMapBy )
import Test.Hspec.QuickCheck.Classes
    ( testLaws )

import qualified Test.QuickCheck.Classes as Laws

spec :: Spec
spec = do

    parallel $ describe "Lawfulness of type class instances" $ do
        testLaws @(Prefix "A" Int)
            [ Laws.showLaws
            , Laws.showReadLaws
            ]
        testLaws @(Prefix ":" Int)
            [ Laws.showLaws
            , Laws.showReadLaws
            ]
        testLaws @(Prefix "1" Int)
            [ Laws.showLaws
            , Laws.showReadLaws
            ]
        testLaws @(Prefix "test-prefix:" Int)
            [ Laws.showLaws
            , Laws.showReadLaws
            ]

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (Prefix p a) where
    arbitrary = Prefix <$> arbitrary
    shrink = shrinkMapBy Prefix unPrefix shrink
