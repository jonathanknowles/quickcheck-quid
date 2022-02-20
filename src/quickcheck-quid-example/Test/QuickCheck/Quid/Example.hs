{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Test.QuickCheck.Quid.Example where

import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Arbitrary
    , CoArbitrary
    , Fun
    , Function
    , Property
    , applyFun
    , label
    , property
    )
import Test.QuickCheck.Quid
    ( Latin (..), Prefix (..), Quid (..), Size (..) )

newtype ExampleQuid = ExampleQuid {unExampleQuid :: Quid}
    deriving (Read, Show) via (Prefix "example-quid:" (Latin Quid))
    deriving Arbitrary via (Size 256 Quid)
    deriving CoArbitrary via Quid
    deriving anyclass Function
    deriving stock (Eq, Generic, Ord)

prop_function :: Fun ExampleQuid Bool -> ExampleQuid -> Property
prop_function f q =
    label (show (applyFun f q)) $
    property True
