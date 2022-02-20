{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.QuickCheck.Quid.Example where

import Test.QuickCheck
    ( Arbitrary, CoArbitrary )
import Test.QuickCheck.Quid
    ( Latin (..), Prefix (..), Quid (..), Size (..) )

newtype ExampleQuid = ExampleQuid Quid
    deriving newtype (Eq, Ord)
    deriving (Arbitrary, CoArbitrary) via (Size 256 Quid)
    deriving (Read, Show) via (Prefix "example-quid:" (Latin Quid))
