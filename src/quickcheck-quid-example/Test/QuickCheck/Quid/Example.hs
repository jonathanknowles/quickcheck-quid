{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}

module Test.QuickCheck.Quid.Example where

import Test.QuickCheck
    ( Arbitrary )
import Test.QuickCheck.Quid
    ( Latin (..), Prefix (..), Quid (..), Size (..) )

newtype ExampleQuid = ExampleQuid Quid
    deriving (Eq, Ord)
    deriving Arbitrary via (Size 256 Quid)
    deriving (Read, Show) via (Prefix "example-quid:" (Latin Quid))
