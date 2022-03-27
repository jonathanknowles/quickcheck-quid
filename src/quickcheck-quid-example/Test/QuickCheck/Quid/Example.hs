{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Test.QuickCheck.Quid.Example where

import GHC.Generics
    ( Generic )
import Test.QuickCheck
    ( Arbitrary, CoArbitrary, Function )
import Test.QuickCheck.Quid
    ( Decimal (..), Hexadecimal (..), Latin (..), Quid (..), Size (..) )

newtype ExampleDecimalQuid = ExampleDecimalQuid (Decimal Quid)
    deriving Arbitrary via (Size 256 Quid)
    deriving CoArbitrary via Quid
    deriving anyclass Function
    deriving stock (Eq, Generic, Ord, Read, Show)

newtype ExampleHexadecimalQuid = ExampleHexadecimalQuid (Hexadecimal Quid)
    deriving Arbitrary via (Size 256 Quid)
    deriving CoArbitrary via Quid
    deriving anyclass Function
    deriving stock (Eq, Generic, Ord, Read, Show)

newtype ExampleLatinQuid = ExampleLatinQuid (Latin Quid)
    deriving Arbitrary via (Size 256 Quid)
    deriving CoArbitrary via Quid
    deriving anyclass Function
    deriving stock (Eq, Generic, Ord, Read, Show)
