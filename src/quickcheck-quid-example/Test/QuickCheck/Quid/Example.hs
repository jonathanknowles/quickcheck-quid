{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.QuickCheck.Quid.Example where

import GHC.Generics
    ( Generic )
import Test.QuickCheck
    ( Arbitrary, CoArbitrary, Function )
import Test.QuickCheck.Quid
    ( Decimal (..), Hexadecimal (..), Latin (..), Quid (..), Size (..) )

newtype ExampleDecimalQuid = ExampleDecimalQuid (Decimal Quid)
    deriving stock (Eq, Generic, Ord, Read, Show)
    deriving Arbitrary via (Size 256 Quid)
    deriving CoArbitrary via Quid
    deriving anyclass Function
    deriving newtype Num

newtype ExampleHexadecimalQuid = ExampleHexadecimalQuid (Hexadecimal Quid)
    deriving stock (Eq, Generic, Ord, Read, Show)
    deriving Arbitrary via (Size 256 Quid)
    deriving CoArbitrary via Quid
    deriving anyclass Function
    deriving newtype Num

newtype ExampleLatinQuid = ExampleLatinQuid (Latin Quid)
    deriving stock (Eq, Generic, Ord, Read, Show)
    deriving Arbitrary via (Size 256 Quid)
    deriving CoArbitrary via Quid
    deriving anyclass Function
