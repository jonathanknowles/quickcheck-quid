{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Internal.Test.QuickCheck.Quid.Combinators.Size
    where

import Control.DeepSeq
    ( NFData )
import Data.Data
    ( Data )
import Data.Hashable
    ( Hashable (..) )
import Data.Proxy
    ( Proxy (..) )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal )
import Test.QuickCheck
    ( Arbitrary (..), CoArbitrary, Function, resize, shrinkMapBy )

--------------------------------------------------------------------------------
-- Sizes
--------------------------------------------------------------------------------

newtype Size (n :: Nat) a = Size { unSize :: a }
    deriving (CoArbitrary, Data, Eq, Generic, Hashable, NFData, Ord, Read, Show)

instance (Arbitrary a, KnownNat n) => Arbitrary (Size n a) where
    arbitrary = Size <$> resize (fromIntegral $ natVal $ Proxy @n) arbitrary
    shrink = shrinkMapBy Size unSize shrink
