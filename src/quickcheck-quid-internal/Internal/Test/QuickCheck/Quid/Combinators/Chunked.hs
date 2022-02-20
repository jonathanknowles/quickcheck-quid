{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Internal.Test.QuickCheck.Quid.Combinators.Chunked
    where

import Control.DeepSeq
    ( NFData )
import Data.Data
    ( Data )
import Data.Hashable
    ( Hashable (..) )
import Data.List.Extra
    ( chunksOf )
import Data.Proxy
    ( Proxy (..) )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal )

import qualified Data.List as L

--------------------------------------------------------------------------------
-- Chunked combinator
--------------------------------------------------------------------------------

newtype Chunked (n :: Nat) (s :: Symbol) a = Chunked { unChunked :: a }
    deriving (Data, Eq, Generic, Hashable, NFData, Ord)

instance (KnownNat n, KnownSymbol s, Show a) => Show (Chunked n s a) where
    show (Chunked a)
        = L.intercalate (symbolVal (Proxy @s))
        $ chunksOf (fromIntegral $ natVal $ Proxy @n)
        $ show a
