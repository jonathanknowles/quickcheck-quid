{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Internal.Test.QuickCheck.Quid
    where

import Control.Applicative
    ( many, (<|>) )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( mzero, replicateM, replicateM_ )
import Data.Data
    ( Data )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.Hashable
    ( Hashable (..) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( catMaybes )
import Data.Ord
    ( Down (..) )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal )
import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Arbitrary (..)
    , Function (..)
    , Gen
    , arbitraryBoundedEnum
    , chooseInteger
    , coarbitraryShow
    , functionShow
    , frequency
    , resize
    , shrinkMap
    , shrinkMapBy
    , sized
    )
import Test.QuickCheck.Function
    ( (:->) )
import Text.Read
    ( Read (..), ReadPrec (..), choice, get, look, pfail, readMaybe )

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Test.QuickCheck as QC

--------------------------------------------------------------------------------
-- Quids
--------------------------------------------------------------------------------

newtype Quid = Quid
    { unQuid :: Natural }
    deriving (Data, Eq, Generic, Hashable, NFData, Ord, Read, Show)

--------------------------------------------------------------------------------
-- Sizes
--------------------------------------------------------------------------------

newtype Size (n :: Nat) a = Size { unSize :: a }
    deriving (Data, Eq, Generic, Hashable, NFData, Ord)

instance KnownNat n => Arbitrary (Size n Quid) where
    arbitrary = Size <$> resize (fromIntegral $ natVal $ Proxy @n) arbitraryQuid
    shrink = shrinkMapBy Size unSize shrinkQuid

--------------------------------------------------------------------------------
-- Generation and shrinking of arbitrary quids
--------------------------------------------------------------------------------

arbitraryQuid :: Gen Quid
arbitraryQuid = sized $ \i -> chooseQuid (Quid 0, Quid $ (2 ^ max 0 i) - 1)

chooseQuid :: (Quid, Quid) -> Gen Quid
chooseQuid (Quid n1, Quid n2) = Quid <$> chooseNatural (n1, n2)

coarbitraryQuid :: Quid -> Gen a -> Gen a
coarbitraryQuid = coarbitraryShow

functionQuid :: (Quid -> a) -> Quid :-> a
functionQuid = functionShow

shrinkQuid :: Quid -> [Quid]
shrinkQuid = shrinkMapBy Quid unQuid shrinkNatural

--------------------------------------------------------------------------------
-- Conversion between quids and natural numbers
--------------------------------------------------------------------------------

quidFromNatural :: Natural -> Quid
quidFromNatural = Quid

quidToNatural :: Quid -> Natural
quidToNatural = unQuid

--------------------------------------------------------------------------------
-- Natural number support
--------------------------------------------------------------------------------

chooseNatural :: (Natural, Natural) -> Gen Natural
chooseNatural (p, q) = fromIntegral @Integer @Natural <$>
    chooseInteger (fromIntegral p, fromIntegral q)

shrinkNatural :: Natural -> [Natural]
shrinkNatural n
    | n == 0 = []
    | otherwise = L.nub $ 0 : as <> bs
  where
    as = takeWhile (<= n `div` 2) (iterate (* 2) 1)
    bs = (n -) <$> reverse as
