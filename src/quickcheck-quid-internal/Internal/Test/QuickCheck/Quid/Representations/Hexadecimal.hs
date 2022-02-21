{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Internal.Test.QuickCheck.Quid.Representations.Hexadecimal
    where

import Control.Applicative
    ( many )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( replicateM, void )
import Data.Data
    ( Data )
import Data.Hashable
    ( Hashable (..) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import GHC.Generics
    ( Generic )
import Internal.Test.QuickCheck
    ( shrinkListNonEmpty )
import Internal.Test.QuickCheck.Quid
    ( Quid (..) )
import Internal.Text.Read
    ( readCharMaybe, skipChar )
import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , arbitraryBoundedEnum
    , shrinkMap
    , shrinkMapBy
    , sized
    )
import Text.Read
    ( Read (..), ReadPrec, readMaybe )

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE

--------------------------------------------------------------------------------
-- Hexadecimal representation
--------------------------------------------------------------------------------

newtype Hexadecimal a = Hexadecimal { unHexadecimal :: a }
    deriving (Data, Eq, Generic, Hashable, NFData, Ord)

instance Read (Hexadecimal Quid) where
    readPrec = Hexadecimal . hexadecimalStringToQuid <$> readPrec

instance Show (Hexadecimal Quid) where
    show = show . hexadecimalStringFromQuid . unHexadecimal

--------------------------------------------------------------------------------
-- Hexadecimal characters
--------------------------------------------------------------------------------

data HexadecimalChar
    = H0 | H1 | H2 | H3 | H4 | H5 | H6 | H7
    | H8 | H9 | HA | HB | HC | HD | HE | HF
    deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance Arbitrary HexadecimalChar where
    arbitrary = arbitraryHexadecimalChar
    shrink = shrinkHexadecimalChar

--------------------------------------------------------------------------------
-- Generation and shrinking of arbitrary hexadecimal characters
--------------------------------------------------------------------------------

arbitraryHexadecimalChar :: Gen HexadecimalChar
arbitraryHexadecimalChar = arbitraryBoundedEnum

shrinkHexadecimalChar :: HexadecimalChar -> [HexadecimalChar]
shrinkHexadecimalChar = shrinkMap toEnum fromEnum

--------------------------------------------------------------------------------
-- Conversion between hexadecimal characters and ordinary characters
--------------------------------------------------------------------------------

hexadecimalCharFromChar :: Char -> Maybe HexadecimalChar
hexadecimalCharFromChar c = readMaybe ['H', c]

hexadecimalCharToChar :: HexadecimalChar -> Char
hexadecimalCharToChar = last . show

--------------------------------------------------------------------------------
-- Hexadecimal strings
--------------------------------------------------------------------------------

newtype HexadecimalString = HexadecimalString
    { unHexadecimalString :: NonEmpty HexadecimalChar }
    deriving (Eq, Ord)

instance Arbitrary HexadecimalString where
    arbitrary = arbitraryHexadecimalString
    shrink = shrinkHexadecimalString

--------------------------------------------------------------------------------
-- Conversion between hexadecimal strings and ordinary strings
--------------------------------------------------------------------------------

instance Read HexadecimalString where
    readPrec = do
        void $ many (skipChar ' ')
        HexadecimalString <$> ((:|) <$> readChar <*> many readChar)
      where
        readChar :: ReadPrec HexadecimalChar
        readChar = readCharMaybe hexadecimalCharFromChar

instance Show HexadecimalString where
    show (HexadecimalString cs) = F.foldMap (pure . hexadecimalCharToChar) cs

--------------------------------------------------------------------------------
-- Generation and shrinking of arbitrary hexadecimal strings
--------------------------------------------------------------------------------

arbitraryHexadecimalString :: Gen HexadecimalString
arbitraryHexadecimalString = sized $ \size ->
    fmap HexadecimalString . (:|)
        <$> arbitraryHexadecimalChar
        <*> replicateM size arbitraryHexadecimalChar

shrinkHexadecimalString :: HexadecimalString -> [HexadecimalString]
shrinkHexadecimalString =
    shrinkMapBy HexadecimalString unHexadecimalString $
    shrinkListNonEmpty shrinkHexadecimalChar

--------------------------------------------------------------------------------
-- Conversion between hexadecimal strings and quids
--------------------------------------------------------------------------------

hexadecimalStringToQuid :: HexadecimalString -> Quid
hexadecimalStringToQuid (HexadecimalString xs) = Quid $
    F.foldl' f 0 xs - 1
  where
    f !acc !x = acc * 16 + 1 + fromIntegral (fromEnum x)

hexadecimalStringFromQuid :: Quid -> HexadecimalString
hexadecimalStringFromQuid (Quid q) =
    HexadecimalString . NE.fromList $ go [] q
  where
    go :: [HexadecimalChar] -> Natural -> [HexadecimalChar]
    go !acc !n
        | n < 16 =
            toEnum (fromIntegral n) : acc
        | otherwise =
            go (toEnum (fromIntegral (n `mod` 16)) : acc) (n `div` 16 - 1)
