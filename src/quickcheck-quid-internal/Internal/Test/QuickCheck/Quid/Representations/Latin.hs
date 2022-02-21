{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Internal.Test.QuickCheck.Quid.Representations.Latin
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
import Data.Maybe
    ( mapMaybe )
import GHC.Generics
    ( Generic )
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
    , shrinkList
    , shrinkMap
    , shrinkMapBy
    , sized
    )
import Text.Read
    ( Read (..), ReadPrec, readMaybe )

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE

--------------------------------------------------------------------------------
-- Latin representation
--------------------------------------------------------------------------------

newtype Latin a = Latin { unLatin :: a }
    deriving (Data, Eq, Generic, Hashable, NFData, Ord)

instance Read (Latin Quid) where
    readPrec = Latin . latinStringToQuid <$> readPrec

instance Show (Latin Quid) where
    show = show . latinStringFromQuid . unLatin

--------------------------------------------------------------------------------
-- Latin characters
--------------------------------------------------------------------------------

data LatinChar
    = A | B | C | D | E | F | G | H | I | J | K | L | M
    | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Arbitrary LatinChar where
    arbitrary = arbitraryLatinChar
    shrink = shrinkLatinChar

--------------------------------------------------------------------------------
-- Generation and shrinking of arbitrary Latin characters
--------------------------------------------------------------------------------

arbitraryLatinChar :: Gen LatinChar
arbitraryLatinChar = arbitraryBoundedEnum

shrinkLatinChar :: LatinChar -> [LatinChar]
shrinkLatinChar = shrinkMap toEnum fromEnum

--------------------------------------------------------------------------------
-- Conversion between Latin characters and ordinary characters
--------------------------------------------------------------------------------

latinCharFromChar :: Char -> Maybe LatinChar
latinCharFromChar c = readMaybe [c]

latinCharToChar :: LatinChar -> Char
latinCharToChar = head . show

--------------------------------------------------------------------------------
-- Latin strings
--------------------------------------------------------------------------------

newtype LatinString = LatinString
    { unLatinString :: NonEmpty LatinChar }
    deriving (Eq, Ord)

instance Arbitrary LatinString where
    arbitrary = arbitraryLatinString
    shrink = shrinkLatinString

--------------------------------------------------------------------------------
-- Conversion between Latin strings and ordinary strings
--------------------------------------------------------------------------------

instance Read LatinString where
    readPrec = do
        void $ many (skipChar ' ')
        LatinString <$> ((:|) <$> readChar <*> many readChar)
      where
        readChar :: ReadPrec LatinChar
        readChar = readCharMaybe latinCharFromChar

instance Show LatinString where
    show (LatinString cs) = F.foldMap show cs

--------------------------------------------------------------------------------
-- Generation and shrinking of arbitrary Latin strings
--------------------------------------------------------------------------------

arbitraryLatinString :: Gen LatinString
arbitraryLatinString = sized $ \size ->
    fmap LatinString . (:|)
        <$> arbitraryLatinChar
        <*> replicateM size arbitraryLatinChar

shrinkLatinString :: LatinString -> [LatinString]
shrinkLatinString =
    shrinkMapBy LatinString unLatinString $ shrinkListNonEmpty shrinkLatinChar

--------------------------------------------------------------------------------
-- Conversion between Latin strings and quids
--------------------------------------------------------------------------------

latinStringToQuid :: LatinString -> Quid
latinStringToQuid (LatinString xs) = Quid $
    F.foldl' f 0 xs - 1
  where
    f !acc !x = acc * 26 + 1 + fromIntegral (fromEnum x)

latinStringFromQuid :: Quid -> LatinString
latinStringFromQuid (Quid q) =
    LatinString . NE.fromList $ go [] q
  where
    go :: [LatinChar] -> Natural -> [LatinChar]
    go !acc !n
        | n < 26 =
            toEnum (fromIntegral n) : acc
        | otherwise =
            go (toEnum (fromIntegral (n `mod` 26)) : acc) (n `div` 26 - 1)

--------------------------------------------------------------------------------
-- Shrinking support
--------------------------------------------------------------------------------

shrinkListNonEmpty :: (a -> [a]) -> NonEmpty a -> [NonEmpty a]
shrinkListNonEmpty shrinkFn =
    mapMaybe NE.nonEmpty . shrinkList shrinkFn . F.toList
