{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Internal.Test.QuickCheck.Quid.Representations.Latin
    where

import Control.Applicative
    ( many, (<|>) )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( mzero, replicateM, replicateM_ )
import Data.Data
    ( Data )
import Data.Hashable
    ( Hashable (..) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import GHC.Generics
    ( Generic )
import Internal.Test.QuickCheck.Quid
    ( Quid (..) )
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
    , shrinkMap
    , shrinkMapBy
    )
import Text.Read
    ( Read (..), ReadPrec (..), choice, get, look, pfail, readMaybe )

import qualified Data.Foldable as F
import qualified Data.List as L
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

--------------------------------------------------------------------------------
-- Conversion between Latin strings and ordinary strings
--------------------------------------------------------------------------------

instance Read LatinString where
    readPrec = do
        many (skipChar ' ')
        LatinString <$> ((:|) <$> readChar <*> many readChar)
      where
        readChar :: ReadPrec LatinChar
        readChar = readCharMaybe latinCharFromChar

instance Show LatinString where
    show (LatinString cs) = F.foldMap show cs

--------------------------------------------------------------------------------
-- Generation of arbitrary Latin strings
--------------------------------------------------------------------------------

arbitraryLatinString :: Int -> Gen LatinString
arbitraryLatinString stringLen =
    LatinString . NE.fromList <$>
    replicateM (max 1 stringLen) arbitraryLatinChar

--------------------------------------------------------------------------------
-- Conversion between Latin strings and quids
--------------------------------------------------------------------------------

latinStringToQuid :: LatinString -> Quid
latinStringToQuid (LatinString xs) = Quid $
    F.foldl' f 0 xs - 1
  where
    f !acc !x = acc * 26 + 1 + fromIntegral (fromEnum x)

latinStringFromQuid :: Quid -> LatinString
latinStringFromQuid (Quid n) =
    LatinString . NE.fromList $ go [] n
  where
    go :: [LatinChar] -> Natural -> [LatinChar]
    go !acc !n
        | n < 26 =
            toEnum (fromIntegral n) : acc
        | otherwise =
            go (toEnum (fromIntegral (n `mod` 26)) : acc) (n `div` 26 - 1)

--------------------------------------------------------------------------------
-- Reading suppport
--------------------------------------------------------------------------------

readCharMaybe :: (Char -> Maybe a) -> ReadPrec a
readCharMaybe f = look >>= \case
    a : _ | Just c <- f a ->
        get >> pure c
    _ ->
        pfail

skipString :: String -> ReadPrec ()
skipString stringToSkip = do
    remainder <- look
    if stringToSkip `L.isPrefixOf` remainder
    then replicateM_ (length stringToSkip) get
    else pfail

skipChar :: Char -> ReadPrec ()
skipChar charToSkip = readCharMaybe
    (\char -> if char == charToSkip then Just () else Nothing)

readWith :: (String -> Maybe a) -> (Int -> String -> [(a, String)])
readWith f _ = maybe [] (pure . (, "")) . f
