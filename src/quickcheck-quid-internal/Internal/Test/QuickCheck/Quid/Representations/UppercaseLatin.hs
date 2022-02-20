{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Internal.Test.QuickCheck.Quid.Representations.UppercaseLatin
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
-- Uppercase Latin representation
--------------------------------------------------------------------------------

newtype UppercaseLatin a = UppercaseLatin { unUppercaseLatin :: a }
    deriving (Data, Eq, Generic, Hashable, NFData, Ord)

instance Read (UppercaseLatin Quid) where
    readPrec = UppercaseLatin . uppercaseLatinStringToQuid <$> readPrec

instance Show (UppercaseLatin Quid) where
    show = show . uppercaseLatinStringFromQuid . unUppercaseLatin

--------------------------------------------------------------------------------
-- Uppercase Latin characters
--------------------------------------------------------------------------------

data UppercaseLatinChar
    = A | B | C | D | E | F | G | H | I | J | K | L | M
    | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

--------------------------------------------------------------------------------
-- Generation and shrinking of arbitrary uppercase Latin characters
--------------------------------------------------------------------------------

arbitraryUppercaseLatinChar :: Gen UppercaseLatinChar
arbitraryUppercaseLatinChar = arbitraryBoundedEnum

shrinkUppercaseLatinChar :: UppercaseLatinChar -> [UppercaseLatinChar]
shrinkUppercaseLatinChar = shrinkMap toEnum fromEnum

--------------------------------------------------------------------------------
-- Conversion between uppercase Latin characters and ordinary characters
--------------------------------------------------------------------------------

uppercaseLatinCharFromChar :: Char -> Maybe UppercaseLatinChar
uppercaseLatinCharFromChar c = readMaybe [c]

uppercaseLatinCharToChar :: UppercaseLatinChar -> Char
uppercaseLatinCharToChar = head . show

--------------------------------------------------------------------------------
-- Uppercase Latin strings
--------------------------------------------------------------------------------

newtype UppercaseLatinString = UppercaseLatinString
    { unUppercaseLatinString :: NonEmpty UppercaseLatinChar }
    deriving (Eq, Ord)

--------------------------------------------------------------------------------
-- Conversion between uppercase Latin strings and ordinary strings
--------------------------------------------------------------------------------

instance Read UppercaseLatinString where
    readPrec = do
        many (skipChar ' ')
        UppercaseLatinString <$> ((:|) <$> readChar <*> many readChar)
      where
        readChar :: ReadPrec UppercaseLatinChar
        readChar = readCharMaybe uppercaseLatinCharFromChar

instance Show UppercaseLatinString where
    show (UppercaseLatinString cs) = F.foldMap show cs

--------------------------------------------------------------------------------
-- Generation of arbitrary uppercase Latin strings
--------------------------------------------------------------------------------

arbitraryUppercaseLatinString :: Int -> Gen UppercaseLatinString
arbitraryUppercaseLatinString stringLen =
    UppercaseLatinString . NE.fromList <$>
    replicateM (max 1 stringLen) arbitraryUppercaseLatinChar

--------------------------------------------------------------------------------
-- Conversion between uppercase Latin strings and quids
--------------------------------------------------------------------------------

uppercaseLatinStringToQuid :: UppercaseLatinString -> Quid
uppercaseLatinStringToQuid (UppercaseLatinString xs) = Quid $
    F.foldl' f 0 xs - 1
  where
    f !acc !x = acc * 26 + 1 + fromIntegral (fromEnum x)

uppercaseLatinStringFromQuid :: Quid -> UppercaseLatinString
uppercaseLatinStringFromQuid (Quid n) =
    UppercaseLatinString . NE.fromList $ go [] n
  where
    go :: [UppercaseLatinChar] -> Natural -> [UppercaseLatinChar]
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
