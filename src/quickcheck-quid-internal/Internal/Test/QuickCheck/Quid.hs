{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Internal.Test.QuickCheck.Quid
    where

import Control.Applicative
    ( many, (<|>) )
import Control.Arrow
    ( (&&&) )
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
import Data.List.Extra
    ( chunksOf )
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
    , shrinkMap
    , shrinkMapBy
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
-- Uppercase Latin representation
--------------------------------------------------------------------------------

newtype UppercaseLatin a = UppercaseLatin { unUppercaseLatin :: a }
    deriving (Data, Eq, Generic, Hashable, NFData, Ord)

instance Read (UppercaseLatin Quid) where
    readPrec = UppercaseLatin . uppercaseLatinStringToQuid <$> readPrec

instance Show (UppercaseLatin Quid) where
    show = show . uppercaseLatinStringFromQuid . unUppercaseLatin

--------------------------------------------------------------------------------
-- Chunking
--------------------------------------------------------------------------------

newtype Chunked (n :: Nat) (s :: Symbol) a = Chunked { unChunked :: a }
    deriving (Data, Eq, Generic, Hashable, NFData, Ord)

instance (KnownNat n, KnownSymbol s, Show a) => Show (Chunked n s a) where
    show (Chunked a)
        = L.intercalate (symbolVal (Proxy @s))
        $ chunksOf (fromIntegral $ natVal $ Proxy @n)
        $ show a

--------------------------------------------------------------------------------
-- Prefixes
--------------------------------------------------------------------------------

newtype Prefix (prefix :: Symbol) a = Prefix { unPrefix :: a }
    deriving (Data, Eq, Generic, Hashable, NFData, Ord)

instance (KnownSymbol prefix, Read a) => Read (Prefix prefix a) where
    readPrec = do
        many $ skipChar ' '
        skipString $ symbolVal $ Proxy @prefix
        Prefix <$> readPrec @a

instance (KnownSymbol prefix, Show a) => Show (Prefix prefix a) where
    show (Prefix a) = symbolVal (Proxy @prefix) <> show a

--------------------------------------------------------------------------------
-- Sizes
--------------------------------------------------------------------------------

newtype Size (n :: Nat) a = Size { unSize :: a }
    deriving (Data, Eq, Generic, Hashable, NFData, Ord)

instance KnownNat n => Arbitrary (Size n Quid) where
    arbitrary = Size <$> arbitraryQuid (fromIntegral $ natVal $ Proxy @n)
    shrink = shrinkMapBy Size unSize shrinkQuid

--------------------------------------------------------------------------------
-- Generation and shrinking of arbitrary quids
--------------------------------------------------------------------------------

arbitraryQuid :: Int -> Gen Quid
arbitraryQuid i = chooseQuid (Quid 0, Quid $ (2 ^ max 0 i) - 1)

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

--------------------------------------------------------------------------------
-- Natural number support
--------------------------------------------------------------------------------

arbitraryNatural :: Gen Natural
arbitraryNatural = frequency
    [ (1, pure 0)
    , (1, pure 1)
    , (8, chooseNatural (2, 2 ^ 128))
    ]

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

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

newtype Frequency = Frequency {unFrequency :: Natural}
    deriving (Eq, Ord, Show)

instance Semigroup Frequency where
    Frequency f1 <> Frequency f2 = Frequency (f1 + f2)

instance Monoid Frequency where
    mempty = Frequency 1

frequencies :: (Foldable f, Ord k) => f k -> [(k, Frequency)]
frequencies
    = L.sortOn ((Down . snd) &&& fst)
    . Map.toList
    . L.foldr (flip (Map.insertWith (<>)) mempty) Map.empty
