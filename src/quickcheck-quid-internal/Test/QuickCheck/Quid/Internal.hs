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

module Test.QuickCheck.Quid.Internal
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
    ( NonEmpty )
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
    readPrec = UppercaseLatin . quidStringToQuid <$> readPrec

instance Show (UppercaseLatin Quid) where
    show = show . quidStringFromQuid . unUppercaseLatin

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
-- Quid characters
--------------------------------------------------------------------------------

data QuidChar
    = A | B | C | D | E | F | G | H | I | J | K | L | M
    | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

--------------------------------------------------------------------------------
-- Generation and shrinking of arbitrary quid characters
--------------------------------------------------------------------------------

arbitraryQuidChar :: Gen QuidChar
arbitraryQuidChar = arbitraryBoundedEnum

shrinkQuidChar :: QuidChar -> [QuidChar]
shrinkQuidChar = shrinkMap toEnum fromEnum

--------------------------------------------------------------------------------
-- Conversion between quid characters and ordinary characters
--------------------------------------------------------------------------------

quidCharFromChar :: Char -> Maybe QuidChar
quidCharFromChar c = readMaybe [c]

quidCharToChar :: QuidChar -> Char
quidCharToChar = head . show

--------------------------------------------------------------------------------
-- Quid strings
--------------------------------------------------------------------------------

newtype QuidString = QuidString
    { unQuidString :: NonEmpty QuidChar }
    deriving (Eq, Ord)

--------------------------------------------------------------------------------
-- Conversion between quid strings and ordinary strings
--------------------------------------------------------------------------------

instance Read QuidString where
    readPrec = do
        many (skipChar ' ')
        blocks <- many readBlock
        terminator <- readTerminator
        pure $ QuidString $ NE.fromList $ foldr (<>) terminator blocks
      where
        readBlock :: ReadPrec [QuidChar]
        readBlock = replicateM 4 readChar <* skipChar '-'

        readChar :: ReadPrec QuidChar
        readChar = readCharMaybe quidCharFromChar

        readTerminator :: ReadPrec [QuidChar]
        readTerminator = choice $ (`replicateM` readChar) <$> [4, 3, 2, 1]

instance Show QuidString where
    show (QuidString cs) = F.foldMap show cs

--------------------------------------------------------------------------------
-- Generation of arbitrary quid strings
--------------------------------------------------------------------------------

arbitraryQuidString :: Int -> Gen QuidString
arbitraryQuidString stringLen =
    QuidString . NE.fromList <$> replicateM (max 1 stringLen) arbitraryQuidChar

--------------------------------------------------------------------------------
-- Conversion between quid strings and quids
--------------------------------------------------------------------------------

quidStringToQuid :: QuidString -> Quid
quidStringToQuid (QuidString xs) = Quid $
    F.foldl' f 0 xs - 1
  where
    f !acc !x = acc * 26 + 1 + fromIntegral (fromEnum x)

quidStringFromQuid :: Quid -> QuidString
quidStringFromQuid (Quid n) =
    QuidString . NE.fromList $ go [] n
  where
    go :: [QuidChar] -> Natural -> [QuidChar]
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

frequencies :: (Foldable f, Ord k) => f k -> [(k, Int)]
frequencies
    = L.sortOn ((Down . snd) &&& fst)
    . Map.toList
    . L.foldr (flip (Map.insertWith (+)) 1) Map.empty
