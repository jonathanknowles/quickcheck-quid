{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.QuickCheck.Quid.Internal
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
import Data.List.Extra
    ( chunksOf )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Maybe
    ( catMaybes )
import Data.Text
    ( Text )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Function (..)
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
import qualified Data.Text as T
import qualified Test.QuickCheck as QC

--------------------------------------------------------------------------------
-- Quids
--------------------------------------------------------------------------------

newtype Quid = Quid
    { unQuid :: Natural }
    deriving (Data, Eq, Generic, Hashable, NFData, Ord)

instance Read Quid where
    readPrec = quidStringToQuid <$> readPrec

instance Show Quid where
    show = show . quidStringFromQuid

--------------------------------------------------------------------------------
-- Generation and shrinking of arbitrary quids
--------------------------------------------------------------------------------

arbitraryQuid :: Int -> Gen Quid
arbitraryQuid = fmap quidStringToQuid . arbitraryQuidString

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
    show (QuidString cs) =
        L.intercalate "-" $ F.foldMap show <$> chunksOf 4 (NE.toList cs)

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
