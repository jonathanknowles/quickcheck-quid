{-# LANGUAGE TypeApplications #-}

module Test.QuickCheck.Natural
    ( arbitraryNatural
    , chooseNatural
    , shrinkNatural
    ) where

import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Gen, chooseInteger, frequency )

import qualified Data.List as L

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
