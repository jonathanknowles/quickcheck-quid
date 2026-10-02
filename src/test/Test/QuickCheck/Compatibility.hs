{-# LANGUAGE CPP #-}

module Test.QuickCheck.Compatibility
  ( withNumTests
  ) where

import qualified Test.QuickCheck as Q

withNumTests :: Q.Testable prop => Int -> prop -> Q.Property
withNumTests =
#if MIN_VERSION_QuickCheck(2,18,0)
    Q.withNumTests
#else
    Q.withMaxSuccess
#endif
