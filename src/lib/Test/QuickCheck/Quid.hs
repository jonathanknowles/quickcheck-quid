{-# LANGUAGE BangPatterns #-}

module Test.QuickCheck.Quid
    ( Quid
    , arbitraryQuid
    , chooseQuid
    , coarbitraryQuid
    , functionQuid
    , shrinkQuid
    , quidFromNatural
    , quidToNatural
    )
    where

import Test.QuickCheck.Quid.Internal
    ( Quid
    , arbitraryQuid
    , chooseQuid
    , coarbitraryQuid
    , functionQuid
    , quidFromNatural
    , quidToNatural
    , shrinkQuid
    )
