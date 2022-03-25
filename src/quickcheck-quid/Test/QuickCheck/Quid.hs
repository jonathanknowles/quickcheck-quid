module Test.QuickCheck.Quid
    (
    -- * The Quid type
      Quid (..)

    -- * Combinators
    , Size (..)

    -- * Representations
    , Hexadecimal (..)
    , Latin (..)
    )
    where

import Internal.Test.QuickCheck.Quid
    ( Quid (..) )
import Internal.Test.QuickCheck.Quid.Combinators.Size
    ( Size (..) )
import Internal.Test.QuickCheck.Quid.Representations.Hexadecimal
    ( Hexadecimal (..) )
import Internal.Test.QuickCheck.Quid.Representations.Latin
    ( Latin (..) )
