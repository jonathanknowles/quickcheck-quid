{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Internal.Test.QuickCheck.Quid.Combinators.Prefix
    where

import Control.Applicative
    ( many, (<|>) )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( replicateM_ )
import Data.Data
    ( Data )
import Data.Hashable
    ( Hashable (..) )
import Data.Proxy
    ( Proxy (..) )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal )
import Internal.Text.Read
    ( skipChar, skipString )
import Text.Read
    ( Read (..) )

import qualified Data.List as L

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
