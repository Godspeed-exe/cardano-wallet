{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Primitive.Types.TokenQuantity
    (
      -- * Type
      TokenQuantity (..)

      -- * Values
    , zero

      -- * Arithmetic operations
    , add
    , subtract
    , pred
    , predZero
    , succ
    , difference

      -- * Partitioning
    , equipartition
    , partition
    , partitionDefault

      -- * Tests
    , isNonZero
    , isZero

    ) where

import Prelude hiding
    ( pred
    , subtract
    , succ
    )

import Cardano.Numeric.Util
    ( equipartitionNatural
    , partitionNatural
    )
import Control.DeepSeq
    ( NFData (..)
    )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    )
import Data.Hashable
    ( Hashable
    )
import Data.List.NonEmpty
    ( NonEmpty (..)
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Monoid
    ( Sum (..)
    )
import Data.Monoid.Cancellative
    ( LeftReductive
    , Reductive ((</>))
    , RightReductive
    )
import Data.Monoid.GCD
    ( GCDMonoid
    , LeftGCDMonoid
    , RightGCDMonoid
    )
import Data.Monoid.Monus
    ( Monus ((<\>))
    , OverlappingGCDMonoid
    )
import Data.Monoid.Null
    ( MonoidNull
    )
import Data.Semigroup.Commutative
    ( Commutative
    )
import Data.Text.Class
    ( FromText (..)
    , ToText (..)
    )
import Fmt
    ( Buildable (..)
    )
import GHC.Generics
    ( Generic
    )
import Numeric.Natural
    ( Natural
    )
import Quiet
    ( Quiet (..)
    )

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

-- | Represents an integral quantity of tokens.
--
-- At present, we use 'Natural' as our underlying type, as the only use case
-- for these quantities is to be included in token bundles held within
-- transaction outputs, and these must never be negative.
--
-- When we build support for minting and burning of tokens, we may wish to
-- parameterize this type and allow it to be instantiated with 'Integer'.
--
newtype TokenQuantity = TokenQuantity
    { unTokenQuantity :: Natural }
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (NFData, Hashable)
    deriving (Read, Show) via Quiet TokenQuantity
    deriving (Commutative, Semigroup, Monoid, MonoidNull) via Sum Natural
    deriving (LeftReductive, RightReductive, Reductive) via Sum Natural
    deriving (LeftGCDMonoid, RightGCDMonoid, GCDMonoid) via Sum Natural
    deriving (OverlappingGCDMonoid, Monus) via Sum Natural

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Buildable TokenQuantity where
    build = build . toText . unTokenQuantity

instance ToText TokenQuantity where
    toText = toText . unTokenQuantity

instance FromText TokenQuantity where
    fromText = fmap (TokenQuantity . fromIntegral @Integer) . fromText

instance FromJSON TokenQuantity where
    parseJSON = fmap TokenQuantity . parseJSON
instance ToJSON TokenQuantity where
    toJSON = toJSON . unTokenQuantity

--------------------------------------------------------------------------------
-- Values
--------------------------------------------------------------------------------

zero :: TokenQuantity
zero = TokenQuantity 0

--------------------------------------------------------------------------------
-- Arithmetic operations
--------------------------------------------------------------------------------

add :: TokenQuantity -> TokenQuantity -> TokenQuantity
add = (<>)

-- | Subtracts the second token quantity from the first.
--
-- Returns 'Nothing' if the first quantity is less than the second quantity.
--
subtract :: TokenQuantity -> TokenQuantity -> Maybe TokenQuantity
subtract = (</>)

-- | Finds the predecessor of a given token quantity.
--
-- Returns 'Nothing' if the given quantity is zero.
--
pred :: TokenQuantity -> Maybe TokenQuantity
pred = (`subtract` TokenQuantity 1)

-- | Finds the predecessor of a given token quantity.
--
-- Returns 'zero' if the given quantity is 'zero'.
--
-- Satisfies the following property:
--
-- >>> predZero x == x `difference` 1
--
predZero :: TokenQuantity -> TokenQuantity
predZero = fromMaybe zero . pred

-- | Finds the successor of a given token quantity.
--
succ :: TokenQuantity -> TokenQuantity
succ = (`add` TokenQuantity 1)

-- | Subtracts the second token quantity from the first.
--
-- Returns 'zero' if the first quantity is less than the second quantity.
--
difference :: TokenQuantity -> TokenQuantity -> TokenQuantity
difference = (<\>)

--------------------------------------------------------------------------------
-- Partitioning
--------------------------------------------------------------------------------

-- | Computes the equipartition of a token quantity into 'n' smaller quantities.
--
-- An /equipartition/ of a token quantity is a /partition/ of that quantity
-- into 'n' smaller quantities whose values differ by no more than 1.
--
-- The resultant list is sorted in ascending order.
--
equipartition
    :: TokenQuantity
    -- ^ The token quantity to be partitioned.
    -> NonEmpty a
    -- ^ Represents the number of portions in which to partition the quantity.
    -> NonEmpty TokenQuantity
    -- ^ The partitioned quantities.
equipartition q =
    fmap TokenQuantity . equipartitionNatural (unTokenQuantity q)

-- | Partitions a token quantity into a number of parts, where the size of each
--   part is proportional (modulo rounding) to the size of its corresponding
--   element in the given list of weights, and the number of parts is equal to
--   the number of weights.
--
-- Returns 'Nothing' if the sum of weights is equal to zero.
--
partition
    :: TokenQuantity
    -- ^ The token quantity to be partitioned.
    -> NonEmpty TokenQuantity
    -- ^ The list of weights.
    -> Maybe (NonEmpty TokenQuantity)
    -- ^ The partitioned token quantities.
partition c
    = fmap (fmap TokenQuantity)
    . partitionNatural (unTokenQuantity c)
    . fmap unTokenQuantity

-- | Partitions a token quantity into a number of parts, where the size of each
--   part is proportional (modulo rounding) to the size of its corresponding
--   element in the given list of weights, and the number of parts is equal to
--   the number of weights.
--
-- This function always satisfies the following properties:
--
-- prop> fold   (partitionDefault q ws) == c
-- prop> length (partitionDefault q ws) == length ws
--
-- If the sum of weights is equal to zero, then this function returns an
-- 'equipartition' satisfying the following property:
--
-- prop> partitionDefault q ws == equipartition q ws
--
partitionDefault
    :: TokenQuantity
    -- ^ The token quantity to be partitioned.
    -> NonEmpty TokenQuantity
    -- ^ The list of weights.
    -> NonEmpty TokenQuantity
    -- ^ The partitioned token quantities.
partitionDefault q ws = fromMaybe (equipartition q ws) (partition q ws)

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

isNonZero :: TokenQuantity -> Bool
isNonZero = (/= zero)

isZero :: TokenQuantity -> Bool
isZero = (== zero)
