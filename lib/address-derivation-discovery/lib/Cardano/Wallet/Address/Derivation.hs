{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Primitives for performing address derivation for some given schemes. This is
-- where most of the crypto happens in the wallet and, it is quite important to
-- ensure that the implementations match with other Cardano wallets
-- (like cardano-sl, Yoroi/Icarus, or cardano-cli)
--
-- The actual implementations are in the following modules:
--
--  * "Cardano.Wallet.Address.Derivation.Shelley"
--  * "Cardano.Wallet.Address.Derivation.Byron"

module Cardano.Wallet.Address.Derivation
    (
    -- * HD Derivation
      Depth (..)
    , Index (..)
    , Role (..)
    , roleVal
    , utxoExternal
    , utxoInternal
    , mutableAccount
    , zeroAccount
    , stakeDerivationPath
    , DerivationType (..)
    , HardDerivation (..)
    , SoftDerivation (..)
    , DerivationPrefix (..)
    , DerivationIndex (..)
    , liftIndex

    -- * Delegation
    , RewardAccount (..)
    , ToRewardAccount(..)
    , AccountIxForStaking (..)
    , deriveRewardAccount

    -- * Helpers
    , hex
    , fromHex



    -- * Backends Interoperability
    , PaymentAddress(..)
    , DelegationAddress(..)
    , PersistPublicKey(..)
    , MkKeyFingerprint(..)
    , ErrMkKeyFingerprint(..)
    , KeyFingerprint(..)
    , unsafePaymentKeyFingerprint
    , AddressParts (..)
    , toAddressParts
    , liftPaymentAddressS
    , liftDelegationAddressS
    , paymentAddressS
    , delegationAddressS
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv
    , XPub
    )
import Cardano.Mnemonic
    ( SomeMnemonic
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId (..)
    , SNetworkId
    )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..)
    )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..)
    )
import Control.Applicative
    ( (<|>)
    )
import Control.DeepSeq
    ( NFData
    )
import Control.Monad
    ( (>=>)
    )
import Data.Bifunctor
    ( first
    )
import Data.Bits
    ( (.&.)
    )
import Data.ByteArray
    ( ByteArray
    , ByteArrayAccess
    )
import Data.ByteArray.Encoding
    ( Base (..)
    , convertFromBase
    , convertToBase
    )
import Data.ByteString
    ( ByteString
    )
import Data.Kind
    ( Type
    )
import Data.List.NonEmpty
    ( NonEmpty (..)
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Scientific
    ( Scientific
    , toBoundedInteger
    )
import Data.String
    ( fromString
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( CaseStyle (..)
    , FromText (..)
    , TextDecodingError (..)
    , ToText (..)
    , fromTextToBoundedEnum
    , toTextFromBoundedEnum
    )
import Data.Type.Equality
    ( (:~:) (..)
    , testEquality
    )
import Data.Word
    ( Word32
    , Word8
    )
import Fmt
    ( Buildable (..)
    )
import GHC.Generics
    ( Generic
    )
import GHC.Stack
    ( HasCallStack
    )
import GHC.TypeLits
    ( Symbol
    )
import Quiet
    ( Quiet (..)
    )
import Safe
    ( readMay
    , toEnumMay
    )
import Type.Reflection
    ( Typeable
    , typeRep
    )

import qualified Data.ByteString as BS
import qualified Data.Text as T

{-------------------------------------------------------------------------------
                                HD Hierarchy
-------------------------------------------------------------------------------}

-- | Typically used as a phantom type parameter, a witness to the type of the
-- key being used.
--
-- For example, @key 'RootK XPrv@, represents the private key at the root of the
-- HD hierarchy.
--
-- According to BIP-0044 / CIP-1852, we have the following keys in our HD
-- hierarchy:
--
-- @m | purpose' | cointype' | account' | role | address@
--
-- Plus, we also have script keys (which are used in shared wallets) and policy
-- keys (which are used in minting and burning).
data Depth
    = RootK
    | PurposeK
    | CoinTypeK
    | AccountK
    | RoleK
    | CredFromKeyK
    | CredFromScriptK
    | PolicyK

-- | Marker for addresses type engaged. We want to handle four cases here.
-- The first two are pertinent to UTxO accounting,
-- next handles rewards from participation in staking
-- the last one is used for getting verification keys used in scripts.
-- (a) external chain is used for addresses that are part of the 'advertised'
--     targets of a given transaction
-- (b) internal change is for addresses used to handle the change of a
--     the transaction within a given wallet
-- (c) the addresses for a reward account
-- (d) used for keys used inside scripts
data Role
    = UtxoExternal
    | UtxoInternal
    | MutableAccount
    deriving (Generic, Typeable, Show, Eq, Ord, Bounded)

instance NFData Role

-- Not deriving 'Enum' because this could have a dramatic impact if we were
-- to assign the wrong index to the corresponding constructor (by swapping
-- around the constructor above for instance).
instance Enum Role where
    toEnum = \case
        0 -> UtxoExternal
        1 -> UtxoInternal
        2 -> MutableAccount
        _ -> error "Role.toEnum: bad argument"
    fromEnum = \case
        UtxoExternal -> 0
        UtxoInternal -> 1
        MutableAccount -> 2

instance ToText Role where
    toText = toTextFromBoundedEnum SnakeLowerCase

instance FromText Role where
    fromText = fromTextToBoundedEnum SnakeLowerCase

-- | Bring a 'Role' type back to the term-level. This requires a type
-- application and either a scoped type variable, or an explicit passing of a
-- 'Role'.
--
-- >>> roleVal @'UtxoExternal
-- UtxoExternal
--
-- >>> roleVal @chain
-- ...
roleVal :: forall (c :: Role). Typeable c => Role
roleVal = fromMaybe (error $ "role: unmatched type" <> show (typeRep @c))
       (tryUtxoExternal <|> tryUtxoInternal <|> tryMutableAccount)
  where
    tryUtxoExternal =
        case testEquality (typeRep @c) (typeRep @'UtxoExternal) of
            Just Refl  -> Just UtxoExternal
            Nothing -> Nothing
    tryUtxoInternal =
        case testEquality (typeRep @c) (typeRep @'UtxoInternal) of
            Just Refl  -> Just UtxoInternal
            Nothing -> Nothing
    tryMutableAccount =
        case testEquality (typeRep @c) (typeRep @'MutableAccount) of
            Just Refl  -> Just MutableAccount
            Nothing -> Nothing

-- | smart-constructor for getting a derivation index that refers to external
-- utxo.
utxoExternal :: Index 'Soft 'RoleK
utxoExternal = toEnum $ fromEnum UtxoExternal

-- | smart-constructor for getting a derivation index that refers to internal
-- utxo.
utxoInternal :: Index 'Soft 'RoleK
utxoInternal = toEnum $ fromEnum UtxoInternal

-- | smart-constructor for getting a derivation index that refers to stake
-- key level (a.k.a mutable account)
mutableAccount :: Index 'Soft 'RoleK
mutableAccount = toEnum $ fromEnum MutableAccount

zeroAccount :: Index 'Soft 'CredFromKeyK
zeroAccount = minBound

-- | Full path to the stake key. There's only one.
stakeDerivationPath :: DerivationPrefix -> NonEmpty DerivationIndex
stakeDerivationPath (DerivationPrefix (purpose, coin, acc)) =
    (fromIndex purpose) :| [
      fromIndex coin
    , fromIndex acc
    , fromIndex mutableAccount
    , fromIndex zeroAccount]
  where
    fromIndex :: Index t l -> DerivationIndex
    fromIndex = DerivationIndex . getIndex

-- | A thin wrapper around derivation indexes. This can be used to represent
-- derivation path as homogeneous lists of 'DerivationIndex'. This is slightly
-- more convenient than having to carry heterogeneous lists of 'Index depth type'
-- and works fine because:
--
-- 1. The 'depth' matters not because what the depth captures is actually the
--    position of the index in that list. It makes sense to carry at the type
--    level when manipulating standalone indexes to avoid mistakes, but when
--    treating them as a part of a list it is redundant.
--
-- 2. The derivationType is captured by representing indexes as plain Word32.
--    The Soft / Hardened notation is for easing human-readability but in the
--    end, a soft index is simply a value < 2^31, whereas a "hardened" index is
--    simply a value >= 2^31. Therefore, instead of representing indexes as
--    derivationType + relative index within 0 and 2^31, we can represent them
--    as just an index between 0 and 2^32, which is what DerivationIndex does.
newtype DerivationIndex
    = DerivationIndex { getDerivationIndex :: Word32 }
    deriving (Eq, Ord, Generic)
    deriving Show via (Quiet DerivationIndex)

instance NFData DerivationIndex

instance ToText DerivationIndex where
    toText (DerivationIndex ix)
        | ix >= firstHardened  = T.pack $ show (ix - firstHardened) <> "H"
        | otherwise = T.pack $ show ix
      where
        firstHardened = getIndex @'Hardened minBound

instance FromText DerivationIndex where
    fromText source =
        if "H" `T.isSuffixOf` source then do
            DerivationIndex ix <- castNumber (T.init source) >>= parseAsScientific
            pure $ DerivationIndex $ ix + firstHardened
        else
            castNumber source >>= parseAsScientific
      where
        firstHardened = getIndex @'Hardened minBound

        errMalformed = TextDecodingError $ unwords
            [ "A derivation index must be a natural number between"
            , show (getIndex @'Soft minBound)
            , "and"
            , show (getIndex @'Soft maxBound)
            , "with an optional 'H' suffix (e.g. '1815H' or '44')."
            , "Indexes without suffixes are called 'Soft'"
            , "Indexes with suffixes are called 'Hardened'."
            ]

        parseAsScientific :: Scientific -> Either TextDecodingError DerivationIndex
        parseAsScientific x =
            case toBoundedInteger x of
                Just ix | ix < firstHardened ->
                    pure $ DerivationIndex ix
                _ ->
                    Left errMalformed

        castNumber :: Text -> Either TextDecodingError Scientific
        castNumber txt =
            case readMay (T.unpack txt) of
                Nothing ->
                    Left errMalformed
                Just s ->
                    pure s

-- | A derivation index, with phantom-types to disambiguate derivation type.
--
-- @
-- let accountIx = Index 'Hardened 'AccountK
-- let addressIx = Index 'Soft 'CredFromKeyK
-- @
newtype Index (derivationType :: DerivationType) (level :: Depth) = Index
    { getIndex :: Word32 }
    deriving stock (Generic, Show, Eq, Ord)

instance NFData (Index derivationType level)

instance Bounded (Index 'Hardened level) where
    minBound = Index 0x80000000
    maxBound = Index maxBound

instance Bounded (Index 'Soft level) where
    minBound = Index minBound
    maxBound = let Index ix = minBound @(Index 'Hardened _) in Index (ix - 1)

instance Bounded (Index 'WholeDomain level) where
    minBound = Index minBound
    maxBound = Index maxBound

instance Enum (Index 'Hardened level) where
    fromEnum (Index ix) = fromIntegral ix
    toEnum ix
        | ix >= minIndex && ix <= maxIndex = Index (fromIntegral ix)
        | otherwise =
            error $ concat
                [ "Index@Hardened.toEnum: value "
                , show ix
                , " is not within bounds: "
                , show minIndex
                , " <= value <= "
                , show maxIndex
                ]
      where
        minIndex = fromIntegral (getIndex (minBound @(Index 'Hardened _)))
        maxIndex = fromIntegral (getIndex (maxBound @(Index 'Hardened _)))

instance Enum (Index 'Soft level) where
    fromEnum (Index ix) = fromIntegral ix
    toEnum ix
        | ix >= minIndex && ix <= maxIndex = Index (fromIntegral ix)
        | otherwise =
            error $ concat
                [ "Index@Soft.toEnum: value "
                , show ix
                , " is not within bounds: "
                , show minIndex
                , " <= value <= "
                , show maxIndex
                ]
      where
        minIndex = fromIntegral (getIndex (minBound @(Index 'Soft _)))
        maxIndex = fromIntegral (getIndex (maxBound @(Index 'Soft _)))

instance Enum (Index 'WholeDomain level) where
    fromEnum (Index ix) = fromIntegral ix
    toEnum ix
        | ix >= minIndex && ix <= maxIndex = Index (fromIntegral ix)
        | otherwise =
            error $ concat
                [ "Index@WholeDomain.toEnum: value "
                , show ix
                , " is not within bounds: "
                , show minIndex
                , " <= value <= "
                , show maxIndex
                ]
      where
        minIndex = fromIntegral (getIndex (minBound @(Index 'WholeDomain _)))
        maxIndex = fromIntegral (getIndex (maxBound @(Index 'WholeDomain _)))

instance Buildable (Index derivationType level) where
    build (Index ix) = fromString (show ix)

instance
  ( Enum (Index derivation level)
  , Bounded (Index derivation level)
  ) => FromText (Index derivation level) where
    fromText = fromText >=> \n -> case toEnumMay n of
        Just ix -> pure ix
        Nothing -> Left $ TextDecodingError $ unwords
            [ "Couldn't parse derivation index. Expected an integer between"
            , show (minBound @(Index derivation level))
            , "and"
            , show (maxBound @(Index derivation level))
            ]

-- Safe coercion to WholeDomain from smaller domains.
class LiftIndex derivation where
    liftIndex :: Index derivation level -> Index 'WholeDomain level

instance LiftIndex 'Hardened where
    liftIndex (Index ix) = Index ix

instance LiftIndex 'Soft where
    liftIndex (Index ix) = Index ix

-- | Each 'SeqState' is like a bucket of addresses associated with an 'account'.
-- An 'account' corresponds to a subset of an HD tree as defined in BIP-0039.
--
-- cardano-wallet implements two similar HD schemes on top of BIP-0039 that are:
--
-- - BIP-0044 (for so-called Icarus wallets)
-- - CIP-1815 (for so-called Shelley and Jormungandr wallets)
--
-- Both scheme works by considering 5 levels of derivation from an initial root
-- key (see also 'Depth' from Cardano.Wallet.Address.Derivation). A
-- SeqState keeps track of indexes from the two last levels of a derivation
-- branch. The 'DerivationPrefix' defines the first three indexes chosen for
-- this particular 'SeqState'.
newtype DerivationPrefix = DerivationPrefix
    ( Index 'Hardened 'PurposeK
    , Index 'Hardened 'CoinTypeK
    , Index 'Hardened 'AccountK
    ) deriving (Show, Generic, Eq, Ord)

instance NFData DerivationPrefix

instance ToText DerivationPrefix where
    toText (DerivationPrefix (purpose, coinType, account))
        = T.intercalate "/"
        $ map toText
        [getIndex purpose, getIndex coinType, getIndex account]

instance FromText DerivationPrefix where
    fromText txt =
        DerivationPrefix <$> case T.splitOn "/" txt of
            [purposeT, coinTypeT, accountT] -> (,,)
                <$> fromText purposeT
                <*> fromText coinTypeT
                <*> fromText accountT
            _ ->
                Left $ TextDecodingError "expected exactly 3 derivation paths"

-- | Type of derivation that should be used with the given indexes.
--
-- In theory, we should only consider two derivation types: soft and hard.
--
-- However, historically, addresses in Cardano used to be generated across the
-- both soft and hard domain. We therefore introduce a 'WholeDomain' derivation
-- type that is the exact union of `Hardened` and `Soft`.
data DerivationType = Hardened | Soft | WholeDomain

-- | An interface for doing hard derivations from the root private key
class HardDerivation (key :: Depth -> Type -> Type) where
    type AddressIndexDerivationType key :: DerivationType
    type AddressCredential key :: Depth

    -- | Derives account private key from the given root private key, using
    -- derivation scheme 2 (see <https://github.com/input-output-hk/cardano-crypto/ cardano-crypto>
    -- package for more details).
    --
    -- NOTE: The caller is expected to provide the corresponding passphrase (and
    -- to have checked that the passphrase is valid). Providing a wrong passphrase
    -- will not make the function fail but will instead, yield an incorrect new
    -- key that doesn't belong to the wallet.
    deriveAccountPrivateKey
        :: Passphrase "encryption"
        -> key 'RootK XPrv
        -> Index 'Hardened 'AccountK
        -> key 'AccountK XPrv

    -- | Derives address private key from the given account private key, using
    -- derivation scheme 2 (see <https://github.com/input-output-hk/cardano-crypto/ cardano-crypto>
    -- package for more details).
    --
    -- It is preferred to use 'deriveAddressPublicKey' whenever possible to avoid
    -- having to manipulate passphrases and private keys.
    --
    -- NOTE: The caller is expected to provide the corresponding passphrase (and
    -- to have checked that the passphrase is valid). Providing a wrong passphrase
    -- will not make the function fail but will instead, yield an incorrect new
    -- key that doesn't belong to the wallet.
    deriveAddressPrivateKey
        :: Passphrase "encryption"
        -> key 'AccountK XPrv
        -> Role
        -> Index (AddressIndexDerivationType key) (AddressCredential key)
        -> key (AddressCredential key) XPrv

-- | An interface for doing soft derivations from an account public key
class HardDerivation key => SoftDerivation (key :: Depth -> Type -> Type) where
    -- | Derives address public key from the given account public key, using
    -- derivation scheme 2 (see <https://github.com/input-output-hk/cardano-crypto/ cardano-crypto>
    -- package for more details).
    --
    -- This is the preferred way of deriving new sequential address public keys.
    deriveAddressPublicKey
        :: key 'AccountK XPub
        -> Role
        -> Index 'Soft (AddressCredential key)
        -> key (AddressCredential key) XPub

-- | Derivation of a reward account, as a type-class because different between
-- key types (in particular, Jörmungandr vs Shelley).
class ToRewardAccount k where
    toRewardAccount :: k 'CredFromKeyK XPub -> RewardAccount
    someRewardAccount :: SomeMnemonic -> (XPrv, RewardAccount, NonEmpty DerivationIndex)

-- | Derive a reward account from a root private key. It is agreed by standard
-- that every HD wallet will use only a single reward account. This account is
-- located into a special derivation path and uses the first index of that path.
deriveRewardAccount
    :: ( HardDerivation k
       , Bounded (Index (AddressIndexDerivationType k) (AddressCredential k)) )
    => Passphrase "encryption"
    -> k 'RootK XPrv
    -> Index 'Hardened 'AccountK
    -> k (AddressCredential k) XPrv
deriveRewardAccount pwd rootPrv accIx =
    let accPrv = deriveAccountPrivateKey pwd rootPrv accIx
    in deriveAddressPrivateKey pwd accPrv MutableAccount minBound

-- | This class is used to determine account index in the context of script
-- staking. It is supposed to be not Nothing only for shared wallets
class AccountIxForStaking s where
    getAccountIx :: s -> Maybe (Index 'Hardened 'AccountK)

-- | Encoding of addresses for certain key types and backend targets.
class MkKeyFingerprint key Address
    => PaymentAddress key ktype where
    -- | Convert a public key to a payment 'Address' valid for the given
    -- network discrimination.
    --
    -- Note that 'paymentAddress' is ambiguous and requires therefore a type
    -- application.
    paymentAddress
        :: SNetworkId n
        -> key ktype XPub
        -> Address

    -- | Lift a payment fingerprint back into a payment address.
    liftPaymentAddress
        :: SNetworkId n
        -> KeyFingerprint "payment" key
            -- ^ Payment fingerprint
        -> Address

paymentAddressS :: forall n key ktype
    . (PaymentAddress key ktype, HasSNetworkId n)
    => key ktype XPub
    -> Address
paymentAddressS = paymentAddress @_ @ktype (sNetworkId @n)

liftPaymentAddressS
    :: forall n key ktype
     . (PaymentAddress key ktype, HasSNetworkId n)
    => KeyFingerprint "payment" key
    -> Address
liftPaymentAddressS = liftPaymentAddress @_ @ktype (sNetworkId @n)

class PaymentAddress key ktype
    => DelegationAddress key ktype where
    -- | Convert a public key and a staking key to a delegation 'Address' valid
    -- for the given network discrimination. Funds sent to this address will be
    -- delegated according to the delegation settings attached to the delegation
    -- key.
    --
    -- Note that 'delegationAddress' is ambiguous and requires therefore a type
    -- application.
    delegationAddress
        :: SNetworkId n
        -> key ktype XPub
            -- ^ Payment key
        -> key ktype XPub
            -- ^ Staking key / Reward account
        -> Address

    -- | Lift a payment fingerprint back into a delegation address.
    liftDelegationAddress
        :: SNetworkId n
        -> KeyFingerprint "payment" key
            -- ^ Payment fingerprint
        -> key ktype XPub
            -- ^ Staking key / Reward account
        -> Address

delegationAddressS :: forall n key ktype
    . (DelegationAddress key ktype, HasSNetworkId n)
    => key ktype XPub
    -> key ktype XPub
    -> Address
delegationAddressS = delegationAddress @_ @ktype (sNetworkId @n)

liftDelegationAddressS
    :: forall n key ktype
     . (DelegationAddress key ktype, HasSNetworkId n)
    => KeyFingerprint "payment" key
    -> key ktype XPub
    -> Address
liftDelegationAddressS = liftDelegationAddress @_ @ktype (sNetworkId @n)

-- | Operations for saving a public key into a database, and restoring it from
-- a database. The keys should be encoded in hexadecimal strings.
class PersistPublicKey (key :: Type -> Type) where
    -- | Convert a private key and its password hash into hexadecimal strings
    -- suitable for storing in a text file or database column.
    serializeXPub
        :: key XPub
        -> ByteString

    -- | Convert a public key into hexadecimal strings suitable for storing in
    -- a text file or database column.
    unsafeDeserializeXPub
        :: ByteString
        -> key XPub

-- | Something that uniquely identifies a public key. Typically,
-- a hash of that key or the key itself.
newtype KeyFingerprint (s :: Symbol) key = KeyFingerprint ByteString
    deriving (Generic, Show, Eq, Ord)

instance NFData (KeyFingerprint s key)

-- | Produce 'KeyFingerprint' for existing types. A fingerprint here uniquely
-- identifies part of an address. It can refer to either the payment key or, if
-- any, the delegation key of an address.
--
-- The fingerprint obeys the following rules:
--
-- - If two addresses are the same, then they have the same fingerprints
-- - It is possible to lift the fingerprint back into an address
--
-- This second rule pretty much fixes what can be chosen as a fingerprint for
-- various key types:
--
-- 1. For 'ByronKey', it can only be the address itself!
-- 2. For 'ShelleyKey', then the "payment" fingerprint refers to the payment key
--    within a single or grouped address.
class Show from => MkKeyFingerprint (key :: Depth -> Type -> Type) from where
    paymentKeyFingerprint
        :: from
        -> Either
            (ErrMkKeyFingerprint key from)
            (KeyFingerprint "payment" key)

data ErrMkKeyFingerprint key from
    = ErrInvalidAddress from (Proxy key) deriving (Show, Eq)

data AddressParts = AddressParts
    { addrType :: Word8
    , addrNetwork :: Word8
    , rest :: ByteString
    } deriving (Show,Eq)

-- this is supposed to be used only for Shelley and Shared style
-- as only for the furst byte in any address contains information
-- about network tag and address type
toAddressParts :: Address -> AddressParts
toAddressParts (Address bytes) = AddressParts {..}
  where
    (fstByte, rest) = first BS.head $ BS.splitAt 1 bytes
    addrType = fstByte .&. 0b11110000
    addrNetwork = fstByte .&. 0b00001111

-- Extract the fingerprint from an 'Address', we expect the caller to
-- provide addresses that are compatible with the key scheme being used.
--
-- Actually, addresses passed as asgument should have been "generated" by
-- the address pool itself in the past, so they ought to be valid!
unsafePaymentKeyFingerprint
    :: forall k from. (HasCallStack, MkKeyFingerprint k from)
    => from
    -> KeyFingerprint "payment" k
unsafePaymentKeyFingerprint from = case paymentKeyFingerprint @k @from from of
    Right a -> a
    Left err -> error $ unwords
        [ "unsafePaymentKeyFingerprint was given a source invalid with its"
        , "key type:"
        , show err
        ]

{-------------------------------------------------------------------------------
                                Helpers
-------------------------------------------------------------------------------}

-- | Encode a 'ByteString' in base16
hex :: ByteArrayAccess bin => bin -> ByteString
hex = convertToBase Base16

-- | Decode a 'ByteString' from base16
fromHex :: ByteArray bout => ByteString -> Either String bout
fromHex = convertFromBase Base16
