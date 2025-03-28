{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- Orphan instances for {Encode,Decode}Address until we get rid of the
-- Jörmungandr dual support.
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Conversion functions and static chain settings for Shelley.
module Cardano.Wallet.Shelley.Compatibility
    ( CardanoBlock
    , StandardCrypto
    , StandardShelley

      -- * Protocol Parameters
    , NetworkId (..)
    , NodeToClientVersionData
    , nodeToClientVersions

      -- * Node Connection
    , localNodeConnectInfo

      -- * Genesis
    , emptyGenesis

      -- * Eras
    , AnyCardanoEra (..)
    , AnyShelleyBasedEra (..)
    , CardanoEra (..)
    , ShelleyBasedEra (..)
    , shelleyBasedToCardanoEra
    , shelleyToCardanoEra
    , getShelleyBasedEra

      -- * Conversions
    , toCardanoHash
    , unsealShelleyTx
    , toPoint
    , fromPoint
    , toCardanoTxId
    , toCardanoTxIn
    , toCardanoUTxO
    , fromCardanoTxIn
    , fromCardanoTxOut
    , fromCardanoWdrls
    , cardanoCertKeysForWitnesses
    , toCardanoTxOut
    , toCardanoLovelace
    , toStakeKeyRegCert
    , toStakeKeyDeregCert
    , toStakePoolDlgCert
    , toLedgerStakeCredential
    , fromStakeCredential
    , toShelleyCoin
    , toCardanoStakeCredential
    , toCardanoValue
    , fromCardanoValue
    , fromCardanoLovelace
    , rewardAccountFromAddress
    , fromShelleyPParams
    , fromAllegraPParams
    , fromMaryPParams
    , fromAlonzoPParams
    , fromBabbagePParams
    , fromConwayPParams
    , fromLedgerExUnits
    , toLedgerExUnits
    , fromCardanoAddress
    , toSystemStart
    , fromShelleyTxIn
    , toCostModelsAsArray
    , toCardanoPolicyId
    , toCardanoSimpleScript
    , toCardanoSimpleScriptV1
    , fromCardanoSimpleScript

      -- * Unsafe conversions
    , unsafeLovelaceToWalletCoin
    , unsafeValueToLovelace

      -- ** Stake pools
    , fromPoolId
    , fromPoolDistr
    , fromNonMyopicMemberRewards
    , optimumNumberOfPools
    , getProducer
    , fromBlockNo
    , fromCardanoBlock
    , toCardanoEra
    , toCardanoBlockHeader
    , toShelleyBlockHeader
    , toBabbageBlockHeader
    , toConwayBlockHeader
    , fromShelleyHash
    , fromShelleyTxOut
    , fromCardanoHash
    , fromChainHash
    , fromPrevHash
    , fromGenesisData
    , fromTip
    , fromTip'
    , toTip
    , fromShelleyBlock
    , fromAllegraBlock
    , slottingParametersFromGenesis
    , fromMaryBlock
    , fromAlonzoBlock
    , fromBabbageBlock
    , fromConwayBlock
    , getBabbageProducer
    , getConwayProducer

      -- * Internal Conversions
    , decentralizationLevelFromPParams

      -- * Utilities
    , invertUnitInterval
    , interval0
    , interval1
    , numberOfTransactionsInBlock
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPub
    , xpubPublicKey
    )
import Cardano.Address.Script
    ( KeyHash (..)
    , KeyRole (..)
    , Script (..)
    )
import Cardano.Api
    ( AllegraEra
    , AlonzoEra
    , AnyCardanoEra (..)
    , AsType (..)
    , BabbageEra
    , CardanoEra (..)
    , CardanoEraStyle (..)
    , CardanoMode
    , ConsensusModeParams (CardanoModeParams)
    , ConwayEra
    , EraInMode (..)
    , File (..)
    , InAnyCardanoEra (..)
    , IsCardanoEra (..)
    , LocalNodeConnectInfo (LocalNodeConnectInfo)
    , MaryEra
    , NetworkId
    , ShelleyEra
    , TxInMode (..)
    , cardanoEraStyle
    )
import Cardano.Api.Shelley
    ( InAnyShelleyBasedEra (..)
    , IsShelleyBasedEra (..)
    , ShelleyBasedEra (..)
    , ShelleyGenesis (..)
    )
import Cardano.Chain.Block
    ( ABlockOrBoundary (ABOBBlock, ABOBBoundary)
    , blockTxPayload
    )
import Cardano.Chain.UTxO
    ( unTxPayload
    )
import Cardano.Crypto.Hash.Class
    ( Hash (UnsafeHash)
    , hashToBytes
    )
import Cardano.Launcher.Node
    ( CardanoNodeConn
    , nodeSocketFile
    )
import Cardano.Ledger.Api
    ( ppCollateralPercentageL
    , ppDL
    , ppKeyDepositL
    , ppMaxCollateralInputsL
    , ppMaxTxExUnitsL
    , ppMaxTxSizeL
    , ppMaxValSizeL
    , ppMinFeeAL
    , ppMinFeeBL
    , ppNOptL
    , ppPricesL
    )
import Cardano.Ledger.BaseTypes
    ( strictMaybeToMaybe
    , urlToText
    )
import Cardano.Ledger.Binary
    ( EncCBORGroup
    )
import Cardano.Ledger.Era
    ( Era (..)
    , TxSeq
    )
import Cardano.Ledger.PoolParams
    ( PoolMetadata (..)
    , PoolParams (..)
    )
import Cardano.Ledger.Shelley.Genesis
    ( fromNominalDiffTimeMicro
    )
import Cardano.Pool.Metadata.Types
    ( StakePoolMetadataHash (..)
    , StakePoolMetadataUrl (..)
    )
import Cardano.Pool.Types
    ( PoolId (..)
    , PoolOwner (..)
    )
import Cardano.Slotting.Slot
    ( EpochNo (..)
    , EpochSize (..)
    )
import Cardano.Slotting.Time
    ( SystemStart (..)
    )
import Cardano.Wallet.Address.Encoding
    ( fromStakeCredential
    )
import Cardano.Wallet.Byron.Compatibility
    ( fromByronBlock
    , fromTxAux
    , maryTokenBundleMaxSize
    , toByronBlockHeader
    )
import Cardano.Wallet.Primitive.Types
    ( ChainPoint (..)
    , PoolCertificate
    , PoolRegistrationCertificate (..)
    , ProtocolParameters (txParameters)
    , TxParameters (getTokenBundleMaxSize)
    )
import Cardano.Wallet.Read.Primitive.Tx.Allegra
    ( fromAllegraTx
    )
import Cardano.Wallet.Read.Primitive.Tx.Alonzo
    ( fromAlonzoTx
    )
import Cardano.Wallet.Read.Primitive.Tx.Babbage
    ( fromBabbageTx
    )
import Cardano.Wallet.Read.Primitive.Tx.Conway
    ( fromConwayTx
    )
import Cardano.Wallet.Read.Primitive.Tx.Features.Inputs
    ( fromShelleyTxIn
    )
import Cardano.Wallet.Read.Primitive.Tx.Features.Outputs
    ( fromCardanoValue
    , fromShelleyAddress
    , fromShelleyTxOut
    )
import Cardano.Wallet.Read.Primitive.Tx.Mary
    ( fromMaryTx
    )
import Cardano.Wallet.Read.Primitive.Tx.Shelley
    ( fromShelleyTx
    )
import Cardano.Wallet.Read.Tx.Hash
    ( fromShelleyTxId
    )
import Cardano.Wallet.Transaction
    ( WitnessCountCtx (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeIntToWord
    , unsafeMkPercentage
    )
import Cardano.Wallet.Util
    ( internalError
    , tina
    )
import Control.Applicative
    ( Const (..)
    )
import Control.Lens
    ( view
    , (&)
    , (^.)
    )
import Crypto.Hash.Extra
    ( blake2b224
    )
import Data.Array
    ( Array
    )
import Data.Bifunctor
    ( bimap
    )
import Data.ByteString
    ( ByteString
    )
import Data.ByteString.Short
    ( fromShort
    , toShort
    )
import Data.Coerce
    ( coerce
    )
import Data.Either.Extra
    ( eitherToMaybe
    )
import Data.Foldable
    ( toList
    )
import Data.IntCast
    ( intCast
    , intCastMaybe
    )
import Data.List
    ( unzip6
    )
import Data.Map.Strict
    ( Map
    )
import Data.Maybe
    ( fromMaybe
    , mapMaybe
    )
import Data.Quantity
    ( Percentage
    , Quantity (..)
    , clipToPercentage
    , mkPercentage
    )
import Data.Type.Equality
    ( (:~:) (..)
    , testEquality
    )
import Data.Word
    ( Word16
    , Word32
    )
import Fmt
    ( Buildable (..)
    , Builder
    , (+|)
    , (+||)
    , (||+)
    )
import GHC.Stack
    ( HasCallStack
    )
import Numeric.Natural
    ( Natural
    )
import Ouroboros.Consensus.Byron.Ledger
    ( byronBlockRaw
    )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock
    , CardanoEras
    , HardForkBlock (..)
    , StandardAllegra
    , StandardAlonzo
    , StandardBabbage
    , StandardConway
    , StandardMary
    , StandardShelley
    )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
    ( OneEraHash (..)
    )
import Ouroboros.Consensus.HardFork.History.Summary
    ( Bound (..)
    )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardCrypto
    )
import Ouroboros.Consensus.Shelley.Ledger
    ( ShelleyCompatible
    , ShelleyHash (..)
    )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..)
    )
import Ouroboros.Network.Block
    ( BlockNo (..)
    , ChainHash
    , Point (..)
    , Tip (..)
    , getTipPoint
    )
import Ouroboros.Network.NodeToClient
    ( ConnectionId (..)
    , LocalAddress (..)
    , NodeToClientVersion (..)
    , NodeToClientVersionData
    )
import Ouroboros.Network.Point
    ( WithOrigin (..)
    )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Ledger.Address as SL
import qualified Cardano.Ledger.Allegra as Allegra
import qualified Cardano.Ledger.Alonzo as Alonzo
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.TxSeq as Alonzo
import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.Babbage as Babbage
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Conway as Conway
import qualified Cardano.Ledger.Credential as SL
import qualified Cardano.Ledger.Crypto as SL
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Mary as Mary
import qualified Cardano.Ledger.Shelley as Shelley
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.API as SLAPI
import qualified Cardano.Ledger.Shelley.BlockChain as SL
import qualified Cardano.Protocol.TPraos.BHeader as SL
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Address as W
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.RewardAccount as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenPolicy as W
import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as W
import qualified Cardano.Wallet.Primitive.Types.Tx.Constraints as W
import qualified Cardano.Wallet.Primitive.Types.Tx.SealedTx as W
    ( SealedTx
    , cardanoTxIdeallyNoLaterThan
    )
import qualified Cardano.Wallet.Primitive.Types.Tx.Tx as W
    ( Tx (..)
    )
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn as W
    ( TxIn (TxIn)
    )
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W
    ( TxOut (TxOut)
    )
import qualified Cardano.Wallet.Primitive.Types.UTxO as W
import qualified Cardano.Wallet.Shelley.Compatibility.Ledger as Ledger
import qualified Data.Array as Array
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import qualified Data.ListMap as ListMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Internal.Cardano.Write.ProtocolParameters as Write
import qualified Internal.Cardano.Write.Tx as Write
import qualified Ouroboros.Consensus.Protocol.Praos as Consensus
import qualified Ouroboros.Consensus.Protocol.Praos.Header as Consensus
import qualified Ouroboros.Consensus.Protocol.TPraos as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger as O
import qualified Ouroboros.Consensus.Shelley.Protocol.Abstract as Consensus
import qualified Ouroboros.Network.Block as O
import qualified Ouroboros.Network.Point as Point

--------------------------------------------------------------------------------
--
-- Chain Parameters

-- NOTE
-- For MainNet and TestNet, we can get away with empty genesis blocks with
-- the following assumption:
--
-- - Users won't ever restore a wallet that has genesis UTxO.
--
-- This assumption is _true_ for any user using HD wallets (sequential or
-- random) which means, any user of cardano-wallet.
emptyGenesis :: W.GenesisParameters -> W.Block
emptyGenesis gp = W.Block
    { transactions = []
    , delegations  = []
    , header = W.BlockHeader
        { slotNo =
            W.SlotNo 0
        , blockHeight =
            Quantity 0
        , headerHash =
            coerce $ W.getGenesisBlockHash gp
        , parentHeaderHash =
            Nothing
        }
    }

--------------------------------------------------------------------------------
--
-- Network Parameters

-- | The protocol client version. Distinct from the codecs version.
nodeToClientVersions :: [NodeToClientVersion]
nodeToClientVersions = [NodeToClientV_13]

--------------------------------------------------------------------------------
--
-- Type Conversions

toCardanoHash :: W.Hash "BlockHeader" -> OneEraHash (CardanoEras sc)
toCardanoHash (W.Hash bytes) =
    OneEraHash $ toShort bytes

toPoint :: W.ChainPoint -> O.Point (CardanoBlock sc)
toPoint ChainPointAtGenesis = O.GenesisPoint
toPoint (ChainPoint slot h) = O.BlockPoint slot (toCardanoHash h)

fromPoint :: O.Point (CardanoBlock sc) -> W.ChainPoint
fromPoint O.GenesisPoint = ChainPointAtGenesis
fromPoint (O.BlockPoint slot h) = ChainPoint slot (fromCardanoHash h)

toCardanoBlockHeader
    :: W.GenesisParameters
    -> CardanoBlock StandardCrypto
    -> W.BlockHeader
toCardanoBlockHeader gp = \case
    BlockByron blk ->
        toByronBlockHeader gp blk
    BlockShelley blk ->
        toShelleyBlockHeader (W.getGenesisBlockHash gp) blk
    BlockAllegra blk ->
        toShelleyBlockHeader (W.getGenesisBlockHash gp) blk
    BlockMary blk ->
        toShelleyBlockHeader (W.getGenesisBlockHash gp) blk
    BlockAlonzo blk ->
        toShelleyBlockHeader (W.getGenesisBlockHash gp) blk
    BlockBabbage blk ->
        toBabbageBlockHeader (W.getGenesisBlockHash gp) blk
    BlockConway blk ->
        toBabbageBlockHeader (W.getGenesisBlockHash gp) blk

toShelleyBlockHeader
    :: (ShelleyCompatible (Consensus.TPraos StandardCrypto) era)
    => W.Hash "Genesis"
    -> ShelleyBlock (Consensus.TPraos StandardCrypto) era
    -> W.BlockHeader
toShelleyBlockHeader genesisHash blk =
    let
        ShelleyBlock (SL.Block header _txSeq) _headerHash = blk
    in
        W.BlockHeader
            { slotNo =
                Consensus.pHeaderSlot header
            , blockHeight =
                fromBlockNo $ Consensus.pHeaderBlock header
            , headerHash =
                fromShelleyHash $ Consensus.pHeaderHash header
            , parentHeaderHash = Just $
                fromPrevHash (coerce genesisHash) $
                    Consensus.pHeaderPrevHash header
            }

toBabbageBlockHeader
    :: (ShelleyCompatible (Consensus.Praos StandardCrypto) era)
    => W.Hash "Genesis"
    -> ShelleyBlock (Consensus.Praos StandardCrypto) era
    -> W.BlockHeader
toBabbageBlockHeader genesisHash blk =
    W.BlockHeader
        { slotNo = Consensus.pHeaderSlot header
        , blockHeight = fromBlockNo $ Consensus.pHeaderBlock header
        , headerHash = fromShelleyHash $ Consensus.pHeaderHash header
        , parentHeaderHash = Just $ fromPrevHash (coerce genesisHash) $
            Consensus.pHeaderPrevHash header
        }
  where
    ShelleyBlock (SL.Block header _txSeq) _headerHash = blk

toConwayBlockHeader
    :: (ShelleyCompatible (Consensus.Praos StandardCrypto) era)
    => W.Hash "Genesis"
    -> ShelleyBlock (Consensus.Praos StandardCrypto) era
    -> W.BlockHeader
toConwayBlockHeader genesisHash blk =
    W.BlockHeader
        { slotNo = Consensus.pHeaderSlot header
        , blockHeight = fromBlockNo $ Consensus.pHeaderBlock header
        , headerHash = fromShelleyHash $ Consensus.pHeaderHash header
        , parentHeaderHash = Just $ fromPrevHash (coerce genesisHash) $
            Consensus.pHeaderPrevHash header
        }
  where
    ShelleyBlock (SL.Block header _txSeq) _headerHash = blk

getProducer
    :: (Era era, EncCBORGroup (TxSeq era))
    => ShelleyBlock (Consensus.TPraos StandardCrypto) era -> PoolId
getProducer (ShelleyBlock (SL.Block (SL.BHeader header _) _) _) =
    fromPoolKeyHash $ SL.hashKey (SL.bheaderVk header)

getBabbageProducer
    :: (Era era, EncCBORGroup (TxSeq era))
    => ShelleyBlock (Consensus.Praos StandardCrypto) era -> PoolId
getBabbageProducer (ShelleyBlock (SL.Block (Consensus.Header header _) _) _) =
    fromPoolKeyHash $ SL.hashKey (Consensus.hbVk header)

getConwayProducer
    :: (Era era, EncCBORGroup (TxSeq era))
    => ShelleyBlock (Consensus.Praos StandardCrypto) era -> PoolId
getConwayProducer (ShelleyBlock (SL.Block (Consensus.Header header _) _) _) =
    fromPoolKeyHash $ SL.hashKey (Consensus.hbVk header)

fromCardanoBlock
    :: W.GenesisParameters
    -> CardanoBlock StandardCrypto
    -> W.Block
fromCardanoBlock gp = \case
    BlockByron blk ->
        fromByronBlock gp blk
    BlockShelley blk ->
        fst $ fromShelleyBlock gp blk
    BlockAllegra blk ->
        fst $ fromAllegraBlock gp blk
    BlockMary blk ->
        fst $ fromMaryBlock gp blk
    BlockAlonzo blk ->
        fst $ fromAlonzoBlock gp blk
    BlockBabbage blk ->
        fst $ fromBabbageBlock gp blk
    BlockConway blk ->
        fst $ fromConwayBlock gp blk

numberOfTransactionsInBlock
    :: CardanoBlock StandardCrypto -> (Int, (Quantity "block" Word32, O.SlotNo))
numberOfTransactionsInBlock = \case
    BlockByron byb -> transactionsByron byb
    BlockShelley shb -> transactions shb
    BlockAllegra shb -> transactions shb
    BlockMary shb -> transactions shb
    BlockAlonzo shb -> transactionsAlonzo shb
    BlockBabbage shb -> transactionsBabbage shb
    BlockConway shb -> transactionsConway shb
  where
    transactions
        (ShelleyBlock
            (SL.Block (SL.BHeader header _) (SL.ShelleyTxSeq txs'))
            _
        ) =
            ( length txs'
            , (fromBlockNo $ SL.bheaderBlockNo header, SL.bheaderSlotNo header)
            )
    transactionsAlonzo
        (ShelleyBlock
            (SL.Block (SL.BHeader header _) (Alonzo.AlonzoTxSeq txs'))
            _
        ) =
            ( length txs'
            , (fromBlockNo $ SL.bheaderBlockNo header, SL.bheaderSlotNo header)
            )
    transactionsBabbage
        :: ShelleyBlock
            (Consensus.Praos StandardCrypto)
            (Babbage.BabbageEra StandardCrypto)
        -> (Int, (Quantity "block" Word32, O.SlotNo))
    transactionsBabbage
        (ShelleyBlock
            (SL.Block (Consensus.Header header _)
            (Alonzo.AlonzoTxSeq txs')) _) =
                ( length txs'
                , ( fromBlockNo $ Consensus.hbBlockNo header
                  , Consensus.hbSlotNo header
                  )
                )
    transactionsByron blk =
        (, (fromBlockNo $ O.blockNo blk, O.blockSlot blk)) $
            case byronBlockRaw blk of
            ABOBBlock blk' ->
                length $ fromTxAux <$> unTxPayload (blockTxPayload blk')
            ABOBBoundary _ ->
                0

    transactionsConway
        :: ShelleyBlock
            (Consensus.Praos StandardCrypto)
            (Conway.ConwayEra StandardCrypto)
        -> (Int, (Quantity "block" Word32, O.SlotNo))
    transactionsConway
        (ShelleyBlock
            (SL.Block (Consensus.Header header _)
            (Alonzo.AlonzoTxSeq txs')) _) =
                ( length txs'
                , ( fromBlockNo $ Consensus.hbBlockNo header
                  , Consensus.hbSlotNo header
                  )
                )

toCardanoEra :: CardanoBlock c -> AnyCardanoEra
toCardanoEra = \case
    BlockByron{}   -> AnyCardanoEra ByronEra
    BlockShelley{} -> AnyCardanoEra ShelleyEra
    BlockAllegra{} -> AnyCardanoEra AllegraEra
    BlockMary{}    -> AnyCardanoEra MaryEra
    BlockAlonzo{}  -> AnyCardanoEra AlonzoEra
    BlockBabbage{} -> AnyCardanoEra BabbageEra
    BlockConway{}  -> AnyCardanoEra ConwayEra

fromShelleyBlock
    :: W.GenesisParameters
    -> ShelleyBlock
        (Consensus.TPraos StandardCrypto)
        (Shelley.ShelleyEra StandardCrypto)
    -> (W.Block, [PoolCertificate])
fromShelleyBlock gp blk@(ShelleyBlock (SL.Block _ (SL.ShelleyTxSeq txs')) _) =
    let
       (txs, certs, _, _, _, _) = unzip6 $ map fromShelleyTx $ toList txs'
       certs' = mconcat certs
    in
        ( W.Block
            { header = toShelleyBlockHeader (W.getGenesisBlockHash gp) blk
            , transactions = txs
            , delegations  = toDelegationCertificates certs'
            }
        , toPoolCertificates certs'
        )

fromAllegraBlock
    :: W.GenesisParameters
    -> ShelleyBlock
        (Consensus.TPraos StandardCrypto)
        (Allegra.AllegraEra StandardCrypto)
    -> (W.Block, [PoolCertificate])
fromAllegraBlock gp blk@(ShelleyBlock (SL.Block _ (SL.ShelleyTxSeq txs')) _) =
    let
       (txs, certs, _, _, _, _) = unzip6 $ map fromAllegraTx $ toList txs'
       certs' = mconcat certs
    in
        ( W.Block
            { header = toShelleyBlockHeader (W.getGenesisBlockHash gp) blk
            , transactions = txs
            , delegations  = toDelegationCertificates certs'
            }
        , toPoolCertificates certs'
        )

fromMaryBlock
    :: W.GenesisParameters
    -> ShelleyBlock
        (Consensus.TPraos StandardCrypto)
        (Mary.MaryEra StandardCrypto)
    -> (W.Block, [PoolCertificate])
fromMaryBlock gp blk@(ShelleyBlock (SL.Block _ (SL.ShelleyTxSeq txs')) _) =
    let
       (txs, certs, _, _, _, _) = unzip6 $
           map (`fromMaryTx` AnyWitnessCountCtx) $ toList txs'
       certs' = mconcat certs
    in
        ( W.Block
            { header = toShelleyBlockHeader (W.getGenesisBlockHash gp) blk
            , transactions = txs
            , delegations  = toDelegationCertificates certs'
            }
        , toPoolCertificates certs'
        )

-- TODO: We could use the cardano-api `Block` pattern to very elegently get the
-- header and txs of any era block.
--
-- We would need to remove the previous block hash from our `W.BlockHeader`,
-- which shouldn't be needed modulo some hacks w.r.t. the genesis point which
-- would need to be cleaned up too. We probably will need to use `Point block`,
-- in all chain followers (including the DBLayer).
fromAlonzoBlock
    :: ShelleyCompatible
        (Consensus.TPraos StandardCrypto)
        (Alonzo.AlonzoEra StandardCrypto)
    => W.GenesisParameters
    -> ShelleyBlock
        (Consensus.TPraos StandardCrypto)
        (Alonzo.AlonzoEra StandardCrypto)
    -> (W.Block, [PoolCertificate])
fromAlonzoBlock gp blk@(ShelleyBlock (SL.Block _ txSeq) _) =
    let
        Alonzo.AlonzoTxSeq txs' = txSeq
        (txs, certs, _, _, _, _) = unzip6 $
            map (`fromAlonzoTx` AnyWitnessCountCtx) $ toList txs'
        certs' = mconcat certs
    in
        ( W.Block
            { header = toShelleyBlockHeader (W.getGenesisBlockHash gp) blk
            , transactions = txs
            , delegations  = toDelegationCertificates certs'
            }
        , toPoolCertificates certs'
        )

fromBabbageBlock
    :: W.GenesisParameters
    -> ShelleyBlock
        (Consensus.Praos StandardCrypto)
        (Babbage.BabbageEra StandardCrypto)
    -> (W.Block, [PoolCertificate])
fromBabbageBlock gp blk@(ShelleyBlock (SL.Block _ txSeq) _) =
    let
        Alonzo.AlonzoTxSeq txs' = txSeq
        (txs, certs, _, _, _, _) = unzip6 $
            map (`fromBabbageTx` AnyWitnessCountCtx) $ toList txs'
        certs' = mconcat certs
    in
        ( W.Block
            { header = toBabbageBlockHeader (W.getGenesisBlockHash gp) blk
            , transactions = txs
            , delegations  = toDelegationCertificates certs'
            }
        , toPoolCertificates certs'
        )

fromConwayBlock
    :: W.GenesisParameters
    -> ShelleyBlock
        (Consensus.Praos StandardCrypto)
        (Conway.ConwayEra StandardCrypto)
    -> (W.Block, [PoolCertificate])
fromConwayBlock gp blk@(ShelleyBlock (SL.Block _ txSeq) _) =
    let
        Alonzo.AlonzoTxSeq txs' = txSeq
        (txs, certs, _, _, _, _) = unzip6 $
            map (`fromConwayTx` AnyWitnessCountCtx) $ toList txs'
        certs' = mconcat certs
    in
        ( W.Block
            { header = toConwayBlockHeader (W.getGenesisBlockHash gp) blk
            , transactions = txs
            , delegations  = toDelegationCertificates certs'
            }
        , toPoolCertificates certs'
        )

fromShelleyHash :: ShelleyHash crypto -> W.Hash "BlockHeader"
fromShelleyHash (ShelleyHash h) = W.Hash (hashToBytes h)

fromCardanoHash :: O.HeaderHash (CardanoBlock sc) -> W.Hash "BlockHeader"
fromCardanoHash = W.Hash . fromShort . getOneEraHash

fromPrevHash
    :: W.Hash "BlockHeader"
    -> SL.PrevHash crypto
    -> W.Hash "BlockHeader"
fromPrevHash genesisHash = \case
    SL.GenesisHash -> genesisHash
    SL.BlockHash (SL.HashHeader h) -> W.Hash (hashToBytes h)

fromChainHash
    :: W.Hash "Genesis"
    -> ChainHash (CardanoBlock sc)
    -> W.Hash "BlockHeader"
fromChainHash genesisHash = \case
    O.GenesisHash -> coerce genesisHash
    O.BlockHash (OneEraHash h) -> W.Hash $ fromShort h

-- FIXME unsafe conversion (Word64 -> Word32)
fromBlockNo :: BlockNo -> Quantity "block" Word32
fromBlockNo (BlockNo h) = Quantity (fromIntegral h)

fromTip' :: W.GenesisParameters -> Tip (CardanoBlock sc) -> W.BlockHeader
fromTip' gp = fromTip (W.getGenesisBlockHash gp)

fromTip
    :: W.Hash "Genesis"
    -> Tip (CardanoBlock sc)
    -> W.BlockHeader
fromTip genesisHash tip = case getPoint (getTipPoint tip) of
    Origin -> W.BlockHeader
        { slotNo = W.SlotNo 0
        , blockHeight = Quantity 0
        , headerHash = coerce genesisHash
        , parentHeaderHash = Nothing
        }
    At blk -> W.BlockHeader
        { slotNo = Point.blockPointSlot blk
        , blockHeight = fromBlockNo $ getLegacyTipBlockNo tip
        , headerHash = fromCardanoHash $ Point.blockPointHash blk
        -- TODO: parentHeaderHash could be removed.
        , parentHeaderHash = Just $ W.Hash "parentHeaderHash - unused in Shelley"
        }
  where
    -- TODO: This function was marked deprecated in ouroboros-network.
    -- It is wrong, because `Origin` doesn't have a block number.
    -- We should remove it.
    getLegacyTipBlockNo t = case O.getTipBlockNo t of
        Origin -> BlockNo 0
        At x -> x

toTip :: W.Hash "Genesis" -> W.BlockHeader -> Tip (CardanoBlock sc)
toTip genesisHash (W.BlockHeader sl bl h _)
    | h == (coerce genesisHash) = O.TipGenesis
    | otherwise = O.Tip sl
        (toCardanoHash h)
        (BlockNo $ fromIntegral $ getQuantity bl)

-- NOTE: Unsafe conversion from Natural -> Word16
fromMaxSize :: Natural -> Quantity "byte" Word16
fromMaxSize = Quantity . fromIntegral

fromShelleyPParams
    :: W.EraInfo Bound
    -> Ledger.PParams StandardShelley
    -> W.ProtocolParameters
fromShelleyPParams eraInfo pp =
    W.ProtocolParameters
        { decentralizationLevel = decentralizationLevelFromPParams pp
        , txParameters =
            txParametersFromPParams
                maryTokenBundleMaxSize (W.ExecutionUnits 0 0) pp
        , desiredNumberOfStakePools =
            desiredNumberOfStakePoolsFromPParams pp
        , stakeKeyDeposit = stakeKeyDepositFromPParams pp
        , eras = fromBoundToEpochNo <$> eraInfo
        -- Collateral inputs were not supported or required in Shelley:
        , maximumCollateralInputCount = 0
        , minimumCollateralPercentage = 0
        , executionUnitPrices = Nothing
        , currentLedgerProtocolParameters = Write.InNonRecentEraShelley
        }

fromAllegraPParams
    :: W.EraInfo Bound
    -> Ledger.PParams StandardAllegra
    -> W.ProtocolParameters
fromAllegraPParams eraInfo pp =
    W.ProtocolParameters
        { decentralizationLevel = decentralizationLevelFromPParams pp
        , txParameters =
            txParametersFromPParams
                maryTokenBundleMaxSize (W.ExecutionUnits 0 0) pp
        , desiredNumberOfStakePools =
            desiredNumberOfStakePoolsFromPParams pp
        , stakeKeyDeposit = stakeKeyDepositFromPParams pp
        , eras = fromBoundToEpochNo <$> eraInfo
        -- Collateral inputs were not supported or required in Allegra:
        , maximumCollateralInputCount = 0
        , minimumCollateralPercentage = 0
        , executionUnitPrices = Nothing
        , currentLedgerProtocolParameters = Write.InNonRecentEraAllegra
        }

fromMaryPParams
    :: W.EraInfo Bound
    -> Ledger.PParams StandardMary
    -> W.ProtocolParameters
fromMaryPParams eraInfo pp =
    W.ProtocolParameters
        { decentralizationLevel = decentralizationLevelFromPParams pp
        , txParameters =
            txParametersFromPParams
                maryTokenBundleMaxSize (W.ExecutionUnits 0 0) pp
        , desiredNumberOfStakePools =
            desiredNumberOfStakePoolsFromPParams pp
        , stakeKeyDeposit = stakeKeyDepositFromPParams pp
        , eras = fromBoundToEpochNo <$> eraInfo
        -- Collateral inputs were not supported or required in Mary:
        , maximumCollateralInputCount = 0
        , minimumCollateralPercentage = 0
        , executionUnitPrices = Nothing
        , currentLedgerProtocolParameters = Write.InNonRecentEraMary
        }

fromBoundToEpochNo :: Bound -> W.EpochNo
fromBoundToEpochNo (Bound _relTime _slotNo (EpochNo e)) =
    W.EpochNo $ fromIntegral e

fromAlonzoPParams
    :: HasCallStack
    => W.EraInfo Bound
    -> Ledger.PParams StandardAlonzo
    -> W.ProtocolParameters
fromAlonzoPParams eraInfo pp =
    W.ProtocolParameters
        { decentralizationLevel = decentralizationLevelFromPParams pp
        , txParameters = txParametersFromPParams
            (W.TokenBundleMaxSize $ W.TxSize $ pp ^. ppMaxValSizeL)
            (fromLedgerExUnits (pp ^. ppMaxTxExUnitsL))
            pp
        , desiredNumberOfStakePools =
            desiredNumberOfStakePoolsFromPParams pp
        , stakeKeyDeposit = stakeKeyDepositFromPParams pp
        , eras = fromBoundToEpochNo <$> eraInfo
        , maximumCollateralInputCount =
            unsafeIntToWord $ pp ^. ppMaxCollateralInputsL
        , minimumCollateralPercentage =
            pp ^. ppCollateralPercentageL
        , executionUnitPrices =
            Just $ executionUnitPricesFromPParams pp
        , currentLedgerProtocolParameters = Write.InNonRecentEraAlonzo
        }

fromBabbagePParams
    :: HasCallStack
    => W.EraInfo Bound
    -> Ledger.PParams StandardBabbage
    -> W.ProtocolParameters
fromBabbagePParams eraInfo pp =
    W.ProtocolParameters
        { decentralizationLevel =
            W.fromFederationPercentage $ clipToPercentage 0
        , txParameters = txParametersFromPParams
            (W.TokenBundleMaxSize $ W.TxSize $ pp ^. ppMaxValSizeL)
            (fromLedgerExUnits (pp ^. ppMaxTxExUnitsL))
            pp
        , desiredNumberOfStakePools =
            desiredNumberOfStakePoolsFromPParams pp
        , stakeKeyDeposit = stakeKeyDepositFromPParams pp
        , eras = fromBoundToEpochNo <$> eraInfo
        , maximumCollateralInputCount =
            unsafeIntToWord $ pp ^. ppMaxCollateralInputsL
        , minimumCollateralPercentage =
            pp ^. ppCollateralPercentageL
        , executionUnitPrices =
            Just $ executionUnitPricesFromPParams pp
        , currentLedgerProtocolParameters =
            Write.InRecentEraBabbage $ Write.ProtocolParameters pp
        }

fromConwayPParams
    :: HasCallStack
    => W.EraInfo Bound
    -> Ledger.PParams StandardConway
    -> W.ProtocolParameters
fromConwayPParams eraInfo pp =
    W.ProtocolParameters
        { decentralizationLevel =
            W.fromFederationPercentage $ clipToPercentage 0
        , txParameters = txParametersFromPParams
            (W.TokenBundleMaxSize $ W.TxSize $ pp ^. ppMaxValSizeL)
            (fromLedgerExUnits (pp ^. ppMaxTxExUnitsL))
            pp
        , desiredNumberOfStakePools = desiredNumberOfStakePoolsFromPParams pp
        , stakeKeyDeposit = stakeKeyDepositFromPParams pp
        , eras = fromBoundToEpochNo <$> eraInfo
        , maximumCollateralInputCount =
            intCastMaybe (pp ^. ppMaxCollateralInputsL)
                & fromMaybe
                    (error "Maximum count of collateral inputs exceeds 2^16")
        , minimumCollateralPercentage = pp ^. ppCollateralPercentageL
        , executionUnitPrices = Just $ executionUnitPricesFromPParams pp
        , currentLedgerProtocolParameters =
            Write.InRecentEraConway $ Write.ProtocolParameters pp
        }

-- | Extract the current network decentralization level from the given set of
-- protocol parameters.
decentralizationLevelFromPParams
    :: (Ledger.EraPParams era, Ledger.ProtVerAtMost era 6)
    => Ledger.PParams era -> W.DecentralizationLevel
decentralizationLevelFromPParams pp =
    W.fromFederationPercentage $ fromUnitInterval $ pp ^. ppDL

executionUnitPricesFromPParams
    :: Ledger.AlonzoEraPParams era
    => Ledger.PParams era
    -> W.ExecutionUnitPrices
executionUnitPricesFromPParams pp = fromAlonzoPrices (pp ^. ppPricesL)
  where
    fromAlonzoPrices Alonzo.Prices{prMem, prSteps} =
        W.ExecutionUnitPrices
        { W.pricePerStep = SL.unboundRational prSteps
        , W.pricePerMemoryUnit = SL.unboundRational prMem
        }

fromLedgerExUnits
    :: Alonzo.ExUnits
    -> W.ExecutionUnits
fromLedgerExUnits (Alonzo.ExUnits mem steps) =
    W.ExecutionUnits
    { executionSteps = steps
    , executionMemory = mem
    }

toLedgerExUnits :: W.ExecutionUnits -> Alonzo.ExUnits
toLedgerExUnits W.ExecutionUnits{executionSteps,executionMemory} =
    Alonzo.ExUnits
    { Alonzo.exUnitsMem = executionMemory
    , Alonzo.exUnitsSteps = executionSteps
    }

txParametersFromPParams
    :: Ledger.EraPParams era
    => W.TokenBundleMaxSize
    -> W.ExecutionUnits
    -> Ledger.PParams era
    -> W.TxParameters
txParametersFromPParams maxBundleSize getMaxExecutionUnits pp = W.TxParameters
    { getFeePolicy = W.LinearFee $ W.LinearFunction
        { intercept = coinToDouble (pp ^. ppMinFeeBL)
        , slope = coinToDouble (pp ^. ppMinFeeAL)
        }
    , getTxMaxSize = fromMaxSize $ pp ^. ppMaxTxSizeL
    , getTokenBundleMaxSize = maxBundleSize
    , getMaxExecutionUnits
    }
  where
    coinToDouble :: Ledger.Coin -> Double
    coinToDouble = fromRational . Ledger.coinToRational

toCostModelsAsArray
    :: Map Alonzo.Language Alonzo.CostModel
    -> Array Alonzo.Language Alonzo.CostModel
toCostModelsAsArray costModels =
    Array.array (minBound, maxBound) [ (k, v) | (k, v) <- Map.toList costModels ]

--------------------------------------------------------------------------------

desiredNumberOfStakePoolsFromPParams
    :: (HasCallStack, Ledger.EraPParams era) => Ledger.PParams era -> Word16
desiredNumberOfStakePoolsFromPParams pp =
    intCastMaybe (pp ^. ppNOptL)
        & fromMaybe (error "Desired number of stake pools exceeds 2^16")

stakeKeyDepositFromPParams
    :: Ledger.EraPParams era => Ledger.PParams era -> W.Coin
stakeKeyDepositFromPParams = toWalletCoin . view ppKeyDepositL

slottingParametersFromGenesis :: ShelleyGenesis e -> W.SlottingParameters
slottingParametersFromGenesis g =
    W.SlottingParameters
        { getSlotLength =
            W.SlotLength . fromNominalDiffTimeMicro $ sgSlotLength g
        , getEpochLength =
            W.EpochLength . fromIntegral . unEpochSize $ sgEpochLength g
        , getActiveSlotCoefficient =
            W.ActiveSlotCoefficient . fromRational . SL.unboundRational $ sgActiveSlotsCoeff g
        , getSecurityParameter =
            Quantity . fromIntegral $ sgSecurityParam g
        }

-- note: upcasts Word32 -> Word64
getCardanoEpochSlots :: W.SlottingParameters -> Cardano.EpochSlots
getCardanoEpochSlots =
    Cardano.EpochSlots . fromIntegral . W.unEpochLength . W.getEpochLength

localNodeConnectInfo
    :: W.SlottingParameters
    -> NetworkId
    -> CardanoNodeConn
    -> LocalNodeConnectInfo CardanoMode
localNodeConnectInfo slottingParameters networkId nodeConn =
    LocalNodeConnectInfo
        (CardanoModeParams (getCardanoEpochSlots slottingParameters))
        networkId
        (File (nodeSocketFile nodeConn))

-- | Convert genesis data into blockchain params and an initial set of UTxO
fromGenesisData
    :: ShelleyGenesis StandardCrypto
    -> (W.NetworkParameters, W.Block, [PoolCertificate])
fromGenesisData g =
    ( W.NetworkParameters
        { genesisParameters = W.GenesisParameters
            { getGenesisBlockHash = dummyGenesisHash
            , getGenesisBlockDate = W.StartTime $ sgSystemStart g
            }
        , slottingParameters = slottingParametersFromGenesis g
        , protocolParameters =
            fromShelleyPParams W.emptyEraInfo (sgProtocolParams g)
        }
    , genesisBlockFromTxOuts (ListMap.toList $ sgInitialFunds g)
    , poolCerts $ sgStaking g
    )
  where
    -- TODO: There is not yet any agreed upon definition of a
    -- genesis hash for a shelley-only testnet.
    --
    -- For now we use a dummy value.
    dummyGenesisHash = W.Hash . BS.pack $ replicate 32 1

    poolCerts :: SLAPI.ShelleyGenesisStaking StandardCrypto -> [PoolCertificate]
    poolCerts (SLAPI.ShelleyGenesisStaking pools _stake) = do
        (_, pp) <- ListMap.toList pools
        pure $ W.Registration $ PoolRegistrationCertificate
            { W.poolId = fromPoolKeyHash $ ppId pp
            , W.poolOwners = fromOwnerKeyHash <$> Set.toList (ppOwners pp)
            , W.poolMargin = fromUnitInterval (ppMargin pp)
            , W.poolCost = toWalletCoin (ppCost pp)
            , W.poolPledge = toWalletCoin (ppPledge pp)
            , W.poolMetadata =
                fromPoolMetadata <$> strictMaybeToMaybe (ppMetadata pp)
            }

    -- | Construct a ("fake") genesis block from genesis transaction outputs.
    --
    -- The genesis data on haskell nodes is not a block at all, unlike the
    -- block0 on jormungandr. This function is a method to deal with the
    -- discrepancy.
    genesisBlockFromTxOuts :: [(SL.Addr StandardCrypto, SL.Coin)] -> W.Block
    genesisBlockFromTxOuts outs = W.Block
        { delegations  = []
        , header = W.BlockHeader
            { slotNo = W.SlotNo 0
            , blockHeight = Quantity 0
            , headerHash = dummyGenesisHash
            , parentHeaderHash = Nothing
            }
        , transactions = mkTx <$> outs
        }
      where
        mkTx (addr, c) = W.Tx
            { txId = pseudoHash
            , txCBOR = Nothing
            , fee = Nothing
            , resolvedInputs = []
            , resolvedCollateralInputs = []
            , outputs =
                [W.TxOut
                    (fromShelleyAddress addr)
                    (TokenBundle.fromCoin $ Ledger.toWalletCoin c)
                ]
            -- Collateral outputs were not supported at the time of genesis:
            , collateralOutput = Nothing
            , withdrawals = mempty
            , metadata = Nothing
            , scriptValidity = Nothing
            }
          where
            W.TxIn pseudoHash _ = fromShelleyTxIn $
                SL.initialFundsPseudoTxIn @StandardCrypto addr

--
-- Stake pools
--

fromPoolId :: forall crypto. SL.KeyHash 'SL.StakePool crypto -> PoolId
fromPoolId (SL.KeyHash x) = PoolId $ hashToBytes x

fromPoolDistr
    :: forall crypto. ()
    => SL.PoolDistr crypto
    -> Map PoolId Percentage
fromPoolDistr =
    Map.map (unsafeMkPercentage . SL.individualPoolStake)
    . Map.mapKeys fromPoolId
    . SL.unPoolDistr

-- NOTE: This function disregards results that are using staking keys
fromNonMyopicMemberRewards
    :: forall era. ()
    => O.NonMyopicMemberRewards era
    -> Map (Either W.Coin W.RewardAccount) (Map PoolId W.Coin)
fromNonMyopicMemberRewards =
    Map.map (Map.map toWalletCoin . Map.mapKeys fromPoolId)
    . Map.mapKeys (bimap Ledger.toWalletCoin fromStakeCredential)
    . O.unNonMyopicMemberRewards

optimumNumberOfPools
    :: (HasCallStack, Ledger.EraPParams era) => Ledger.PParams era -> Int
optimumNumberOfPools = intCast . desiredNumberOfStakePoolsFromPParams

--
-- Txs
--

fromCardanoTxIn :: Cardano.TxIn -> W.TxIn
fromCardanoTxIn (Cardano.TxIn txid (Cardano.TxIx ix)) =
    W.TxIn
        (W.Hash $ fromShelleyTxId $ Cardano.toShelleyTxId txid)
        (fromIntegral ix)

-- | WARNING: Datum hashes are lost in the conversion!
fromCardanoTxOut :: IsCardanoEra era => Cardano.TxOut ctx era -> W.TxOut
fromCardanoTxOut (Cardano.TxOut addr out _datumHash _) =
    W.TxOut
        (W.Address $ Cardano.serialiseToRawBytes addr)
        (fromCardanoTxOutValue out)
  where
    fromCardanoTxOutValue (Cardano.TxOutValue _ val) = fromCardanoValue val
    fromCardanoTxOutValue (Cardano.TxOutAdaOnly _ lovelace) =
        TokenBundle.fromCoin $ fromCardanoLovelace lovelace

fromCardanoWdrls
    :: Cardano.TxWithdrawals build era
    -> [(W.RewardAccount, W.Coin)]
fromCardanoWdrls = \case
    Cardano.TxWithdrawalsNone -> []
    Cardano.TxWithdrawals _era xs ->
        flip fmap xs $ \((Cardano.StakeAddress _ creds), coin, _) ->
            ( fromStakeCredential creds
            , fromCardanoLovelace coin
            )

cardanoCertKeysForWitnesses
    :: Cardano.TxCertificates build era
    -> [W.RewardAccount]
cardanoCertKeysForWitnesses = \case
    Cardano.TxCertificatesNone -> []
    Cardano.TxCertificates _era certs _witsMap ->
        mapMaybe f certs
 where
    toRewardAccount = Just . fromStakeCredential . Cardano.toShelleyStakeCredential
    f = \case
        Cardano.StakeAddressDeregistrationCertificate cred ->
            toRewardAccount cred
        Cardano.StakeAddressPoolDelegationCertificate cred _ ->
            toRewardAccount cred
        _ ->
            Nothing

toShelleyCoin :: W.Coin -> SL.Coin
toShelleyCoin (W.Coin c) = SL.Coin $ intCast c

-- Lovelace to coin. Quantities from ledger should always fit in Word64.
fromCardanoLovelace :: HasCallStack => Cardano.Lovelace -> W.Coin
fromCardanoLovelace =
    Coin.unsafeFromIntegral . unQuantity . Cardano.lovelaceToQuantity
  where
    unQuantity (Cardano.Quantity q) = q

toDelegationCertificates
    :: [W.Certificate]
    -> [W.DelegationCertificate]
toDelegationCertificates = mapMaybe isDelCert
  where
      isDelCert = \case
          W.CertificateOfDelegation cert -> Just cert
          _ -> Nothing

toPoolCertificates
    :: [W.Certificate]
    -> [PoolCertificate]
toPoolCertificates = mapMaybe isPoolCert
  where
      isPoolCert = \case
          W.CertificateOfPool cert -> Just cert
          _ -> Nothing

toWalletCoin :: HasCallStack => SL.Coin -> W.Coin
toWalletCoin (SL.Coin c) = Coin.unsafeFromIntegral c

fromPoolMetadata :: SL.PoolMetadata -> (StakePoolMetadataUrl, StakePoolMetadataHash)
fromPoolMetadata meta =
    ( StakePoolMetadataUrl (urlToText (pmUrl meta))
    , StakePoolMetadataHash (pmHash meta)
    )

fromPoolKeyHash :: SL.KeyHash rol sc -> PoolId
fromPoolKeyHash (SL.KeyHash h) = PoolId (hashToBytes h)

fromOwnerKeyHash :: SL.KeyHash 'SL.Staking crypto -> PoolOwner
fromOwnerKeyHash (SL.KeyHash h) = PoolOwner (hashToBytes h)

fromCardanoAddress :: Cardano.Address Cardano.ShelleyAddr -> W.Address
fromCardanoAddress = W.Address . Cardano.serialiseToRawBytes

fromUnitInterval :: HasCallStack => SL.UnitInterval -> Percentage
fromUnitInterval x =
    either bomb id . mkPercentage . toRational . SL.unboundRational $ x
  where
    bomb = internalError $
        "fromUnitInterval: encountered invalid parameter value: "+||x||+""

toSystemStart :: W.StartTime -> SystemStart
toSystemStart (W.StartTime t) = SystemStart t

toCardanoTxId :: W.Hash "Tx" -> Cardano.TxId
toCardanoTxId (W.Hash h) = Cardano.TxId $ UnsafeHash $ toShort h

toCardanoTxIn :: W.TxIn -> Cardano.TxIn
toCardanoTxIn (W.TxIn tid ix) =
    Cardano.TxIn (toCardanoTxId tid) (Cardano.TxIx (fromIntegral ix))

toCardanoStakeCredential :: W.RewardAccount -> Cardano.StakeCredential
toCardanoStakeCredential = \case
    W.FromKeyHash bs ->
        Cardano.StakeCredentialByKey
        . Cardano.StakeKeyHash
        . SL.KeyHash
        . UnsafeHash
        . SBS.toShort
        $ bs
    W.FromScriptHash bs ->
        Cardano.StakeCredentialByScript
        . Cardano.fromShelleyScriptHash
        . SL.ScriptHash
        . unsafeHashFromBytes
        $ bs

toCardanoLovelace :: W.Coin -> Cardano.Lovelace
toCardanoLovelace (W.Coin c) = Cardano.Lovelace $ intCast c

toCardanoUTxO :: ShelleyBasedEra era -> W.UTxO -> Cardano.UTxO era
toCardanoUTxO era = Cardano.UTxO
    . Map.fromList
    . map (bimap toCardanoTxIn (toCardanoTxOut era Nothing))
    . Map.toList
    . W.unUTxO

toCardanoTxOut
    :: HasCallStack
    => ShelleyBasedEra era
    -> Maybe (Script KeyHash)
    -> W.TxOut
    -> Cardano.TxOut ctx era
toCardanoTxOut era refScriptM = case era of
    ShelleyBasedEraShelley -> toShelleyTxOut
    ShelleyBasedEraAllegra -> toAllegraTxOut
    ShelleyBasedEraMary    -> toMaryTxOut
    ShelleyBasedEraAlonzo  -> toAlonzoTxOut
    ShelleyBasedEraBabbage -> toBabbageTxOut
    ShelleyBasedEraConway  -> toConwayTxOut
  where
    toShelleyTxOut :: HasCallStack => W.TxOut -> Cardano.TxOut ctx ShelleyEra
    toShelleyTxOut (W.TxOut (W.Address addr) tokens) =
        Cardano.TxOut
            addrInEra
            (adaOnly $ toCardanoLovelace $ TokenBundle.getCoin tokens)
            Cardano.TxOutDatumNone
            Cardano.ReferenceScriptNone
      where
        adaOnly = Cardano.TxOutAdaOnly Cardano.AdaOnlyInShelleyEra
        addrInEra = tina "toCardanoTxOut: malformed address"
            [ Cardano.AddressInEra
                (Cardano.ShelleyAddressInEra Cardano.ShelleyBasedEraShelley)
                <$> eitherToMaybe
                    (Cardano.deserialiseFromRawBytes AsShelleyAddress addr)

            , Cardano.AddressInEra Cardano.ByronAddressInAnyEra
                <$> eitherToMaybe
                    (Cardano.deserialiseFromRawBytes AsByronAddress addr)
            ]

    toAllegraTxOut :: HasCallStack => W.TxOut -> Cardano.TxOut ctx AllegraEra
    toAllegraTxOut (W.TxOut (W.Address addr) tokens) =
        Cardano.TxOut
            addrInEra
            (adaOnly $ toCardanoLovelace $ TokenBundle.getCoin tokens)
            Cardano.TxOutDatumNone
            Cardano.ReferenceScriptNone
      where
        adaOnly = Cardano.TxOutAdaOnly Cardano.AdaOnlyInAllegraEra
        addrInEra = tina "toCardanoTxOut: malformed address"
            [ Cardano.AddressInEra
                (Cardano.ShelleyAddressInEra Cardano.ShelleyBasedEraAllegra)
                <$> eitherToMaybe
                    (Cardano.deserialiseFromRawBytes AsShelleyAddress addr)

            , Cardano.AddressInEra Cardano.ByronAddressInAnyEra
                <$> eitherToMaybe
                    (Cardano.deserialiseFromRawBytes AsByronAddress addr)
            ]

    toMaryTxOut :: HasCallStack => W.TxOut -> Cardano.TxOut ctx MaryEra
    toMaryTxOut (W.TxOut (W.Address addr) tokens) =
        Cardano.TxOut
            addrInEra
            (Cardano.TxOutValue Cardano.MultiAssetInMaryEra
                $ toCardanoValue tokens)
            Cardano.TxOutDatumNone
            Cardano.ReferenceScriptNone
      where
        addrInEra = tina "toCardanoTxOut: malformed address"
            [ Cardano.AddressInEra
                (Cardano.ShelleyAddressInEra Cardano.ShelleyBasedEraMary)
                    <$> eitherToMaybe
                        (Cardano.deserialiseFromRawBytes AsShelleyAddress addr)

            , Cardano.AddressInEra Cardano.ByronAddressInAnyEra
                <$> eitherToMaybe
                    (Cardano.deserialiseFromRawBytes AsByronAddress addr)
            ]

    toAlonzoTxOut :: HasCallStack => W.TxOut -> Cardano.TxOut ctx AlonzoEra
    toAlonzoTxOut (W.TxOut (W.Address addr) tokens) =
        Cardano.TxOut
            addrInEra
            (Cardano.TxOutValue Cardano.MultiAssetInAlonzoEra
                $ toCardanoValue tokens)
            datumHash
            refScript
      where
        refScript = Cardano.ReferenceScriptNone
        datumHash = Cardano.TxOutDatumNone
        addrInEra = tina "toCardanoTxOut: malformed address"
            [ Cardano.AddressInEra
                (Cardano.ShelleyAddressInEra Cardano.ShelleyBasedEraAlonzo)
                    <$> eitherToMaybe
                        (Cardano.deserialiseFromRawBytes AsShelleyAddress addr)

            , Cardano.AddressInEra Cardano.ByronAddressInAnyEra
                <$> eitherToMaybe
                    (Cardano.deserialiseFromRawBytes AsByronAddress addr)
            ]

    toBabbageTxOut :: HasCallStack => W.TxOut -> Cardano.TxOut ctx BabbageEra
    toBabbageTxOut (W.TxOut (W.Address addr) tokens) =
        Cardano.TxOut
            addrInEra
            (Cardano.TxOutValue Cardano.MultiAssetInBabbageEra
                $ toCardanoValue tokens)
            datumHash
            refScript
      where
        refScript = case refScriptM of
            Nothing ->
                Cardano.ReferenceScriptNone
            Just script ->
                let aux = Cardano.ReferenceTxInsScriptsInlineDatumsInBabbageEra
                    scriptApi = Cardano.toScriptInAnyLang $ Cardano.SimpleScript $
                        toCardanoSimpleScript script
                in Cardano.ReferenceScript aux scriptApi
        datumHash = Cardano.TxOutDatumNone
        addrInEra = tina "toCardanoTxOut: malformed address"
            [ Cardano.AddressInEra
                (Cardano.ShelleyAddressInEra Cardano.ShelleyBasedEraBabbage)
                    <$> eitherToMaybe
                        (Cardano.deserialiseFromRawBytes AsShelleyAddress addr)
            , Cardano.AddressInEra Cardano.ByronAddressInAnyEra
                <$> eitherToMaybe
                    (Cardano.deserialiseFromRawBytes AsByronAddress addr)
            ]

    toConwayTxOut :: HasCallStack => W.TxOut -> Cardano.TxOut ctx ConwayEra
    toConwayTxOut (W.TxOut (W.Address addr) tokens) =
        Cardano.TxOut
            addrInEra
            (Cardano.TxOutValue Cardano.MultiAssetInConwayEra
                $ toCardanoValue tokens)
            datumHash
            refScript
      where
        refScript = case refScriptM of
            Nothing ->
                Cardano.ReferenceScriptNone
            Just script ->
                let aux = Cardano.ReferenceTxInsScriptsInlineDatumsInConwayEra
                    scriptApi = Cardano.toScriptInAnyLang $ Cardano.SimpleScript $
                        toCardanoSimpleScript script
                in Cardano.ReferenceScript aux scriptApi
        datumHash = Cardano.TxOutDatumNone
        addrInEra = tina "toCardanoTxOut: malformed address"
            [ Cardano.AddressInEra
                (Cardano.ShelleyAddressInEra Cardano.ShelleyBasedEraConway)
                    <$> eitherToMaybe
                        (Cardano.deserialiseFromRawBytes AsShelleyAddress addr)

            , Cardano.AddressInEra Cardano.ByronAddressInAnyEra
                <$> eitherToMaybe
                    (Cardano.deserialiseFromRawBytes AsByronAddress addr)
            ]

toCardanoValue :: TokenBundle.TokenBundle -> Cardano.Value
toCardanoValue tb = Cardano.valueFromList $
    (Cardano.AdaAssetId, coinToQuantity coin) :
    map (bimap toCardanoAssetId toQuantity) bundle
  where
    (coin, bundle) = TokenBundle.toFlatList tb
    toCardanoAssetId (TokenBundle.AssetId pid name) =
        Cardano.AssetId (toCardanoPolicyId pid) (toCardanoAssetName name)

    toCardanoAssetName (W.UnsafeTokenName name) =
        just "toCardanoValue" "TokenName"
        [ eitherToMaybe
            $ Cardano.deserialiseFromRawBytes Cardano.AsAssetName name
        ]

    coinToQuantity = fromIntegral . W.unCoin
    toQuantity = fromIntegral . W.unTokenQuantity

toCardanoPolicyId :: W.TokenPolicyId -> Cardano.PolicyId
toCardanoPolicyId (W.UnsafeTokenPolicyId (W.Hash pid)) =
    just "toCardanoPolicyId" "PolicyId"
    [eitherToMaybe $ Cardano.deserialiseFromRawBytes Cardano.AsPolicyId pid]

toCardanoSimpleScript
    :: Script KeyHash
    -> Cardano.SimpleScript
toCardanoSimpleScript = \case
    RequireSignatureOf (KeyHash _ keyhash) ->
        case eitherToMaybe $ Cardano.deserialiseFromRawBytes
            (Cardano.AsHash Cardano.AsPaymentKey) keyhash of
                Just payKeyHash -> Cardano.RequireSignature payKeyHash
                Nothing -> error "Hash key not valid"
    RequireAllOf contents ->
        Cardano.RequireAllOf $ map toCardanoSimpleScript contents
    RequireAnyOf contents ->
        Cardano.RequireAnyOf $ map toCardanoSimpleScript contents
    RequireSomeOf num contents ->
        Cardano.RequireMOf (fromIntegral num) $
            map toCardanoSimpleScript contents
    ActiveFromSlot slot ->
        Cardano.RequireTimeAfter
        (O.SlotNo $ fromIntegral slot)
    ActiveUntilSlot slot ->
        Cardano.RequireTimeBefore
        (O.SlotNo $ fromIntegral slot)

fromCardanoSimpleScript
    :: Cardano.SimpleScript
    -> Script KeyHash
fromCardanoSimpleScript = \case
    Cardano.RequireSignature (Cardano.PaymentKeyHash (Ledger.KeyHash h)) ->
        let payload = hashToBytes h
        in RequireSignatureOf (KeyHash Policy payload)
    Cardano.RequireAllOf contents ->
        RequireAllOf $ map fromCardanoSimpleScript contents
    Cardano.RequireAnyOf contents ->
        RequireAnyOf $ map fromCardanoSimpleScript contents
    Cardano.RequireMOf num contents ->
         RequireSomeOf (fromIntegral num) $
            map fromCardanoSimpleScript contents
    Cardano.RequireTimeAfter (O.SlotNo s) ->
         ActiveFromSlot $ fromIntegral s
    Cardano.RequireTimeBefore (O.SlotNo s) ->
        ActiveUntilSlot $ fromIntegral s

toCardanoSimpleScriptV1
    :: Script KeyHash
    -> Cardano.SimpleScript
toCardanoSimpleScriptV1 = \case
    RequireSignatureOf (KeyHash _ keyhash) ->
        case eitherToMaybe $ Cardano.deserialiseFromRawBytes
            (Cardano.AsHash Cardano.AsPaymentKey) keyhash of
                Just payKeyHash -> Cardano.RequireSignature payKeyHash
                Nothing -> error "Hash key not valid"
    RequireAllOf contents ->
        Cardano.RequireAllOf $ map toCardanoSimpleScriptV1 contents
    RequireAnyOf contents ->
        Cardano.RequireAnyOf $ map toCardanoSimpleScriptV1 contents
    RequireSomeOf num contents ->
        Cardano.RequireMOf (fromIntegral num) $
            map toCardanoSimpleScriptV1 contents
    _ -> error "timelocks not available in SimpleScriptV1"

just :: Builder -> Builder -> [Maybe a] -> a
just t1 t2 = tina (t1+|": unable to deserialise "+|t2)

toLedgerStakeCredential
    :: (Crypto.HashAlgorithm (SL.ADDRHASH crypto))
    => W.RewardAccount
    -> SL.StakeCredential crypto
toLedgerStakeCredential = \case
    W.FromKeyHash bs ->
          SL.KeyHashObj
        . SL.KeyHash
        . unsafeHashFromBytes
        $ bs
    W.FromScriptHash bs ->
          SL.ScriptHashObj
        . SL.ScriptHash
        . unsafeHashFromBytes
        $ bs

unsafeHashFromBytes :: Crypto.HashAlgorithm h => ByteString -> Hash h a
unsafeHashFromBytes =
    fromMaybe (error "unsafeHashFromBytes: wrong length")
    . Crypto.hashFromBytes

toStakeKeyDeregCert :: Either XPub (Script KeyHash) -> Cardano.Certificate
toStakeKeyDeregCert = \case
    Left xpub ->
        Cardano.makeStakeAddressDeregistrationCertificate
        . Cardano.StakeCredentialByKey
        . Cardano.StakeKeyHash
        . SL.KeyHash
        . UnsafeHash
        . toShort
        . blake2b224
        $ xpubPublicKey xpub
    Right script ->
        Cardano.makeStakeAddressDeregistrationCertificate
        . Cardano.StakeCredentialByScript
        . Cardano.hashScript
        . Cardano.SimpleScript
        $ toCardanoSimpleScript script

toStakeKeyRegCert :: Either XPub (Script KeyHash) -> Cardano.Certificate
toStakeKeyRegCert cred = case cred of
    Left xpub ->
        Cardano.makeStakeAddressRegistrationCertificate
        . Cardano.StakeCredentialByKey
        . Cardano.StakeKeyHash
        . SL.KeyHash
        . UnsafeHash
        . toShort
        . blake2b224
        $ xpubPublicKey xpub
    Right script ->
        Cardano.makeStakeAddressRegistrationCertificate
        . Cardano.StakeCredentialByScript
        . Cardano.hashScript
        . Cardano.SimpleScript
        $ toCardanoSimpleScript script

toStakePoolDlgCert :: Either XPub (Script KeyHash) -> PoolId -> Cardano.Certificate
toStakePoolDlgCert cred (PoolId pid) = case cred of
    Left xpub ->
        Cardano.makeStakeAddressPoolDelegationCertificate
        (Cardano.StakeCredentialByKey $ Cardano.StakeKeyHash (toKeyHash xpub))
        (Cardano.StakePoolKeyHash pool)
    Right script ->
        Cardano.makeStakeAddressPoolDelegationCertificate
        (Cardano.StakeCredentialByScript
        . Cardano.hashScript
        . Cardano.SimpleScript
        $ toCardanoSimpleScript script)
        (Cardano.StakePoolKeyHash pool)
  where
    toKeyHash = SL.KeyHash . UnsafeHash . toShort . blake2b224 . xpubPublicKey
    pool = SL.KeyHash $ UnsafeHash $ toShort pid

-- | Extract a stake reference / `RewardAccount` from an address, if it exists.
--
-- Note that this returns `Nothing` for pointer addresses, not just enterprise
-- addresses.
rewardAccountFromAddress :: W.Address -> Maybe W.RewardAccount
rewardAccountFromAddress (W.Address bytes) = refToAccount . ref =<< parseAddr bytes
  where
    parseAddr :: ByteString -> Maybe (Cardano.Address Cardano.ShelleyAddr)
    parseAddr = eitherToMaybe . Cardano.deserialiseFromRawBytes AsShelleyAddress

    ref :: Cardano.Address Cardano.ShelleyAddr -> SL.StakeReference StandardCrypto
    ref (Cardano.ShelleyAddress _n _paymentKey stakeRef) = stakeRef

    refToAccount :: SL.StakeReference StandardCrypto -> Maybe W.RewardAccount
    refToAccount (SL.StakeRefBase cred) = Just $ fromStakeCredential cred
    refToAccount (SL.StakeRefPtr _) = Nothing
    refToAccount SL.StakeRefNull = Nothing

-- | Converts 'SealedTx' to something that can be submitted with the
-- 'Cardano.Api' local tx submission client.
unsealShelleyTx
    :: AnyCardanoEra
    -- ^ Preferred latest era (see 'ideallyNoLaterThan')
    -> W.SealedTx
    -> TxInMode CardanoMode
unsealShelleyTx era wtx = case W.cardanoTxIdeallyNoLaterThan era wtx of
    Cardano.InAnyCardanoEra ByronEra tx ->
        TxInMode tx ByronEraInCardanoMode
    Cardano.InAnyCardanoEra ShelleyEra tx ->
        TxInMode tx ShelleyEraInCardanoMode
    Cardano.InAnyCardanoEra AllegraEra tx ->
        TxInMode tx AllegraEraInCardanoMode
    Cardano.InAnyCardanoEra MaryEra tx ->
        TxInMode tx MaryEraInCardanoMode
    Cardano.InAnyCardanoEra AlonzoEra tx ->
        TxInMode tx AlonzoEraInCardanoMode
    Cardano.InAnyCardanoEra BabbageEra tx ->
        TxInMode tx BabbageEraInCardanoMode
    Cardano.InAnyCardanoEra ConwayEra tx ->
        TxInMode tx ConwayEraInCardanoMode

-- | Converts a 'ShelleyBasedEra' to the broader 'CardanoEra'.
shelleyBasedToCardanoEra :: ShelleyBasedEra era -> CardanoEra era
shelleyBasedToCardanoEra = \case
    Cardano.ShelleyBasedEraShelley -> ShelleyEra
    Cardano.ShelleyBasedEraAllegra -> AllegraEra
    Cardano.ShelleyBasedEraMary    -> MaryEra
    Cardano.ShelleyBasedEraAlonzo  -> AlonzoEra
    Cardano.ShelleyBasedEraBabbage -> BabbageEra
    Cardano.ShelleyBasedEraConway  -> ConwayEra

-- | An existential type like 'AnyCardanoEra', but for 'ShelleyBasedEra'.
data AnyShelleyBasedEra where
     AnyShelleyBasedEra :: IsShelleyBasedEra era -- Provide class constraint
                        => ShelleyBasedEra era   -- and explicit value.
                        -> AnyShelleyBasedEra    -- and that's it.

instance Show AnyShelleyBasedEra where
    show (AnyShelleyBasedEra era) = "AnyShelleyBasedEra " ++ show era

anyShelleyBasedEra :: InAnyShelleyBasedEra (Const ()) -> AnyShelleyBasedEra
anyShelleyBasedEra (InAnyShelleyBasedEra era _) = AnyShelleyBasedEra era

shelleyToCardanoEra :: AnyShelleyBasedEra -> AnyCardanoEra
shelleyToCardanoEra (AnyShelleyBasedEra era) =
    AnyCardanoEra (shelleyBasedToCardanoEra era)

getShelleyBasedEra :: AnyCardanoEra -> Maybe AnyShelleyBasedEra
getShelleyBasedEra (AnyCardanoEra e) = case cardanoEraStyle e of
    LegacyByronEra -> Nothing
    ShelleyBasedEra era -> Just
        (anyShelleyBasedEra (InAnyShelleyBasedEra era (Const ())))

instance (forall era. IsCardanoEra era => Show (thing era)) =>
    Show (InAnyCardanoEra thing) where
    show (InAnyCardanoEra era thing) =
        "InAnyCardanoEra " ++ show era ++ " (" ++ show thing ++ ")"

instance (forall era. IsCardanoEra era => Eq (thing era)) =>
    Eq (InAnyCardanoEra thing) where
    InAnyCardanoEra e1 a == InAnyCardanoEra e2 b = case testEquality e1 e2 of
        Just Refl -> a == b
        Nothing -> False


--------------------------------------------------------------------------------
-- Unsafe conversions
--------------------------------------------------------------------------------

-- | Extracts a 'Coin' value from a 'Cardano.Lovelace' value.
--
-- Fails with a run-time error if the value is negative.
--
unsafeLovelaceToWalletCoin :: HasCallStack => Cardano.Lovelace -> W.Coin
unsafeLovelaceToWalletCoin (Cardano.Lovelace v) =
  case intCastMaybe @Integer @Natural v of
      Nothing -> error $ unwords
          [ "unsafeLovelaceToWalletCoin:"
          , "encountered negative value:"
          , show v
          ]
      Just lovelaceNonNegative ->
          W.Coin lovelaceNonNegative

-- | Extracts a 'Cardano.Lovelace' value from a 'Cardano.Value'.
--
-- Fails with a run-time error if the 'Cardano.Value' contains any non-ada
-- assets.
--
unsafeValueToLovelace :: HasCallStack => Cardano.Value -> Cardano.Lovelace
unsafeValueToLovelace v =
    case Cardano.valueToLovelace v of
        Nothing -> error $ unwords
            [ "unsafeValueToLovelace:"
            , "encountered value with non-ada assets:"
            , show v
            ]
        Just lovelace -> lovelace

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

-- Compact representation of connection id for log messages.
instance Buildable addr => Buildable (ConnectionId addr) where
   build (ConnectionId a b) = "conn:" <> build a <> ":" <> build b

instance Buildable LocalAddress where
    build (LocalAddress p) = build p

{-------------------------------------------------------------------------------
                                 Utilities
-------------------------------------------------------------------------------}

-- Inverts a value in the unit interval [0, 1].
--
-- Examples:
--
-- >>> invertUnitInterval interval0 == interval1
-- >>> invertUnitInterval interval1 == interval0
--
-- Satisfies the following properties:
--
-- >>> invertUnitInterval . invertUnitInterval == id
-- >>> intervalValue (invertUnitInterval i) + intervalValue i == 1
--
invertUnitInterval :: HasCallStack => SL.UnitInterval -> SL.UnitInterval
invertUnitInterval = unsafeBoundRational . (1 - ) . SL.unboundRational
  where
    unsafeBoundRational :: Rational -> SL.UnitInterval
    unsafeBoundRational = tina "invertUnitInterval: the impossible happened"
        . pure . SL.boundRational

interval1 :: SL.UnitInterval
interval1 = maxBound

interval0 :: SL.UnitInterval
interval0 = minBound
