{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Copyright: © 2021–2023 IOHK
-- License: Apache-2.0
--
-- 'Store' implementation for the 'Checkpoints' type,
-- specialized to 'WalletCheckpoint'.
module Cardano.Wallet.DB.Store.Checkpoints.Store
    ( PersistAddressBook (..)
    , blockHeaderFromEntity
    , mkStoreCheckpoints
    )
    where
import Prelude

import Cardano.Address.Derivation
    ( XPub
    )
import Cardano.Address.Script
    ( Cosigner (..)
    , ScriptTemplate (..)
    )
import Cardano.DB.Sqlite
    ( dbChunked
    )
import Cardano.Wallet.Address.Book
    ( AddressBookIso (..)
    , Discoveries (..)
    , Prologue (..)
    , SeqAddressMap (..)
    , SharedAddressMap (..)
    )
import Cardano.Wallet.Address.Derivation
    ( Depth (..)
    , HardDerivation (..)
    , MkKeyFingerprint (..)
    , PaymentAddress (..)
    , PersistPublicKey (..)
    , Role (..)
    , SoftDerivation (..)
    , roleVal
    , unsafePaymentKeyFingerprint
    )
import Cardano.Wallet.Address.Derivation.SharedKey
    ( SharedKey (..)
    )
import Cardano.Wallet.Address.Discovery
    ( PendingIxs
    , pendingIxsFromList
    , pendingIxsToList
    )
import Cardano.Wallet.Address.Discovery.RandomAny
    ( Discoveries (..)
    , Prologue (..)
    )
import Cardano.Wallet.Address.Discovery.SequentialAny
    ( Discoveries (..)
    , Prologue (..)
    )
import Cardano.Wallet.Address.Discovery.Shared
    ( CredentialType (..)
    )
import Cardano.Wallet.Address.Keys.WalletKey
    ( getRawKey
    , liftRawKey
    )
import Cardano.Wallet.Checkpoints
    ( DeltaCheckpoints (..)
    , DeltasCheckpoints
    , loadCheckpoints
    )
import Cardano.Wallet.DB.Errors
    ( ErrBadFormat (..)
    )
import Cardano.Wallet.DB.Sqlite.Schema
    ( Checkpoint (..)
    , CosignerKey (..)
    , EntityField (..)
    , Key (..)
    , RndState (..)
    , RndStateAddress (..)
    , RndStatePendingAddress (..)
    , SeqState (..)
    , SeqStateAddress (..)
    , SeqStatePendingIx (..)
    , SharedState (..)
    , SharedStatePendingIx (..)
    , UTxO (..)
    , UTxOToken (..)
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( BlockId (..)
    , HDPassphrase (..)
    , TxId (..)
    , fromMaybeHash
    , hashOfNoParent
    , toMaybeHash
    )
import Cardano.Wallet.DB.WalletState
    ( WalletCheckpoint (..)
    , getSlot
    )
import Cardano.Wallet.Flavor
    ( KeyFlavorS (..)
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId (..)
    , NetworkDiscriminantCheck
    )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle
    )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..)
    )
import Control.Monad
    ( forM
    , forM_
    , unless
    , void
    , when
    )
import Control.Monad.Trans.Class
    ( lift
    )
import Control.Monad.Trans.Maybe
    ( MaybeT (..)
    )
import Data.Bifunctor
    ( bimap
    , second
    )
import Data.Functor
    ( (<&>)
    )
import Data.Generics.Internal.VL.Lens
    ( (^.)
    )
import Data.Kind
    ( Type
    )
import Data.Map.Strict
    ( Map
    )
import Data.Maybe
    ( fromJust
    , isJust
    , isNothing
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Quantity
    ( Quantity (..)
    )
import Data.Store
    ( UpdateStore
    , mkUpdateStore
    )
import Data.Type.Equality
    ( type (==)
    )
import Data.Typeable
    ( Typeable
    )
import Database.Persist.Sql
    ( Entity (..)
    , SelectOpt (..)
    , deleteWhere
    , insertMany_
    , insert_
    , repsert
    , selectFirst
    , selectList
    , (!=.)
    , (/<-.)
    , (==.)
    , (>.)
    )
import Database.Persist.Sqlite
    ( SqlPersistT
    )
import UnliftIO.Exception
    ( toException
    )

import qualified Cardano.Wallet.Address.Derivation as W
import qualified Cardano.Wallet.Address.Discovery.Random as Rnd
import qualified Cardano.Wallet.Address.Discovery.RandomAny as Rnd
import qualified Cardano.Wallet.Address.Discovery.Sequential as Seq
import qualified Cardano.Wallet.Address.Discovery.SequentialAny as Seq
import qualified Cardano.Wallet.Address.Discovery.Shared as Shared
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Address as W
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn as W
    ( TxIn (TxIn)
    )
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W
    ( TxOut (TxOut)
    )
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W.TxOut
import qualified Cardano.Wallet.Primitive.Types.UTxO as W
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map

{-------------------------------------------------------------------------------
    Store for 'Checkpoints'
-------------------------------------------------------------------------------}

-- | Store for the 'Checkpoints' belonging to a 'WalletState'.
mkStoreCheckpoints
    :: forall s. PersistAddressBook s
    => W.WalletId
    -> UpdateStore (SqlPersistT IO) (DeltasCheckpoints (WalletCheckpoint s))
mkStoreCheckpoints wid =
    mkUpdateStore load write (\_ -> update)
  where
    load = bimap toException loadCheckpoints <$> selectAllCheckpoints wid

    write cps = forM_ (Map.toList $ cps ^. #checkpoints) $ \(pt,cp) ->
            update1 (PutCheckpoint pt cp)

         -- first update in list is the last to be applied!
    update = mapM_ update1 . reverse

    update1 (PutCheckpoint _ state) =
        insertCheckpoint wid state
    update1 (RollbackTo (W.At slot)) =
        deleteWhere [ CheckpointWalletId ==. wid, CheckpointSlot >. slot ]
    update1 (RollbackTo W.Origin) =
        deleteWhere
            [ CheckpointWalletId ==. wid
            , CheckpointParentHash !=. BlockId hashOfNoParent
            ]
    update1 (RestrictTo pts) = do
        let points = W.Origin : pts
        let pseudoSlot W.Origin    = W.SlotNo 0
            pseudoSlot (W.At slot) = slot
        let slots = map pseudoSlot points
        deleteWhere [ CheckpointWalletId ==. wid, CheckpointSlot /<-. slots ]

        -- We may have to delete the checkpoint at SlotNo 0 that is not genesis.
        let slot0 = W.At $ W.SlotNo 0
        unless (slot0 `elem` points) $
            deleteWhere
                [ CheckpointWalletId ==. wid
                , CheckpointSlot ==. W.SlotNo 0
                , CheckpointParentHash !=. BlockId hashOfNoParent
                ]

{-------------------------------------------------------------------------------
    Database operations
-------------------------------------------------------------------------------}
selectAllCheckpoints
    :: forall s. PersistAddressBook s
    => W.WalletId
    -> SqlPersistT IO (Either ErrBadFormat [(W.Slot, WalletCheckpoint s)])
selectAllCheckpoints wid = do
    cpRefs <- fmap entityVal <$> selectList
        [ CheckpointWalletId ==. wid ]
        [ Desc CheckpointSlot ]
    cps <- forM cpRefs $ \cp -> do
        -- FIXME during APD-1043: Internal consistency of this table?
        utxo <- selectUTxO cp
        discoveries <- loadDiscoveries wid (checkpointSlot cp)
        let c = checkpointFromEntity @s cp utxo discoveries
        pure (getSlot c, c)
    pure $ case cps of
        [] -> Left ErrBadFormatCheckpoints
        _  -> Right cps


selectUTxO
    :: Checkpoint
    -> SqlPersistT IO ([UTxO], [UTxOToken])
selectUTxO cp = do
    coins <- fmap entityVal <$>
        selectList
            [ UtxoWalletId ==. checkpointWalletId cp
            , UtxoSlot ==. checkpointSlot cp
            ] []
    tokens <- fmap entityVal <$>
        selectList
            [ UtxoTokenWalletId ==. checkpointWalletId cp
            , UtxoTokenSlot ==. checkpointSlot cp
            ] []
    return (coins, tokens)

insertCheckpoint
    :: forall s. (PersistAddressBook s)
    => W.WalletId
    -> WalletCheckpoint s
    -> SqlPersistT IO ()
insertCheckpoint wid wallet@(WalletCheckpoint currentTip _ discoveries) = do
    let (cp, utxo, utxoTokens) = mkCheckpointEntity wid wallet
    let sl = currentTip ^. #slotNo
    deleteWhere [CheckpointWalletId ==. wid, CheckpointSlot ==. sl]
    insert_ cp
    dbChunked insertMany_ utxo
    dbChunked insertMany_ utxoTokens
    insertDiscoveries wid sl discoveries

{-------------------------------------------------------------------------------
    Database type conversions
-------------------------------------------------------------------------------}
blockHeaderFromEntity :: Checkpoint -> W.BlockHeader
blockHeaderFromEntity cp = W.BlockHeader
    { slotNo = checkpointSlot cp
    , blockHeight = Quantity (checkpointBlockHeight cp)
    , headerHash = getBlockId (checkpointHeaderHash cp)
    , parentHeaderHash = toMaybeHash (checkpointParentHash cp)
    }

mkCheckpointEntity
    :: W.WalletId
    -> WalletCheckpoint s
    -> (Checkpoint, [UTxO], [UTxOToken])
mkCheckpointEntity wid (WalletCheckpoint header wutxo _) =
    (cp, utxo, utxoTokens)
  where
    sl = header ^. #slotNo
    (Quantity bh) = header ^. #blockHeight
    cp = Checkpoint
        { checkpointWalletId = wid
        , checkpointSlot = sl
        , checkpointParentHash = fromMaybeHash (header ^. #parentHeaderHash)
        , checkpointHeaderHash = BlockId (header ^. #headerHash)
        , checkpointBlockHeight = bh
        }
    utxo =
        [ UTxO wid sl (TxId input) ix addr (TokenBundle.getCoin tokens)
        | (W.TxIn input ix, W.TxOut addr tokens) <- utxoMap
        ]
    utxoTokens =
        [ UTxOToken wid sl (TxId input) ix policy token quantity
        | (W.TxIn input ix, W.TxOut {tokens}) <- utxoMap
        , let tokenList = snd (TokenBundle.toFlatList tokens)
        , (AssetId policy token, quantity) <- tokenList
        ]
    utxoMap = Map.assocs (W.unUTxO wutxo)

-- note: TxIn records must already be sorted by order
-- and TxOut records must already by sorted by index.
checkpointFromEntity
    :: Checkpoint
    -> ([UTxO], [UTxOToken])
    -> Discoveries s
    -> WalletCheckpoint s
checkpointFromEntity cp (coins, tokens) =
    WalletCheckpoint header utxo
  where
    header = blockHeaderFromEntity cp

    utxo = W.UTxO $ Map.merge
        (Map.mapMissing (const mkFromCoin)) -- No assets, only coins
        (Map.dropMissing) -- Only assets, impossible.
        (Map.zipWithMatched (const mkFromBoth)) -- Both assets and coins
        (Map.fromList
            [ (W.TxIn input ix, (addr, coin))
            | (UTxO _ _ (TxId input) ix addr coin) <- coins
            ])
        (Map.fromListWith TokenBundle.add
            [ (W.TxIn input ix, mkTokenEntry token)
            | (token@(UTxOToken _ _ (TxId input) ix _ _ _)) <- tokens
            ])

    mkFromCoin :: (W.Address, W.Coin) -> W.TxOut
    mkFromCoin (addr, coin) =
        W.TxOut addr (TokenBundle.fromCoin coin)

    mkFromBoth :: (W.Address, W.Coin) -> TokenBundle -> W.TxOut
    mkFromBoth (addr, coin) bundle =
        W.TxOut addr (TokenBundle.add (TokenBundle.fromCoin coin) bundle)

    mkTokenEntry token = TokenBundle.fromFlatList (W.Coin 0)
        [ ( AssetId (utxoTokenPolicyId token) (utxoTokenName token)
          , utxoTokenQuantity token
          )
        ]

{-------------------------------------------------------------------------------
    AddressBook storage
-------------------------------------------------------------------------------}
-- | Functions for saving / loading the wallet's address book to / from SQLite
class AddressBookIso s => PersistAddressBook s where
    insertPrologue
        :: W.WalletId -> Prologue s -> SqlPersistT IO ()
    insertDiscoveries
        :: W.WalletId -> W.SlotNo -> Discoveries s -> SqlPersistT IO ()

    loadPrologue
        :: W.WalletId -> SqlPersistT IO (Maybe (Prologue s))
    loadDiscoveries
        :: W.WalletId -> W.SlotNo -> SqlPersistT IO (Discoveries s)

{-------------------------------------------------------------------------------
    Sequential address book storage
-------------------------------------------------------------------------------}
-- piggy-back on SeqState existing instance, to simulate the same behavior.
instance
    ( Eq (Seq.SeqState n k)
    , (k == SharedKey) ~ 'False
    , PersistAddressBook (Seq.SeqState n k)
    )
    => PersistAddressBook (Seq.SeqAnyState n k p)
  where
    insertPrologue wid (PS s) = insertPrologue wid s
    insertDiscoveries wid sl (DS s) = insertDiscoveries wid sl s
    loadPrologue wid = fmap PS <$> loadPrologue wid
    loadDiscoveries wid sl = DS <$> loadDiscoveries wid sl

instance
    ( PersistPublicKey (key 'AccountK)
    , PersistPublicKey (key 'CredFromKeyK)
    , PersistPublicKey (key 'PolicyK)
    , MkKeyFingerprint key (Proxy n, key 'CredFromKeyK XPub)
    , PaymentAddress key 'CredFromKeyK
    , AddressCredential key ~ 'CredFromKeyK
    , SoftDerivation key
    , NetworkDiscriminantCheck key
    , HasSNetworkId n
    , (key == SharedKey) ~ 'False
    , Eq (Seq.SeqState n key)
    ) => PersistAddressBook (Seq.SeqState n key) where

    insertPrologue wid (SeqPrologue st) = do
        repsert (SeqStateKey wid) $ SeqState
            { seqStateWalletId = wid
            , seqStateExternalGap = Seq.getGap $ Seq.externalPool st
            , seqStateInternalGap = Seq.getGap $ Seq.internalPool st
            , seqStateAccountXPub = serializeXPub $ Seq.accountXPub st
            , seqStatePolicyXPub = serializeXPub <$> Seq.policyXPub st
            , seqStateRewardXPub = serializeXPub $ Seq.rewardAccountKey st
            , seqStateDerivationPrefix = Seq.derivationPrefix st
            }
        deleteWhere [SeqStatePendingWalletId ==. wid]
        dbChunked
            insertMany_
            (mkSeqStatePendingIxs wid $ Seq.pendingChangeIxs st)

    insertDiscoveries wid sl (SeqDiscoveries ints exts) = do
        insertSeqAddressMap @n wid sl ints
        insertSeqAddressMap @n wid sl exts

    loadPrologue wid = runMaybeT $ do
        st <- MaybeT $ selectFirst [SeqStateWalletId ==. wid] []
        let SeqState _ eGap iGap accountBytes policyBytes rewardBytes prefix =
                entityVal st
        let accountXPub = unsafeDeserializeXPub accountBytes
        let rewardXPub = unsafeDeserializeXPub rewardBytes
        let policyXPub = unsafeDeserializeXPub <$> policyBytes
        let intPool = Seq.newSeqAddressPool @n accountXPub iGap
        let extPool = Seq.newSeqAddressPool @n accountXPub eGap
        pendingChangeIxs <- lift $ selectSeqStatePendingIxs wid
        pure $ SeqPrologue $ Seq.SeqState
            intPool
            extPool
            pendingChangeIxs
            accountXPub
            policyXPub
            rewardXPub
            prefix

    loadDiscoveries wid sl =
        SeqDiscoveries
            <$> selectSeqAddressMap wid sl
            <*> selectSeqAddressMap wid sl

mkSeqStatePendingIxs :: W.WalletId -> PendingIxs 'CredFromKeyK -> [SeqStatePendingIx]
mkSeqStatePendingIxs wid =
    fmap (SeqStatePendingIx wid . W.getIndex) . pendingIxsToList

selectSeqStatePendingIxs :: W.WalletId -> SqlPersistT IO (PendingIxs 'CredFromKeyK)
selectSeqStatePendingIxs wid =
    pendingIxsFromList . fromRes <$> selectList
        [SeqStatePendingWalletId ==. wid]
        [Desc SeqStatePendingIxIndex]
  where
    fromRes = fmap (W.Index . seqStatePendingIxIndex . entityVal)

insertSeqAddressMap
    :: forall n c key
     . ( PaymentAddress key 'CredFromKeyK
       , Typeable c
       , HasSNetworkId n
       )
    => W.WalletId
    -> W.SlotNo
    -> SeqAddressMap c key
    -> SqlPersistT IO ()
insertSeqAddressMap wid sl (SeqAddressMap pool) =
    void
        $ dbChunked
            insertMany_
            [ SeqStateAddress
                wid
                sl
                (liftPaymentAddress @key @'CredFromKeyK (sNetworkId @n) addr)
                (W.getIndex ix)
                (roleVal @c)
                status
            | (addr, (ix, status)) <- Map.toList pool
            ]

-- MkKeyFingerprint key (Proxy n, key 'CredFromKeyK XPub)
selectSeqAddressMap :: forall (c :: Role) key.
    ( MkKeyFingerprint key W.Address
    , Typeable c
    ) => W.WalletId -> W.SlotNo -> SqlPersistT IO (SeqAddressMap c key)
selectSeqAddressMap wid sl = do
    SeqAddressMap . Map.fromList . map (toTriple . entityVal) <$> selectList
        [ SeqStateAddressWalletId ==. wid
        , SeqStateAddressSlot ==. sl
        , SeqStateAddressRole ==. roleVal @c
        ] [Asc SeqStateAddressIndex]
  where
    toTriple x =
        ( unsafePaymentKeyFingerprint @key (seqStateAddressAddress x)
        ,   ( toEnum $ fromIntegral $ seqStateAddressIndex x
            , seqStateAddressStatus x
            )
        )

{-------------------------------------------------------------------------------
    Shared key address book storage
-------------------------------------------------------------------------------}
instance
    ( PersistPublicKey (key 'AccountK)
    , Shared.SupportsDiscovery n key
    , key ~ SharedKey
    ) => PersistAddressBook (Shared.SharedState n key) where

    insertPrologue wid (SharedPrologue st) = do
        let Shared.SharedState prefix accXPub pTemplate dTemplateM rewardAcctM
                gap readiness = st
        insertSharedState prefix accXPub gap pTemplate dTemplateM rewardAcctM
        insertCosigner (cosigners pTemplate) Payment
        when (isJust dTemplateM) $
            insertCosigner (cosigners $ fromJust dTemplateM) Delegation
        case readiness of
            Shared.Pending {} -> pure ()
            Shared.Active (Shared.SharedAddressPools _ _ pendingIxs) -> do
                deleteWhere [SharedStatePendingWalletId ==. wid]
                dbChunked insertMany_ (mkSharedStatePendingIxs pendingIxs)
      where
        insertSharedState prefix accXPub gap pTemplate dTemplateM rewardAcctM =
            do
                deleteWhere [SharedStateWalletId ==. wid]
                insert_ $ SharedState
                    { sharedStateWalletId = wid
                    , sharedStateAccountXPub = serializeXPub accXPub
                    , sharedStateScriptGap = gap
                    , sharedStatePaymentScript = template pTemplate
                    , sharedStateDelegationScript = template <$> dTemplateM
                    , sharedStateRewardAccount = rewardAcctM
                    , sharedStateDerivationPrefix = prefix
                    }

        insertCosigner cs cred = do
            deleteWhere
                [CosignerKeyWalletId ==. wid, CosignerKeyCredential ==. cred]
            dbChunked insertMany_
                [ CosignerKey wid cred (serializeXPub @(key 'AccountK)
                    $ liftRawKey SharedKeyS xpub) c
                | ((Cosigner c), xpub) <- Map.assocs cs
                ]

        mkSharedStatePendingIxs
            :: PendingIxs 'CredFromScriptK
            -> [SharedStatePendingIx]
        mkSharedStatePendingIxs =
            fmap (SharedStatePendingIx wid . W.getIndex) . pendingIxsToList

    insertDiscoveries wid sl sharedDiscoveries = do
        dbChunked insertMany_
            [ SeqStateAddress wid sl addr ix UtxoExternal status
            | (ix, addr, status) <- map convert $ Map.toList extAddrs
            ]
        dbChunked insertMany_
            [ SeqStateAddress wid sl addr ix UtxoInternal status
            | (ix, addr, status) <- map convert $ Map.toList intAddrs
            ]
      where
        SharedDiscoveries (SharedAddressMap extAddrs) (SharedAddressMap intAddrs) =
            sharedDiscoveries
        convert (addr,(ix,status)) =
            (fromIntegral $ fromEnum ix, Shared.liftPaymentAddress @n addr, status)

    loadPrologue wid = runMaybeT $ do
        st <- MaybeT $ selectFirst [SharedStateWalletId ==. wid] []
        let SharedState _ accountBytes gap pScript dScriptM rewardAcctM prefix =
                entityVal st
        let accXPub = unsafeDeserializeXPub accountBytes
        pCosigners <- lift $ selectCosigners @key wid Payment
        dCosigners <- lift $ selectCosigners @key wid Delegation

        let prepareKeys = fmap $ second $ getRawKey SharedKeyS
            pTemplate =
                ScriptTemplate (Map.fromList $ prepareKeys pCosigners) pScript
            dTemplateM =
                ScriptTemplate (Map.fromList $ prepareKeys dCosigners)
                <$> dScriptM
            mkSharedState =
                Shared.SharedState prefix accXPub pTemplate dTemplateM rewardAcctM gap
        pendingIxs <- lift selectSharedStatePendingIxs
        prologue <- lift $ multisigPoolAbsent wid <&> \case
            True ->  mkSharedState Shared.Pending
            False -> mkSharedState $ Shared.Active $ Shared.SharedAddressPools
                (Shared.newSharedAddressPool @n @'UtxoExternal gap pTemplate dTemplateM)
                (Shared.newSharedAddressPool @n @'UtxoInternal gap pTemplate dTemplateM)
                pendingIxs
        pure $ SharedPrologue prologue
      where
        selectSharedStatePendingIxs :: SqlPersistT IO (PendingIxs 'CredFromScriptK)
        selectSharedStatePendingIxs =
            pendingIxsFromList . fromRes <$> selectList
                [SharedStatePendingWalletId ==. wid]
                [Desc SharedStatePendingIxIndex]
          where
              fromRes = fmap (W.Index . sharedStatePendingIxIndex . entityVal)

    loadDiscoveries wid sl = do
        extAddrMap <- loadAddresses @'UtxoExternal
        intAddrMap <- loadAddresses @'UtxoInternal
        pure $ SharedDiscoveries extAddrMap intAddrMap
      where
        loadAddresses
            :: forall (c :: Role) (k :: Depth -> Type -> Type).
               ( MkKeyFingerprint k W.Address
               , Typeable c )
            => SqlPersistT IO (SharedAddressMap c k)
        loadAddresses = do
            addrs <- map entityVal <$> selectList
                [ SeqStateAddressWalletId ==. wid
                , SeqStateAddressSlot ==. sl
                , SeqStateAddressRole ==. roleVal @c
                ] [Asc SeqStateAddressIndex]
            pure $ SharedAddressMap $ Map.fromList
                [ (fingerprint, (toEnum $ fromIntegral ix, status))
                | SeqStateAddress _ _ addr ix _ status <- addrs
                , Right fingerprint <- [paymentKeyFingerprint addr]
                ]

selectCosigners
    :: forall k. PersistPublicKey (k 'AccountK)
    => W.WalletId
    -> CredentialType
    -> SqlPersistT IO [(Cosigner, k 'AccountK XPub)]
selectCosigners wid cred = do
    fmap (cosignerFromEntity . entityVal) <$> selectList
        [ CosignerKeyWalletId ==. wid
        , CosignerKeyCredential ==. cred
        ] []
 where
   cosignerFromEntity (CosignerKey _ _ key c) =
       (Cosigner c, unsafeDeserializeXPub key)

-- | Check whether we have ever stored checkpoints for a multi-signature pool
--
-- FIXME during APD-1043:
-- Whether the 'SharedState' is 'Pending' or 'Active' should be apparent
-- from the data in the table corresponding to the 'Prologue'.
-- Testing whether the table corresponding to 'Discoveries' is present
-- or absent is a nice idea, but it ultimately complicates the separation
-- between Prologue and Discoveries.
-- Solution: Add a 'Ready' column in the next version of the database format.
multisigPoolAbsent :: W.WalletId -> SqlPersistT IO Bool
multisigPoolAbsent wid =
    isNothing <$> selectFirst
        [ SeqStateAddressWalletId ==. wid
        , SeqStateAddressRole ==. UtxoExternal
        ] []

{-------------------------------------------------------------------------------
    HD Random address book storage
-------------------------------------------------------------------------------}
-- piggy-back on RndState existing instance, to simulate the same behavior.
instance PersistAddressBook (Rnd.RndAnyState n p)
  where
    insertPrologue wid (PR s) = insertPrologue wid s
    insertDiscoveries wid sl (DR s) = insertDiscoveries wid sl s
    loadPrologue wid = fmap PR <$> loadPrologue wid
    loadDiscoveries wid sl = DR <$> loadDiscoveries wid sl

-- | Persisting 'RndState' requires that the wallet root key has already been
-- added to the database with 'putPrivateKey'. Unlike sequential AD, random
-- address discovery requires a root key to recognize addresses.
instance PersistAddressBook (Rnd.RndState n) where
    insertPrologue wid (RndPrologue st) = do
        let ix  = W.getIndex (st ^. #accountIndex)
        let gen = st ^. #gen
        let pwd = st ^. #hdPassphrase
        repsert (RndStateKey wid) (RndState wid ix gen (HDPassphrase pwd))
        insertRndStatePending wid (st ^. #pendingAddresses)

    insertDiscoveries wid sl (RndDiscoveries addresses) = do
        dbChunked insertMany_
            [ RndStateAddress wid sl accIx addrIx addr st
            | ((W.Index accIx, W.Index addrIx), (addr, st))
                <- Map.assocs addresses
            ]

    loadPrologue wid = runMaybeT $ do
        st <- MaybeT $ selectFirst
            [ RndStateWalletId ==. wid
            ] []
        let (RndState _ ix gen (HDPassphrase pwd)) = entityVal st
        pendingAddresses <- lift $ selectRndStatePending wid
        pure $ RndPrologue $ Rnd.RndState
            { hdPassphrase = pwd
            , accountIndex = W.Index ix
            , discoveredAddresses = Map.empty
            , pendingAddresses = pendingAddresses
            , gen = gen
            }

    loadDiscoveries wid sl = do
        addrs <- map (assocFromEntity . entityVal) <$> selectList
            [ RndStateAddressWalletId ==. wid
            , RndStateAddressSlot ==. sl
            ] []
        pure $ RndDiscoveries $ Map.fromList addrs
      where
        assocFromEntity (RndStateAddress _ _ accIx addrIx addr st) =
            ((W.Index accIx, W.Index addrIx), (addr, st))

insertRndStatePending
    :: W.WalletId
    -> Map Rnd.DerivationPath W.Address
    -> SqlPersistT IO ()
insertRndStatePending wid addresses = do
    deleteWhere [RndStatePendingAddressWalletId ==. wid]
    dbChunked insertMany_
        [ RndStatePendingAddress wid accIx addrIx addr
        | ((W.Index accIx, W.Index addrIx), addr) <- Map.assocs addresses
        ]

selectRndStatePending
    :: W.WalletId
    -> SqlPersistT IO (Map Rnd.DerivationPath W.Address)
selectRndStatePending wid = do
    addrs <- fmap entityVal <$> selectList
        [ RndStatePendingAddressWalletId ==. wid
        ] []
    pure $ Map.fromList $ map assocFromEntity addrs
  where
    assocFromEntity (RndStatePendingAddress _ accIx addrIx addr) =
        ((W.Index accIx, W.Index addrIx), addr)
