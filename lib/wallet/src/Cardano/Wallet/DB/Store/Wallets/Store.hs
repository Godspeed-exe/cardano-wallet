{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Implementation of a store for 'TxWalletsHistory'
module Cardano.Wallet.DB.Store.Wallets.Store
  ( mkStoreTxWalletsHistory
  , DeltaTxWalletsHistory (..)
  )
where

import Cardano.Wallet.DB.Store.Meta.Layer
  ( QueryTxMeta (..)
  )
import Cardano.Wallet.DB.Store.Meta.Model
  ( DeltaTxMetaHistory
  , mkTxMetaHistory
  )
import qualified Cardano.Wallet.DB.Store.Meta.Model as TxMetaStore
import Cardano.Wallet.DB.Store.Transactions.Model
  ( DeltaTxSet (..)
  , mkTxSet
  )
import Cardano.Wallet.DB.Store.Wallets.Model
  ( DeltaTxWalletsHistory (..)
  )
import Control.Applicative
  ( liftA2
  )
import Data.Store
  ( Store (..)
  , UpdateStore
  , mkUpdateStore
  )
import Database.Persist.Sql
  ( SqlPersistT
  )
import Prelude

mkStoreTxWalletsHistory
  :: Store (SqlPersistT IO) q DeltaTxSet
  -> Store (SqlPersistT IO) QueryTxMeta DeltaTxMetaHistory
  -> UpdateStore (SqlPersistT IO) DeltaTxWalletsHistory
mkStoreTxWalletsHistory storeTransactions storeMeta =
  let
    load =
      liftA2 (,)
        <$> loadS storeTransactions
        <*> loadS storeMeta
    write (txSet, txMetaHistory) = do
      writeS storeTransactions txSet
      writeS storeMeta txMetaHistory
    update ma delta =
      let
        (mTxSet, mWmetas) = (fst <$> ma, snd <$> ma)
      in
        case delta of
          RollbackTxWalletsHistory slot -> do
            tbd <- case mWmetas of
              Nothing -> queryS storeMeta $ GetAfterSlot slot
              Just metas ->
                pure
                  $ snd
                  $ TxMetaStore.rollbackTxMetaHistory
                    slot
                    metas
            updateS storeMeta (mWmetas)
              $ TxMetaStore.Rollback slot
            updateS storeTransactions mTxSet
              $ DeleteTxs tbd
          ExpandTxWalletsHistory wid cs -> do
            updateS storeTransactions mTxSet
              $ Append
              $ mkTxSet
              $ fst <$> cs
            updateS storeMeta mWmetas
              $ TxMetaStore.Expand
              $ mkTxMetaHistory wid cs
  in
    mkUpdateStore load write update
