{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- |
--  Copyright: © 2018-2022 IOHK
--  License: Apache-2.0
--
-- Pure model for the transactions ('Tx') and metadata about them ('TxMeta')
-- in a collection of wallets.
module Cardano.Wallet.DB.Store.Wallets.Model
  ( DeltaTxWalletsHistory (..)
  , TxWalletsHistory
  )
where

import Cardano.Wallet.DB.Store.Meta.Model
  ( TxMetaHistory
  , mkTxMetaHistory
  )
import qualified Cardano.Wallet.DB.Store.Meta.Model as TxMetaStore
import Cardano.Wallet.DB.Store.Transactions.Model
  ( TxSet (..)
  , mkTxSet
  )
import qualified Cardano.Wallet.DB.Store.Transactions.Model as TxStore
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Tx as WT
import Data.Delta
  ( Delta (..)
  )
import Fmt
  ( Buildable
  , build
  )
import Prelude

data DeltaTxWalletsHistory
  = ExpandTxWalletsHistory W.WalletId [(WT.Tx, WT.TxMeta)]
  | -- | Roll back a single wallet
    RollbackTxWalletsHistory W.SlotNo
  deriving (Show, Eq)

instance Buildable DeltaTxWalletsHistory where
  build = build . show

type TxWalletsHistory =
  (TxSet, TxMetaHistory)

instance Delta DeltaTxWalletsHistory where
  type Base DeltaTxWalletsHistory = TxWalletsHistory
  apply (ExpandTxWalletsHistory wid cs) (txh, mtxmh) =
    ( apply (TxStore.Append $ mkTxSet $ fst <$> cs) txh
    , apply (TxMetaStore.Expand $ mkTxMetaHistory wid cs) mtxmh
    )
  apply (RollbackTxWalletsHistory slot) (txset, mtxmh) =
    -- Roll back all wallets to a given slot (number)
    -- and garbage collect transactions that no longer
    -- have a 'TxMeta' associated with them.
    let
      (metas', toBeDeletedTxSet) =
        TxMetaStore.rollbackTxMetaHistory slot mtxmh
      txSet' = apply (TxStore.DeleteTxs toBeDeletedTxSet) txset
    in
      (txSet', metas')
