{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.DB.Store.WalletState.Migration
    ( migratePrologue
    ) where

import Prelude

import Cardano.DB.Sqlite
    ( ReadDBHandle )
import Cardano.Wallet.DB.Migration
    ( Migration, mkMigration )

migratePrologue :: Migration (ReadDBHandle IO) 2 3
migratePrologue = mkMigration $ undefined
