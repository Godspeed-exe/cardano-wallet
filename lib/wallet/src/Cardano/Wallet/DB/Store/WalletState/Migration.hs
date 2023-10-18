{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.DB.Store.WalletState.Migration
    ( migratePrologue
    ) where

import Prelude

import Cardano.DB.Sqlite
    ( DBField (..)
    , ReadDBHandle
    , dbBackend
    , dbConn
    , fieldName
    , fieldType
    , tableName
    )
import Cardano.Wallet.Address.Derivation.SharedKey
    ( SharedKey )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Address.Discovery.Sequential
    ( SeqState )
import Cardano.Wallet.Address.Discovery.Shared
    ( SharedState (..) )
import Cardano.Wallet.DB.Migration
    ( Migration, mkMigration )
import Cardano.Wallet.DB.Sqlite.Migration.Old
    ( SqlColumnStatus (..), isFieldPresent, runSql )
import Cardano.Wallet.DB.Sqlite.Schema
    ( EntityField (..) )
import Cardano.Wallet.DB.Store.Checkpoints.Store
    ( PersistAddressBook )
import Cardano.Wallet.DB.Store.WalletState.Store
    ( mkStoreWallet )
import Cardano.Wallet.DB.WalletState
    ( DeltaWalletState )
import Cardano.Wallet.Flavor
    ( WalletFlavorS (..) )
import Control.Monad
    ( void )
import Control.Monad.Reader
    ( asks, liftIO, withReaderT )
import Data.Store
    ( Store (..), UpdateStore )
import Data.Text
    ( Text )
import Data.Text.Class
    ( fromText )
import Database.Persist.Sqlite
    ( SqlPersistT )
import Database.Persist.Types
    ( PersistValue (..) )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.Text as T
import qualified Database.Sqlite as Sqlite

migratePrologue
    :: PersistAddressBook s
    => Migration (ReadDBHandle IO) 3 4
migratePrologue = mkMigration oneChangeAddrMigration
  where
    oneChangeAddrMigration = do
        let sharedWid = DBField SharedStateWalletId
            seqWid= DBField SeqStateWalletId
            defaultValue = "FALSE"
        conn <- asks dbConn
        r1 <- liftIO $ isFieldPresent conn seqWid
        r2 <- liftIO $ isFieldPresent conn sharedWid
        case (r1, r2) of
            (ColumnPresent, ColumnMissing) -> withReaderT dbBackend $ do
                liftIO $ addColumn_ conn True (DBField SeqStateOneChangeAddrMode) defaultValue
                wid <- liftIO $ getWalletId conn seqWid seqWid
                let store = mkStoreWallet ShelleyWallet wid :: UpdateStore (SqlPersistT IO) (DeltaWalletState s)
                writeS store undefined
            (ColumnMissing, ColumnPresent) -> withReaderT dbBackend $ do
                liftIO $ addColumn_ conn True (DBField SharedStateOneChangeAddrMode) defaultValue
                wid <- liftIO $ getWalletId conn sharedWid sharedWid
                let store = mkStoreWallet SharedWallet wid :: UpdateStore (SqlPersistT IO) (DeltaWalletState s)
                writeS store undefined
            _ ->
                return ()

addColumn_
    :: Sqlite.Connection
    -> Bool
    -> DBField
    -> Text
    -> IO ()
addColumn_ a b c =
    void . addColumn a b c
  where
    addColumn
        :: Sqlite.Connection
        -> Bool
        -> DBField
        -> Text
        -> IO SqlColumnStatus
    addColumn conn notNull field value = do
        isFieldPresent conn field >>= \st -> st <$ case st of
            TableMissing ->
                return ()
            ColumnMissing -> do
                query <- Sqlite.prepare conn $ T.unwords
                    [ "ALTER TABLE", tableName field
                    , "ADD COLUMN", fieldName field
                    , fieldType field, if notNull then "NOT NULL" else ""
                    , "DEFAULT", value
                    , ";"
                    ]
                _ <- Sqlite.step query
                Sqlite.finalize query
            ColumnPresent ->
                return ()

getWalletId
    :: Sqlite.Connection
    -> DBField
    -> DBField
    -> IO W.WalletId
getWalletId conn table column = do
    let qry = T.unwords
            [ "SELECT", fieldName column
            , "FROM", tableName table
            , ";"
            ]
    runSql conn qry >>= \case
        [[PersistText text]] -> do
            case fromText @W.WalletId text of
                Right wid -> pure wid
                Left e -> error $ show e
        _ -> error $ "migration failed for " <> T.unpack (tableName table) <>
             " table when reading " <> T.unpack (fieldName column)
