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
import Cardano.Wallet.DB.Migration
    ( Migration, mkMigration )
import Cardano.Wallet.DB.Sqlite.Migration.Old
    ( SqlColumnStatus (..), isFieldPresent )
import Cardano.Wallet.DB.Sqlite.Schema
    ( EntityField (..) )
import Control.Monad
    ( void )
import Control.Monad.Reader
    ( asks, liftIO, withReaderT )
import Data.Text
    ( Text )

import qualified Data.Text as T
import qualified Database.Sqlite as Sqlite

migratePrologue
    :: Migration (ReadDBHandle IO) 3 4
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
            (ColumnMissing, ColumnPresent) -> withReaderT dbBackend $ do
                liftIO $ addColumn_ conn True (DBField SharedStateOneChangeAddrMode) defaultValue
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
