{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.DB.Store.Checkpoints.MigrationSpec where

import Prelude

import Cardano.Wallet.DB.LayerSpec
    ( withinCopiedFile )
import Cardano.Wallet.DB.Migration
    ( runMigrations )
import Cardano.Wallet.DB.Sqlite.Migration.New
    ( newMigrationInterface )
import Cardano.Wallet.DB.Store.Checkpoints.Migration
    ( migratePrologue )
import Control.Tracer
    ( nullTracer )
import Data.String.Interpolate
    ( i )
import Database.Persist.Sql
    ( Single (Single) )
import Test.Hspec
    ( Spec, describe, it, shouldBe )

import qualified Data.Text as T
import qualified Database.Persist.Sqlite as Sqlite


spec :: Spec
spec =
    describe "Checkpoints migration"
        $ it "'migrate' db sequential table"
        $ testMigrationStateTable
            "api-bench/sha.a1d5337305630db051fac6da5f8038abf4067068.sqlite"

testMigrationStateTable :: FilePath -> IO ()
testMigrationStateTable dbName = do
    let performMigrations path =
            runMigrations (newMigrationInterface nullTracer)
                path migratePrologue
        testOnCopiedAndMigrated test = fmap snd
            $ withinCopiedFile dbName $ \path _  -> do
                _ <- test path False
                performMigrations path
                test path True
    testOnCopiedAndMigrated testSeqStateColumnExists

    where
        testSeqStateColumnExists path result = do
            row <- Sqlite.runSqlite (T.pack path) $
                    Sqlite.rawSql seqStateTable []
            let isPresent = case row of
                 [Single txt]
                     | "change_addr_mode" `T.isInfixOf` txt -> True
                     | otherwise                            -> False
                 _ -> False
            isPresent `shouldBe` result

        seqStateTable = [i|
            SELECT
                sql
            FROM
                sqlite_master
            WHERE
                type = 'table' AND
                name = 'seq_state';
            |]
