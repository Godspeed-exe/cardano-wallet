{-# LANGUAGE FlexibleContexts #-}
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
import Data.Text
    ( Text )
import Database.Persist.Sql
    ( Single (Single) )
import Test.Hspec
    ( Spec, describe, it, shouldBe )

import qualified Data.Text as T
import qualified Database.Persist.Sqlite as Sqlite


spec :: Spec
spec =
    describe "Checkpoints migration" $ do
        it "'migrate' db shared table"
            (testMigrationStateTable
            "api-bench/sha.a1d5337305630db051fac6da5f8038abf4067068.sqlite"
            sharedStateTable
            (False, True))
        it "'migrate' db sequential table"
            (testMigrationStateTable
            "api-bench/she.1ceb45b37a94c7022837b5ca14045f11a5927c65.sqlite"
            seqStateTable
            (False, True))
        it "'migrate' db byron table"
            (testMigrationStateTable
            "api-bench/rnd.423b423718660431ebfe9c761cd72e64ee5065ac.sqlite"
            rndStateTable
            (False, False))

testMigrationStateTable :: FilePath -> Text -> (Bool, Bool) -> IO ()
testMigrationStateTable dbName sql (expBefore, expAfter) = do
    let performMigrations path =
            runMigrations (newMigrationInterface nullTracer)
                path migratePrologue
        testOnCopiedAndMigrated test = fmap snd
            $ withinCopiedFile dbName $ \path _  -> do
                _ <- test path expBefore
                performMigrations path
                test path expAfter
    testOnCopiedAndMigrated (testStateColumnExists sql)
  where
        testStateColumnExists sql' path result = do
            row <- Sqlite.runSqlite (T.pack path) $
                    Sqlite.rawSql sql' []
            let isPresent = case row of
                 [Single txt]
                     | "change_addr_mode" `T.isInfixOf` txt -> True
                     | otherwise                            -> False
                 _ -> False
            isPresent `shouldBe` result

seqStateTable :: Text
seqStateTable = [i|
    SELECT
        sql
    FROM
        sqlite_master
    WHERE
        type = 'table' AND
        name = 'seq_state';
    |]

sharedStateTable :: Text
sharedStateTable = [i|
    SELECT
        sql
    FROM
        sqlite_master
    WHERE
        type = 'table' AND
        name = 'shared_state';
    |]

rndStateTable :: Text
rndStateTable = [i|
    SELECT
        sql
    FROM
        sqlite_master
    WHERE
        type = 'table' AND
        name = 'rnd_state';
    |]
