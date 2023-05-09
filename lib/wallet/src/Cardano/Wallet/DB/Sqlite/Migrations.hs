

type Version = Nat


-- | Don't export the constructor
data Migration (to :: Version) (from :: Version) s
    = Migration [s]

mkMigration :: s -> Migration (v+1) v s
mkMigration s = Migration [s]

instance Functor Migration

combine
    :: Migration v2 v1 s
    -> Migration v1 v0 s
    -> Migration v2 v0 s
combine (Migration s10) (Migration s21) = Migration (s21 <> s10)

data VersionedData m = VersionedData
    { getVersion :: m Version
    , setVersion :: Version -> m ()
    , backupData :: m ()
        -- Close the file, make backup, reopen
        -- The monad m has a state, which is the currently open handle
        -- m ~ StateT SqliteContext IO
    }

runOurMigrations
    :: Migration vb va (ReaderT SqliteContext IO) -> FilePath -> IO ()
runOurMigrations migrations filepath = do
    sql <- Sqlite.openConnection
    void $ flip runStateT sql $ …
        runMigrationSteps
            (fmap withCurrentSqliteContext migrations)
            versionedData
  where
    versionedData :: VersionedData (StateT SqliteContext IO)
    versionedData = … filepath …

{-
withCurrentSqliteContext
    :: (SqliteContext -> IO a)
    -> StateT SqliteContext IO
-}

runMigrationSteps
    :: (Monad m, KnownNat va, KnownNat vb)
    => Migration vb va (m ()) -> VersionedData m -> m (Either Err ())
runMigrationSteps (Migration steps) vdata =
    let na = natVal @va
        nb = natVal @vb
    forM_ (zip [na..nb] (reverse steps))
        $ \(version,action) -> runMigrationStep version action vdata
    …

runMigrationStep
    :: (Monad m)
    => Version -> m () -> VersionedData m -> m (Either Err ())
runMigrationStep migrateVersion (Migration steps) VersionedData{..} = do
    actualVersion <- getVersion
    case actualVersion `compare` migrateVersion of
        LT -> pure $ Left "Wrong version: Expected … but got …"
        GT -> pure () -- skip the migration
        EQ -> do
            backup
            sequence_ steps
            setVersion (version + 1) -- wrong.
            pure $ Right ()
