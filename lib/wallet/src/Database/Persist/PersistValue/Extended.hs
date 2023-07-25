module Database.Persist.PersistValue.Extended
  ( module Database.Persist.PersistValue
  , fromPersistValueFromText
  , fromPersistValueRead
  )
where

import Control.Monad
  ( (>=>)
  )
import Data.Bifunctor
  ( first
  )
import Data.Text
  ( Text
  )
import qualified Data.Text as T
import Data.Text.Class.Extended
  ( FromText
  , fromText'
  )
import Database.Persist
  ( fromPersistValue
  )
import Database.Persist.PersistValue
import Text.Read
  ( readMaybe
  )
import Prelude

-- | 'fromPersistValue' defined in terms of 'fromText'
fromPersistValueFromText :: FromText a => PersistValue -> Either Text a
fromPersistValueFromText = fromPersistValue >=> fromTextWithErr
  where
    fromTextWithErr = first ("not a valid value: " <>) . fromText'

-- | 'fromPersistValue' defined in terms of the 'Read' class
fromPersistValueRead :: Read a => PersistValue -> Either Text a
fromPersistValueRead pv = fromPersistValue pv >>= readWithErr
  where
    readWithErr = toEither . readMaybe . T.unpack
    toEither = maybe (Left $ "not a valid value: " <> T.pack (show pv)) Right
