
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE NoDeriveAnyClass           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module DB where

import           Conf
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Pool
import           Data.Time
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           GHC.Generics
import           Orphan.UUID
import           Protolude


type Session            = UUID


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Email
   Id            Text -- the e-mail itself, Primary Key
   name          Text
   -- we can add more info about the user...like its profile photo...
   deriving      Show Eq Ord Read

Link
   Id            Session
   account       EmailId -- Foerign Key
   deriving      Show Eq Ord Read Generic

Subscription
   kind          Service
   account       EmailId  -- Foerign Key
   token         Text
   refreshToken  Text     -- TODO remove
   expires       UTCTime  -- TODO remove
   Primary       account  kind
   deriving      Show Eq Ord Read Generic
|]

data DB = DB (Pool SqlBackend)
type Transaction a = ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a



transaction :: (MonadIO io) => DB -> Transaction a -> io a
transaction (DB pool) t =  liftIO (runSqlPersistMPool t pool)





connections = 5


getDB :: Text -> IO DB
getDB conn = do pool <- fmap DB . runNoLoggingT  $ createPostgresqlPool (toSL conn) connections

                transaction pool $ runMigration migrateAll
                return pool









