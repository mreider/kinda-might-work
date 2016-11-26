
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE NoDeriveAnyClass           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module DB where

import           Conf
import           Data.Aeson
import           Data.Time
import           Orphan.UUID
import           GHC.Generics
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Protolude
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource

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
   account       EmailId -- Foerign Key
   token         Text
   refreshToken  Text
   expires       UTCTime
   Primary       account  kind
   deriving      Show Eq Ord Read Generic
|]



type Transaction a = ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a

dbConnector = ":memory:"

transaction :: (MonadIO io) => Transaction a -> io a
transaction =  liftIO . runSqlite dbConnector



migrate :: IO ()
migrate = runSqlite dbConnector $ runMigration migrateAll -- Once we are using views, we won't be able to use auto migrations :s
           

