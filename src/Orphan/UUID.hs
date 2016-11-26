

module Orphan.UUID ( module Data.UUID
                   ) where



import           Control.Lens               hiding ((&),get)
import           Control.Monad.Fail
import           Data.Aeson
import           Data.UUID                  hiding (null)
import           Database.Persist.Sql
import           Database.Persist
import           Protolude
import           Web.HttpApiData
import           Web.PathPieces


------------------------------------------------------------------------------------
--------------- Instances:

instance PersistFieldSql UUID where
  sqlType = const $ SqlOther "uuid"

instance PersistField UUID where
  toPersistValue   = toPersistValueUUID
  fromPersistValue = fromPersistValueUUID

toPersistValueUUID :: UUID -> PersistValue
toPersistValueUUID = PersistDbSpecific . toASCIIBytes

fromPersistValueUUID :: PersistValue -> Either Text UUID
fromPersistValueUUID (PersistDbSpecific bs) = note "Could not parse UUID" 
                                            $ fromASCIIBytes bs

fromPersistValueUUID x                      = Left $ "Invalid value for UUID: " <> show x

-------------------------------------------------------------------------

instance FromHttpApiData UUID where
  parseQueryParam = maybe (Left "could not parse") Right . readMaybe . toSL

instance PathPiece UUID where
  fromPathPiece = readMaybe . toSL
  toPathPiece   = show



instance ToJSON UUID where
    toJSON = toJSON . toText


instance FromJSON UUID where
    -- NOTICE is "fail" (pure, without io-exception) and not "error"
    parseJSON = maybe (fail "could not decode uuid") return . fromText <=< parseJSON


instance ToHttpApiData   UUID where
  toQueryParam    = show








