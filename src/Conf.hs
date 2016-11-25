


module Conf where



import           Control.Lens               hiding ((&),get)
import           Control.Monad.Fail
import           Data.Aeson
import           Data.UUID
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.TH
import           Protolude
import           Web.HttpApiData
import           Web.PathPieces
{-
   Parsing and Marshalling of the configuration and schema data.
-}


data Service   = Trello | WuList
                   deriving (Show,Eq,Ord,Read,Generic,FromJSON,ToJSON)


type Creds         = (PubCred, PrivCred)

data PrivCred      = PrivCred
      { google_secret      :: Text
      , trello_secret      :: Text
      , wuList_secret      :: Text 
      } deriving(Show,Read,Eq,Ord,Generic,ToJSON,FromJSON)

data OAuthCred     = OAuthCred
      { _clientId    :: Text
      , _authUrl     :: Text
      , _uriRedirect :: Text
      , _scope       :: Text
      }  deriving(Show,Read,Eq,Ord,Generic,ToJSON,FromJSON)

data PubCred       = PubCred
      { _siteVerification :: Text
      , _googleCred       :: OAuthCred
      , _trelloCred       :: OAuthCred
      , _wuListCred       :: OAuthCred
      } deriving(Show,Read,Eq,Ord,Generic,ToJSON,FromJSON)

$(makeLenses ''PubCred       )
$(makeLenses ''OAuthCred     )



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
---- We do not need ANYTHING this, but without this instance persist would complain :(.

instance PathPiece UUID where
  fromPathPiece = readMaybe . toSL
  toPathPiece   = show


derivePersistField "Service"



instance ToJSON UUID where
    toJSON = toJSON . toText


instance FromJSON UUID where
    -- NOTICE is "fail" (pure, without io-exception) and not "error"
    parseJSON = maybe (fail "could not decode uuid") return . fromText <=< parseJSON


instance ToHttpApiData   UUID where
  toQueryParam    = show

instance FromHttpApiData UUID where
  parseQueryParam = maybe (Left "could not parse") Right . readMaybe . toSL









