


module Conf where



import           Control.Lens               hiding ((&),get)
import           Data.Aeson
import           Data.UUID
import           Database.Persist.TH
import           Protolude
{-
   Parsing and Marshalling of the configuration and schema data.
-}


data Service   = Trello | WuList
                   deriving (Show,Eq,Ord,Read,Generic,FromJSON,ToJSON)


type Creds         = (PubCred, PrivCred)

data PrivCred      = PrivCred
      { googleSecret      :: Text
      , trelloSecret      :: Text
      , wuListSecret      :: Text
      , csfrSecret        :: Text
      } deriving(Show,Read,Eq,Ord,Generic,ToJSON,FromJSON)

data OAuthCred     = OAuthCred
      { _clientId         :: Text
      , _authUrl          :: Text
      , _uriRedirect      :: Text
      , _scope            :: Text
      }  deriving(Show,Read,Eq,Ord,Generic,ToJSON,FromJSON)

data PubCred       = PubCred
      { _siteVerification :: Text
      , _googleCred       :: OAuthCred
      , _trelloCred       :: OAuthCred
      , _wuListCred       :: OAuthCred
      } deriving(Show,Read,Eq,Ord,Generic,ToJSON,FromJSON)


$(makeLenses ''PubCred       )
$(makeLenses ''OAuthCred     )

derivePersistField "Service"



