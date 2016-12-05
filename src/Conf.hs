
{-# LANGUAGE UndecidableInstances #-}


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


data OAuthCred  = OAuthCred
      { _clientId         :: Text
      , _authDomain       :: Text
      , _verifyURL        :: Text
      , _authPath         :: Text
      , _uriRedirect      :: Text
      , _scope            :: Text
      , _secret           :: Text
      } deriving(Show,Eq,Ord,Read,Generic,FromJSON,ToJSON)

data Creds = Creds
      { _siteVerification :: Text
      , _csfrSecret       :: Text
      , _googleCred       :: OAuthCred
      , _trelloCred       :: OAuthCred
      , _wuListCred       :: OAuthCred
      } deriving(Show,Eq,Ord,Read,Generic,FromJSON,ToJSON)



$(makeLenses ''Creds         )
$(makeLenses ''OAuthCred     )

derivePersistField "Service"



