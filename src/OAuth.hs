

module OAuth where

import           Conf
import           DB
import           Servant.Server (ServantErr)


import           Data.Map
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.UUID
import           Protolude             hiding(to)
import           Network.Wreq
import qualified Web.JWT                    as JWT
import           Control.Lens


-- TODO: Change the name of this
data GoogleProfile = GoogleProfile
      { email :: Text
      , name  :: Text
      } deriving(Show,Read,Eq,Ord,Generic,ToJSON,FromJSON) 



refresh        :: Creds -> Service -> Subscription -> ExceptT ServantErr IO (Maybe ())
refresh        = undefined

postCode       :: (FromJSON r) => OAuthCred -> Text -> ExceptT ServantErr IO r
postCode       = undefined

postCode'      :: OAuthCred -> Text ->  IO (Either SomeException (Response LByteString))
postCode'      = undefined
              --( PubCred{..}
              --, PrivCred{..}
              --) code         = try $ post "https://www.googleapis.com/oauth2/v4/token" 
              --                        [ "code"          := code
              --                        , "redirect_uri"  := google_redirect
              --                        , "client_id"     := google_client_id
              --                        , "client_secret" := google_secret
              --                        , "scope"         := google_scope
              --                        , "grant_type"    := ("authorization_code" :: Text)
              --                        , "access_type"   := ("online"             :: Text) -- we just need the email once and we are done.
              --                        ]



extractProfile :: Either SomeException (Response LByteString) -> Maybe (UUID, GoogleProfile)
extractProfile x =  do valueMap <- x ^? _Right . responseBody 
                                               .  key "id_token"
                                               . _String
                                               .  to JWT.decode 
                                               . _Just 
                                               . to (JWT.unregisteredClaims . JWT.claims) 
                       
                       profile  <- (,) <$> (fromJSON<$>lookup "email" valueMap) 
                                       <*> (fromJSON<$>lookup "name"  valueMap)
                       
                       case profile of
                        (Success a,Success b) -> Just (undefined, GoogleProfile a b)
                        _                     -> Nothing

--checkParseResp :: SessionCred -> Maybe (UUID, a) -> ExceptT ServantErr IO a
--checkParseResp = undefined






