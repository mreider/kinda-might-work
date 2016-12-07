

module OAuth where

import           Conf
import           DB
import           Servant.Server


import           Data.Map
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.UUID
import           Protolude             hiding(to)
import           Network.Wreq
import           Control.Lens
import qualified Data.Text              as T
import qualified Data.ByteString.Base64 as B
import           Control.Monad.Fail

-- TODO: Change the name of this
data GoogleProfile = GoogleProfile
      { email :: Text
      , name  :: Text
      } deriving(Show,Read,Eq,Ord,Generic) 

data TrelloProfile = TrelloProfile
      { _trelloAccessToken :: Text
      } deriving(Show,Read,Eq,Ord,Generic,ToJSON,FromJSON)

data WuListProfile = WuListProfile
      { _wuListAccessToken :: Text
      } deriving(Show,Read,Eq,Ord,Generic)


refresh        :: Creds -> Service -> Subscription -> ExceptT ServantErr IO (Maybe ())
refresh        = error "OAuth.hs at A"

postCode       :: (FromJSON r) => OAuthCred -> Text -> ExceptT ServantErr IO r
postCode  auth code     = do result <- liftIO $ postCode' auth code
                             print "::::::::::::::::::::::::::::::::::::::::::::::::::"
                             print result
                             print "::::::::::::::::::::::::::::::::::::::::::::::::::"
                             -- todo, also use internal error
                             case result ^? _Right . responseBody . to decode of  
                              Just (Just r) -> return   r
                              _             -> throwError err401{errBody = "Not authorized."}


postCode'      :: OAuthCred -> Text ->  IO (Either SomeException (Response LByteString))
postCode' OAuthCred{..} code = try $ post (toSL _verifyURL)
                                      [ "code"          := code
                                      , "redirect_uri"  := _uriRedirect
                                      , "client_id"     := _clientId
                                      , "client_secret" := _secret
                                      , "scope"         := _scope
                                      , "grant_type"    := ("authorization_code" :: Text)
                                      , "access_type"   := ("online"             :: Text) -- we just need the email once and we are done.
                                      ]




instance FromJSON GoogleProfile where
  

  parseJSON obj = let profile = do  t     <- obj ^? key "id_token" . _String
                                    t'    <- rightToMaybe . B.decode . toSL 
                                           . T.takeWhile (/='.') . T.drop 1 . T.dropWhile (/='.')
                                           $ t

                                    name  <- t' ^? key "name"  . _String
                                    email <- t' ^? key "email" . _String
                                    return GoogleProfile{..}

                   in maybe (fail "could not parse google profile" ) return profile



instance FromJSON WuListProfile where
  parseJSON (Object obj) = WuListProfile <$> obj .: "access_token"
  parseJSON x            = fail $ "unexpected json while parsing WuListProfile: " <> show x 




