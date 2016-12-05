

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
      { 
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




--Right 
--  ( Response { responseStatus = Status {statusCode = 200, statusMessage = "OK"}
--             , responseVersion = HTTP/1.1
--             , responseHeaders = [ ("Content-Type","application/json; charset=utf-8")
--                                 , ("Date","Mon, 05 Dec 2016 16:32:01 GMT")
--                                 , ("Server","lastbalanserare")
--                                 , ("Set-Cookie","_wl=38L_TLzWQKLqgsmpoX6tjA.d1U62LauMjUEct95otwNTzimE2hHF9sm4irs2l0dm0XN7VlGfaTyTqQsmvMheH98r5VG9QvQ2QmiheIPMQaY3A.1480955521114.7776000000.f5CyB1KMwPGlpb41480Ir_7jBE9W12uz2-FoBHqc8Ak; path=/; expires=Sun, 05 Mar 2017 16:32:02 GMT; secure; httponly")
--                                 , ("Strict-Transport-Security","max-age=31536000")
--                                 , ("X-Xss-Protection","1; mode=block")
--                                 , ("Content-Length","79")
--                                 , ("Connection","keep-alive")
--                                 ]
--             , responseBody = "{\"access_token\":\"0761797b665e46409637483f6c7cbf7978028bcb2546c3c60574c81ad0fa\"}"
--           }
--  )


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




