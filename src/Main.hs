
module Main where

import           Conf 
import           API
import           Profile
import           OAuth
import           Synchro
import           TokenCSFR
import           WebPage

import           Conf                         (_postgresConn)          
import           Control.Lens
import           Data.Aeson
import           Data.UUID
import           DB
import           Network.HTTP.Client        (newManager,Manager)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.Wai                  (Application)
import           Network.Wai.Handler.Warp     (setPort,defaultSettings)
import           Network.Wai.Handler.WarpTLS
import           Protolude                  hiding(to,get,(&))
import           Servant.API
import           Servant.Server
import           Servant.Utils.StaticFiles (serveDirectory)
import           Servant.WithSession

main :: IO ()
main = do raw_conf  <- runExceptT configuration
          case raw_conf of

            Left ServantErr{..} -> putStrLn errBody
            
            Right conf          -> do db     <- getDB $ _postgresConn conf
                                      client <- newManager tlsManagerSettings
                                      runTLS tls setttings
                                          . serve api 
                                          $ server db client :<|> serveDirectory "./static"
 where

    tls       = tlsSettings "conf/app.crt" "conf/app.key"
    setttings = defaultSettings & setPort port
    port      = 443




---------------------------------------------------------------------------
server :: DB -> Manager -> Server KindaMightWork_API
server db client session =    callback  withGoogle _googleCred
                         :<|> callback' withTrello
                         :<|> callback  withWuList _wuListCred 
                         :<|> (loggingOut db session)
                         :<|> remove Trello
                         :<|> remove WuList
                         :<|> syncTrelloWunder
                          
                         :<|> indexPage

  where

    -- TODO: update and get profile different!

    callback' :: (TrelloProfile -> PartialProfile)
              -> Maybe Text
              -> ExceptT ServantErr IO ()
    
    callback' setter = \case
                         Just c  -> do updateProfile db session $ setter (TrelloProfile c)
                                       return ()
                           
                         Nothing -> return ()



    callback :: (FromJSON r,Show r) => ( r -> PartialProfile)
                             -> (Creds  -> OAuthCred)
                             -> Maybe Text
                             -> ExceptT ServantErr IO ()
    
    callback setter getter = \case
                               Just c  -> do creds   <- configuration
                                             token   <- postCode (getter creds) c
                                             updateProfile db session $ setter token

                               
                               Nothing -> return ()


    generatePage creds = return . webPage (csfrChallenge (_csfrSecret creds) session) creds
    

    remove  service csfr = do creds   <- configuration
                              checkTokenCSFR    session creds csfr
                              removeService  db session service
                               
    
    -- TODO: it should parametrize which board to sync..
    syncTrelloWunder  csfr (OurForm boards) = do creds   <- configuration
                                                 checkTokenCSFR session creds csfr
                                                 syncProfile client db session creds boards



    indexPage        = do creds   <- configuration
                          profile <- getProfile client db session creds
                          generatePage creds profile


-- TODO: implement
checkTokenCSFR :: UUID -> Creds -> TokenCSFR -> ExceptT ServantErr IO () 
checkTokenCSFR _ _ _ = return ()



configuration :: ExceptT ServantErr IO Creds
configuration =  do raw <- decode.toSL <$> liftIO (readFile conf_file)
                    maybe problems return raw 
 where
  conf_file  = "conf/public.json"

  problems = throwError err500{errBody = "Could not parse: "<> toSL conf_file}




