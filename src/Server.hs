
module Server where

import           Conf 
import           API
import           Profile
import           Servant.WithSession
import           OAuth
import           Synchro
import           TokenCSFR

import           WebPage
import           Protolude      
import           Servant.API
import           Servant.Server
import           Data.UUID
import           Data.Aeson
import           DB
import           Control.Lens


---------------------------------------------------------------------------
server :: DB -> Creds -> Server KindaMightWork_API
server db creds session =    callback  withGoogle _googleCred
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
                               Just c  -> updateProfile db session . setter =<< postCode (getter creds) c

                               
                               Nothing -> return ()


    generatePage     = return . webPage (csfrChallenge (_csfrSecret creds) session) creds
    

    remove  service csfr = do checkTokenCSFR    session creds csfr
                              removeService  db session service
                               
    
    -- TODO: it should parametrize which board to sync..
    syncTrelloWunder  csfr boards = do checkTokenCSFR session creds csfr
                                       syncProfile db session creds boards



    indexPage        = do profile <- getProfile db session creds
                          generatePage profile


-- TODO: implement
checkTokenCSFR :: UUID -> Creds -> TokenCSFR -> ExceptT ServantErr IO () 
checkTokenCSFR _ _ _ = return ()













