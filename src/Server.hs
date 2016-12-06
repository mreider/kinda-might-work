
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
server db creds session =    callback (set withGoogle) _googleCred
                        :<|> callback (set withTrello) _trelloCred
                        :<|> callback (set withWuList) _wuListCred 
                        :<|> (loggingOut db session >> generatePage Nothing)
                        :<|> remove Trello
                        :<|> remove WuList
                        :<|> syncTrelloWunder
                        :<|> indexPage
                        
  where

    callback :: (FromJSON r) => ( Maybe r -> PartialProfile -> PartialProfile)
                             -> (Creds  -> OAuthCred)
                             -> Maybe Text
                             -> ExceptT ServantErr IO WebPage
    
    callback setter getter = \case
                               Just c  -> do permision <- postCode (getter creds) c
                                             profile   <- updateProfile  session $ partial permision
                                             generatePage profile
                               
                               Nothing -> indexPage
      where
        partial x = setter (Just x) blankProfile

    generatePage     = return . webPage (csfrChallenge (_csfrSecret creds) session) creds
    

    remove  service csfr = do checkTokenCSFR    session creds csfr
                              removeService  db session service
                              indexPage
                               
    
    syncTrelloWunder  csfr = do checkTokenCSFR session creds csfr
                                profile <- updateProfile session blankProfile
                                case profile of
                                  Just ( _
                                       , Just trello 
                                       , Just wuList
                                       )             -> syncTrelloWuList creds 
                                                                         trello wuList
                                  Nothing            -> return () -- not logged

                                generatePage profile

    indexPage        = do profile <- updateProfile session blankProfile
                          generatePage profile

    blankProfile     = PartialProfile Nothing Nothing Nothing
    
    updateProfile    = getUpdateProfile db creds



-- TODO: implement
checkTokenCSFR :: UUID -> Creds -> TokenCSFR -> ExceptT ServantErr IO () 
checkTokenCSFR _ _ _ = return ()













