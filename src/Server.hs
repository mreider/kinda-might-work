
module Server where

import           Conf 
import           API
import           Profile
import           WithSession
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
server :: Creds -> Server KindaMightWork_API
server (creds,secret) session =    callback (set withGoogle) _googleCred
                              :<|> callback (set withTrello) _trelloCred
                              :<|> callback (set withWuList) _wuListCred 
                              :<|> (loggingOut session >> generatePage Nothing)
                              :<|> remove Trello
                              :<|> remove WuList
                              :<|> syncTrelloWunder
                              :<|> indexPage
  where

    callback :: (FromJSON r) => ( Maybe r -> PartialProfile -> PartialProfile)
                             -> (PubCred  -> OAuthCred)
                             -> Maybe Text
                             -> ExceptT ServantErr IO WebPage
    
    callback setter getter = \case
                               Just c  -> do permision <- postCode (getter creds) c
                                             profile   <- updateProfile  session $ partial permision
                                             generatePage profile
                               
                               Nothing -> indexPage
      where
        partial x = setter (Just x) blankProfile

    generatePage     = return . webPage (csfrChallenge (csfrSecret secret) session) creds
    

    remove  service csfr = do checkTokenCSFR session secret csfr
                              removeService  session service
                              indexPage
                               
    
    syncTrelloWunder  csfr = do checkTokenCSFR session secret csfr
                                profile <- updateProfile session blankProfile
                                case profile of
                                  Just ( _
                                       , Just trello 
                                       , Just wuList
                                       )             -> syncTrelloWuList (creds,secret)  
                                                                         trello wuList
                                  Nothing            -> return () -- not logged

                                generatePage profile

    indexPage        = do profile <- updateProfile session blankProfile
                          generatePage profile

    blankProfile     = PartialProfile Nothing Nothing Nothing
    
    updateProfile    = getUpdateProfile (creds,secret)



-- TODO: implement
checkTokenCSFR :: UUID -> PrivCred -> TokenCSFR -> ExceptT ServantErr IO () 
checkTokenCSFR _ _ _ = return ()













