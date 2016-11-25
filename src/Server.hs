
module Server where

import           Conf 
import           Profile
import           WithSession
import           OAuth
import           Synchro

import           WebPage
import           Protolude      
import           Servant.API
import           Servant.Server
import           Data.UUID
import           Data.Aeson
import           DB
import           Servant.HTML.Lucid
import           Control.Lens

type KindaMightWork_API = WithSession "kmw_session" 
                        :> (     GoogleCallback
                           :<|>  TrelloCallback
                           :<|>  WuListCallback
                           :<|>  LogOut
                           :<|>  RemoveTrello
                           :<|>  RemoveWunder
                           :<|>  SyncTrelloWunder
                           :<|>  IndexPage
                           )     


-- TODO: use redirects for post
-- TODO: use redirects for Oauth loggin
-- TODO: pages should indicate the opration result
type GoogleCallback     = "go-clb" :> QueryParam "code"       Text
                                   :> Get        [HTML,JSON]  WebPage

type TrelloCallback     = "tr-clb" :> QueryParam "code"       Text
                                   :> Get        [HTML,JSON]  WebPage

type WuListCallback     = "wl-clb" :> QueryParam "code"       Text
                                   :> Get        [HTML,JSON]  WebPage

type LogOut             = "go-out" :> Post       [HTML,JSON]  WebPage

type RemoveTrello       = "tr-out" :> Capture    "csfr-token" Session
                                   :> Post       [HTML,JSON]  WebPage

type RemoveWunder       = "wl-out" :> Capture    "csfr-token" Session
                                   :> Post       [HTML,JSON]  WebPage

type SyncTrelloWunder   = "sync"   :> Capture    "csfr-token" Session
                                   :> Post       [HTML,JSON]  WebPage

type IndexPage          =             Get        [HTML,JSON]  WebPage   
---------------------------------------------------------------------------

api :: Proxy (KindaMightWork_API)
api = Proxy


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

    generatePage     = return . webPage session (creds,secret)
    

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




checkTokenCSFR :: Session -> PrivCred -> UUID -> ExceptT ServantErr IO () 
checkTokenCSFR = undefined













