


module API where

import           WithSession
import           Servant
import           Servant.HTML.Lucid
import           Protolude
import           Data.Aeson

---------------------------------------------
-- TODO: Take this imports out
import           Conf
import           TokenCSFR
import           OAuth
import           Profile
import           Control.Lens
---------------------------------------------
{-
-- TODO here:
   -- without session

   Raw_API home = (     GoogleCallback   home
                  :<|>  TrelloCallback   home
                  :<|>  WuListCallback   home
                  :<|>  LogOut           home
                  :<|>  RemoveTrello     home
                  :<|>  RemoveWunder     home
                  :<|>  SyncTrelloWunder home
                  )

-- TODO on the server file:

   Website = WithSession "kmw_session"
           :> (    IndexPage
              :<|> Raw_API IndexPage
              )
-}


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
                                   --  :> Get Redirect home

type TrelloCallback     = "tr-clb" :> QueryParam "code"       Text
                                   :> Get        [HTML,JSON]  WebPage
                                   --  :> Get Redirect home

type WuListCallback     = "wl-clb" :> QueryParam "code"       Text
                                   :> Get        [HTML,JSON]  WebPage
                                   --  :> Get Redirect home

type LogOut             = "go-out" :> Post       [HTML,JSON]  WebPage
                                   -- :> type Redirect home

type RemoveTrello       = "tr-out" :> Capture    "csfr-token" TokenCSFR
                                   :> Post       [HTML,JSON]  WebPage
                                   -- :> Post Redirect home

type RemoveWunder       = "wl-out" :> Capture    "csfr-token" TokenCSFR
                                   :> Post       [HTML,JSON]  WebPage
                                   -- :> Post Redirect home

type SyncTrelloWunder   = "sync"   :> Capture    "csfr-token" TokenCSFR
                                   :> Post       [HTML,JSON]  WebPage
                                   -- :> Post Redirect home

type IndexPage          =             Get        [HTML,JSON]  WebPage   
---------------------------------------------------------------------------

data Redirec -- TODO: Implement 


api :: Proxy (KindaMightWork_API)
api = Proxy


_LogOut       :: Proxy LogOut
_LogOut       =  Proxy
_RemoveTrello :: Proxy RemoveTrello
_RemoveTrello =  Proxy
_RemoveWunder :: Proxy RemoveWunder
_RemoveWunder =  Proxy


------------------------------------------------------------------------------
-- TODO: Take this out from here
data WebPage = WebPage
      { verification  :: Text
      , google_conf   :: OAuthCred
      , csrf_token    :: TokenCSFR
      , innerPage     :: Maybe InnerPage
      } deriving(Show,Read,Eq,Ord,Generic,ToJSON)

-- | Page content after user has logged
data InnerPage = InnerPage
      { userName      :: Text
      , trelloAccount :: Bool
      , wuListAccount :: Bool
      , trello_conf   :: OAuthCred
      , wuList_conf   :: OAuthCred
      , userEmail     :: Text
      , outOfSync     :: [Text]
      } deriving(Show,Read,Eq,Ord,Generic,ToJSON)


webPage :: TokenCSFR -> PubCred -> Profile -> WebPage 
webPage token PubCred{..} prof =  WebPage{ google_conf  = _googleCred 
                                         , verification = _siteVerification
                                         , csrf_token   = token
                                         , ..
                                         }
   where
    innerPage = case prof of

                 Just ( GoogleProfile{..}
                      , trello
                      , wuList 
                      )             -> Just $ InnerPage
                                         { userName      = name
                                         , trelloAccount = isJust trello
                                         , wuListAccount = isJust wuList
                                         , trello_conf   = _trelloCred  
                                         , wuList_conf   = _wuListCred 
                                         , userEmail     = email
                                         , outOfSync     = []
                                         }

                 Nothing                  -> Nothing
