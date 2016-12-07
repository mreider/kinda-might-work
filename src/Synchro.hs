



module Synchro where


import           Conf
import           Data.Aeson
import           Data.Map
import           DB
import           Network.Wreq
import           OAuth
import           Protolude        hiding(get,to, (&))
import           Data.Aeson.Lens
import           Control.Lens

data RawBoard = RawBoard
      { rbName :: Text
      , rbId   :: Text
      }deriving(Show,Eq,Ord,Generic,Read)

data Board = Board [(Text, Map Text Card)] 
           deriving(Show,Eq,Ord,Generic,Read)

data Card  = Card
       { name        :: Text
       , comments    :: [Text]
       , description :: Text
       , tasks       :: [(Bool,Text)]
       } deriving(Show,Eq,Ord,Generic,Read)

--data RawCard = RawCard
--      { id           :: Text
--      , name         :: Text
--      , desc         :: Text
--      , idChecklists :: Text
--      }deriving(Show,Eq,Ord,Generic,Read,FromJSON,ToJSON)

-- Comments...



-- /member/me/boards
-- 
getBoards     :: (MonadIO io) => Creds -> TrelloProfile -> WuListProfile -> io [(Text,Board)]
getBoards Creds{..}  
          (TrelloProfile trelloT)  
          (WuListProfile wulistT) = do boardWuList  <- getFromWunder _wuListCred wulistT 
                                       boardTrello  <- getFromTrello _trelloCred trelloT
                                       return boardTrello

-- TODO: use the actual path...
getFromTrello :: (MonadIO io) => OAuthCred -> Text -> io [(Text,Board)]
getFromTrello OAuthCred{..} token = liftIO
                                  $ do resp <- get . toSL $ "https://api.trello.com/1/members/me/boards?key="
                                                          <> _clientId <>"&token=" <> token
                                       
                                       return $ resp ^.. responseBody 
                                                       . values
                                                       . key "name" 
                                                       . _String
                                                       . to (flip (,) $ Board [])

getFromWunder :: (MonadIO io) => OAuthCred -> Text -> io [(Text,Board)]
getFromWunder  OAuthCred{..} token = do liftIO $ print =<< action
                                        return []
    where
      action = getWith ( defaults & headers <>~ 
      	                    [ ("X-Access-Token", toSL token     )
      	                    , ("X-Client-ID"   , toSL _clientId )
      	                    ]
      	               ) 
                       "https://a.wunderlist.com/api/v1/user"

-- 3aeaa876277ca228218313f1c9f8dbc81617358fb3747177c894331cce2e
-- 

-- curl -H "X-Access-Token: 3aeaa876277ca228218313f1c9f8dbc81617358fb3747177c894331cce2e" -H "X-Client-ID: e9809daeab1b8e680395" https://a.wunderlist.com/api/v1/user

--syncTrelloWuList :: (MonadIO io) => Creds -> TrelloProfile -> WuListProfile -> io ()
syncTrelloWuList = error "error at syncTrelloWuList"


--trelloSubscription :: (MonadIO io) => Creds -> Subscription -> io (Maybe TrelloProfile)
trelloSubscription = error "error at trelloSubscription"


--wuListSubscription :: (MonadIO io) => Creds -> Subscription -> io (Maybe WuListProfile)
wuListSubscription = error "error at wuListSubscription"

