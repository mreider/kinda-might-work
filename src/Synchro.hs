



module Synchro where


import           Conf
import           Control.Lens       hiding((.=),List)
import           Control.Monad.Fail
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Map           hiding(foldr, toList)
import           DB
import           Network.Wreq
import           OAuth
import           Protolude          hiding(get,to, (&))
import qualified Data.Text        as T

data RawBoard = RawBoard
      { rbName :: Text
      , rbId   :: Text
      }deriving(Show,Eq,Ord,Generic,Read)


data RawCard  = RawCard
      { rcName  :: Text
      , rcId    :: Text
      }deriving(Show,Eq,Ord,Generic,Read) 


data Card  = Card
      { cName       :: Text
      , cId         :: Text
      , cPreference :: Int 
      }deriving(Show,Eq,Ord,Generic,Read)

data Board     = Board
      { bName   :: Text
      , bId     :: Text
      , bCards  :: [Card]
      }deriving(Show,Eq,Ord,Generic,Read)

data List      = List
      { lName      :: Text
      , lId        :: Integer
      , lRevision  :: Integer
      , lTasks     :: Map Text RawTask
      }deriving(Show,Eq,Ord,Generic,Read)

data RawList = RawList
      { rlName     :: Text
      , rlId       :: Integer
      , rlRevision :: Integer
      }deriving(Show,Eq,Ord,Generic,Read)

data RawTask = RawTask
      { rtName     :: Text
      , rtId       :: Integer
      , rtRevision :: Integer
      }deriving(Show,Eq,Ord,Generic,Read)

data RawGroup = RawGroup
      { rgName     :: Text
      , rgCards    :: [RawCard]
      }deriving(Show,Eq,Ord,Generic,Read)

-- TODO: fetch all at once?
data RawPosition = RawPosition
     { rpIndexes   :: [Integer]
     , rpRevision  :: Integer
     , rpId        :: Integer
     }deriving(Show,Eq,Ord,Generic,Read)


--data Board = Board [(Text, Map Text Card)] 
--           deriving(Show,Eq,Ord,Generic,Read)

--data Card  = Card
--       { name        :: Text
--       , comments    :: [Text]
--       , description :: Text
--       , tasks       :: [(Bool,Text)]
--       } deriving(Show,Eq,Ord,Generic,Read)

instance FromJSON RawGroup where
  parseJSON obj = maybe (fail $ "Could not parsed: "<> toSL(encode obj)) return
                      $ RawGroup <$> obj ^? key "name"   . _String
                                 <*> obj ^? key "cards"  . to fromJ . _Just
     where
      fromJ x = case fromJSON x of
                   Success y -> Just y
                   _         -> Nothing

instance FromJSON RawPosition where
  parseJSON obj = maybe (fail $ "Could not parsed: "<> toSL(encode obj)) return
                      $ RawPosition <$> obj ^? key "values"   . _JSON
                                    <*> obj ^? key "revision" . _Integer
                                    <*> obj ^? key "id"       . _Integer

instance FromJSON RawBoard where
  parseJSON obj = maybe (fail $ "Could not parsed: "<> toSL(encode obj)) return
                      $ RawBoard <$> obj ^? key "name"    . _String
                                 <*> obj ^? key "id"      . _String

instance FromJSON RawCard where
  parseJSON obj = maybe (fail $ "Could not parsed: "<> toSL(encode obj)) return
                      $ RawCard  <$> obj ^? key "name"    . _String
                                 <*> obj ^? key "id"      . _String
instance FromJSON RawTask where
  parseJSON obj = maybe (fail $ "Could not parsed: "<> toSL(encode obj)) return
                      $ RawTask <$> obj ^? key "title"    . _String
                                <*> obj ^? key "id"       . _Integer
                                <*> obj ^? key "revision" . _Integer

instance FromJSON RawList where
  parseJSON obj = maybe (fail $ "Could not parsed: "<> toSL(encode obj)) return
                      $ RawList <$> obj ^? key "title"    . _String
                                <*> obj ^? key "id"       . _Integer
                                <*> obj ^? key "revision" . _Integer

type ErrIO = ExceptT [Char] IO


groupPreference :: [Text]
groupPreference = [ "to-do"          -- ^ most prefered, first
                  , "doing"
                  , "review"
                  , "done"
                  , "discovered"     -- ^ last
                  ]

preference :: Text -> Int
preference = let n       = length groupPreference
                 prefMap = fromList $ zip groupPreference [0..]
              
              in \label -> fromMaybe n $ lookup label prefMap

{-
Right (Array 
        [ Object (fromList [("subscribed",Bool False)
                           ,("closed",Bool False)
                           ,("pos",Number 16384.0)
                           ,("name",String "Basics")
                           ,("idBoard",String "557ea811c7a27e98211655b1")
                           ,("id",String "557ea811c7a27e98211655b3")
                           ]
                  )
         , Object (fromList [("subscribed",Bool False),("closed",Bool False),("pos",Number 32768.0),("name",String "Intermediate"),("idBoard",String "557ea811c7a27e98211655b1"),("id",String "557ea811c7a27e98211655b4")]),Object (fromList [("subscribed",Bool False),("closed",Bool False),("pos",Number 49152.0),("name",String "Advanced"),("idBoard",String "557ea811c7a27e98211655b1")
                           ,("id",String "557ea811c7a27e98211655b5")])])
-}


--data RawCard = RawCard
--      { id           :: Text
--      , name         :: Text
--      , desc         :: Text
--      , idChecklists :: Text
--      }deriving(Show,Eq,Ord,Generic,Read,FromJSON,ToJSON)
-- Comments...

-- order by: 
--      - trello_list name
--      - trello_last activity


trello_get' :: (FromJSON a) => Text -> ErrIO a
trello_get' = trello_get "2e997d720d84f2af062fe52de5ab1ba6" 
                         "05241c988e4ed3dfda92b35cc8c769b856a0ac956a152e62c9daa518c860657a"

wulist_get' :: (FromJSON a)=> Text -> ErrIO a
wulist_get' = wulist_get "e9809daeab1b8e680395" 
                         "43796a0dcff8ccff27b8f61a5856002bd169f9f809efed3de286886b1343"

wulist_post' :: (ToJSON a)=> Text -> a -> ErrIO Integer
wulist_post' = wulist_post "e9809daeab1b8e680395" 
                         "43796a0dcff8ccff27b8f61a5856002bd169f9f809efed3de286886b1343"
wulist_patch' :: (ToJSON a)=> Text -> a -> ErrIO Integer
wulist_patch' = wulist_patch "e9809daeab1b8e680395" 
                             "43796a0dcff8ccff27b8f61a5856002bd169f9f809efed3de286886b1343"
-- Operations todo:
-- "557ea811c7a27e98211655b1"
getBoard :: Text -> ErrIO Board
getBoard boardId = do RawBoard name id <- trello_get' $ "/boards/" <> boardId
                      groups           <- trello_get' $ "/boards/" <> boardId <> "/lists"
                      return $ Board name id [ Card rcName rcId (preference rgName) 
                                             | RawGroup{..} <- groups
                                             , RawCard{..}  <- rgCards 
                                             ]

getAllLists :: ErrIO [List]
getAllLists = do rLists <- wulist_get' "/lists"
                 
                 let mapTask ts = fromList [ (id,t) 
                                           | t@RawTask{..} <- ts
                                           , Just id       <- [fromNameId rtName]
                                           ]
                 
                 sequence [ do tasks     <- wulist_get' ("/tasks?list_id=" <> show i)                               
                               return $ List n i r (mapTask tasks)

                          | RawList n i r <- rLists
                          ]

-- Redundant
getSyncLists :: ErrIO (Map Text List)
getSyncLists = do lists <- getAllLists
                  return $ fromList [ (id,l) | l <- lists, Just id <- [idFromTittle l]]

-- TODO:
synchList :: Board -> Maybe List -> ErrIO ()
synchList b@Board{..} = \case 
                         
                       Nothing       -> do let req     = object [ "title" .= newName 
                                                             ]

                                           newId     <- wulist_post' "/lists" req
                                           synchList b $ Just List{ lName     = newName
                                                                  , lId       = newId
                                                                  , lRevision = 0 -- ^we won't need to modify it so any value is ok
                                                                  , lTasks    = fromList []
                                                                  }

                       Just List{..} -> do let req  = object [ "title"    .= newName
                                                             , "revision" .= lRevision
                                                             ]

                                           when (newName /= bName) . void
                                                 $ wulist_patch' ("/lists/"<> show lId) req
                                           
                                           syncPosition  lId . fromList =<< synchAllTasks lId bCards lTasks

  where
    
    synchAllTasks parent cards tasks = sequence [ synchTask parent c $ lookup (cId c) tasks 
                                                | c <- cards
                                                ]

    newName = toNameId bName bId


synchTask :: Integer -> Card -> Maybe RawTask -> ErrIO (Integer,Int)
synchTask parent Card{..} = \case
                               Nothing          -> do let req  = object [ "title"    .= newName
                                                                        , "list_id"  .= parent
                                                                        ]

                                                      newId <- wulist_post' "/tasks" req
                                                      return (newId, cPreference)

                               Just RawTask{..} -> do let req  = object [ "title"    .= newName
                                                                        , "revision" .= rtRevision
                                                                        ]

                                                      when (newName /= rtName) . void
                                                          $ wulist_patch' ("/tasks/"<> show rtId) req

                                                      return (rtId, cPreference)
  where
    newName = toNameId cName cId


syncPosition :: Integer -> Map Integer Int -> ErrIO ()
syncPosition  lId taskPreference = do RawPosition{..} <- wulist_get' ("/task_positions/"<> show lId)
                                      
                                      let rpIndexes'  = prefSort rpIndexes
                                          req         = object [ "revision" .= rpRevision
                                                               , "values"   .= rpIndexes'
                                                               ]

                                      when (rpIndexes /= rpIndexes') . void
                                        $ wulist_patch' ("/task_positions/" <> show rpId) req
  where
   prefSort :: [Integer] -> [Integer]
   prefSort old = let (monitored, free) = partitionEithers 
                                          [ case lookup n taskPreference of
                                              Nothing    -> Right n
                                              Just pref  -> Left  (pref,n)
                                          
                                          | n <- old
                                          ]

                   in (snd<$>sort monitored) ++ free


--    syncPosition i tasks    

toNameId   :: Text -> Text -> Text
toNameId name id = name <> "[["<>id<>"]]"
  
fromNameId :: Text -> Maybe Text
fromNameId name 
     | _: id_ : _  <- T.splitOn "[[" name
     , id : _      <- T.splitOn "]]" id_     = Just id 

     | otherwise                             = Nothing 

idFromTittle :: List -> Maybe Text
idFromTittle List{..} = fromNameId lName



synchronize :: Text -> ErrIO ()
synchronize boardId = do board <- getBoard boardId
                         lists  <- getSyncLists
                         synchList board $ lookup (bId board) lists

--------------------------------------------------------------------------------------------

trello_get :: (FromJSON a)=>Text -> Text -> Text -> ErrIO a
trello_get clientId token endpoint = describe ("On trello endpoint: "<> endpoint)
                                    $ get . toSL $ "https://api.trello.com/1" <> endpoint 
                                                 <>"?key="                    <> clientId
                                                 <>"&token="                  <> token
                                                 <>"&cards=open&card_fields=name"


wulist_get :: (FromJSON a)=>Text -> Text -> Text -> ErrIO a
wulist_get  clientId token endpoint = describe ("On wunderlist endpoint: "<> endpoint)
                                    $ getWith ( defaults & headers <>~ 
                                                    [ ("X-Access-Token", toSL token     )
                                                    , ("X-Client-ID"   , toSL clientId )
                                                    ]
                                              )
                                              ( toSL $ "https://a.wunderlist.com/api/v1" <> endpoint
                                              )

describe ::(FromJSON a) => Text -> IO (Response LByteString) -> ErrIO a
describe descr x = do print descr
                      ExceptT $ fmap (first (toSL descr<>) . eitherDecode . view responseBody)
                                x

wulist_post :: (ToJSON a)=>Text -> Text -> Text -> a -> ErrIO Integer
wulist_post = wulist_payload "POST"

wulist_patch :: (ToJSON a)=>Text -> Text -> Text -> a -> ErrIO Integer
wulist_patch = wulist_payload "PATCH"

-- 

wulist_payload :: (ToJSON a)=> [Char] -> Text -> Text -> Text -> a -> ErrIO Integer
wulist_payload method 
               clientId 
               token 
               endpoint 
               obj       = do print ("Posting oon wunderlist:" <>endpoint)
                              resp <- liftIO $ customPayloadMethodWith method 
                                                  ( defaults & headers <>~ 
                                                        [ ("X-Access-Token", toSL token     )
                                                        , ("X-Client-ID"   , toSL clientId )
                                                        ]
                                                   )
                                                   ( toSL $ "https://a.wunderlist.com/api/v1" <> endpoint
                                                   )
                                                   ( toJSON obj
                                                   )
                              ExceptT . return . maybe (Left . toSL $"could not decode response after: " <>endpoint)
                                                       Right
                                               $ resp ^? responseBody . key "id" . _Integer 

{-

-}




-- /member/me/boards
-- 
--getBoards     :: (MonadIO io) => Creds -> TrelloProfile -> WuListProfile -> io [(Text,Board)]
--getBoards Creds{..}  
--          (TrelloProfile trelloT)  
--          (WuListProfile wulistT) = do boardWuList  <- getFromWunder _wuListCred wulistT 
--                                       boardTrello  <- getFromTrello _trelloCred trelloT
--                                       return boardTrello

---- TODO: use the actual path...
--getFromTrello :: (MonadIO io) => OAuthCred -> Text -> io [(Text,Board)]
--getFromTrello OAuthCred{..} token = liftIO
--                                  $ do resp <- get . toSL $ "https://api.trello.com/1/members/me/boards?key="
--                                                          <> _clientId <>"&token=" <> token
                                       
--                                       return $ resp ^.. responseBody 
--                                                       . values
--                                                       . key "name" 
--                                                       . _String
--                                                       . to (flip (,) $ Board [])

--getFromWunder :: (MonadIO io) => OAuthCred -> Text -> io [(Text,Board)]
--getFromWunder  OAuthCred{..} token = do liftIO $ print =<< action
--                                        return []
--    where
--      action = getWith ( defaults & headers <>~ 
--                            [ ("X-Access-Token", toSL token     )
--                            , ("X-Client-ID"   , toSL _clientId )
--                            ]
--                       ) 
--                       "https://a.wunderlist.com/api/v1/user"

-- 3aeaa876277ca228218313f1c9f8dbc81617358fb3747177c894331cce2e
-- 

-- curl -H "X-Access-Token: 3aeaa876277ca228218313f1c9f8dbc81617358fb3747177c894331cce2e" -H "X-Client-ID: e9809daeab1b8e680395" https://a.wunderlist.com/api/v1/user

--syncTrelloWuList :: (MonadIO io) => Creds -> TrelloProfile -> WuListProfile -> io ()
--syncTrelloWuList = error "error at syncTrelloWuList"


--trelloSubscription :: (MonadIO io) => Creds -> Subscription -> io (Maybe TrelloProfile)
--trelloSubscription = error "error at trelloSubscription"


--wuListSubscription :: (MonadIO io) => Creds -> Subscription -> io (Maybe WuListProfile)
--wuListSubscription = error "error at wuListSubscription"

