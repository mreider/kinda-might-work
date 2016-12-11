



module Synchro where


import           Conf
import           Control.Concurrent.Async
import           Control.Lens               hiding((.=),List)
import           Control.Monad.Fail
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader hiding(ask)
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Map                   hiding(foldr, toList)
import           Data.Time
import           DB
import           Network.Wreq
import           OAuth
import           Protolude                  hiding(get,to, (&))
import qualified Data.Text        as T
import           Control.Concurrent.MVar
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.HTTP.Client        (newManager,Manager)

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

-- | A custom monad to compute symcronization
type SyncM =  ExceptT Shortcut (ReaderT ConfM IO)

-- IO (ConfM -> Either Shortcut a)

-- ReaderT ConfM IO (Either Shortcut a)
-- ConfM -> IO (Either Shortcut a)

-- | A computation shortcut, aka, stop computing the monad because:
data Shortcut = Problem Text -- ^ An unexpect problem occured.

              | RequiredSync -- ^ We are only interested on whether a sync is need, and
                             --   we already found it out.
              deriving(Show,Eq,Read)

-- | The SyncM monad configuration:
data ConfM    = ConfM
      { smTrelloClientId :: Text
      , smWulistClientId :: Text
      , smTrelloToken    :: Text
      , smWulistToken    :: Text
      , smOnlyCheck      :: Bool
      , smPrecedence     :: Text -> Int
      , smLogger         :: Text -> IO ()
      , smManager        :: Manager
      }

toLog :: Text -> SyncM ()
toLog t = do ConfM{..} <- ask
             liftIO $ smLogger t

syncConcurrently :: SyncM a -> SyncM b -> SyncM (a, b) 
syncConcurrently actionA actionB = ExceptT . ReaderT $ syncConcurrently_
  where
    syncConcurrently_ conf = do (a,b) <- concurrently (actionA_ conf) (actionB_ conf)
                                return $ (,) <$> a <*> b
    
    actionA_               = runReaderT $ runExceptT actionA
    actionB_               = runReaderT $ runExceptT actionB

    -- | Drops all wrappers from the monad
    unwrapp :: SyncM b -> ConfM -> IO (Either Shortcut b)
    unwrapp =  runReaderT . runExceptT

    wrap    :: (ConfM -> IO (Either Shortcut b)) -> SyncM b
    wrap    =  ExceptT . ReaderT

mapSyncConcurrently :: (a -> SyncM b) -> [a] -> SyncM [b]
mapSyncConcurrently f = foldr step (return [])
   where
    step x acc = do (x',acc') <- syncConcurrently (f x) acc
                    return (x':acc')

exampleRun :: Bool -> SyncM a -> IO (Either Shortcut a)
exampleRun mode action = do logger  <- newEmptyMVar
                            manager <- newManager tlsManagerSettings
                            let conf = ConfM{ smTrelloClientId = "2e997d720d84f2af062fe52de5ab1ba6"
                                            , smWulistClientId = "e9809daeab1b8e680395"
                                            , smTrelloToken    = "05241c988e4ed3dfda92b35cc8c769b856a0ac956a152e62c9daa518c860657a"
                                            , smWulistToken    = "43796a0dcff8ccff27b8f61a5856002bd169f9f809efed3de286886b1343"
                                            , smOnlyCheck      = mode
                                            , smPrecedence     = preference
                                            , smLogger         = putMVar logger
                                            , smManager        = manager
                                            }
                            
                            forkIO . forever
                                   $ do out <- takeMVar logger
                                        putStrLn out
                            
                            t0 <- liftIO getCurrentTime
                            result <- runReaderT (runExceptT action) conf
                            t1 <- liftIO getCurrentTime
                            print $ diffUTCTime t1 t0 -- mesuring the time it needs to complete
                            return result
   where
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
-- 8 sec more or less...
-- 30 to 40 sec

-- Operations todo:
-- "557ea811c7a27e98211655b1"
getBoard :: Text -> SyncM Board
getBoard boardId = do RawBoard name id <- trello_get $ "/boards/" <> boardId
                      groups           <- trello_get $ "/boards/" <> boardId <> "/lists"
                      ConfM{..}        <- ask
                      return $ Board name id [ Card rcName rcId (smPrecedence rgName) 
                                             | RawGroup{..} <- groups
                                             , RawCard{..}  <- rgCards 
                                             ]

getAllLists :: SyncM [List]
getAllLists = do rLists <- wulist_get "/lists"
                 
                 let mapTask ts = fromList [ (id,t) 
                                           | t@RawTask{..} <- ts
                                           , Just id       <- [fromNameId rtName]
                                           ]
                 
                 mapSyncConcurrently identity
                          [ do tasks     <- wulist_get ("/tasks?list_id=" <> show i)                               
                               return $ List n i r (mapTask tasks)

                          | RawList n i r <- rLists
                          ]

-- Redundant
getSyncLists :: SyncM (Map Text List)
getSyncLists = do lists <- getAllLists
                  return $ fromList [ (id,l) | l <- lists, Just id <- [idFromTittle l]]

-- TODO:
synchList :: Board -> Maybe List -> SyncM ()
synchList b@Board{..} = \case 
                         
                       Nothing       -> do let req     = object [ "title" .= newName 
                                                             ]

                                           newId     <- wulist_post "/lists" req
                                           synchList b $ Just List{ lName     = newName
                                                                  , lId       = newId
                                                                  , lRevision = 0 -- ^we won't need to modify it so any value is ok
                                                                  , lTasks    = fromList []
                                                                  }

                       Just List{..} -> do let req  = object [ "title"    .= newName
                                                             , "revision" .= lRevision
                                                             ]
                                           -- we can not do this part on parallel, cause
                                           -- if we end up updating a task before the list, it would
                                           -- change it revision. 
                                           when (newName /= lName) . void
                                                 $ wulist_patch ("/lists/"<> show lId) req
                                           
                                           -- we can not do this part parallel either
                                           syncPosition  lId . fromList =<< synchAllTasks lId bCards lTasks

  where
    
    synchAllTasks parent cards tasks = mapSyncConcurrently identity
                                                [ synchTask parent c $ lookup (cId c) tasks 
                                                | c <- cards
                                                ]

    newName = toNameId bName bId


synchTask :: Integer -> Card -> Maybe RawTask -> SyncM (Integer,Int)
synchTask parent Card{..} = \case
                               Nothing          -> do let req  = object [ "title"    .= newName
                                                                        , "list_id"  .= parent
                                                                        ]

                                                      newId <- wulist_post "/tasks" req
                                                      return (newId, cPreference)

                               Just RawTask{..} -> do let req  = object [ "title"    .= newName
                                                                        , "revision" .= rtRevision
                                                                        ]

                                                      when (newName /= rtName) . void
                                                          $ wulist_patch ("/tasks/"<> show rtId) req

                                                      return (rtId, cPreference)
  where
    newName = toNameId cName cId


syncPosition :: Integer -> Map Integer Int -> SyncM ()
syncPosition  lId taskPreference = do RawPosition{..} <- wulist_get ("/task_positions/"<> show lId)
                                      
                                      let rpIndexes'  = prefSort rpIndexes
                                          req         = object [ "revision" .= rpRevision
                                                               , "values"   .= rpIndexes'
                                                               ]

                                      when (rpIndexes /= rpIndexes') . void
                                        $ wulist_patch ("/task_positions/" <> show rpId) req
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



synchronize :: Text -> SyncM ()
synchronize boardId = do (board,lists)  <- syncConcurrently (getBoard boardId) getSyncLists
                         synchList board $ lookup (bId board) lists

--------------------------------------------------------------------------------------------

trello_get :: (FromJSON a)=> Text -> SyncM a
trello_get endpoint = do ConfM{..} <- ask
                         describe ("On trello endpoint: "<> endpoint)
                           $ getWith ( defaults & manager .~ Right smManager
                                     ) 
                                     ( toSL $ "https://api.trello.com/1" <> endpoint 
                                            <>"?key="                    <> smTrelloClientId
                                            <>"&token="                  <> smTrelloToken
                                            <>"&cards=open&card_fields=name"
                                     )

wulist_get :: (FromJSON a)=> Text -> SyncM a
wulist_get  endpoint = do ConfM{..} <- ask
                          describe ("On wunderlist endpoint: "<> endpoint)
                            $ getWith ( defaults & headers <>~ 
                                            [ ("X-Access-Token", toSL smWulistToken    )
                                            , ("X-Client-ID"   , toSL smWulistClientId )
                                            ]
                                           & manager .~ Right smManager 
                                      )
                                      ( toSL $ "https://a.wunderlist.com/api/v1" <> endpoint
                                      )

describe ::(FromJSON a) => Text -> IO (Response LByteString) -> SyncM a
describe descr action = do toLog descr
                           result <- liftIO action
                           case eitherDecode $ view responseBody result of
                            Right a   -> return a
                            Left  err -> throwError $ Problem  (descr<>" -> "<>toSL err) 


wulist_post :: (ToJSON a)=> Text -> a -> SyncM Integer
wulist_post = wulist_payload "POST"

wulist_patch :: (ToJSON a)=> Text -> a -> SyncM Integer
wulist_patch = wulist_payload "PATCH"

-- 

wulist_payload :: (ToJSON a)=> [Char] -> Text -> a -> SyncM Integer
wulist_payload method 
               endpoint 
               obj       = do ConfM{..} <- ask
                              toLog ("Posting oon wunderlist:" <>endpoint)
                              
                              when smOnlyCheck
                                 $ throwError  RequiredSync

                              resp <- liftIO $ customPayloadMethodWith method 
                                                  ( defaults & headers <>~ 
                                                        [ ("X-Access-Token", toSL smWulistToken    )
                                                        , ("X-Client-ID"   , toSL smWulistClientId )
                                                        ]
                                                             & manager .~ Right smManager
                                                   )
                                                   ( toSL $ "https://a.wunderlist.com/api/v1" <> endpoint
                                                   )
                                                   ( toJSON obj
                                                   )
                              
                              note (Problem . toSL $"could not decode response after: " <>endpoint)
                                 $ resp ^? responseBody . key "id" . _Integer





--syncTrelloWuList :: (MonadIO io) => Creds -> TrelloProfile -> WuListProfile -> io ()
--syncTrelloWuList = error "error at syncTrelloWuList"


--trelloSubscription :: (MonadIO io) => Creds -> Subscription -> io (Maybe TrelloProfile)
--trelloSubscription = error "error at trelloSubscription"


--wuListSubscription :: (MonadIO io) => Creds -> Subscription -> io (Maybe WuListProfile)
--wuListSubscription = error "error at wuListSubscription"

