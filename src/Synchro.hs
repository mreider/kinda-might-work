



module Synchro where


import           Conf
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Lens               hiding((.=),List)
import           Control.Monad.Fail
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader hiding(ask)
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Char                  (isSpace)
import           Data.Map                   hiding(foldr, toList)
import           Data.Time
import           DB
import           Network.HTTP.Client        (Manager)
import           Network.Wreq
import           OAuth
import           Protolude                  hiding(get,to, (&))
import qualified Data.Text        as T

data Card  = Card
      { cName       :: Text
      , cId         :: Text
      , cPreference :: Int
      , cGroup      :: Text
      , cDesc       :: Text
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

data SyncOptions = ToRetrieve     Text (Maybe Text)
                 | AlreadySync
                 deriving(Show,Eq,Ord,Generic,Read)

-------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------
getBoards :: (MonadIO io) => Manager -> Creds -> TrelloProfile -> WuListProfile -> io [(Text, SyncOptions)]
getBoards client creds trello wulist  = runSyncM creds trello wulist client True 
                                      $ getBoardsSync


syncBoards :: (MonadIO io) => Manager -> Creds -> TrelloProfile -> WuListProfile -> [Text] -> io ()
syncBoards client  creds trello wulist bIds = do print ("selected to sync:",bIds)
                                                 runSyncM creds trello wulist client False
                                                   . void 
                                                   $ mapSyncConcurrently synchronize bIds


getBoardsSync :: SyncM [(Text, SyncOptions)]
getBoardsSync = do rawBoards <- trello_get "/members/me/boards"
                   mapSyncConcurrently step rawBoards
  where
    step :: RawBoard -> SyncM (Text, SyncOptions)
    step RawBoard{..} = do result <- (Right<$>synchronize rbId) `catchError` (return . Left)

                           let syncOpt = case result of
                                          Left RequiredSync  -> ToRetrieve rbId Nothing
                                          Left (Problem err) -> ToRetrieve rbId (Just err)
                                          Right _            -> AlreadySync

                           return (rbName,syncOpt)


-- TODO: add the global manager.
-- TODO: use safe exception here!
runSyncM :: (MonadIO io) => Creds -> TrelloProfile -> WuListProfile -> Manager -> Bool -> SyncM a -> io a
runSyncM Creds{..} 
         TrelloProfile{..}
         WuListProfile{..}
         manager 
         mode action       = do result <- liftIO rawAction
                                case result of
                                  Right x  -> return x
                                  Left err -> error  $ show err -- use something else!
   where
       rawAction = do logger  <- newEmptyMVar
                      let conf = ConfM{ smTrelloClientId = _clientId _trelloCred
                                      , smWulistClientId = _clientId _wuListCred
                                      , smTrelloToken    = _trelloAccessToken
                                      , smWulistToken    = _wuListAccessToken
                                      , smOnlyCheck      = mode
                                      , smPrecedence     = preference
                                      , smLogger         = void . return -- no logging
                                      , smManager        = manager
                                      }
                      runReaderT (runExceptT action) conf

       preference :: Text -> Int
       preference = let n       = length _groupPrecedence
                        prefMap = fromList $ zip _groupPrecedence [0..]
                    
                     in \label -> fromMaybe n $ lookup label prefMap

-------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------


data RawBoard = RawBoard
      { rbName :: Text
      , rbId   :: Text
      }deriving(Show,Eq,Ord,Generic,Read)


data RawCard  = RawCard
      { rcName  :: Text
      , rcId    :: Text
      , rcDesc  :: Text
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

data RawPosition = RawPosition
     { rpIndexes   :: [Integer]
     , rpRevision  :: Integer
     , rpId        :: Integer
     }deriving(Show,Eq,Ord,Generic,Read)


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
                                 <*> obj ^? key "desc"    . _String
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

-- Operations todo:
-- "557ea811c7a27e98211655b1"
getBoard :: Text -> SyncM Board
getBoard boardId = do RawBoard name id <- trello_get $ "/boards/" <> boardId
                      groups           <- trello_get $ "/boards/" <> boardId <> "/lists"
                      ConfM{..}        <- ask
                      return $ Board name id [ Card rcName rcId (smPrecedence rgName) rgName rcDesc
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
                                           syncPosition  lId =<< synchAllTasks lId bCards lTasks

  where
    
    synchAllTasks parent cards tasks = mapSyncConcurrently identity
                                                [ synchTask parent c $ lookup (cId c) tasks 
                                                | c <- cards
                                                ]

    newName = toNameId bName bId


synchTask :: Integer -> Card -> Maybe RawTask -> SyncM ((Int,Text),Integer)
synchTask parent Card{..} = \case
                               Nothing          -> do newId <- createNewTask 
                                                      toLog $ "new tasks: "<> show newId
                                                      return ((cPreference,cId),newId)

                               Just RawTask{..} -> do let req  = object [ "title"    .= newName
                                                                        , "revision" .= rtRevision
                                                                        ]

                                                      when (newName /= rtName) . void
                                                          $ wulist_patch ("/tasks/"<> show rtId) req

                                                      return ((cPreference,cId),rtId)
  where
    newName = toTaskNameId cGroup cName cId

    getComments :: SyncM [Text]
    getComments = do raws <- trello_get $ "/cards/"<>cId<>"/actions" :: SyncM [Value]
                     return [ comment
                            | r             <- raws
                            , "commentCard" <- r   ^.. key "type" . _String
                            , obj           <- r   ^.. key "data"
                            , comment       <- obj ^.. key "text" . _String
                            ]

    createNewTask :: SyncM Integer 
    createNewTask = do let req = object [ "title"    .= newName
                                        , "list_id"  .= parent
                                        ]

                       (id,comments) <- syncConcurrently (wulist_post "/tasks" req)
                                        getComments         
                       
                       let notes   = if not $ T.all isSpace cDesc
                                      then ("[[DESCRIPTION]]" <> cDesc) : comments
                                      else comments

                           post t  = wulist_post "/task_comments"
                                        $ object [ "task_id" .= id
                                                 , "text"    .= t
                                                 ]
                       
                       -- TODO: we do not need to wait for the result...
                       mapSyncConcurrently post notes

                       return id
-- 
-- NOTICE:
-- Wunderlist is not consistent with its api result, there are read-after-write
-- issues, a.k.a, if something is read after it had recently changed, the read might
-- not observe the change.

syncPosition :: Integer -> [((Int,Text),Integer)] -> SyncM ()
syncPosition  lId taskPreference = do RawPosition{..} <- wulist_get ("/task_positions/"<> show lId)
                                      
                                      let rpIndexes'  = snd<$>sort taskPreference
                                          req         = object [ "revision" .= rpRevision
                                                               , "values"   .= rpIndexes'
                                                               ]

                                      toLog $ show rpIndexes
                                      toLog $ show rpIndexes'
                                      toLog $ show (rpIndexes'==rpIndexes)
                                      when (rpIndexes /= rpIndexes') . void
                                        $ wulist_patch ("/task_positions/" <> show rpId) req


toNameId   :: Text -> Text -> Text
toNameId name id = name <> "[["<>id<>"]]"

toTaskNameId   :: Text -> Text -> Text -> Text
toTaskNameId groupName name taskId = "{{ "<> groupName<> " }}  " 
                                   <> name 
                                   <> "[["<>taskId<>"]]"

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
                                            <>"&cards=open&card_fields=name,desc"
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


