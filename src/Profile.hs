



module Profile where

import           DB
import           Conf
import           OAuth

import           Control.Lens       hiding (get)
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Database.Persist
import           Protolude          hiding (get)
import           Synchro            -- TODO: do not call that from here
-- TODO: use better names and explain what each module do!
-- Maybe call this module "Account"??
-- use tha neme account somewhere

-- or call this module PersistenceLogic? or similar?

-- TODO: remove this even if it makes a bit less efficient, it is not worth it
-- TODO, make a prism instead of a lens

type PartialProfile = Either GoogleProfile (Either TrelloProfile WuListProfile)

withGoogle :: GoogleProfile -> PartialProfile 
withGoogle = Left

withTrello :: TrelloProfile -> PartialProfile 
withTrello = Right . Left

withWuList :: WuListProfile -> PartialProfile 
withWuList = Right  . Right

-- TODO: remove
-- TODO: do not name profiles but `Authotization`
type Profile       = Maybe ( GoogleProfile
                           , Maybe TrelloProfile
                           , Maybe WuListProfile
                           , [(Text,SyncOptions)]
                           )

type BasicProfile  = Maybe ( GoogleProfile
                           , Maybe TrelloProfile
                           , Maybe WuListProfile
                           )

-- TODO: do not use
getProfile    :: (MonadIO io) => DB ->  Session -> Creds          -> io Profile
getProfile db s  creds = do result <- getBasicProfile db s  creds
                            case result of
                              Just (g, Just t, Just w) -> do bs <- getBoards creds t w
                                                             return $ Just (g, Just t, Just w, bs)
                              
                              Just (g, t     , w)      -> do return $ Just (g, t     , w     , [])

                              Nothing                  -> do return Nothing


getAuthorization :: (MonadIO io) => DB ->  Session -> Creds          -> io (Maybe (TrelloProfile,WuListProfile))
getAuthorization db s creds = do result <- getBasicProfile db s  creds
                                 case result of
                                    Just (g, Just t, Just w) -> return $ Just (t,w)
                                    _                        -> return Nothing 

syncProfile :: (MonadIO io) => DB -> Session -> Creds -> [Text] -> io ()
syncProfile  db s creds boards = do auth <- getAuthorization db s creds
                                    case auth of
                                     Just (t,w) -> syncBoards creds t w boards
                                     Nothing    -> return ()

-- TODO: remove this and getProfile
getBasicProfile :: (MonadIO io) => DB ->  Session -> Creds          -> io BasicProfile
getBasicProfile db s  creds = transaction db $ traverse extractProfiles =<< extractGoogle s
   where
    extractProfiles (prof,key) = do mayTrello <- fmap (TrelloProfile . subscriptionToken) <$> get (SubscriptionKey key Trello)
                                    mayWulist <- fmap (WuListProfile . subscriptionToken) <$> get (SubscriptionKey key WuList)

                                    return (prof,mayTrello,mayWulist)


updateProfile :: (MonadIO io) => DB ->  Session -> PartialProfile -> io ()
updateProfile db s p = transaction db $ case p of

                        Left (GoogleProfile email name) -> do repsert (EmailKey email) (Email name)
                                                              repsert (LinkKey  s)     (Link $ EmailKey email)

                        Right p' -> do result <- extractGoogle s
                                       case result of
                                        Nothing    -> return ()

                                        Just (_,k) -> let serv = either (const Trello)     (const WuList)     p'
                                                          code = either _trelloAccessToken _wuListAccessToken p'
                                                       
                                                       in repsert (SubscriptionKey k serv) $ Subscription serv k code  


extractGoogle  :: Session -> Transaction (Maybe (GoogleProfile,EmailId))
extractGoogle s = runMaybeT
                 $ do Link  kEmail <- MaybeT $ get (LinkKey s)
                      Email name   <- MaybeT $ get kEmail
                      
                      return ( GoogleProfile (unEmailKey kEmail)
                                             name
                             , kEmail
                             )


removeService    :: (MonadIO io) => DB -> Session -> Service -> io ()
removeService db s serv = transaction db
                        $ do result <- get (LinkKey s)
                             case result of
                                Nothing           -> return () -- user not logged...
                                Just (Link email) -> delete (SubscriptionKey email serv)


loggingOut       :: (MonadIO io) => DB -> Session            -> io ()
loggingOut  db   = transaction db . delete . LinkKey







