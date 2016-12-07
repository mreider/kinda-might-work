



module Profile where

import           DB
import           Conf
import           OAuth

import           Control.Lens       hiding (get)
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Database.Persist
import           Protolude          hiding (get)
import           Synchro
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

type Profile       = Maybe ( GoogleProfile
                           , Maybe TrelloProfile
                           , Maybe WuListProfile
                           , [(Text,Board)]
                           )


getProfile    :: (MonadIO io) => DB ->  Session -> Creds          -> io Profile
getProfile db s  creds = transaction db $ do traverse extractProfiles =<< extractGoogle s
   where
    extractProfiles :: (GoogleProfile,EmailId) -> Transaction (GoogleProfile, Maybe TrelloProfile, Maybe WuListProfile, [(Text,Board)])
    extractProfiles (prof,key) = do mayTrello <- fmap (TrelloProfile . subscriptionToken) <$> get (SubscriptionKey key Trello)
                                    mayWulist <- fmap (WuListProfile . subscriptionToken) <$> get (SubscriptionKey key WuList)
                                    maySync   <- case (mayTrello,mayWulist) of
                                                   (Just trello, Just wulist) -> getBoards creds trello wulist
                                                   _                          -> return []

                                    return (prof,mayTrello,mayWulist,maySync)



syncProfile   :: (MonadIO io) => DB ->  Session -> Creds          -> io ()
syncProfile   = undefined

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







