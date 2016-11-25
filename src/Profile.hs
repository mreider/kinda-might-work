



module Profile where

import           DB
import           Conf
import           OAuth

import           Synchro
import           Protolude          hiding (get)
import           Data.Aeson
import           Database.Persist
import           Control.Lens       hiding (get)
-- TODO: use better names and explain what each module do!
-- Maybe call this module "Account"??
-- use tha neme account somewhere

-- or call this module PersistenceLogic? or similar?

-- TODO: remove this even if it makes a bit less efficient, it is not worth it
data PartialProfile = PartialProfile
      { _withGoogle :: Maybe GoogleProfile
      , _withTrello :: Maybe TrelloProfile
      , _withWuList :: Maybe WuListProfile
      } deriving (Show,Read,Eq)


type Profile       = Maybe ( GoogleProfile
                           , Maybe TrelloProfile
                           , Maybe WuListProfile
                           )
$(makeLenses ''PartialProfile)



-- TODO: refactor
-- TODO: to its own module...
-- TODO: better comments...
getUpdateProfile :: (MonadIO io) => Creds
                                 -> Session
                                 -> PartialProfile
                                 -> io Profile
getUpdateProfile creds s p = transaction $ do 


      mayProf <- case _withGoogle of
        
                Nothing   -> extractGoogle 
        
                Just prof -> do updateGoogle prof
                                return $ Just (prof,EmailKey $ email prof)
      case mayProf of

        Nothing        -> return Nothing
        
        Just (prof, k) -> do mayTrello <- updateService k Trello _withTrello trelloSubscription
                             mayWuList <- updateService k WuList _withWuList wuListSubscription
                             return $ Just ( prof
                                           , mayTrello
                                           , mayWuList
                                           )
  where
    
    extractGoogle  :: io (Maybe (GoogleProfile,EmailId))
    extractGoogle  = undefined
    

    updateService  :: EmailId -> Service -> Maybe a 
                   -> (Creds -> Subscription -> io (Maybe a)) 
                   -> io (Maybe a)
    
    updateService  = undefined

    extractService :: EmailId -> Service -> io (Maybe Subscription)
    extractService = undefined


    updateGoogle   :: GoogleProfile -> io ()
    updateGoogle   = undefined



    PartialProfile{..} = p

removeService    :: (MonadIO io) => Session -> Service -> io ()
removeService s serv = transaction
                     $ do result <- get (LinkKey s)
                          case result of
                            Nothing           -> return () -- user not logged...
                            Just (Link email) -> delete (SubscriptionKey email serv)


loggingOut       :: (MonadIO io) => Session            -> io ()
loggingOut       = transaction . delete . LinkKey







