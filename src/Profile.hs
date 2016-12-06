



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
data PartialProfile = PartialProfile
      { _withGoogle :: Maybe GoogleProfile
      , _withTrello :: Maybe TrelloProfile
      , _withWuList :: Maybe WuListProfile
      } deriving (Show,Read,Eq)
-- TODO, make a prism instead of a lens

type Profile       = Maybe ( GoogleProfile
                           , Maybe TrelloProfile
                           , Maybe WuListProfile
                           )
$(makeLenses ''PartialProfile)



-- TODO: refactor
-- TODO: to its own module...
-- TODO: better comments...
getUpdateProfile :: (MonadIO io) => DB
                                 -> Creds
                                 -> Session
                                 -> PartialProfile
                                 -> io Profile
getUpdateProfile db creds s p = transaction db $ do 


      mayProf <- case _withGoogle of
        
                Nothing   -> extractGoogle 
        
                Just prof -> do updateGoogle prof
                                return $ Just (prof,EmailKey $ email prof)
      case mayProf of

        Nothing        -> return Nothing
        
        Just (prof, k) -> do mayTrello <- return Nothing
                             
                             mayWuList <- updateService WuListProfile k WuList (_wuListAccessToken<$>_withWuList)
                             
                             return $ Just ( prof
                                           , mayTrello
                                           , mayWuList
                                           )
  where
    
    extractGoogle  :: Transaction (Maybe (GoogleProfile,EmailId))
    extractGoogle = runMaybeT
                   $ do Link  kEmail <- MaybeT $ get (LinkKey s)
                        Email name   <- MaybeT $ get kEmail
                        
                        return ( GoogleProfile (unEmailKey kEmail)
                                               name
                               , kEmail
                               )

    updateService :: (Text -> a) -> EmailId -> Service -> Maybe Text
                    -> Transaction (Maybe a)

    updateService cons k serv may = case may of
                                     Nothing    -> fmap (cons.subscriptionToken) <$> get key
                                     Just tok   -> do repsert key (Subscription serv k tok)
                                                      return $ Just (cons tok)
         where
          key = SubscriptionKey k serv




    updateGoogle   :: GoogleProfile -> Transaction ()
    updateGoogle  (GoogleProfile email name) = do repsert (EmailKey email) (Email name)
                                                  repsert (LinkKey  s)     (Link $ EmailKey email)


    PartialProfile{..} = p

removeService    :: (MonadIO io) => DB -> Session -> Service -> io ()
removeService db s serv = transaction db
                        $ do result <- get (LinkKey s)
                             case result of
                                Nothing           -> return () -- user not logged...
                                Just (Link email) -> delete (SubscriptionKey email serv)


loggingOut       :: (MonadIO io) => DB -> Session            -> io ()
loggingOut  db   = transaction db . delete . LinkKey







