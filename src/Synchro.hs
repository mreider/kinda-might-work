



module Synchro where


import           Protolude
import           Conf
import           DB
import           Data.Aeson

data TrelloProfile = TrelloProfile
      {
      } deriving(Show,Read,Eq,Ord,Generic,ToJSON,FromJSON)

data WuListProfile = WuListProfile
      {
      } deriving(Show,Read,Eq,Ord,Generic,ToJSON,FromJSON)


syncTrelloWuList :: (MonadIO io) => Creds -> TrelloProfile -> WuListProfile -> io ()
syncTrelloWuList = undefined


-- use refresh!

trelloSubscription :: (MonadIO io) => Creds -> Subscription -> io (Maybe TrelloProfile)
trelloSubscription = undefined


wuListSubscription :: (MonadIO io) => Creds -> Subscription -> io (Maybe WuListProfile)
wuListSubscription = undefined

