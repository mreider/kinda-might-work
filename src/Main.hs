module Main where

import           DB
import           PageTemplate

import           Protolude
import           Servant.API
import           Servant.Client
import           Servant.HTML.Lucid
import           Servant.Server
import           Network.Wai                  (Application)
import           Network.Wai.Handler.Warp     (run)




type KindaMightWork_API =     GoogleCallback 
                        :<|>  TrelloCallback
                        :<|>  WunderlistCallback
                        :<|>  LogOut
                        :<|>  RemoveTrello
                        :<|>  RemoveWunder
                        :<|>  SyncTrelloWunder
                        :<|>  IndexPage


type GoogleCallback     = "go-clb" :> Get [HTML,JSON] Page
type TrelloCallback     = "tr-clb" :> Get [HTML,JSON] Page
type WunderlistCallback = "wl-clb" :> Get [HTML,JSON] Page
type LogOut             = "go-out" :> Get [HTML,JSON] Page
type RemoveTrello       = "tr-out" :> Get [HTML,JSON] Page
type RemoveWunder       = "wl-out" :> Get [HTML,JSON] Page
type SyncTrelloWunder   = "sync"   :> Get [HTML,JSON] Page

type IndexPage          =             Get [HTML,JSON] Page   




api :: Proxy KindaMightWork_API
api = Proxy


server :: Server KindaMightWork_API
server =    return LandingPage
       :<|> return LandingPage
       :<|> return LandingPage
       :<|> return LandingPage
       :<|> return LandingPage
       :<|> return LandingPage
       :<|> return LandingPage
       :<|> return LandingPage


main :: IO ()
main = do putStrLn $ "Running on port "<> (show port::Text)   
          run port . serve api $ server 
 where
    port = 443










