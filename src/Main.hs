module Main where

import           DB
import           PageTemplate

import           Protolude
import           Data.Aeson
import           Servant.API
import           Servant.Client
import           Servant.HTML.Lucid
import           Servant.Server
import           Network.Wai                  (Application)
import           Network.Wai.Handler.Warp     (run,setPort,defaultSettings)
import           Network.Wai.Handler.WarpTLS



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


{-
<html>
  <head>
    <meta name="google-site-verification" content="auj20bE8W7nWGXthtzXrkuv4f4u6-VXZ9fQBNLefIlo" />
    <title> My title </title>
  </head> 
  <body>
    page contents
  </body>
</html>

-}


api :: Proxy KindaMightWork_API
api = Proxy


server :: PubCred -> PrivCred -> Server KindaMightWork_API
server PubCred{..} PrivCred{..} =    landPage
                                :<|> landPage
                                :<|> landPage
                                :<|> landPage
                                :<|> landPage
                                :<|> landPage
                                :<|> landPage
                                :<|> landPage
  where
    landPage = return (LandingPage, site_verification)


data PubCred = PubCred
      { google_client_id  :: Text
      , site_verification :: Text
      } deriving(Show,Read,Eq,Ord,Generic,ToJSON,FromJSON)

data PrivCred   = PrivCred
      { google_secret      :: Text
      } deriving(Show,Read,Eq,Ord,Generic,ToJSON,FromJSON)


main :: IO ()
main = do pub_conf  <- decode.toSL <$> readFile pub_file
          priv_conf <- decode.toSL <$> readFile priv_file
          
          case (pub_conf,priv_conf) of

            (Nothing , _        ) -> putStrLn $ "Could not parse: "<> pub_file
            
            (_       , Nothing  ) -> putStrLn $ "Could not parse: "<> priv_file

            (Just pub, Just priv) -> runTLS tls setttings
                                          . debuging 
                                          . serve api 
                                          $ server pub priv

 where

    pub_file  = "conf/public.json"
    priv_file = "conf/private.json"

    tls       = tlsSettings "conf/app.crt" "conf/app.key"
    setttings = defaultSettings & setPort port
    port      = 443


debuging :: Application -> Application
debuging app req cont = print req >> app req cont






