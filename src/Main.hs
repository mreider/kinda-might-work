module Main where

import           DB
import           PageTemplate
import           WithSession

import           Data.Aeson
import           Data.UUID
import           Network.Wai                  (Application)
import           Network.Wai.Handler.Warp     (run,setPort,defaultSettings)
import           Network.Wai.Handler.WarpTLS
import           Protolude
import           Servant.API
import           Servant.Client
import           Servant.HTML.Lucid
import           Servant.Server
import           System.Random
--------------------------------------------------------------------------
type KindaMightWork_API = WithSession "kmw_session" 
                        :> (     GoogleCallback
                           :<|>  TrelloCallback
                           :<|>  WunderlistCallback
                           :<|>  LogOut
                           :<|>  RemoveTrello
                           :<|>  RemoveWunder
                           :<|>  SyncTrelloWunder
                           :<|>  IndexPage
                           )     


type GoogleCallback     = "go-clb" :> Get [HTML,JSON] Page
type TrelloCallback     = "tr-clb" :> Get [HTML,JSON] Page
type WunderlistCallback = "wl-clb" :> Get [HTML,JSON] Page
type LogOut             = "go-out" :> Get [HTML,JSON] Page
type RemoveTrello       = "tr-out" :> Get [HTML,JSON] Page
type RemoveWunder       = "wl-out" :> Get [HTML,JSON] Page
type SyncTrelloWunder   = "sync"   :> Get [HTML,JSON] Page
type IndexPage          =             Get [HTML,JSON] Page   
---------------------------------------------------------------------------

{-
Request { requestMethod = "GET"
        , httpVersion = HTTP/2.0
        , rawPathInfo = "/"
        , rawQueryString = ""
        , requestHeaders = [ ("Host","localhost")
                           , ("User-Agent","Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:50.0) Gecko/20100101 Firefox/50.0")
                           , ("Accept","text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
                           , ("Accept-Language","en-US,en;q=0.5")
                           , ("Accept-Encoding","gzip, deflate, br")
                           , ("upgrade-insecure-requests","1")
                           , ("Cache-Control","max-age=0")
                           ]
        , isSecure = True
        , remoteHost = 127.0.0.1:50134
        , pathInfo = []
        , queryString = []
        , requestBody = <IO ByteString>
        , vault = <Vault>
        , requestBodyLength = KnownLength 0
        , requestHeaderHost = Just "localhost"
        , requestHeaderRange = Nothing}
-}



api :: Proxy (KindaMightWork_API)
api = Proxy


server :: PubCred -> PrivCred -> Server KindaMightWork_API
server PubCred{..} PrivCred{..} session =    landPage
                                        :<|> landPage
                                        :<|> landPage
                                        :<|> landPage
                                        :<|> landPage
                                        :<|> landPage
                                        :<|> landPage
                                        :<|> landPage
  where
    landPage = return Page{ client_id    = google_client_id
                          , verification = site_verification
                          , csrf_token   = session
                          } 


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
debuging app req cont = do print req 
                           putStrLn ("\n-----------------------------------------------------\n"::Text)
                           app req cont

