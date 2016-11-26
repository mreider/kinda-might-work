module Main where
  
import           Server
import           DB
import           API

import           Data.Aeson
import           Network.Wai                  (Application)
import           Network.Wai.Handler.Warp     (setPort,defaultSettings)
import           Network.Wai.Handler.WarpTLS
import           Protolude                  hiding(to,get)
import           Servant.Server
--------------------------------------------------------------------------

main :: IO ()
main = do pub_conf  <- decode.toSL <$> readFile pub_file
          priv_conf <- decode.toSL <$> readFile priv_file
          migrate
          case (pub_conf,priv_conf) of

            (Nothing , _        ) -> putStrLn $ "Could not parse: "<> pub_file
            
            (_       , Nothing  ) -> putStrLn $ "Could not parse: "<> priv_file

            (Just pub, Just priv) -> runTLS tls setttings
                                          . debuging 
                                          . serve api 
                                          $ server (pub, priv)
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

