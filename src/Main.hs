module Main where
  
import           Server
import           DB
import           API

import           Data.Aeson
import           Conf                         (_postgresConn)          
import           Network.Wai                  (Application)
import           Network.Wai.Handler.Warp     (setPort,defaultSettings)
import           Network.Wai.Handler.WarpTLS
import           Protolude                  hiding(to,get)
import           Servant.Server
--------------------------------------------------------------------------

main :: IO ()
main = do raw_conf  <- decode.toSL <$> readFile conf_file
          case raw_conf of

            Nothing   -> putStrLn $ "Could not parse: "<> conf_file
            
            Just conf -> do db <- getDB $ _postgresConn conf
                            runTLS tls setttings
                                 . debuging 
                                 . serve api 
                                 $ server db conf
 where

    conf_file  = "conf/public.json"

    tls       = tlsSettings "conf/app.crt" "conf/app.key"
    setttings = defaultSettings & setPort port
    port      = 443


debuging :: Application -> Application
debuging app req cont = do print req 
                           putStrLn ("\n------------------------------------------------------\n"::Text)
                           app req cont

