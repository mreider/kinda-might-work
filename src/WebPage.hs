

module WebPage where


import           Profile
import           Conf      
import           TokenCSFR
import           OAuth
import           API

import           Lucid
import           Protolude
import           Control.Lens
import           Data.Aeson
import           Orphan.UUID
import           Network.URL -- to use

-- TODO: change module name to HtmlTemplate

-- TODO: not to use plain text...

googleLink ::Text -> TokenCSFR -> Text
googleLink clientID token =  "https://accounts.google.com/o/oauth2/v2/auth"
                          <> "?redirect_uri=https%3A%2F%2Fkindamight.work%2Fgo-clb"
                          <> "&response_type=code"
                          <> "&client_id=" <> clientID
                          <> "&state="     <> show token
                          <> "&prompt=consent"           -- only while debugging
                          <> "&scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.profile"
                          <> "&access_type=offline"

-- TODO: implement
urlToLog :: OAuthCred -> TokenCSFR -> Text
urlToLog _ _     = "https://www.example.com"

--urlToAction :: (HasLink endpoint) => proxy endpoint -> TokenCSFR -> Text
urlToAction :: a             -> TokenCSFR -> Text
urlToAction _ _ =  "https://www.example.com"
--------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------

instance ToHtml WebPage where
    toHtmlRaw           = toHtml

    toHtml  WebPage{..} = doctype_ >> html

     where    
      html =  html_       $ do 
                        
                head_     $ do 
                
                  meta_ [name_    "google-site-verification", content_ verification]
                  meta_ [charset_ "UTF-8"]
                  meta_ [title_   "Kinda might work" ]
                
                body_ body

      urlToAction' :: (Monad m) => a -> HtmlT m () -> HtmlT m ()
      urlToAction' endpoint = a_ [href_ $ urlToAction endpoint  csrf_token]
      
      urlTolog'    cred     = a_ [href_ $ urlToLog    cred      csrf_token]


      body = case innerPage of
              
              Nothing             -> do h3_ "Kinda Might Work:"
                                        p_  "Get your Trello and WunderList accounts on sync!"
                                        urlTolog' google_conf "log with google to continue" 
              
              Just InnerPage{..}  -> do h3_ $ "Wellcome " <> toHtml userName <> " !!"
                                        p_  $ do "Using accounts for email:" <> toHtml userEmail <> "( "
                                                 urlToAction' _LogOut "log out"
                                                 " )"

                                        br_ []
                                        
                                        if trelloAccount
                                          then p_ [] $ do 
                                                "Your trello acount is linked ("
                                                urlToAction' _RemoveTrello "unlink"
                                                ") "

                                          else urlTolog' trello_conf "link your trello account"

                                        br_ []

                                        if wuListAccount
                                          then p_ [] $ do 
                                                "Your WunderList acount is linked ("
                                                urlToAction' _RemoveWunder "unlink"
                                                ") "

                                          else urlTolog' wuList_conf"link your wunderlist account"

                                        br_ []

                                        let ready        = wuListAccount && trelloAccount
                                            somthingToDo = not (null outOfSync)

                                        h4_ "Click to Sync!"
                                        if | not ready    -> p_ [] $ do 
                                                             "You need to link your Trello and "
                                                             "Wunderlist account to sync."

                                           | somthingToDo -> p_  [] $ do 
                                                            "The follogin will be link: "
                                                            ul_ [] $ sequence_  
                                                               [ toHtml stuff
                                                               | stuff <- outOfSync
                                                               ]

                                           | otherwise    -> p_  [] $ do 
                                                            "Currently we detect nothing out of sync, "
                                                            "press F5 to check again."
