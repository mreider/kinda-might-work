

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


urlToLog :: OAuthCred -> TokenCSFR -> Text
urlToLog OAuthCred{..} t  = toSL $ exportURL URL
                              { url_type   = Absolute $ Host (HTTP True) (toSL _authDomain) Nothing
                              , url_path   = toSL _authPath
                              , url_params = [ ("redirect_uri"  , toSL _uriRedirect )
                                             , ("response_type" , "code"            )
                                             , ("client_id"     , toSL _clientId    )
                                             , ("state"         , show t            )
                                            -- , ("prompt"        , "consent"         ) -- only while debugging
                                             , ("scope"         , toSL _scope       )
                                             , ("access_type"   , "offline"         )
                                             ]
                              }



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
                  meta_ [title_   "Kinda Might Work" ]
                
                body_ body

      urlToAction :: (Monad m) => Text -> HtmlT m () -> HtmlT m ()
      urlToAction path = a_ [href_ $ path ]
      
      urlToAction' path = urlToAction (path <> "/" <> show csrf_token)

      urlTolog'    cred     = a_ [href_ $ urlToLog    cred      csrf_token]


      body = case innerPage of
              
              Nothing             -> do h3_ "Kinda Might Work:"
                                        p_  "Get your Trello and WunderList accounts on sync!"
                                        urlTolog' google_conf "log with google to continue" 
              
              Just InnerPage{..}  -> do h3_ $ "Wellcome " <> toHtml userName <> " !!"
                                        p_  $ do "Using accounts for email: "   
                                                 strong_ (toHtml userEmail) 
                                                 "( "<> urlToAction "/go-out" "log out" <> " )"

                                        br_ []
                                        
                                        if trelloAccount
                                          then p_ [] $ do 
                                                "Your trello acount is linked ("
                                                urlToAction' "/tr-out" "unlink"
                                                ") "

                                          else urlTolog' trello_conf "link your trello account"

                                        br_ []

                                        if wuListAccount
                                          then p_ [] $ do 
                                                "Your WunderList acount is linked ("
                                                urlToAction' "/wl-out" "unlink"
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
