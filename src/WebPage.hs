

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
import           Synchro

urlToLog :: OAuthCred -> TokenCSFR -> Text
urlToLog OAuthCred{..} t  = toSL $ exportURL URL
                              { url_type   = Absolute $ Host (HTTP True) (toSL _authDomain) Nothing
                              , url_path   = toSL _authPath
                              , url_params = [ ("redirect_uri"  , toSL _uriRedirect )
                                             , ("response_type" , "code"            )
                                             , ("client_id"     , toSL _clientId    )
                                             , ("state"         , show t            )
                                             , ("prompt"        , "consent"         ) -- only while debugging
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
                  
                  case innerPage of
                    Nothing            -> ""
                    Just InnerPage{..} -> do script_ [src_  "https://code.jquery.com/jquery-1.7.1.min.js"] (""::Text)
                                             script_ [src_$ "https://api.trello.com/1/client.js?key="<> _clientId trello_conf] (""::Text)
                                             script_ [src_  "/trelloAuth.js"] (""::Text)

                                                                
                body_ body

      urlToAction :: (Monad m) => Text -> HtmlT m () -> HtmlT m ()
      urlToAction path = a_ [href_ $ path ]
      
      urlToAction' path = urlToAction (path <> "/" <> show csrf_token)

      urlTolog'    cred     = a_ [href_ $ urlToLog    cred      csrf_token]

      body = case innerPage of
              
              Nothing             -> do h3_ "Kinda Might Work:"
                                        p_  "Get your Trello and WunderList accounts on sync!"
                                        urlTolog' google_conf "log with google to continue" 
              
              Just InnerPage{..}  -> do h3_ $ "Welcome " <> toHtml userName <> " !!"
                                        p_  $ do "Using accounts for email: "   
                                                 strong_ (toHtml userEmail) 
                                                 "( "<> urlToAction "/go-out" "log out" <> " )"

                                        br_ []

                                        if trelloAccount
                                          then p_ [] $ do 
                                                "Your trello acount is linked ("
                                                urlToAction' "/tr-out" "unlink"
                                                ") "

                                          else a_ [ onclick_ $ "authenticateTrello('"<> show csrf_token <>"')"
                                                  , href_    "#"
                                                  ]
                                                  "link your trello account"

                                        br_ []

                                        if wuListAccount
                                          then p_ [] $ do 
                                                "Your WunderList acount is linked ("
                                                urlToAction' "/wl-out" "unlink"
                                                ") "

                                          else urlTolog' wuList_conf"link your wunderlist account"

                                        br_ []

                                        let ready        = wuListAccount && trelloAccount

                                        h4_ "Click to Sync!"
                                        if not ready 
                                          then p_ [] $ do "You need to link your Trello and "
                                                          "Wunderlist account to sync."

                                          else toSyncForm outOfSync



      toSyncForm  :: (Monad m) => [(Text,SyncOptions)] -> HtmlT m ()
      toSyncForm fields        = form_ [action_ $"/sync/"<>show csrf_token, method_ "post"] $ do
                                  fieldset_ $ do
                                    legend_ "Boards to syncrhonize"
                                    
                                    let toRetrieve = [ (name,ref) 
                                                     | (name,ToRetrieve ref _) <- fields
                                                     ]
                                    ul_ $
                                      sequence_    
                                           [ li_ [] $ do 
                                                input_ [type_ "checkbox", name_"board", value_ ref]
                                                toHtml (show name :: Text)

                                           | (name,ref) <- toRetrieve
                                           ] 

                                    br_ []
                                    if null toRetrieve
                                      then h4_ "Everything sync'ed, press F5 to check again."
                                      else input_ [type_ "submit",value_ "sync selected boards"]
                                  
                                  br_ []
                                  
                                  fieldset_ $ do
                                    legend_ "Already sync'ed:"
                                    
                                    ul_ $
                                      sequence_ 
                                         [ li_ [] $ do 
                                              p_ $ toHtml (show name :: Text)
                                              br_ []
                                              
                                         | (name,AlreadySync) <- fields
                                         ]
