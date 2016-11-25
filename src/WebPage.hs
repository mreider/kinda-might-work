

module WebPage where


import           Profile
import           Conf

import           Lucid
import           Protolude
import           Data.Aeson
import           Data.UUID
import           Network.URL -- to use


data WebPage = WebPage
      { verification :: Text
      , client_id    :: Text
      , csrf_token   :: UUID
      , page_mail    :: Maybe Text
      } deriving(Show,Read,Eq,Ord,Generic,ToJSON)

webPage :: UUID -> Creds -> Profile -> WebPage 
webPage = undefined
 
--generatePage ( session 
--             , ( PubCred{..}
--               , PrivCred{..}
--               )
--             ) profile       = let page_mail = case profile of
--                                                Nothing                  -> Nothing
--                                                Just ( GoogleProfile{..}
--                                                     , _
--                                                     , _
--                                                     )                   -> Just email

--                                in Page{ client_id    = googleCred ^. clientId
--                                       , verification = site_verification
--                                       , csrf_token   = session -- TODO: we have to salt it with a secret.
--                                       , ..
--                                       }


-- TODO: not to use plain text...

googleLink ::Text -> UUID -> Text
googleLink clientID token =  "https://accounts.google.com/o/oauth2/v2/auth"
                          <> "?redirect_uri=https%3A%2F%2Fkindamight.work%2Fgo-clb"
                          <> "&response_type=code"
                          <> "&client_id=" <> clientID
                          <> "&state="     <> show token
                          <> "&prompt=consent"           -- only while debugging
                          <> "&scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.profile"
                          <> "&access_type=offline"


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


      body =  do h3_ "This page is under construction"
                 case page_mail of
                  
                  Nothing -> a_  [href_ $ googleLink client_id csrf_token] "log with google"
                  
                  Just  x -> do p_  [] $ "your email is: " <> toHtml x
                                a_ [href_ "www.example.com"] "log out"
                                a_ [href_ "www.example.com"] "log with trello"  
                                a_ [href_ "www.example.com"] "log with wunderlist"

