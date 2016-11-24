

module PageTemplate where




import           Lucid
import           Protolude
import           Data.Aeson
import           Data.UUID

data Page = Page
      { verification :: Text
      , client_id    :: Text
      , csrf_token   :: UUID
      } deriving(Show,Read,Eq,Ord,Generic,ToJSON)

instance ToJSON UUID where
    toJSON = String . show 

googleLink ::Text -> UUID -> Text
googleLink clientID token =  "https://accounts.google.com/o/oauth2/v2/auth"
                          <> "?redirect_uri=https%3A%2F%2Fkindamight.work%2Fgo-clb"
                          <> "&response_type=code"
                          <> "&client_id=" <> clientID
                          <> "&state="     <> show token
                          <> "&scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.profile"
                          <> "&access_type=offline"


instance ToHtml Page where
    toHtmlRaw        = toHtml

    toHtml  Page{..} = doctype_ >> html

     where    
      html =  html_       $ do 
                        
                head_     $ do 
                
                  meta_ [name_    "google-site-verification", content_ verification]
                  meta_ [charset_ "UTF-8"]
                  meta_ [title_   "Kinda might work" ]
                
                body_ body


      body =  do h3_ "This page is under construction"
                 a_  [href_ $ googleLink client_id csrf_token] "log with google"


