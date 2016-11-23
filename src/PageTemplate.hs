

module PageTemplate where




import           Lucid
import           Protolude
import           Data.Aeson

type Page     = (PageKind,Text)
data PageKind = LoggedPage
              | LandingPage
              | SyncResult
              deriving(Show,Read,Eq,Ord,Generic,ToJSON)


instance ToHtml Page where
    toHtml  x    = do doctype_    
                      html_       $ do 
                        
                        header_   $ do 
                        
                          meta_ [name_    "google-site-verification", content_ (snd x)]
                          meta_ [charset_ "UTF-8"]
                          meta_ [title_   "Kinda might work" ]
                        
                        body_ (toHtmlRaw x)


    toHtmlRaw _ = h1_ "This page is under construction"

