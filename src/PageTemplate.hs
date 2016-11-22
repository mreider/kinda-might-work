

module PageTemplate where




import           Lucid
import           Protolude
import           Data.Aeson

data Page     = LoggedPage
              | LandingPage
              | SyncResult
              deriving(Show,Read,Eq,Ord,Generic,ToJSON)


instance ToHtml Page where
    toHtmlRaw _ = h1_ "This page is under construction"
    toHtml      = toHtmlRaw



