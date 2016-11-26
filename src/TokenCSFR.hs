{-# LANGUAGE NoDeriveAnyClass           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module TokenCSFR ( TokenCSFR()
                 , csfrChallenge
                 , correctCSFR
                 ) where


import           Database.Persist
import           Lucid
import           Orphan.UUID
import           Protolude          hiding(show)
import           Web.HttpApiData
import           Web.PathPieces
import           Prelude(Show(..))
import           Data.Aeson

newtype TokenCSFR  = TokenCSFR UUID 
                   deriving (Read,Eq,Ord,FromHttpApiData,ToJSON,FromJSON)


instance Show TokenCSFR where
    show (TokenCSFR t) = show t
-- TODO: implement
csfrChallenge :: Text -> UUID -> TokenCSFR
csfrChallenge _ = TokenCSFR


correctCSFR   :: Text -> UUID -> TokenCSFR -> Bool
correctCSFR _ x (TokenCSFR y) = x == y


instance ToHtml TokenCSFR where
    toHtmlRaw             = toHtml
    toHtml  (TokenCSFR t) = toHtml (toText t)
