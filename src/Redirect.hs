
module Redirect where

import           Network.HTTP.Types
import           Network.Wai
import           Servant.Server.Internal
import           Servant.API
import           Servant.Server
import           Protolude
-- TODO: use responseServantErr
data IndexRedirect

instance HasServer IndexRedirect config where

  type ServerT IndexRedirect m = m ()


  route Proxy _ action = leafRouter route'
   where

      route_uri ()        = Route $ responseLBS seeOther303
                                                [(hLocation, "/" )] 
                                                ""

      route' env req resp = runAction action env req resp route_uri

