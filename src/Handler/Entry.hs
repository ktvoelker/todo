
module Handler.Entry where

import Network.HTTP.Types (status500)

-- import Event
import Types

handleCreateEntry :: Request -> Handler
handleCreateEntry ReqCreateEntry{..} = undefined
handleCreateEntry _ = throwStatus status500

handleUpdateEntry :: Request -> Handler
handleUpdateEntry ReqUpdateEntry{..} = undefined
handleUpdateEntry _ = throwStatus status500

handleGetEntries :: Request -> Handler
handleGetEntries ReqGetEntries{..} = undefined
handleGetEntries _ = throwStatus status500

