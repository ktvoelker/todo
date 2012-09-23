
module Handler.CurUser where

import Event
import Types

handleCurUser :: Handler
handleCurUser = do
  uId <- getf sessionUser
  case uId of
    Just uId -> do
      user <- ask >>= liftIO . flip query (GetUserProfile uId)
      case user of
        Just user -> return $ RespUserProfile uId user
        Nothing -> return $ RespError ENotFound ""
    _ -> return RespEmpty

