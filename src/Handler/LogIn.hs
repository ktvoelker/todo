
module Handler.LogIn where

import Event
import Types

handleLogIn :: String -> String -> Handler
handleLogIn reqUser reqPassword = do
  user <- ask >>= liftIO . flip query (FindUser reqUser)
  case user of
    Just User{..} | _uPassword == reqPassword -> do
      putf sessionUser . Just $ _uId
      return RespEmpty
    _ -> return $ RespError EBadLogin ""

