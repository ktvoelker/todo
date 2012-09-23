
module Handler.Register where

import Event
import Types

handleRegister :: String -> String -> Handler
handleRegister reqUser reqPassword = do
  if null reqPassword
  then return $ RespError EInvalidPassword ""
  else do
    result <- ask >>= liftIO . flip update (Register reqUser reqPassword)
    case result of
      Left err -> return $ RespError err ""
      Right uId -> do
        putf sessionUser . Just $ uId
        return RespEmpty

