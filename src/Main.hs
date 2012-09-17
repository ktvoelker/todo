
module Main where

import Data.String
import Network.Wai (Application, pathInfo)
import Network.Wai.Application.Static
import System.Environment
import Web.Vorple

import qualified Network.Wai.Handler.Warp as Warp

import Event
import Types

ni :: (Monad m) => m Response
ni = return $ RespError ENotImplemented ""

options :: Options
options = defaultOptions
  { optCookieName = "todo"
  , optLogLevel = VorpleDebug
  }

app :: AcidState Database -> Application
app db = vorpleIO options db defaultSession $ \req -> case req of
  ReqLogIn{..} -> do
    user <- ask >>= liftIO . flip query (FindUser reqUser)
    case user of
      Just User{..} | _uPassword == reqPassword -> do
        putf sessionUser $ Just _uId
        return RespEmpty
      _ -> return $ RespError EBadLogin ""
  ReqRegister{..} ->
    if null reqPassword
    then return $ RespError EInvalidPassword ""
    else do
      result <- ask >>= liftIO . flip update (Register reqUser reqPassword)
      case result of
        Left err -> return $ RespError err ""
        Right uId -> do
          putf sessionUser (Just uId)
          return RespEmpty
  ReqCreateEntry{..} -> ni
  ReqUpdateEntry{..} -> ni
  ReqGetTags{..} -> ni
  ReqGetEntries{..} -> ni
  ReqParseTime{..} -> ni
  ReqAddExternal{..} -> ni

main :: IO ()
main = do
  [staticPath] <- getArgs
  let static = staticApp $ defaultFileServerSettings $ fromString staticPath
  db <- openLocalState emptyDatabase
  Warp.run 8008 $ \req -> do
    if take 1 (pathInfo req) == ["api"]
    then app db req
    else static req

