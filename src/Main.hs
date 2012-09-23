
module Main where

import Data.String
import Network.Wai (Application, pathInfo)
import Network.Wai.Application.Static
import System.Environment

import qualified Network.Wai.Handler.Warp as Warp

import Handler.CurUser
import Handler.LogIn
import Handler.Register
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
  ReqCurUser -> handleCurUser
  ReqLogIn{..} -> handleLogIn reqUser reqPassword
  ReqRegister{..} -> handleRegister reqUser reqPassword
  ReqCreateEntry{..} -> ni
  ReqUpdateEntry{..} -> ni
  ReqGetTags{..} -> ni
  ReqGetEntries{..} -> ni
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

