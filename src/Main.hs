
module Main where

import Data.String
import Network.Wai (Application, pathInfo)
import Network.Wai.Application.Static
import System.Environment
import Web.Vorple

import qualified Network.Wai.Handler.Warp as Warp

import Types

ni :: (Monad m) => m Response
ni = return $ RespError 1 "Not implemented"

app :: Application
app = vorpleIO defaultOptions () defaultSession $ \req -> case req of
  ReqLogIn{..} -> ni
  ReqRegister{..} -> ni
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
  Warp.run 8008 $ \req -> do
    if take 1 (pathInfo req) == ["api"]
    then app req
    else static req

