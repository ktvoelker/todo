
module Main where

import Web.Vorple

import qualified Network.Wai.Handler.Warp as Warp

import Types

ni :: (Monad m) => m Response
ni = return $ RespError 1 "Not implemented"

main :: IO ()
main = Warp.run 8008 . vorpleIO defaultOptions () defaultSession $ \req -> case req of
  ReqLogIn{..} -> ni
  ReqRegister{..} -> ni
  ReqCreateEntry{..} -> ni
  ReqUpdateEntry{..} -> ni
  ReqGetTags{..} -> ni
  ReqGetEntries{..} -> ni
  ReqParseTime{..} -> ni
  ReqAddExternal{..} -> ni

