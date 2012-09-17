
module Update where

import qualified Data.Map as M

import Data.Acid
import Data.Record.StateFields

import Query
import Types.Core
import Types.Database
import Types.User
import Types.Response

register :: String -> String -> Update Database (Either Error Id)
register name password = do
  user <- runQuery $ findUser name
  case user of
    Just _ -> return $ Left EUserExists
    Nothing -> do
      uId <- getf dbUsers >>= return . M.size
      let user = User name password M.empty M.empty uId
      modf dbUsers $ M.insert uId user
      return $ Right uId

