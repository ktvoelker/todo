
module Query where

import qualified Data.Map as M

import Data.Acid
import Data.Record.StateFields

import Types.Database
import Types.User

findUser :: String -> Query Database (Maybe User)
findUser name =
  askf dbUsers >>= return . fmap fst . M.minView . M.filter ((== name) . _uName)

