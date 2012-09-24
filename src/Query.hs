
module Query where

import qualified Data.Map as M

import Data.Acid
import Data.Record.StateFields.Containers

import Types

findUser :: String -> Query Database (Maybe User)
findUser name =
  askf dbUsers
  >>=
  return . fmap fst . M.minView . M.filter ((== name) . _upName . _uProfile)

getUserProfile :: Id -> Query Database (Maybe UserProfile)
getUserProfile id = askf f''
  where
    f' :: IdField Database (Maybe User)
    f' = dbUsers // h
    h :: IdField (M.Map Id User) (Maybe User)
    h = mapKey id
    g :: IdField (Maybe User) (Maybe UserProfile)
    g = inA uProfile
    f'' :: IdField Database (Maybe UserProfile)
    f'' = f' // g

getEntries :: Request -> Query Database [Entry]
getEntries ReqGetEntries{..} = undefined
getEntries _ = return []

