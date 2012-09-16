
module Main where

-- import Data.Acid
import Data.Time.Clock
-- import Data.Record.StateFields
-- import Data.Record.StateFields.Containers
import Web.Vorple

-- import qualified Data.Map as M
-- import qualified Network.Wai.Handler.Warp as Warp

type Id = Int

data External = ExGoogleDoc | ExGoogleSpreadsheet | ExGitHub
  deriving (Eq, Ord, Read, Show)

$(deriveJSON (drop 2) ''External)

data Request =
  ReqLogIn
  { reqUser :: String
  , reqPassword :: String
  } |
  ReqRegister
  { reqUser :: String
  , reqPassword :: String
  } |
  ReqCreateEntry
  { reqTitle :: String
  , reqTags :: [Id]
  , reqDue :: Maybe UTCTime
  , reqDepends :: [Id]
  } |
  ReqUpdateEntry
  { reqSetTitle :: Maybe String
  , reqAddTags :: [Id]
  , reqDelTags :: [Id]
  , reqSetDue :: Maybe (Maybe UTCTime)
  , reqAddDepends :: [Id]
  , reqDelDepends :: [Id]
  } |
  ReqGetTags
  { reqIsMood :: Maybe Bool
  , reqSearch :: Maybe String
  } |
  ReqGetEntries
  { reqSearch :: Maybe String
  , reqTags :: [Id]
  , reqDueBefore :: Maybe (UTCTime)
  , reqDueAfter :: Maybe (UTCTime)
  } |
  ReqParseTime
  { reqInput :: String
  } |
  ReqAddExternal
  { reqEntry :: Id
  , reqExternalType :: External
  } deriving (Eq, Ord, Read, Show)

$(deriveJSON (drop 3) ''Request)

main :: IO ()
main = return ()

