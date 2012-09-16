
module Types.Request where

import Data.Time.Clock
import Web.Vorple

import Types.Core

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

deriveJSON (drop 3) ''Request

