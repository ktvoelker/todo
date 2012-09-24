
module Types.Request where

import Data.Time.Clock

import Types.Core

data EntryFilter =
  EntryFilter
  { efSearch :: Maybe String
  , efTags :: [Id]
  , efDueBefore :: Maybe (UTCTime)
  , efDueAfter :: Maybe (UTCTime)
  , efMaxCount :: Int
  , efSkipCount :: Int
  } deriving (Eq, Ord, Read, Show)

data Request =
  ReqCurUser |
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
  , reqFilter :: EntryFilter
  } |
  ReqUpdateEntry
  { reqSetTitle :: Maybe String
  , reqAddTags :: [Id]
  , reqDelTags :: [Id]
  , reqSetDue :: Maybe (Maybe UTCTime)
  , reqAddDepends :: [Id]
  , reqDelDepends :: [Id]
  , reqFilter :: EntryFilter
  } |
  ReqGetTags
  { reqIsMood :: Maybe Bool
  , reqSearch :: Maybe String
  } |
  ReqGetEntries
  { reqFilter :: EntryFilter
  } |
  ReqAddExternal
  { reqEntry :: Id
  , reqExternalType :: External
  } deriving (Eq, Ord, Read, Show)

