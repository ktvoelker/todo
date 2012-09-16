
module Types.Response where

import Data.Time.Clock

import Types.Entry
import Types.Tag

data Response =
  RespEmpty |
  RespEntry Entry |
  RespEntries [Entry] |
  RespTags [Tag] |
  RespTime UTCTime |
  RespUrl String |
  RespError
  { respCode :: Int
  , respMessage :: String
  } deriving (Eq, Ord, Read, Show)

