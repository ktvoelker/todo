
module Types.Response where

import Data.Time.Clock

import Types.Entry
import Types.Tag

data Error = ENotImplemented | EBadLogin | EUserExists | EInvalidPassword
  deriving (Eq, Ord, Read, Show)

data Response =
  RespEmpty |
  RespEntry Entry |
  RespEntries [Entry] |
  RespTags [Tag] |
  RespTime UTCTime |
  RespUrl String |
  RespError
  { respType :: Error
  , respMessage :: String
  } deriving (Eq, Ord, Read, Show)

