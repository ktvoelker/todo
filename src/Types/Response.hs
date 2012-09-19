
module Types.Response where

import Data.Time.Clock

import Types.Core
import Types.Entry
import Types.User
import Types.Tag

data Error =
    ENotImplemented
  | EBadLogin
  | EUserExists
  | EInvalidPassword
  | EForbidden
  | ENotFound
  deriving (Eq, Ord, Read, Show)

data Response =
  RespEmpty |
  RespEntry Entry |
  RespEntries [Entry] |
  RespTags [Tag] |
  RespTime UTCTime |
  RespUrl String |
  RespUserProfile Id UserProfile |
  RespError
  { respType :: Error
  , respMessage :: String
  } deriving (Eq, Ord, Read, Show)

