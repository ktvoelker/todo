
module Types.Instances where

import Data.Typeable

import Types.Database
import Types.Response
import Types.User

deriving instance Typeable Database
deriving instance Typeable UserProfile
deriving instance Typeable User
deriving instance Typeable Error
deriving instance Eq User
deriving instance Ord User
deriving instance Read User
deriving instance Show User
deriving instance Eq Database
deriving instance Ord Database
deriving instance Read Database
deriving instance Show Database

