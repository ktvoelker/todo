
module Types.SafeCopy where

import Data.SafeCopy

import Types.Core
import Types.Entry
import Types.Tag
import Types.User
import Types.Database

deriveSafeCopy 1 'base ''External

deriveSafeCopy 1 'base ''Entry

deriveSafeCopy 1 'base ''Tag

deriveSafeCopy 1 'base ''User

deriveSafeCopy 1 'base ''Database

