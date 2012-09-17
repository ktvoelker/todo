
module Event where

import Data.Acid

import Query
import Types.Database
import Types.SafeCopy ()
import Update

makeAcidic ''Database ['findUser, 'register]

