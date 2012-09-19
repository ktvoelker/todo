
module Event where

import Data.Acid

import Query
import Types
import Update

makeAcidic ''Database ['findUser, 'getUserProfile, 'register]

