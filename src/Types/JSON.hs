
module Types.JSON where

import Web.Vorple

import Types.Entry
import Types.Request
import Types.Response
import Types.Session
import Types.Tag
import Types.User

deriveJSON (drop 2) ''Entry

deriveJSON (drop 3) ''Request

deriveJSON id ''Error

deriveJSON (drop 4) ''Response

deriveJSON (drop 8) ''Session

deriveJSON (drop 2) ''Tag

deriveJSON (drop 3) ''UserProfile

