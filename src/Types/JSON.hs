
module Types.JSON where

import Web.Vorple

import Types.Entry
import Types.Request
import Types.Response
import Types.Session
import Types.Tag

deriveJSON (drop 1) ''Entry

deriveJSON (drop 3) ''Request

deriveJSON id ''Error

deriveJSON (drop 4) ''Response

deriveJSON (drop 7) ''Session

deriveJSON (drop 1) ''Tag

