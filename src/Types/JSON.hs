
module Types.JSON where

import Web.Vorple

import Types.Entry
import Types.Request
import Types.Response
import Types.Tag

deriveJSON (drop 1) ''Entry

deriveJSON (drop 3) ''Request

deriveJSON (drop 4) ''Response

deriveJSON (drop 1) ''Tag

