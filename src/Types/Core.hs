
module Types.Core where

import Data.Typeable
import Web.Vorple

type Id = Int

data External = ExGoogleDoc | ExGoogleSpreadsheet | ExGitHub
  deriving (Eq, Ord, Read, Show, Typeable)

$(deriveJSON (drop 2) ''External)

