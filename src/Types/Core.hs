
module Types.Core where

import Web.Vorple

type Id = Int

data External = ExGoogleDoc | ExGoogleSpreadsheet | ExGitHub
  deriving (Eq, Ord, Read, Show)

$(deriveJSON (drop 2) ''External)

