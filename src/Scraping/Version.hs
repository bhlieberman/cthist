{-# LANGUAGE DeriveGeneric #-}

module Scraping.Version where

import Data.Aeson
import GHC.Generics

data TrialVersion = TrialVersion
  { version :: Integer,
    date :: String,
    status :: String,
    moduleLabels :: [String]
  }
  deriving (Generic, Show)

instance FromJSON TrialVersion

newtype VersionHistory = VersionHistory
  { changes :: [TrialVersion]
  }
  deriving (Generic, Show)

instance FromJSON VersionHistory
