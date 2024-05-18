{-# LANGUAGE DeriveGeneric #-}

module Scraping.Study where

import Data.Aeson
import GHC.Generics

data NCT = Text

data IdentificationModule = IdentificationModule
  { nctId :: String,
    orgStudyIdInfo :: Object,
    organization :: Object,
    briefTitle :: String
  }
  deriving (Generic, Show)

instance FromJSON IdentificationModule

data StatusModule = StatusModule
  { statusVerifiedDate :: String,
    overallStatus :: String,
    expandedAccessInfo :: Object,
    startDateStruct :: Object,
    studyFirstSubmitDate :: String,
    studyFirstSubmitQcDate :: String,
    studyFirstPostDateStruct :: Object,
    lastUpdateSubmitDate :: String,
    lastUpdatePostDateStruct :: Object
  }
  deriving (Generic, Show)

instance FromJSON StatusModule

data DescriptionModule = DescriptionModule
  { briefSummary :: String,
    detailedDescription :: String
  }
  deriving (Generic, Show)

instance FromJSON DescriptionModule

newtype SponsorCollaboratorsModule = SponsorCollaboratorsModule
  { leadSponsor :: Object
  }
  deriving (Generic, Show)

instance FromJSON SponsorCollaboratorsModule

newtype ConditionsModule = ConditionsModule
  { conditions :: [String]
  }
  deriving (Generic, Show)

instance FromJSON ConditionsModule

data DesignModule = DesignModule
  { studyType :: String,
    phases :: [String],
    designInfo :: Object
  }
  deriving (Generic, Show)

instance FromJSON DesignModule

newtype ArmsInterventionsModule = ArmsInterventionsModule
  { interventions :: [Object]
  }
  deriving (Generic, Show)

instance FromJSON ArmsInterventionsModule

data EligibilityModule = EligibilityModule
  { eligibilityCriteria :: String,
    healthyVolunteers :: Bool,
    sex :: String,
    minimumAge :: String,
    maximumAge :: String,
    stdAges :: [String]
  }
  deriving (Generic, Show)

instance FromJSON EligibilityModule

newtype ContactsLocationsModule = ContactsLocationsModule
  { overallOfficials :: [Object]
  }
  deriving (Generic, Show)

instance FromJSON ContactsLocationsModule

newtype ReferencesModule = ReferencesModule
  { references :: [Object]
  }
  deriving (Generic, Show)

instance FromJSON ReferencesModule

data ProtocolSection = ProtocolSection
  { identificationModule :: IdentificationModule,
    statusModule :: StatusModule,
    sponsorCollaboratorsModule :: SponsorCollaboratorsModule,
    descriptionModule :: DescriptionModule,
    conditionsModule :: ConditionsModule,
    designModule :: DesignModule,
    armsInterventionsModule :: ArmsInterventionsModule,
    eligibilityModule :: EligibilityModule,
    contactsLocationsModule :: ContactsLocationsModule,
    referencesModule :: ReferencesModule
  }
  deriving (Generic, Show)

instance FromJSON ProtocolSection

data StudyInternal = StudyInternal
  { protocolSection :: ProtocolSection,
    hasResults :: Bool
  }
  deriving (Generic, Show)

instance FromJSON StudyInternal

data Study = Study
  { studyVersion :: Integer,
    study :: StudyInternal
  }
  deriving (Generic, Show)

instance FromJSON Study
