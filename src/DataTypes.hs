{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module DataTypes
  ( CreditBand(..)
  , Applicant(..)
  , LoanRequest(..)
  , Decision(..)
  , EmiResult(..)
  , EligibilityMetrics(..)
  , LoanScenario(..)
  , ScenarioEvaluation(..)
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

data CreditBand = Good | Average | Poor
  deriving (Show, Eq, Read, Generic, NFData)

data Applicant = Applicant
  { monthlyIncome :: Double
  , monthlyExpenses :: Double
  , existingEmi :: Double
  , creditBand :: CreditBand
  } deriving (Show, Eq, Generic, NFData)

data LoanRequest = LoanRequest
  { principal :: Double
  , annualRate :: Double     -- e.g., 12.5 means 12.5%
  , tenureMonths :: Int
  } deriving (Show, Eq, Generic, NFData)

data Decision
  = Approved
  | Rejected [String]
  deriving (Show, Eq, Generic, NFData)

data EmiResult = EmiResult
  { emi :: Double
  , totalPayable :: Double
  , totalInterest :: Double
  } deriving (Show, Eq, Generic, NFData)

data EligibilityMetrics = EligibilityMetrics
  { metricEmi :: EmiResult
  , disposableBefore :: Double
  , disposableAfter :: Double
  , projectedDti :: Double
  } deriving (Show, Eq, Generic, NFData)

data LoanScenario = LoanScenario
  { scenarioLabel :: String
  , scenarioApplicant :: Applicant
  , scenarioRequest :: LoanRequest
  } deriving (Show, Eq, Generic, NFData)

data ScenarioEvaluation = ScenarioEvaluation
  { evaluationScenario :: LoanScenario
  , evaluationDecision :: Decision
  , evaluationMetrics :: Maybe EligibilityMetrics
  } deriving (Show, Eq, Generic, NFData)
