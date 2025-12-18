module Processing
  ( monthlyRate
  , pow
  , emiAmount
  , emiSummary
  , disposableIncome
  , dtiRatio
  , validateApplicant
  , validateLoan
  , decideEligibility
  , evaluateScenario
  , batchEvaluate
  ) where

import Control.Parallel.Strategies (parMap, rdeepseq)
import DataTypes

monthlyRate :: Double -> Double
monthlyRate annual = annual / 12.0 / 100.0

pow :: Double -> Int -> Double
pow _ 0 = 1
pow x n = x * pow x (n - 1)

emiAmount :: LoanRequest -> Double
emiAmount req =
  let p = principal req
      r = monthlyRate (annualRate req)
      n = tenureMonths req
  in if r == 0
     then p / fromIntegral n
     else
       let a = pow (1 + r) n
       in p * r * a / (a - 1)

emiSummary :: LoanRequest -> EmiResult
emiSummary req =
  let e = emiAmount req
      n = fromIntegral (tenureMonths req)
      p = principal req
      tp = e * n
      ti = tp - p
  in EmiResult e tp ti

disposableIncome :: Applicant -> Double
disposableIncome a = monthlyIncome a - monthlyExpenses a - existingEmi a

dtiRatio :: Applicant -> LoanRequest -> Double
dtiRatio a req =
  let income = monthlyIncome a
      newEmi = emiAmount req
      totalDebt = existingEmi a + newEmi
  in if income <= 0 then 1 else totalDebt / income

validateApplicant :: Applicant -> [String]
validateApplicant a =
  concat
    [ if monthlyIncome a <= 0 then ["Monthly income must be greater than 0"] else []
    , if monthlyExpenses a < 0 then ["Monthly expenses cannot be negative"] else []
    , if existingEmi a < 0 then ["Existing EMIs cannot be negative"] else []
    ]

validateLoan :: LoanRequest -> [String]
validateLoan req =
  concat
    [ if principal req <= 0 then ["Loan amount must be greater than 0"] else []
    , if annualRate req < 0 then ["Interest rate cannot be negative"] else []
    , if annualRate req > 50 then ["Interest rate seems unrealistic (> 50%)"] else []
    , if tenureMonths req <= 0 then ["Tenure must be greater than 0 months"] else []
    , if tenureMonths req > 600 then ["Tenure is capped at 600 months (50 years)"] else []
    ]

maxAllowedDti :: CreditBand -> Double
maxAllowedDti Good    = 0.40
maxAllowedDti Average = 0.30
maxAllowedDti Poor    = 0.00

decideEligibility :: Applicant -> LoanRequest -> (Decision, Maybe EligibilityMetrics)
decideEligibility a req =
  case validateApplicant a ++ validateLoan req of
    (reason:rest) -> (Rejected (reason : rest), Nothing)
    [] ->
      let summary = emiSummary req
          baseDisposable = disposableIncome a
          postDisposable = baseDisposable - emi summary
          dti = dtiRatio a req
          metrics = EligibilityMetrics summary baseDisposable postDisposable dti
          domainReasons = rejectionReasons a metrics
      in if null domainReasons
         then (Approved, Just metrics)
         else (Rejected domainReasons, Just metrics)

rejectionReasons :: Applicant -> EligibilityMetrics -> [String]
rejectionReasons a metrics =
  let allowed = maxAllowedDti (creditBand a)
      dti = projectedDti metrics
      beforeDisp = disposableBefore metrics
      afterDisp = disposableAfter metrics
  in concat
      [ if creditBand a == Poor then ["Credit band is Poor"] else []
      , if beforeDisp <= 0 then ["Disposable income before new EMI is not positive"] else []
      , if afterDisp <= 0 then ["Disposable income becomes non-positive after the new EMI"] else []
      , if dti > allowed
          then ["Debt-to-income ratio is " ++ show (dti * 100) ++ "% but allowed upto " ++ show (allowed * 100) ++ "%"]
          else []
      ]

evaluateScenario :: LoanScenario -> ScenarioEvaluation
evaluateScenario scenario =
  let applicant = scenarioApplicant scenario
      loanReq = scenarioRequest scenario
      (decisionResult, metrics) = decideEligibility applicant loanReq
  in ScenarioEvaluation scenario decisionResult metrics

batchEvaluate :: [LoanScenario] -> [ScenarioEvaluation]
batchEvaluate = parMap rdeepseq evaluateScenario
