module IOHandler
  ( runApp
  ) where

import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd)
import System.IO (hFlush, stdout)

import DataTypes
import Processing
import Utils

runApp :: IO ()
runApp = do
  putStrLn "=== Loan Eligibility & EMI Calculator (Haskell) ==="
  menuLoop

menuLoop :: IO ()
menuLoop = do
  putStrLn "\nSelect an option:"
  putStrLn "  1. Evaluate a new loan application"
  putStrLn "  2. Run the parallel scenario demo"
  putStrLn "  3. Exit"
  choice <- ask "Enter choice (1/2/3): "
  case normalizeChoice choice of
    "1" -> handleSingleEvaluation >> promptReturn >> menuLoop
    "2" -> runScenarioDemo >> promptReturn >> menuLoop
    "3" -> putStrLn "\nThank you for using the calculator."
    _   -> do
      putStrLn "  ! Invalid selection. Please choose 1, 2, or 3."
      menuLoop

handleSingleEvaluation :: IO ()
handleSingleEvaluation = do
  applicant <- captureApplicant
  loanReq <- captureLoanRequest
  let (decisionResult, metrics) = decideEligibility applicant loanReq
  printDecision decisionResult
  maybe (pure ()) printMetrics metrics

captureApplicant :: IO Applicant
captureApplicant = do
  putStrLn "\n-- Applicant Details --"
  inc <- askDouble "Monthly income: "
  expn <- askDouble "Monthly expenses: "
  exEmi <- askDouble "Existing monthly EMIs (0 if none): "
  band <- askBand
  pure (Applicant inc expn exEmi band)

captureLoanRequest :: IO LoanRequest
captureLoanRequest = do
  putStrLn "\n-- Loan Details --"
  amt <- askDouble "Loan amount requested: "
  rate <- askDouble "Annual interest rate (%): "
  months <- askInt "Tenure (months): "
  pure (LoanRequest amt rate months)

runScenarioDemo :: IO ()
runScenarioDemo = do
  putStrLn "\n=== Parallel Scenario Evaluation Demo ==="
  let results = batchEvaluate demoScenarios
  mapM_ printScenarioEvaluation results

promptReturn :: IO ()
promptReturn = do
  _ <- ask "\nPress Enter to return to the main menu..."
  pure ()

ask :: String -> IO String
ask msg = do
  putStr msg
  hFlush stdout
  getLine

askDouble :: String -> IO Double
askDouble msg = do
  s <- ask msg
  case readDouble s of
    Just v  -> pure v
    Nothing -> do
      putStrLn "  ! Invalid number. Please enter a numeric value (e.g., 120000 or 120000.50)."
      askDouble msg

askInt :: String -> IO Int
askInt msg = do
  s <- ask msg
  case readInt s of
    Just v  -> pure v
    Nothing -> do
      putStrLn "  ! Invalid integer. Please enter whole numbers only."
      askInt msg

askBand :: IO CreditBand
askBand = do
  s <- ask "Credit band (Good/Average/Poor): "
  case parseCreditBand s of
    Just b  -> pure b
    Nothing -> do
      putStrLn "  ! Invalid band. Please type exactly one of: Good / Average / Poor."
      askBand

printDecision :: Decision -> IO ()
printDecision Approved = putStrLn "\nDecision: APPROVED"
printDecision (Rejected rs) = do
  putStrLn "\nDecision: REJECTED"
  putStrLn "Reasons:"
  mapM_ (\r -> putStrLn ("- " ++ r)) rs

printMetrics :: EligibilityMetrics -> IO ()
printMetrics = printMetricsWithPrefix ""

printMetricsWithPrefix :: String -> EligibilityMetrics -> IO ()
printMetricsWithPrefix prefix metrics = do
  let emiRes = metricEmi metrics
      fmt label value = putStrLn (prefix ++ label ++ show (round2 value))
      fmtPct label value = putStrLn (prefix ++ label ++ show (round2 value) ++ "%")
  putStrLn ""
  putStrLn (prefix ++ "--- EMI & Affordability Summary ---")
  fmt "Monthly EMI:      " (emi emiRes)
  fmt "Total Payable:    " (totalPayable emiRes)
  fmt "Total Interest:   " (totalInterest emiRes)
  fmt "Disposable (pre): " (disposableBefore metrics)
  fmt "Disposable (post):" (disposableAfter metrics)
  fmtPct "Debt-to-income:  " (projectedDti metrics * 100)

demoScenarios :: [LoanScenario]
demoScenarios =
  [ LoanScenario
      "Tech Lead purchasing an apartment"
      (Applicant 185000 55000 10000 Good)
      (LoanRequest 3500000 8.5 240)
  , LoanScenario
      "Retail entrepreneur expanding store"
      (Applicant 120000 45000 5000 Average)
      (LoanRequest 1800000 10.2 180)
  , LoanScenario
      "Logistics supervisor consolidating debt"
      (Applicant 80000 38000 20000 Average)
      (LoanRequest 900000 12.0 84)
  , LoanScenario
      "Freelance designer with weak credit"
      (Applicant 95000 40000 15000 Poor)
      (LoanRequest 600000 9.5 60)
  ]

printScenarioEvaluation :: ScenarioEvaluation -> IO ()
printScenarioEvaluation eval = do
  let scenario = evaluationScenario eval
      app = scenarioApplicant scenario
      req = scenarioRequest scenario
  putStrLn ("\nScenario: " ++ scenarioLabel scenario)
  putStrLn (" Applicant income: " ++ show (round2 (monthlyIncome app))
            ++ " | expenses: " ++ show (round2 (monthlyExpenses app))
            ++ " | existing EMI: " ++ show (round2 (existingEmi app))
            ++ " | credit band: " ++ show (creditBand app))
  putStrLn (" Loan: amount " ++ show (round2 (principal req))
            ++ " | rate " ++ show (round2 (annualRate req)) ++ "% | tenure "
            ++ show (tenureMonths req) ++ " months")
  case evaluationDecision eval of
    Approved -> putStrLn " Result: APPROVED"
    Rejected reasons -> do
      putStrLn " Result: REJECTED"
      mapM_ (\r -> putStrLn ("  - " ++ r)) reasons
  maybe (putStrLn " Metrics unavailable due to invalid inputs.")
        (printMetricsWithPrefix " ") (evaluationMetrics eval)

trimSpaces :: String -> String
trimSpaces = dropWhileEnd isSpace . dropWhile isSpace

normalizeChoice :: String -> String
normalizeChoice = map toLower . trimSpaces
