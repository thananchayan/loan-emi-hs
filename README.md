# Loan Eligibility & EMI Calculator (Haskell)

Small-but-complete functional program that scores an applicant's loan request, prints EMI/affordability metrics, and showcases parallel evaluation of multiple scenarios.

## Group Members

- Thavamohan Thananchayan (Team Lead)
- Member 2 – update with actual name
- Member 3 – update with actual name
- Member 4 – update with actual name

## Problem Description

Retail lenders and NBFCs need auditable logic that screens hundreds of loan applications every day. Each decision must validate the applicant profile, forecast the monthly EMI, and ensure the debt-to-income (DTI) ratio stays inside limits based on the borrower's credit band. This project delivers that pipeline entirely in Haskell using pure functions so the same code can be embedded inside backend services, scripts, or analytical notebooks without fear of hidden side effects.

## Key Features

- **Pure EMI & affordability engine** (`Processing.emiSummary`, `decideEligibility`) built with algebraic data types from `DataTypes`.
- **Transparent validation** of applicant and loan inputs before expensive calculations (`validateApplicant`, `validateLoan`).
- **Parallel scenario simulator** using `Control.Parallel.Strategies` (`batchEvaluate`) to demonstrate deterministic concurrency.
- **Informative CLI** that prints EMI, total payable amount, disposable income before/after the new EMI, DTI %, and detailed rejection reasons.
- **Reproducible technical report** generated through `scripts/generate_report.py` with `reportlab`, resulting in `report.pdf`.

## How to Build & Run

Prerequisites: GHC 9.6+, cabal-install, and the `parallel`/`deepseq` libraries (added to `loan-emi-hs.cabal`).

```bash
# Install dependencies & compile
cabal v2-build

# Run the interactive CLI
cabal v2-run loan-emi-hs

# Reload inside GHCi
cabal v2-repl
```

## Sample Interaction

```
=== Loan Eligibility & EMI Calculator (Haskell) ===

Monthly income: 185000
Monthly expenses: 55000
Existing monthly EMIs (0 if none): 10000
Credit band (Good/Average/Poor): Good

Loan amount requested: 3500000
Annual interest rate (%): 8.5
Tenure (months): 240

✅ Decision: APPROVED

--- EMI & Affordability Summary ---
Monthly EMI:      30454.69
Total Payable:    7309125.6
Total Interest:   3809125.6
Disposable (pre): 120000.0
Disposable (post):89545.31
Debt-to-income:   21.9%
```

After the single applicant flow the program can optionally run the parallel scenario demo; look for the multiline output that lists four pre-defined profiles processed simultaneously.

## FP Concepts Demonstrated

- **Pure Functions & Immutability** – `emiAmount`, `emiSummary`, `decideEligibility`, and `batchEvaluate` never mutate state; CLI just consumes their results.
- **Recursion** – `pow :: Double -> Int -> Double` implements exponentiation without loops, highlighting structural recursion.
- **Algebraic Data Types** – `Decision`, `EligibilityMetrics`, `LoanScenario`, `ScenarioEvaluation`, etc., make illegal states unrepresentable.
- **Higher-order / Parallelism** – `batchEvaluate` leverages `parMap rdeepseq` to map the pure evaluator across CPU cores.
- **Pipelines & Composition** – validation ➜ EMI estimation ➜ affordability metrics ➜ decision mirrors ETL-style data flows with each stage expressed as a reusable function.

## Repo Layout

```
app/Main.hs              -- Entry-point delegating to IOHandler.runApp
src/DataTypes.hs         -- Domain ADTs + metrics/scenario types
src/Processing.hs        -- Pure business logic, validations, and parallel evaluator
src/IOHandler.hs         -- CLI + printing helpers
src/Utils.hs             -- Parsers & rounding helpers
scripts/generate_report.py
report.pdf               -- 3–4 page technical write-up generated from the script
```

## Technical Report

Regenerate `report.pdf` whenever textual updates are needed:

```bash
# Requires Python 3 + reportlab (`pip install reportlab`)
python3 scripts/generate_report.py
```

Make sure the placeholders for the remaining group members are updated in both this README and `report.pdf` before final submission.
