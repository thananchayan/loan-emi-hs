#!/usr/bin/env python3
"""Generate the functional programming mini-project technical report as PDF."""
from textwrap import wrap

from reportlab.lib.pagesizes import A4
from reportlab.pdfgen import canvas

PAGE_WIDTH, PAGE_HEIGHT = A4
LEFT_MARGIN = 50
TOP_MARGIN = 60
BOTTOM_MARGIN = 40


class PdfBuilder:
    def __init__(self, output_path: str) -> None:
        self.canvas = canvas.Canvas(output_path, pagesize=A4)
        self.cursor_y = PAGE_HEIGHT - TOP_MARGIN

    def _ensure_space(self, needed: float) -> None:
        if self.cursor_y - needed < BOTTOM_MARGIN:
            self.canvas.showPage()
            self.cursor_y = PAGE_HEIGHT - TOP_MARGIN

    def heading(self, text: str, size: int = 16) -> None:
        self._ensure_space(24)
        self.canvas.setFont("Helvetica-Bold", size)
        self.canvas.drawString(LEFT_MARGIN, self.cursor_y, text)
        self.cursor_y -= 28

    def paragraph(self, text: str, size: int = 11, leading: int = 14) -> None:
        self.canvas.setFont("Helvetica", size)
        wrapped = wrap(text, 95)
        for line in wrapped:
            self._ensure_space(leading)
            self.canvas.drawString(LEFT_MARGIN, self.cursor_y, line)
            self.cursor_y -= leading
        self.cursor_y -= 6

    def bullet(self, text: str, size: int = 11, leading: int = 14) -> None:
        self.canvas.setFont("Helvetica", size)
        wrapped = wrap(text, 92)
        for idx, line in enumerate(wrapped):
            self._ensure_space(leading)
            prefix = "• " if idx == 0 else "  "
            self.canvas.drawString(LEFT_MARGIN, self.cursor_y, prefix + line)
            self.cursor_y -= leading
        self.cursor_y -= 4

    def code_line(self, signature: str, info: str = "") -> None:
        self.canvas.setFont("Courier", 10)
        self._ensure_space(14)
        line = signature if not info else f"{signature}    -- {info}"
        self.canvas.drawString(LEFT_MARGIN, self.cursor_y, line)
        self.cursor_y -= 14

    def save(self) -> None:
        self.canvas.save()


def build_report(output_path: str = "report.pdf") -> None:
    pdf = PdfBuilder(output_path)

    pdf.heading("Loan Eligibility & EMI Calculator - Technical Report", size=18)
    pdf.paragraph(
        "Group Members: Thavamohan Thananchayan, Member 2 (update), Member 3 (update), "
        "Member 4 (update). Replace the placeholders with the remaining team members before submission."
    )
    pdf.paragraph(
        "Project Title: Functional Loan Eligibility & EMI Calculator with a concurrent scenario "
        "simulator."
    )

    pdf.heading("Problem Statement & Industrial Motivation", size=14)
    pdf.paragraph(
        "Retail lenders and NBFCs must evaluate thousands of unsecured and secured loan requests "
        "each day. Lending policies focus on the borrower's cash flows, debt burden, and credit "
        "band to avoid defaults. The project delivers a deterministic decisioning microservice "
        "that collects applicant and loan details, validates the request, estimates the EMI, "
        "predicts debt-to-income (DTI) ratios, and emits an approval or rejection explanation."
    )
    pdf.paragraph(
        "The domain is industrially relevant because straight-through processing engines and "
        "loan origination systems demand auditable logic. Functional programming strengths—"
        "pure functions, algebraic data types, and ability to evaluate multiple scenarios in "
        "parallel—mirror the resilience and throughput needs of such production systems."
    )

    pdf.heading("Functional Design", size=14)
    pdf.paragraph(
        "The system is decomposed into small, pure functions housed in dedicated modules:"
    )
    pdf.code_line("emiAmount :: LoanRequest -> Double", "pure EMI computation using recursion")
    pdf.code_line("emiSummary :: LoanRequest -> EmiResult", "aggregates EMI, total payback, interest")
    pdf.code_line("validateApplicant :: Applicant -> [String]", "returns structural issues only")
    pdf.code_line("validateLoan :: LoanRequest -> [String]", "guards unrealistic rates/tenure")
    pdf.code_line(
        "decideEligibility :: Applicant -> LoanRequest -> (Decision, Maybe EligibilityMetrics)",
        "central decision pipeline"
    )
    pdf.code_line(
        "batchEvaluate :: [LoanScenario] -> [ScenarioEvaluation]",
        "parallel map over immutable scenarios"
    )
    pdf.code_line(
        "pow :: Double -> Int -> Double",
        "recursive exponentiation used by emiAmount"
    )
    pdf.paragraph(
        "DataTypes.hs captures the domain via algebraic data types such as Applicant, LoanRequest, "
        "Decision, EmiResult, EligibilityMetrics, LoanScenario, and ScenarioEvaluation. These ADTs "
        "make illegal states unrepresentable while also deriving NFData for parallel evaluation."
    )

    pdf.heading("Application of FP Concepts", size=14)
    pdf.bullet(
        "Purity & Referential Transparency: Processing.emiSummary, Processing.decideEligibility, "
        "and Processing.batchEvaluate are pure and deterministic. Given the same Applicant and "
        "LoanRequest they always return the same decision and metrics."
    )
    pdf.bullet(
        "Recursion & Higher-order Functions: pow demonstrates explicit recursion, while "
        "batchEvaluate relies on the higher-order parMap from Control.Parallel.Strategies to "
        "evaluate multiple scenarios concurrently."
    )
    pdf.bullet(
        "Algebraic Data Types & Pattern Matching: Decision encapsulates Approved vs. Rejected "
        "outcomes together with explanatory strings, enabling the IO layer to pattern match and "
        "produce human-readable narratives."
    )
    pdf.bullet(
        "Immutability & Pipeline Composition: Validation, EMI calculation, affordability metrics, "
        "and DTI checks form a pipeline where each stage receives immutable inputs and produces "
        "new values without side effects."
    )
    pdf.bullet(
        "Parallelism: batchEvaluate orchestrates parMap rdeepseq so that demo scenarios can be "
        "evaluated independently, showcasing the ease of exploiting multicore CPUs from pure code."
    )

    pdf.heading("Expected Outputs & Workflow", size=14)
    pdf.paragraph(
        "Interactive Flow: The CLI collects monthly income, expenses, existing EMIs, and credit "
        "band information, followed by the desired loan amount, interest rate, and tenure. The "
        "system prints approval/rejection together with EMI, total payoff, disposable income "
        "before/after the loan, and the projected DTI percentage."
    )
    pdf.paragraph(
        "Parallel Demo: Users can trigger a scenario simulator that evaluates four contrasting "
        "applicants simultaneously. The output lists each scenario label, applicant profile, "
        "result, and affordability metrics, proving that parallel evaluation yields the same "
        "deterministic answers as the single-scenario pipeline."
    )

    pdf.heading("Possible Extensions", size=14)
    pdf.bullet(
        "Persist applicant histories in a SQLite/PostgreSQL layer and stream them through the "
        "same Processing.decideEligibility pipeline for audit trails."
    )
    pdf.bullet(
        "Import CSV batches and reuse batchEvaluate to produce entire day-level decisions."
    )
    pdf.bullet(
        "Integrate risk-based pricing by tweaking annualRate using scorecards derived from the "
        "Applicant and LoanRequest ADTs."
    )
    pdf.bullet(
        "Expose the pure logic as a servant/Yesod web API while keeping Testing via HSpec or "
        "QuickCheck to assert EMI correctness."
    )

    pdf.heading("Reliability & Concurrency Discussion", size=14)
    pdf.paragraph(
        "Pure functions make correctness reasoning straightforward: EMI calculations and DTI "
        "ratios rely solely on their inputs, so reproducibility and unit testing are trivial. "
        "Because the processing pipeline is side-effect free, it can be exercised concurrently "
        "without locks or shared state. The parallel batch evaluator illustrates how immutable "
        "LoanScenario values can be mapped across CPU cores (parMap rdeepseq) to deliver higher "
        "throughput without changing business logic."
    )
    pdf.paragraph(
        "Rejection reasons are deterministic lists, yielding auditable artifacts that align with "
        "industrial compliance expectations. FP's structural typing and ADTs prevent malformed "
        "cases—tenure cannot be negative, credit bands are constrained to a closed set, and "
        "validation functions detect inconsistent numbers before the EMI math runs."
    )

    pdf.save()


if __name__ == "__main__":
    build_report()
