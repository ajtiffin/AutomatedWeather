# SACU Trade Report Generator
# Uses WITSReport.R to fetch and format SACU trade developments

# Load environment variables
readRenviron("~/.Renviron")

# Source the tool
source("WITSReport.R")

# Generate the report
cat("Fetching trade-related developments in SACU region...\n\n")

result <- wits_report(
  query = "What are the recent trade developments in the Southern African Customs Union (SACU) region? in the past week",
  output_file = "sacu_trade_report.qmd",
  title = "SACU Trade Developments - Weekly Briefing",
  num_findings = 10,
  group_by_theme = FALSE,
  options = list(
    search_recency_filter = "week"
  )
)

# Check result
if (result$success) {
  cat("\nReport generated successfully!\n")
  cat("File:", result$file_path, "\n")
  cat("Findings:", result$findings_count, "\n")
  cat("Citations:", result$citations_count, "\n")
  cat("Tokens used:", result$tokens_used, "\n")

  # Render to PDF
  cat("\nRendering to PDF...\n")
  system("quarto render sacu_trade_report.qmd --to typst")
  cat("PDF created: sacu_trade_report.pdf\n")
} else {
  cat("Error:", result$error, "\n")
}
