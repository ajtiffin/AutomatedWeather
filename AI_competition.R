# AI Report Generator
# Uses WITSReport.R to fetch and format AI developments

# Load environment variables
readRenviron("~/.Renviron")

# Source the tool
source("WITSReport.R")

# Generate the report
cat("Fetching AI related developments from our peers...\n\n")

result <- wits_report(
  query = "Identify all genuinely AI-focused outputs (reports, working papers, blogs, speeches, press releases, seminars/events with substantive AI content) for major international financial institutions (IMF, World Bank Group, OECD, BIS) covering the past 7 days. Ignore items where AI is only tangentially mentioned. For each item, provide:

Institution

Title

Link

Publication or event date

2–3 sentence summary emphasizing (i) the AI angle and (ii) key policy or analytical takeaway",
  output_file = "AI_competition_report.qmd",
  title = "Keeping Track of the Competition - Weekly Briefing",
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
  system("quarto render AI_competition_report.qmd --to typst")
  cat("PDF created: AI_competition_report.pdf\n")
} else {
  cat("Error:", result$error, "\n")
}
