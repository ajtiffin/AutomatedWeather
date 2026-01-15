# AI Weekly Report Generator
# Uses WITSReport.R to fetch and format SACU trade developments

# Load environment variables
readRenviron("~/.Renviron")

# Source the tool
source("WITSReport.R")

# Generate the report
cat("Fetching AI events or publications...\n\n")

result <- wits_report(
  query = "New Chat. Identify and summarize all substantive, AI-focused publications and events released between in the past 7 days by key specific International Financial Institutions (IFIs), such as: 
1. International Monetary Fund (IMF)
2. World Bank 
3. Organisation for Economic Co-operation and Development (OECD)
4. Bank for International Settlements (BIS) - Note: Do NOT confuse with the US Bureau of Industry and Security.
5. World Economic Forum (WEF)
6. US Federal Reserve (FED)
7. Bank of England (BoE)
8. European Central Bank (ECB)

If these institutions don't have any publications or events within the timeframe, there is no need to specify that they don't have any publications to report.

### SCANNING CRITERIA
- Include: Reports, Working Papers, Staff Discussion Notes, official Blogs (e.g., IMF Blog, OECD.AI Wonk), Speeches, Press Releases, and Seminars/Webinars.
- Content Threshold: Only include items where AI is the primary focus or a significant, dedicated section (e.g., an AI chapter in a flagship report like the 'Global Economic Prospects'). 
- Ignore: Items where AI is only mentioned tangentially (e.g., a general list of risks where AI is just one word among many).
- Do not report an absence of reporting if nothing can be found for a specific institution
- If possible try to include reports from a range of institutions, not just the IMF",
  output_file = "AI_weekly_report.qmd",
  title = "AI Publications - Weekly Briefing",
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
  system("quarto render AI_weekly_report.qmd --to typst")
  cat("PDF created: AI_weekly_report.pdf\n")
} else {
  cat("Error:", result$error, "\n")
}
