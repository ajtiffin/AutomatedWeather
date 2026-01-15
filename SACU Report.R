# Script to source WITSReport and search for SACU economic news
# Date: January 6, 2026

# Source the WITSReport functionality
source("WITSReport.R")

# Check if Perplexity API key is set
if (Sys.getenv("PERPLEXITY_API_KEY") == "") {
  stop("Please set PERPLEXITY_API_KEY environment variable before running this script")
}

# Define SACU countries
sacu_countries <- c("South Africa", "Botswana", "Lesotho", "Eswatini", "Namibia")

# Create comprehensive query for today's economic news from SACU region
today_date <- format(Sys.Date(), "%B %d, %Y")
query <- paste0(
  "What are today's top economic news stories from SACU countries (South Africa, Botswana, Lesotho, Eswatini, Namibia) for ", 
  today_date, 
  "? Include developments in trade, monetary policy, markets, inflation, GDP, and business news."
)

cat("Searching for economic news from SACU countries...\n")
cat("Query:", query, "\n\n")

# Set search options for recent news
search_options <- list(
  model = "sonar",
  search_recency_filter = "day",  # Focus on today's news
  temperature = 0.3,              # Lower temperature for factual reporting
  max_tokens = 2000               # Sufficient tokens for detailed response
)

# Generate filename with today's date
output_filename <- paste0("sacu_economic_news_", format(Sys.Date(), "%Y%m%d"), ".qmd")

# Execute the search and generate WiTs report
result <- mcp_perplexity_wits_report(
  query = query,
  output_file = output_filename,
  options = search_options
)

# Display results
if (result$success) {
  cat("✓ Success! SACU economic news report generated:\n")
  cat("  File:", result$file_path, "\n")
  cat("  Citations found:", result$citations_count, "\n")
  cat("  Tokens used:", result$tokens_used %||% "Unknown", "\n\n")
  
  cat("To render the report to PDF:\n")
  cat("quarto render", result$file_path, "\n")
  
  # Optionally read and display first few lines of the generated file
  if (file.exists(result$file_path)) {
    cat("\n--- Report Preview ---\n")
    preview_lines <- readLines(result$file_path, n = 20)
    cat(paste(preview_lines, collapse = "\n"))
    cat("\n--- End Preview ---\n")
  }
  
} else {
  cat("✗ Error generating report:\n")
  cat(" ", result$error, "\n")
}

# Additional function to search for specific SACU country
search_sacu_country_news <- function(country, date = Sys.Date()) {
  if (!country %in% sacu_countries) {
    stop("Country must be one of: ", paste(sacu_countries, collapse = ", "))
  }
  
  date_str <- format(date, "%B %d, %Y")
  country_query <- paste0(
    "What are the latest economic developments in ", country, " for ", date_str, 
    "? Include monetary policy, trade, business news, market updates, and economic indicators."
  )
  
  filename <- paste0(tolower(gsub(" ", "_", country)), "_economic_news_", 
                    format(date, "%Y%m%d"), ".qmd")
  
  result <- mcp_perplexity_wits_report(
    query = country_query,
    output_file = filename,
    options = search_options
  )
  
  return(result)
}

cat("\nScript completed. Use search_sacu_country_news('Country Name') for individual country reports.\n")