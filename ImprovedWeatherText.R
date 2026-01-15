# ImprovedWeatherText.R
# Generates two short paragraphs for WeatherWiTs.qmd:
# 1. Monetary policy, inflation, and debt developments (past week)
# 2. Currency and commodity developments (past week)
# Each with Economist-style punchy side headings

library(jsonlite)
library(httr2)

# API Query Functions -----------------------------------------------------

query_perplexity <- function(prompt, model = "sonar-pro") {
  api_key <- Sys.getenv("PERPLEXITY_API_KEY")
  if (!nzchar(api_key)) {
    stop("Set PERPLEXITY_API_KEY in your environment")
  }

  response <- request("https://api.perplexity.ai/chat/completions") |>
    req_headers(
      "Authorization" = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    ) |>
    req_body_json(list(
      model = model,
      messages = list(
        list(role = "user", content = prompt)
      ),
      temperature = 0.2,
      max_tokens = 1000
    )) |>
    req_perform()

  result <- response |> resp_body_json()
  return(result$choices[[1]]$message$content)
}

query_gemini <- function(prompt, model = "gemini-2.5-flash") {
  api_key <- Sys.getenv("GEMINI_API_KEY")
  if (!nzchar(api_key)) {
    stop("Set GEMINI_API_KEY in your environment")
  }

  api_url <- paste0(
    "https://generativelanguage.googleapis.com/v1beta/models/",
    model, ":generateContent?key=", api_key
  )

  response <- request(api_url) |>
    req_method("POST") |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(list(
      contents = list(
        list(parts = list(list(text = prompt)))
      ),
      generationConfig = list(
        temperature = 0.3,
        maxOutputTokens = 500
      )
    )) |>
    req_perform()

  result <- response |> resp_body_json()
  return(result$candidates[[1]]$content$parts[[1]]$text)
}

# Load context data from charts -------------------------------------------

load("contextJSONs.rdta")

# Paragraph 1: Monetary Policy, Inflation, and Debt -----------------------

policy_debt_prompt <- paste0(
  "You are a senior economist at the International Monetary Fund writing a weekly ",
  "briefing on sub-Saharan Africa. Today's date is ", format(Sys.Date(), "%B %d, %Y"), ". ",
  "Write a single concise paragraph (60-80 words) covering monetary policy, inflation, ",
  "and sovereign debt developments in sub-Saharan Africa.\n\n",
  "CRITICAL: Only include events from ", format(Sys.Date() - 7, "%B %d"), " to ",
  format(Sys.Date(), "%B %d, %Y"), ". Verify all dates. Exclude anything older.\n\n",
  "Guidelines:\n",
  "- Prioritize significance: lead with the most impactful development\n",
  "- Focusing on sub-Saharan Africa, include central bank rate decisions, inflation announcements, or major debt news\n",
  "- Check Reuters Africa, cbrates.com, and official central bank sites\n",
  "- Use the spread movement data below to inform your narrative where relevant\n",
  "- NEVER comment on data availability or lack of news—just report what happened\n",
  "- Wrap ALL country names in double asterisks for bold (e.g., **Kenya**, **South Africa**)\n",
  "- No reference markers\n",
  "- Factual and terse—no speculation or meta-commentary\n\n",
  "Context on recent spread movements:\n",
  spread.context
)

para1_raw <- query_perplexity(policy_debt_prompt, model = "sonar-pro")

# Clean and finalize paragraph 1
para1 <- query_perplexity(
  paste0(
    "Clean this paragraph for publication. Remove reference markers like [1] ",
    "and any speculative language. Remove any sentences about data availability, ",
    "lack of news, or search limitations—focus only on actual developments. ",
    "IMPORTANT: Ensure all country names are wrapped in double asterisks for ",
    "bold formatting (e.g., **Kenya**, **Nigeria**). ",
    "Keep to 60-80 words. Return only the cleaned paragraph:\n\n",
    para1_raw
  ),
  model = "sonar-pro"
)

# Generate Economist-style headline for paragraph 1
headline1 <- query_gemini(
  paste0(
    "Write a punchy, witty headline (maximum 10 words) for this paragraph, ",
    "in the style of The Economist magazine. The headline should capture the ",
    "key theme with clever wordplay or a sharp observation. No quotation marks. ",
    "No markdown. Just the headline:\n\n",
    para1
  )
)
headline1 <- trimws(gsub("[\r\n]", "", headline1))
headline1 <- gsub("\\*+$", "", headline1)  # Remove trailing asterisks

# Paragraph 2: Currency and Commodity Developments ------------------------

currency_commodity_prompt <- paste0(
  "You are a senior economist at the International Monetary Fund writing a weekly ",
  "briefing on sub-Saharan Africa. Today's date is ", format(Sys.Date(), "%B %d, %Y"), ". ",
  "Write a single concise paragraph (50-70 words) covering currency and commodity market ",
  "developments affecting sub-Saharan Africa.\n\n",
  "CRITICAL: Only include events from ", format(Sys.Date() - 7, "%B %d"), " to ",
  format(Sys.Date(), "%B %d, %Y"), ". Verify all dates. Exclude anything older.\n\n",
  "Guidelines:\n",
  "- Prioritize significance: lead with the most impactful development\n",
  "- Focus on major currency movements, commodity price shifts affecting the region\n",
  "- Emphasize trends over specific numbers\n",
  "- Use the exchange rate and commodity data below to inform your narrative\n",
  "- NEVER comment on data availability or lack of news—just report what happened\n",
  "- Wrap ALL country names in double asterisks for bold (e.g., **Kenya**, **South Africa**)\n",
  "- No reference markers\n",
  "- Factual and terse—no speculation or meta-commentary\n\n",
  "Context on recent exchange rate movements:\n",
  er.context, "\n\n",
  "Context on recent commodity price movements:\n",
  com.context
)

para2_raw <- query_perplexity(currency_commodity_prompt, model = "sonar-pro")

# Clean and finalize paragraph 2
para2 <- query_perplexity(
  paste0(
    "Clean this paragraph for publication. Remove reference markers like [1] ",
    "and any speculative language. Remove any sentences about data availability, ",
    "lack of news, or search limitations—focus only on actual developments. ",
    "IMPORTANT: Ensure all country names are wrapped in double asterisks for ",
    "bold formatting (e.g., **Kenya**, **Nigeria**). ",
    "Keep to 50-70 words. Return only the cleaned paragraph:\n\n",
    para2_raw
  ),
  model = "sonar-pro"
)

# Generate Economist-style headline for paragraph 2
headline2 <- query_gemini(
  paste0(
    "Write a punchy, witty headline (maximum 10 words) for this paragraph, ",
    "in the style of The Economist magazine. The headline should capture the ",
    "key theme with clever wordplay or a sharp observation. No quotation marks. ",
    "No markdown. Just the headline:\n\n",
    para2
  )
)
headline2 <- trimws(gsub("[\r\n]", "", headline2))
headline2 <- gsub("\\*+$", "", headline2)  # Remove trailing asterisks

# Capital Flows Summary ---------------------------------------------------

capital_prompt <- paste0(
  "You are a senior economist at the International Monetary Fund. Today's date is ",
  format(Sys.Date(), "%B %d, %Y"), ". Provide a brief factual summary (2-3 sentences, ",
  "maximum 40 words) on portfolio capital flows into sub-Saharan Africa.\n\n",
  "CRITICAL: Only include events from ", format(Sys.Date() - 7, "%B %d"), " to ",
  format(Sys.Date(), "%B %d, %Y"), ". Verify all dates. Exclude anything older.\n\n",
  "Guidelines:\n",
  "- Focus on EPFR data, ETF inflows/outflows, and emerging-market fund allocations\n",
  "- Mention whether flows were positive or negative, and any notable shifts\n",
  "- Compare to recent trends if relevant (e.g., 'continuing a three-week streak')\n",
  "- NEVER comment on data availability or lack of news—just report what happened\n",
  "- If specific SSA data is limited, report on broader EM flows affecting the region\n",
  "- No reference markers\n",
  "- Factual and terse—no speculation or meta-commentary\n"
)

capital_raw <- query_perplexity(capital_prompt, model = "sonar-pro")

# Clean capital summary
capital <- query_perplexity(
  paste0(
    "Clean this text for publication. Remove reference markers like [1]. ",
    "Remove any sentences about data availability, lack of news, or search limitations. ",
    "CRITICAL: Remove any data or events from before ", format(Sys.Date() - 7, "%B %d, %Y"),
    ". Only keep information from the past 7 days. ",
    "Keep to 2-3 sentences maximum. Return only the cleaned text:\n\n",
    capital_raw
  ),
  model = "sonar-pro"
)

# Generate Economist-style headline for capital flows
capital_sq <- query_gemini(
  paste0(
    "Write a punchy, witty headline (maximum 12 words) for this capital flows ",
    "summary, in the style of The Economist magazine. The headline must refer ",
    "to sub-Saharan Africa specifically. Use clever wordplay or a sharp observation. ",
    "No quotation marks. No markdown. Just the headline:\n\n",
    capital
  )
)
capital_sq <- trimws(gsub("[\r\n]", "", capital_sq))
capital_sq <- gsub("\\*+$", "", capital_sq)  # Remove trailing asterisks

# Output ------------------------------------------------------------------

cat("\n===== PARAGRAPH 1: MONETARY POLICY, INFLATION & DEBT =====\n")
cat("Headline:", headline1, "\n\n")
cat(para1, "\n")

cat("\n===== PARAGRAPH 2: CURRENCY & COMMODITIES =====\n")
cat("Headline:", headline2, "\n\n")
cat(para2, "\n")

cat("\n===== CAPITAL FLOWS =====\n")
cat("Headline:", capital_sq, "\n\n")
cat(capital, "\n")

# Save for use in WeatherWiTs.qmd -----------------------------------------

# Using same variable names as original for compatibility
summary <- para1
summary_sq <- headline1
summary2 <- para2
summary2_sq <- headline2

save(summary, summary_sq, summary2, summary2_sq, capital_sq, file = "weathertext.rdta")

cat("\n===== Saved to weathertext.rdta =====\n")
