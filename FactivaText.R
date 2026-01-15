# FactivaText.R
# Generates two short paragraphs for WeatherWiTs.qmd using Factiva news:
# 1. Monetary policy, inflation, and debt developments (past week)
# 2. Currency and commodity developments (past week)
# Each with Economist-style punchy side headings
# Uses Gemini for summarization instead of Perplexity search

library(jsonlite)
library(httr2)

# RTF Parsing Function --------------------------------------------------------

parse_rtf <- function(rtf_file) {
  # Read the RTF file
  rtf_content <- readLines(rtf_file, warn = FALSE, encoding = "UTF-8")
  rtf_text <- paste(rtf_content, collapse = "\n")

  # Remove RTF header/control sequences
  text <- rtf_text

  # Remove binary image data (hex encoded)
  text <- gsub("[0-9A-F]{50,}", "", text, perl = TRUE)

  # Remove RTF control words with arguments
  text <- gsub("\\\\[a-z]+[-]?[0-9]*[ ]?", " ", text, perl = TRUE)

  # Remove curly braces and remaining backslashes
  text <- gsub("[{}\\\\]", " ", text)

  # Remove special characters and clean up
  text <- gsub("\\*", "", text)
  text <- gsub("'[0-9a-fA-F]{2}", "", text) # Remove hex escapes like 'a9

  # Clean up whitespace
  text <- gsub("[ \t]+", " ", text)
  text <- gsub("\n[ ]+", "\n", text)
  text <- gsub("\n{3,}", "\n\n", text)

  # Extract meaningful content (news articles)
  # Split by page breaks and document markers
  articles <- strsplit(text, "Document [A-Z0-9]+")[[1]]

  # Clean each article
  articles <- sapply(
    articles,
    function(a) {
      a <- trimws(a)
      # Keep only articles with substantial content
      if (nchar(a) > 100) {
        return(a)
      }
      return(NULL)
    },
    USE.NAMES = FALSE
  )

  articles <- articles[!sapply(articles, is.null)]

  return(articles) # Return list of articles instead of combined text
}

# Chunk Processing Function ---------------------------------------------------

summarize_in_chunks <- function(articles, chunk_size = 10000) {
  # Group articles into chunks that fit within the character limit
  chunks <- list()
  current_chunk <- ""

  for (article in articles) {
    if (nchar(current_chunk) + nchar(article) + 10 > chunk_size) {
      if (nchar(current_chunk) > 0) {
        chunks <- c(chunks, current_chunk)
      }
      current_chunk <- article
    } else {
      if (nchar(current_chunk) > 0) {
        current_chunk <- paste0(current_chunk, "\n\n---\n\n", article)
      } else {
        current_chunk <- article
      }
    }
  }

  # Add the last chunk
  if (nchar(current_chunk) > 0) {
    chunks <- c(chunks, current_chunk)
  }

  return(chunks)
}

extract_key_points <- function(chunk, topic) {
  prompt <- paste0(
    "Extract key facts from the following news articles related to ",
    topic,
    " in sub-Saharan Africa. Return only bullet points of factual information. ",
    "Be thorough - include all relevant details. Include country names, specific numbers, ",
    "and dates where mentioned. Prioritize central bank rate decisions.\n\n",
    chunk
  )

  result <- query_gemini(prompt, max_tokens = 2048)
  return(trimws(result))
}

# API Query Function ----------------------------------------------------------

query_gemini <- function(
  prompt,
  model = "gemini-3-flash-preview",
  max_tokens = 8192
) {
  api_key <- Sys.getenv("GEMINI_API_KEY")
  if (!nzchar(api_key)) {
    stop("Set GEMINI_API_KEY in your environment")
  }

  api_url <- paste0(
    "https://generativelanguage.googleapis.com/v1beta/models/",
    model,
    ":generateContent?key=",
    api_key
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
        maxOutputTokens = max_tokens
      )
    )) |>
    req_perform()

  result <- response |> resp_body_json()

  # Check for truncation
  finish_reason <- result$candidates[[1]]$finishReason
  if (!is.null(finish_reason) && finish_reason == "MAX_TOKENS") {
    warning("Response was truncated due to max tokens limit")
  }

  text <- result$candidates[[1]]$content$parts[[1]]$text

  # Clean up any model "thinking" artifacts
  # Remove everything after common thinking markers
  text <- gsub("\\n\\s*\\*\\s*\\*?Wait.*$", "", text, perl = TRUE)
  text <- gsub("\\n\\s*Final Polish:.*$", "", text, perl = TRUE)
  text <- gsub("\\n\\s*\\*\\s+\\*.*$", "", text, perl = TRUE)
  text <- gsub("\\n\\s*Word count.*$", "", text, perl = TRUE, ignore.case = TRUE)
  text <- gsub("\\n\\s*Final check.*$", "", text, perl = TRUE, ignore.case = TRUE)
  text <- gsub("\\n\\s*Everything looks.*$", "", text, perl = TRUE, ignore.case = TRUE)
  text <- gsub("\\n\\s*[a-z]+\\}\\}.*$", "", text, perl = TRUE)  # Remove incomplete code
  text <- gsub("\\n\\s*anzania\\*\\*.*$", "", text, perl = TRUE)  # Remove truncated content

  return(text)
}

# Load Data -------------------------------------------------------------------

# Load context data from charts
load("contextJSONs.rdta")

# Parse Factiva news content into individual articles
cat("Parsing Factiva.rtf...\n")
articles <- parse_rtf("Factiva.rtf")
total_chars <- sum(sapply(articles, nchar))
cat(
  "Extracted",
  length(articles),
  "articles (",
  total_chars,
  "characters total)\n\n"
)

# Process articles in chunks for each topic
cat("Processing articles in chunks...\n")

# Create chunks for processing
chunks <- summarize_in_chunks(articles, chunk_size = 50000)
cat("Split into", length(chunks), "chunks for processing\n\n")

# Extract key points from each chunk for monetary policy/debt topic
cat("Extracting key points for monetary policy & debt...\n")
policy_points <- sapply(chunks, function(chunk) {
  extract_key_points(
    chunk,
    "monetary policy, inflation, interest rates, and sovereign debt"
  )
})
policy_summary <- paste(policy_points, collapse = "\n\n")

# Extract key points for currency/commodities topic
cat("Extracting key points for currency & commodities...\n")
currency_points <- sapply(chunks, function(chunk) {
  extract_key_points(
    chunk,
    "currency movements, exchange rates, and commodity prices"
  )
})
currency_summary <- paste(currency_points, collapse = "\n\n")

# Extract key points for capital flows
cat("Extracting key points for capital flows...\n")
capital_points <- sapply(chunks, function(chunk) {
  extract_key_points(
    chunk,
    "capital flows, bond issuances, investor sentiment, and financing deals"
  )
})
capital_summary <- paste(capital_points, collapse = "\n\n")

cat("\n")

# Paragraph 1: Monetary Policy, Inflation, and Debt ---------------------------

para1_prompt <- paste0(
  "You are a senior economist at the International Monetary Fund writing a weekly ",
  "briefing on sub-Saharan Africa. Today's date is ",
  format(Sys.Date(), "%B %d, %Y"),
  ".\n\n",
  "Based on the key facts below, write a single concise paragraph (50-80 words) ",
  "covering monetary policy, inflation, and sovereign debt developments in sub-Saharan Africa.\n\n",
  "Guidelines:\n",
  "- PRIORITY: Always lead with any central bank interest rate decisions - these are the most important developments\n",
  "- Secondary: inflation announcements, major debt news, and spread movements\n",
  "- Use the spread movement data to support the narrative where relevant\n",
  "- Wrap ALL country names in double asterisks for bold (e.g., **Kenya**, **South Africa**)\n",
  "- Factual and terse - no speculation or meta-commentary\n",
  "- Do NOT mention sources\n\n",
  "KEY FACTS FROM NEWS:\n",
  policy_summary,
  "\n\n",
  "SPREAD MOVEMENT DATA:\n",
  spread.context
)

cat("Generating paragraph 1 (Monetary Policy, Inflation & Debt)...\n")
para1 <- query_gemini(para1_prompt, model = "gemini-3-flash-preview")
para1 <- trimws(para1)

# Generate headline for paragraph 1
headline1 <- query_gemini(
  paste0(
    "Write a punchy, witty headline (maximum 10 words) for this paragraph, ",
    "in the style of The Economist magazine. The headline should capture the ",
    "key theme with clever wordplay or a sharp observation. No quotation marks. ",
    "No markdown. Just the headline:\n\n",
    para1
  ),
  model = "gemini-3-flash-preview"
)
headline1 <- trimws(gsub("[\r\n]", "", headline1))
headline1 <- gsub("\\*+$", "", headline1)

# Paragraph 2: Currency and Commodity Developments ----------------------------

para2_prompt <- paste0(
  "You are a senior economist at the International Monetary Fund writing a weekly ",
  "briefing on sub-Saharan Africa. Today's date is ",
  format(Sys.Date(), "%B %d, %Y"),
  ".\n\n",
  "Based on the key facts below, write a single concise paragraph (35-50 words) ",
  "covering currency and commodity market developments affecting sub-Saharan Africa.\n\n",
  "Guidelines:\n",
  "- Prioritize significance: lead with the most impactful development\n",
  "- Focus on major currency movements, commodity price shifts affecting the region\n",
  "- Emphasize trends over specific numbers\n",
  "- Use the exchange rate and commodity data to inform your narrative\n",
  "- Wrap ALL country names in double asterisks for bold (e.g., **Kenya**, **South Africa**)\n",
  "- Factual and terse - no speculation or meta-commentary\n",
  "- Do NOT mention sources\n\n",
  "KEY FACTS FROM NEWS:\n",
  currency_summary,
  "\n\n",
  "EXCHANGE RATE DATA:\n",
  er.context,
  "\n\n",
  "COMMODITY PRICE DATA:\n",
  com.context
)

cat("Generating paragraph 2 (Currency & Commodities)...\n")
para2 <- query_gemini(para2_prompt, model = "gemini-3-flash-preview")
para2 <- trimws(para2)

# Generate headline for paragraph 2
headline2 <- query_gemini(
  paste0(
    "Write a punchy, witty headline (maximum 10 words) for this paragraph, ",
    "in the style of The Economist magazine. The headline should capture the ",
    "key theme with clever wordplay or a sharp observation. No quotation marks. ",
    "No markdown. Just the headline:\n\n",
    para2
  ),
  model = "gemini-3-flash-preview"
)
headline2 <- trimws(gsub("[\r\n]", "", headline2))
headline2 <- gsub("\\*+$", "", headline2)

# Capital Flows Summary -------------------------------------------------------

capital_prompt <- paste0(
  "You are a senior economist at the International Monetary Fund. Today's date is ",
  format(Sys.Date(), "%B %d, %Y"),
  ".\n\n",
  "Based on the key facts below, provide a brief factual summary (1-2 sentences, ",
  "maximum 25 words) on portfolio capital flows into sub-Saharan Africa.\n\n",
  "Guidelines:\n",
  "- Focus on any mentions of bond issuances, investor flows, or financing deals\n",
  "- If specific SSA capital flow data is limited, report on broader EM flows or financing news\n",
  "- Factual and terse - no speculation or meta-commentary\n",
  "- Do NOT mention sources\n\n",
  "KEY FACTS FROM NEWS:\n",
  capital_summary
)

cat("Generating capital flows summary...\n")
capital <- query_gemini(capital_prompt, model = "gemini-3-flash-preview")
capital <- trimws(capital)

# Generate headline for capital flows
capital_sq <- query_gemini(
  paste0(
    "Write a punchy, witty headline (maximum 12 words) for this capital flows ",
    "summary, in the style of The Economist magazine. The headline must refer ",
    "to sub-Saharan Africa specifically. Use clever wordplay or a sharp observation. ",
    "No quotation marks. No markdown. Just the headline:\n\n",
    capital
  ),
  model = "gemini-3-flash-preview"
)
capital_sq <- trimws(gsub("[\r\n]", "", capital_sq))
capital_sq <- gsub("\\*+$", "", capital_sq)

# Output ----------------------------------------------------------------------

cat("\n===== PARAGRAPH 1: MONETARY POLICY, INFLATION & DEBT =====\n")
cat("Headline:", headline1, "\n\n")
cat(para1, "\n")

cat("\n===== PARAGRAPH 2: CURRENCY & COMMODITIES =====\n")
cat("Headline:", headline2, "\n\n")
cat(para2, "\n")

cat("\n===== CAPITAL FLOWS =====\n")
cat("Headline:", capital_sq, "\n\n")
cat(capital, "\n")

# Save for use in WeatherWiTs.qmd ---------------------------------------------

# Escape dollar signs for Typst compatibility ($ starts math mode)
escape_for_typst <- function(text) {
  gsub("\\$", "\\\\$", text)
}

# Using same variable names as original for compatibility
summary <- escape_for_typst(para1)
summary_sq <- escape_for_typst(headline1)
summary2 <- escape_for_typst(para2)
summary2_sq <- escape_for_typst(headline2)
capital_sq <- escape_for_typst(capital_sq)

save(
  summary,
  summary_sq,
  summary2,
  summary2_sq,
  capital_sq,
  file = "weathertext.rdta"
)

cat("\n===== Saved to weathertext.rdta =====\n")
