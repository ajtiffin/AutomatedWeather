# WiTs Report Generator
# Queries Perplexity and formats responses as WiTs-style Typst PDFs

library(httr2)
library(jsonlite)
library(stringr)

# Helper function for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x

# Helper function to escape content for Typst
escape_typst <- function(text) {
  text <- gsub("\\*+", "", text)                    # Remove markdown bold/italic
  text <- gsub("^#+\\s*", "", text)                 # Remove markdown headers
  text <- gsub("\\n#+\\s*", " ", text)
  text <- gsub("\\(\\d+\\)", "", text, perl = TRUE) # Remove citations (1), (2)
  text <- gsub("\\[\\d+\\]", "", text, perl = TRUE) # Remove citations [1], [2]
  text <- gsub("\\s+", " ", text, perl = TRUE)      # Collapse whitespace
  text <- gsub("\\.\\s*\\.", ".", text)             # Clean double periods
  text <- gsub('\\', '\\\\', text, fixed = TRUE)    # Escape backslashes
  text <- gsub('"', '\\"', text, fixed = TRUE)      # Escape quotes
  text <- gsub('\n', ' ', text)
  text <- gsub('\r', '', text)
  text <- gsub('[', '(', text, fixed = TRUE)
  text <- gsub(']', ')', text, fixed = TRUE)
  trimws(text)
}

# Main function
wits_report <- function(
  query,
  output_file = NULL,
  title = NULL,
  num_findings = 5,
  group_by_theme = FALSE,
  options = list()
) {
  # Check for API key
  api_key <- Sys.getenv("PERPLEXITY_API_KEY")
  if (api_key == "") {
    return(list(success = FALSE, error = "PERPLEXITY_API_KEY not set"))
  }

  # Generate output filename if not provided
  if (is.null(output_file)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    output_file <- paste0("wits_report_", timestamp, ".qmd")
  }
  if (!grepl("\\.qmd$", output_file)) {
    output_file <- paste0(output_file, ".qmd")
  }

  # Build system prompt based on parameters
  if (group_by_theme) {
    system_prompt <- sprintf(
      "Provide exactly %d key findings organized by theme. Group related items under theme headings.

For each finding:
THEME: [Theme Name]
HEADLINE: [Short headline, max 50 chars]
SUMMARY: [2-3 sentence summary]

Keep headlines punchy. Summaries should be informative and complete.",
      num_findings
    )
  } else {
    system_prompt <- sprintf(
      "Provide exactly %d key findings, ranked by importance.

For each finding:
HEADLINE: [Short headline, max 50 chars]
SUMMARY: [2-3 sentence summary]

Keep headlines punchy. Summaries should be informative and complete.",
      num_findings
    )
  }

  # Query Perplexity
  cat("Querying Perplexity API...\n")

  body <- list(
    model = options$model %||% "sonar",
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = query)
    )
  )

  if (!is.null(options$search_recency_filter)) {
    body$search_recency_filter <- options$search_recency_filter
  }
  if (!is.null(options$temperature)) {
    body$temperature <- options$temperature
  }

  result <- tryCatch({
    resp <- request("https://api.perplexity.ai/chat/completions") |>
      req_headers(
        Authorization = paste("Bearer", api_key),
        "Content-Type" = "application/json"
      ) |>
      req_body_json(body) |>
      req_perform()

    resp_body_json(resp)
  }, error = function(e) {
    return(list(error = conditionMessage(e)))
  })

  if (!is.null(result$error)) {
    return(list(success = FALSE, error = result$error))
  }

  content <- result$choices[[1]]$message$content %||% ""
  citations <- result$citations %||% list()
  tokens_used <- result$usage$total_tokens %||% 0

  # Parse findings from response
  cat("Parsing findings...\n")
  findings <- parse_findings(content, group_by_theme)

  # Generate document
  cat("Creating WiTs document...\n")
  doc_title <- title %||% generate_title(query)
  date <- format(Sys.time(), "%B %d, %Y")

  qmd_content <- build_qmd(findings, doc_title, date, citations, group_by_theme)
  writeLines(qmd_content, output_file)

  return(list(
    success = TRUE,
    file_path = output_file,
    findings_count = length(findings),
    citations_count = length(citations),
    tokens_used = tokens_used
  ))
}

# Parse findings from Perplexity response
parse_findings <- function(content, group_by_theme = FALSE) {
  findings <- list()

  # Strategy 1: Look for HEADLINE:/SUMMARY: pairs
  if (grepl("HEADLINE:", content, ignore.case = TRUE)) {
    # Track current theme for themed parsing
    current_theme <- NULL

    # Split by HEADLINE: (case-insensitive)
    blocks <- strsplit(content, "(?i)HEADLINE:", perl = TRUE)[[1]]

    for (block in blocks) {
      block <- trimws(block)
      if (nchar(block) == 0) next

      # Check if block starts with theme info (from previous split)
      if (group_by_theme && grepl("THEME:", block, ignore.case = TRUE)) {
        theme_match <- regmatches(block, regexpr("THEME:[^\n]+", block, ignore.case = TRUE))
        if (length(theme_match) > 0) {
          current_theme <- gsub("THEME:\\s*", "", theme_match, ignore.case = TRUE)
          current_theme <- gsub("\\*+", "", current_theme)
          current_theme <- trimws(current_theme)
        }
      }

      # Skip if no SUMMARY
      if (!grepl("SUMMARY:", block, ignore.case = TRUE)) next

      # Split on SUMMARY: (case-insensitive)
      parts <- strsplit(block, "(?i)SUMMARY:", perl = TRUE)[[1]]
      if (length(parts) >= 2) {
        headline <- trimws(parts[1])
        headline <- gsub("\\*+", "", headline)
        headline <- gsub("THEME:.*", "", headline, ignore.case = TRUE)
        headline <- gsub("\n.*", "", headline)  # Take first line only
        headline <- trimws(headline)

        summary <- trimws(parts[2])
        summary <- gsub("\\*+", "", summary)
        # Take until next HEADLINE or THEME or separator
        summary <- gsub("(HEADLINE|THEME):.*", "", summary, ignore.case = TRUE)
        # Remove markdown markers and separators
        summary <- gsub("\\s*#+\\s*$", "", summary)
        summary <- gsub("\\s*##\\s+", " ", summary)
        summary <- gsub("\\s*---+\\s*", " ", summary)
        summary <- gsub("\n", " ", summary)
        summary <- trimws(gsub("\\s+", " ", summary))

        if (nchar(headline) > 2 && nchar(summary) > 10) {
          findings[[length(findings) + 1]] <- list(
            theme = current_theme,
            headline = headline,
            summary = summary
          )
        }
      }
    }
  }

  # Strategy 2: Numbered list fallback
  if (length(findings) == 0) {
    findings <- parse_numbered_list(content)
  }

  # Strategy 3: Bold header fallback
  if (length(findings) == 0) {
    findings <- parse_bold_headers(content)
  }

  findings
}

# Fallback parser for numbered lists
parse_numbered_list <- function(content) {
  findings <- list()
  lines <- strsplit(content, "\n")[[1]]

  current_headline <- NULL
  current_summary <- ""

  for (line in lines) {
    line <- trimws(line)
    if (nchar(line) == 0) next

    # Check for numbered item (1. or 1) format)
    if (grepl("^\\d+[.)]", line)) {
      # Save previous
      if (!is.null(current_headline) && nchar(current_headline) > 0) {
        findings[[length(findings) + 1]] <- list(
          theme = NULL,
          headline = current_headline,
          summary = trimws(current_summary)
        )
      }

      # Parse new item - remove number prefix
      line_content <- gsub("^\\d+[.)]\\s*", "", line)

      # Try to split on bold **Header**: or **Header** rest
      if (grepl("\\*\\*[^*]+\\*\\*", line_content)) {
        bold_match <- regmatches(line_content, regexpr("\\*\\*[^*]+\\*\\*", line_content))
        current_headline <- gsub("\\*\\*", "", bold_match)
        current_summary <- gsub("\\*\\*[^*]+\\*\\*[:\\s]*", "", line_content)
      } else if (grepl(":", line_content)) {
        # Split on first colon
        colon_pos <- regexpr(":", line_content)
        current_headline <- gsub("\\*+", "", trimws(substr(line_content, 1, colon_pos - 1)))
        current_summary <- trimws(substr(line_content, colon_pos + 1, nchar(line_content)))
      } else {
        # Whole line is content - use first few words as headline
        words <- strsplit(line_content, "\\s+")[[1]]
        if (length(words) > 5) {
          current_headline <- paste(words[1:5], collapse = " ")
          current_summary <- paste(words[-(1:5)], collapse = " ")
        } else {
          current_headline <- gsub("\\*+", "", line_content)
          current_summary <- ""
        }
      }
    } else if (!is.null(current_headline)) {
      # Continuation line
      current_summary <- paste(current_summary, gsub("\\*+", "", line))
    }
  }

  # Save last
  if (!is.null(current_headline) && nchar(current_headline) > 0) {
    findings[[length(findings) + 1]] <- list(
      theme = NULL,
      headline = current_headline,
      summary = trimws(current_summary)
    )
  }

  findings
}

# Fallback parser for bold headers
parse_bold_headers <- function(content) {
  findings <- list()

  # Find all **bold** patterns
  matches <- gregexpr("\\*\\*[^*]+\\*\\*", content)
  headers <- regmatches(content, matches)[[1]]
  headers <- gsub("\\*\\*", "", headers)

  # Filter to valid headers (not too short, not just numbers)
  headers <- headers[nchar(headers) >= 5]
  headers <- headers[!grepl("^[\\d.,/%$]+$", headers)]

  if (length(headers) == 0) return(findings)

  for (header in headers) {
    # Find content after this header
    escaped <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", header)
    pattern <- paste0("\\*\\*", escaped, "\\*\\*[:\\s]*")
    match_pos <- regexpr(pattern, content)

    if (match_pos > 0) {
      after <- substring(content, match_pos + attr(match_pos, "match.length"))
      # Get content until next ** or double newline
      end_pos <- regexpr("\\*\\*|\n\n", after)
      if (end_pos > 0) {
        detail <- substring(after, 1, end_pos - 1)
      } else {
        detail <- after
      }
      detail <- trimws(gsub("\\s+", " ", detail))

      if (nchar(detail) > 10) {
        findings[[length(findings) + 1]] <- list(
          theme = NULL,
          headline = header,
          summary = detail
        )
      }
    }
  }

  findings
}

# Generate title from query
generate_title <- function(query) {
  title <- tolower(query)
  title <- gsub("\\?|!|\\.$", "", title)
  title <- gsub("^(what|how|why|when|where|which|who|can you|could you|please|tell me about|explain|describe)\\s+(is|are|was|were|do|does|did|the|a|an)?\\s*", "", title)
  title <- gsub("^(the\\s+)?(top|latest|recent|current|main|key|major)\\s+(\\d+\\s+)?", "", title)
  title <- gsub("^(the|a|an)\\s+", "", title)
  title <- gsub("\\s+in\\s+\\d{4}$", "", title)
  title <- trimws(title)
  stringr::str_to_title(substr(title, 1, 60))
}

# Build QMD document
build_qmd <- function(findings, title, date, citations, group_by_theme) {
  lines <- c(
    "---",
    'font-family: "Arial"',
    "font-size: 9pt",
    "format:",
    "  typst:",
    "    margin:",
    "      x: 2cm",
    "      y: 2.5cm",
    "    papersize: a4",
    "---",
    "",
    "```{=typst}",
    '#set text(font: "Arial", size: 9pt)',
    "#set par(justify: true, leading: 0.65em)",
    "#set block(spacing: 0.5em)",
    "#align(center)[",
    sprintf('  #text(size: 14pt, weight: "bold")[%s]', title),
    "  #v(0.3em)",
    sprintf('  #text(size: 10pt)[%s]', date),
    "]",
    "#v(1em)",
    "",
    "#let wit(data) = {",
    "  table(",
    "    columns: (25%, 75%),",
    "    stroke: (x, y) => (",
    "      top: if y == 0 { 1.5pt } else { 0.5pt + gray },",
    "      bottom: 0.5pt + gray,",
    "      left: none,",
    "      right: none,",
    "    ),",
    '    inset: (x: 10pt, y: 8pt),',
    "    align: (left, left),",
    "    ..data.map(row => (strong(row.at(0)), row.at(1))).flatten()",
    "  )",
    "}",
    ""
  )

  # Group by theme if requested
  if (group_by_theme) {
    themes <- unique(sapply(findings, function(f) f$theme %||% "General"))

    for (theme in themes) {
      theme_findings <- Filter(function(f) (f$theme %||% "General") == theme, findings)
      if (length(theme_findings) > 0) {
        lines <- c(lines, sprintf('#text(size: 11pt, weight: "bold")[%s]', escape_typst(theme)), "#v(0.5em)", "")
        lines <- c(lines, "#wit((")

        for (i in seq_along(theme_findings)) {
          f <- theme_findings[[i]]
          headline <- escape_typst(f$headline)
          summary <- escape_typst(f$summary)
          row <- sprintf('  ("%s", "%s")', headline, summary)
          if (i < length(theme_findings)) row <- paste0(row, ",")
          lines <- c(lines, row)
        }

        lines <- c(lines, "))", "#v(1em)", "")
      }
    }
  } else {
    lines <- c(lines, "#wit((")

    for (i in seq_along(findings)) {
      f <- findings[[i]]
      headline <- escape_typst(f$headline)
      summary <- escape_typst(f$summary)
      row <- sprintf('  ("%s", "%s")', headline, summary)
      if (i < length(findings)) row <- paste0(row, ",")
      lines <- c(lines, row)
    }

    lines <- c(lines, "))")
  }

  lines <- c(lines, "```")

  # Add sources
  if (length(citations) > 0) {
    domains <- unique(sapply(citations, function(url) {
      domain <- gsub("^https?://([^/]+).*", "\\1", url)
      gsub("^www\\.", "", domain)
    }))
    lines <- c(lines, "", sprintf("*Sources: %s*", paste(domains, collapse = ", ")))
  }

  lines
}

# Backward compatibility alias
mcp_perplexity_wits_report <- wits_report

# Load message
cat("WiTs Report Generator loaded!\n\n")
cat("Usage: wits_report(query, output_file, title, num_findings, group_by_theme, options)\n\n")
cat("Parameters:\n")
cat("  query          - Search query\n")
cat("  output_file    - Output filename (default: auto-generated)\n")
cat("  title          - Custom title (default: auto-generated from query)\n")
cat("  num_findings   - Number of findings to return (default: 5)\n")
cat("  group_by_theme - Organize by theme (default: FALSE)\n")
cat("  options        - List with: search_recency_filter, temperature, model\n\n")
cat("Ensure PERPLEXITY_API_KEY is set.\n")
