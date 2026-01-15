library(jsonlite)
library(httr2)
library(tidyverse)
# usethis::edit_r_environ()

# Direct httr request templates for Gemini and perplexity API queries ----


# Function to query Perplexity API directly
query_perplexity <- function(prompt, model = "sonar") {
  
  # Check API key
  api_key <- Sys.getenv("PERPLEXITY_API_KEY")
  if (!nzchar(api_key)) {
    stop("Set PERPLEXITY_API_KEY in your environment")
  }
  
  # Make the API request
  response <- request("https://api.perplexity.ai/chat/completions") |>
    req_headers(
      "Authorization" = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    ) |>
    req_body_json(list(
      model = model,
      messages = list(
        list(
          role = "user",
          content = prompt
        )
      ),
      temperature = 0.2,
      max_tokens = 1000
    )) |>
    req_perform()
  
  # Parse response
  result <- response |> resp_body_json()
  
  # Extract the content
  return(result$choices[[1]]$message$content)
}

# Function to query Gemini API directly

query_gemini <- function(prompt, model = "gemini-2.5-flash") {
  
  # Check API key
  api_key <- Sys.getenv("GEMINI_API_KEY")
  if (!nzchar(api_key)) {
    stop("Set GEMINI_API_KEY in your environment")
  }
  
  # Construct the correct API URL - fix the missing slash
  api_url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", 
                    model, ":generateContent?key=", api_key)
  
  # Make the API request with POST method
  response <- request(api_url) |>
    req_method("POST") |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(list(
      contents = list(
        list(
          parts = list(
            list(text = prompt)
          )
        )
      ),
      generationConfig = list(
        temperature = 0.2,
        maxOutputTokens = 1000
      )
    )) |>
    req_perform()
  
  # Parse response
  result <- response |> resp_body_json()
  
  # Extract the content
  return(result$candidates[[1]]$content$parts[[1]]$text)
}


# Generate automatic text outlining the financial weather for SSA over the past week ----


# load in context from charts
load("contextJSONs.rdta")
policy<- query_perplexity(
  "You are a concise, data-driven economist at the International Monetary Fund. Write a factual and succinct weekly update on recent financial and monetary developments in sub-Saharan Africa over the past 7 days, looking at sites that may have real-time information about policy decisions in Africa such as Reuters Africa and cbrates.com, among any other you find relevant. Past 7 Days (Strict Cutoff: seven days prior to today's date to today): Begin with 3 to 5 bullet points summarizing only the main monetary policy decisions or announcements made within the past 7 days (exclude any events dated earlier than seven days before today's date, even if mentioned in recent sources). Verify event dates from official announcements or content timestamps before inclusion—do not use publish dates as proxies. Next, provide a brief contextual overview (2 to 3 paragraphs) summarizing significant monetary policy developments or trends from the past 30 days, explaining how the last weeks events fit within these broader trends. Avoid repeating or analyzing events older than 30 days. Focus strictly on central bank actions, interest rate changes, inflation-related policy updates, or major institutional financial decisions relevant to sub-Saharan African economies. Cross-check all dates explicitly; err on exclusion if uncertain.",
  model = "sonar-pro"
)
commodity<- query_perplexity(
paste0("You are a helpful, concise, and extremely terse economist at the International Monetary Fund. Write a factual weekly update on recent financial developments affecting sub-Saharan Africa. Past 7 Days (Strict Cutoff: from seven days prior to today's date to today): Begin with 3 to 5 bullet points summarizing only the main global commodity market developments affecting sub-Saharan Africa within the past 7 days (exclude any events dated earlier than seven days ago, even if mentioned in recent sources). Verify event dates from official announcements or content timestamps before inclusion—do not use publish dates as proxies. Highlight any context that matches the top few changes from the past week's comodity price movements identified in the following json file:",com.context," Next, provide a brief contextual overview (2 to 3 paragraphs) summarizing significant commodity market developments or trends from the past 30 days, explaining how the last week's events fit within these broader trends.
Avoid repeating or analyzing events older than 30 days. Focus strictly on global commodity prices, trade flows, or market shifts relevant to sub-Saharan African economies. Cross-check all dates explicitly; err on exclusion if uncertain.", model="sonar-pro"
))

debt<- query_perplexity(
  paste0("You are a helpful, concise, and extremely terse economist at the International Monetary Fund. Write a factual weekly update on recent financial developments affecting sub-Saharan Africa, looking at sites that might have real-time informtion on debt issuance in sub-Saharan Africa including ecofinagency.com, Reuters Africa, and any others you might find relevant. Past 7 Days (Strict Cutoff: Past 7 days from today's date): Begin with 3 to 5 bullet points summarizing only the main sovereign bond issues or ratings announcements from sub-Saharan Africa within the past 7 days (exclude any events dated 7 days ago or earlier from today's date, even if mentioned in recent sources). Verify event dates from official announcements or content timestamps before inclusion—do not use publish dates as proxies.  Highlight any context that matches the top few changes past week's spread movements identified in the following json file:",spread.context,"Next, provide 3 to 5 brief factual bullets summarizing the most important bond issues or ratings decisions from the past 30 days (30 days prior to today's date), focusing on individual country ratings or debt announcements and explaining how the last week's events fit within these broader trends. Avoid repeating or analyzing events older than 30 days from todays date. Exclude broader analysis of debt sustainability or debt service costs. Focus strictly on sovereign bond issuances, ratings changes, or related announcements relevant to sub-Saharan African economies. Cross-check all dates explicitly relative to today's date; err on exclusion if uncertain. Keep the entire answer to around half a page."),
  model = "sonar-pro"
)

currency<- query_perplexity(
  paste0("You are a helpful, concise, and extremely terse economist at the International Monetary Fund. Your are writing a weekly update on recent financial developments affecting sub-Saharan Africa. Provide a few factual bullets on key exchange-rate movements in sub-Saharan Africa in the past 7 days. Numbers are less important tan trends, or sudden strengthening or weakening against the USD. These should exclude any events that happened more than 7 days ago. Highlight any context that matches the top few changes from past week's currency movements identified in the following json file:",er.context," Then, provide a few brief factual bullets on the most important currency-market developments for sub-Saharan Africa over the past 30 days. Exclude any events or analysis stemming from more than a month ago, and exclude broader analysis of debt sustainability or debt service costs. Keep the entire answer to around half a page"),
  model = "sonar-pro"
)
capital<- query_perplexity(
  "You are a helpful, concise, and extremely terse economist at the International Monetary Fund. Write a factual weekly update on recent financial developments affecting sub-Saharan Africa. Past 7 Days (Strict Cutoff: Past 7 days from today's date): Begin with 3 to 5 bullet points summarizing only portfolio capital flows into sub-Saharan Africa within the past 7 days, including emerging-market portfolio fund reports (EPFR) flows into Africa-centered exchange-traded funds (ETFs) (exclude any events dated 7 days ago or earlier from today's date, even if mentioned in recent sources). Verify event dates from official announcements or content timestamps before inclusion—do not use publish dates as proxies. Next, provide 3 to 5 brief factual bullets summarizing the most important portfolio capital flow trends for sub-Saharan Africa from the past 30 days (30 days prior to today's date), focusing on flows and explaining how the last week's events fit within these broader trends. Avoid repeating or analyzing events older than 30 days from today's date. Exclude broader analysis of debt sustainability or debt service costs. Focus strictly on portfolio capital flows, EPFR data, ETF inflows/outflows, or related announcements relevant to sub-Saharan African economies. Cross-check all dates explicitly relative to today's date; err on exclusion if uncertain. Keep the entire answer to around half a page.",
  model = "sonar-pro"
)
# Summaries for the report
summary <- query_gemini(
  paste("You are a helpful, concise, and extremely terse economist at the International Monetary Fund. Your are writing a weekly update on recent financial developments affecting sub-Saharan Africa over the past 7 days only. Provide a single very brief paragraph extracting the most important developments over the past 7 dayes from the following text:", debt, policy, "exclude any analysis or context from more than 7 days ago. Keep to the facts only, and keep the entire answer to less than 80 words. If there is a new debt issue, briefly provide details on amount and yield. Remove any reference markers from the text. Once you're done, remove all markdown formatting and reference markers from the text, and then ensure that all country names are in bold.", model="3-flash")
)

summary_sq<-query_gemini(
  paste("You are a helpful, concise, and extremely terse economist at the International Monetary Fund. Your are writing a weekly update on recent financial developments affecting sub-Saharan Africa. The report comtains single-paragraph summaries of recent events, plus catchy headings summarizing the most important themes or findings within that paragraph. Provide a very short catchy headline for the following summary paragraph:", summary, "The style and tone should be concise and witty, along the lines of a headline in the Economist magazine. The total length should not exceed 10 words. Remvove all markdown formatting from the text", model="3-flash"
))
cat(summary_sq)

summary2 <- query_gemini(
  paste("You are a helpful, concise, and extremely terse economist at the International Monetary Fund. Your are writing a weekly update on recent financial developments affecting sub-Saharan Africa over the past 7 days only. Provide a single very brief paragraph extracting the most important developments over the past 7 dayes from the following text:", commodity, currency, "exclude any analysis or context from more than 7 days ago. Keep to the facts only, and keep the entire answer to less than 60 words. Actual numbers are less important than trends. Remove any reference markers from the text. Once you're done, remove all markdown formatting and reference markers from the text, and then ensure that all country names are in bold.", model="3-flash")
)
summary2_sq<-query_gemini(
  paste("You are a helpful, concise, and extremely terse economist at the International Monetary Fund. Your are writing a weekly update on recent financial developments affecting sub-Saharan Africa. The report comtains single-paragraph summaries of recent events, plus catchy headings summarizing the most important themes or findings within that paragraph. Provide a very short catchy headline for the following summary paragraph:", summary2, "The style and tone should be concise and witty, along the lines of a headline in the Economist magazine. The total length should not exceed 10 words. Remvove all markdown formatting from the text", model="3-flash")
)

capital_sq<-query_gemini(
  paste("You are a helpful, concise, and extremely terse economist at the International Monetary Fund. Your are writing a weekly update on recent financial developments affecting sub-Saharan Africa. The report contains single-paragraph summaries of recent events, plus catchy headings summarizing the most important themes or findings within that paragraph. Provide a very short catchy headline for the following summary paragraph:", capital, " Ensure that the headline refers to sub-Saharan Africa only. The tone and style should be concise and witty, along the lines of the Economist magazine. Keep the total response to less than 15 words.", model="3-flash")
)
# Debt Markets
cat(c("\n", debt, "\n"))
# Policy Decisions
cat(c("\n", policy, "\n"))
# Capital Flows
cat(c("\n", capital, "\n"))
# Currency developments
cat("\n", currency, "\n")
# Commodity developments
cat("\n", commodity, "\n")

# Weather Report ---------------------------------------------------------
cat("------------------------------------------------")
# Summary Para
cat("\n", summary,"\n")
# Summary headline
cat("\n", summary_sq, "\n")

cat("\n", summary2,"\n")
# Summary headline
cat("\n", summary2_sq, "\n")
# Commodity & Currency headline
cat("\n", capital_sq, "\n")
save(summary, summary_sq, summary2, summary2_sq, capital_sq, file="c:\\projects\\AutomatedWeatherNote\\weathertext.rdta")
