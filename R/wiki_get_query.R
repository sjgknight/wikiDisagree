library(httr2)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(reactable)

#' Split search terms into chunks that fit within URL length limits
#'
#' @param terms Character vector of search terms to be combined with OR
#' @param suffix Character string to append to each query (e.g., prefix filter)
#' @param max_len Integer maximum length for the combined query string
#'
#' @return List of character strings, each containing chunked search terms
#'
#' @examples
#' terms <- c("MEDRS", "NPOV", "RS")
#' suffix <- 'prefix:"Talk:COVID-19"'
#' chunk_search_terms(terms, suffix, max_len = 100)
#'
#' @export
chunk_search_terms <- function(terms, suffix, max_len = 280) {
  chunks <- list()
  current <- ""

  for (term in terms) {
    candidate <- if (current == "") term else paste(current, "OR", term)
    if (nchar(paste(candidate, suffix)) > max_len) {
      chunks <- append(chunks, list(current))
      current <- term
    } else {
      current <- candidate
    }
  }

  chunks <- append(chunks, list(current))
  chunks
}

#' Run policy search on Wikipedia talk pages
#'
#' Searches Wikipedia talk pages for policy references using the MediaWiki API.
#' Automatically chunks queries that exceed URL length limits.
#'
#' @param policy_and_page_string Character string containing search terms combined
#'   with OR operators and a prefix filter
#' @param namespace Character or integer namespace ID (default 1 for talk pages)
#' @param srlimit Integer maximum number of results per query (default 50, max 500)
#' @param max_len Integer maximum length for query strings (default 280)
#' @param pslimit Integer limit for page searches (default 500)
#'
#' @return A tibble containing search results with columns: srsearch, title,
#'   snippet, size, ns, wordcount, timestamp, pageid, sectiontitle
#'
#' @examples
#' \dontrun{
#' query <- 'WP:MEDRS OR WP:NPOV prefix:"Talk:COVID-19"'
#' results <- run_policy_search(query, namespace = 1, srlimit = 50)
#' }
#'
#' @export
wiki_get_query <- function(
    policy_and_page_string,
    namespace = 1,
    srlimit = 50,
    max_len = 280,
    pslimit = 500
) {
  parts <- str_split(policy_and_page_string, " OR\\s+")[[1]]
  suffix <- str_extract(policy_and_page_string, 'prefix:.*$')
  terms <- parts[!str_detect(parts, "^prefix:")]
  batches <- chunk_search_terms(terms, suffix, max_len)

  run_query <- function(batch) {
    srsearch <- paste(batch, suffix)

    req <- request("https://en.wikipedia.org/w/api.php") |>
      req_url_query(
        action = "query",
        list = "search",
        srsearch = srsearch,
        srnamespace = namespace,
        format = "json",
        srlimit = srlimit,
        srprop = "size|wordcount|timestamp|snippet|sectiontitle"
      )

    resp <- req_perform(req)
    json <- resp_body_json(resp)

    tibble(
      srsearch = srsearch,
      title = map_chr(json$query$search, ~ .x$title),
      snippet = map_chr(json$query$search, ~ .x$snippet),
      size = length(json$query$search),
      ns = map_chr(json$query$search, ~ .x$ns),
      wordcount = map_dbl(json$query$search, ~ .x$wordcount),
      timestamp = map_chr(json$query$search, ~ .x$timestamp),
      pageid = map_dbl(json$query$search, ~ .x$pageid),
      sectiontitle = map_chr(json$query$search, ~ .x$sectiontitle %||% NA_character_)
    )
  }

  results <- map_dfr(batches, run_query)
  results
}
