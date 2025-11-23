#' Extract Wikipedia policy references from HTML content
#'
#' Parses HTML content to extract all references to Wikipedia policies and
#' guidelines (WP:* or Wikipedia:* links). Returns standardized policy names
#' with the WP: prefix.
#'
#' @param html Character string containing HTML content from Wikipedia
#'
#' @return Character vector of unique policy references (e.g., "WP:MEDRS", "WP:NPOV").
#'   Returns empty character vector if no policies found or if html is NA
#'
#' @details
#' This function searches for policy references in multiple formats:
#' - Links: href="/wiki/WP:POLICY" or href="/wiki/Wikipedia:POLICY"
#' - Text: >WP:POLICY< or >Wikipedia:POLICY<
#'
#' All results are standardized to the WP: prefix format.
#'
#' @examples
#' \dontrun{
#' html <- '<a href="/wiki/WP:MEDRS">WP:MEDRS</a> and <a href="/wiki/Wikipedia:NPOV">NPOV</a>'
#' policies <- extract_wp_policies(html)
#' # Returns: c("WP:MEDRS", "WP:NPOV")
#' }
#'
#' @export
wiki_extract_policies <- function(html) {
  if (is.na(html) || html == "") return(character(0))

  # Pattern to match Wikipedia policy/guideline links
  # Matches WP:SOMETHING or Wikipedia:SOMETHING
  patterns <- c(
    'href="/wiki/WP:([A-Z0-9_]+)"',
    'href="/wiki/Wikipedia:([A-Za-z0-9_]+)"',
    '>WP:([A-Z0-9_]+)<',
    '>Wikipedia:([A-Za-z0-9_]+)<'
  )

  all_policies <- character(0)

  for (pattern in patterns) {
    matches <- str_match_all(html, pattern)[[1]]
    if (nrow(matches) > 0) {
      policies <- matches[, 2]
      all_policies <- c(all_policies, policies)
    }
  }

  # Return unique policies, prefixed with WP:
  unique_policies <- unique(all_policies)
  if (length(unique_policies) == 0) return(character(0))

  # Standardize to WP: prefix
  unique_policies <- ifelse(
    !startsWith(unique_policies, "WP:"),
    paste0("WP:", unique_policies),
    unique_policies
  )

  unique(unique_policies)
}
