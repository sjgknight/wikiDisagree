#' Get raw wikitext content for a Wikipedia page
#'
#' Retrieves the raw wikitext content of a Wikipedia page or specific section
#' using the MediaWiki revisions API. This returns the source wikitext rather
#' than rendered HTML.
#'
#' @param pageid Numeric page ID from Wikipedia
#' @param section_index Character or numeric section index (optional). If NULL,
#'   fetches the entire page content. If provided, fetches only that section's wikitext
#'
#' @return A tibble with columns: pageid (numeric) and content (character).
#'   Returns NA_character_ for content if the page has no revisions
#'
#' @details
#' This function uses the revisions API endpoint to get raw wikitext. For rendered
#' HTML, use \code{\link{get_html_from_api}} instead.
#'
#' @examples
#' \dontrun{
#' # Get entire page wikitext
#' wikitext <- get_page_content(67553694)
#'
#' # Get specific section wikitext
#' section_text <- get_page_content(67553694, section_index = "2")
#' }
#'
#' @seealso \code{\link{get_html_from_api}} for getting rendered HTML instead
#'
#' @export
wiki_get_wikitext <- function(pageid, section_index = NULL) {

  query_list <- list(
    action = "query",
    prop   = "revisions",
    rvprop = "content",
    format = "json",
    pageids = pageid
  )

  if (!is.null(section_index)) {
    query_list$rvsection <- section_index
  }

  req <- request("https://en.wikipedia.org/w/api.php") |>
    req_url_query(!!!query_list)

  resp <- req_perform(req)
  json <- resp_body_json(resp)

  page <- json$query$pages[[as.character(pageid)]]

  if (!is.null(page$revisions)) {
    tibble(
      pageid = pageid,
      content = page$revisions[[1]]$`*`
    )
  } else {
    tibble(
      pageid = pageid,
      content = NA_character_
    )
  }
}
