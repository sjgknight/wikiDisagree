
#' Get wikitext content for a Wikipedia page by title
#'
#' Retrieves the raw wikitext for a Wikipedia page using its title.
#'
#' @param page_title Character string of page title (e.g., "Wikipedia:Verifiability")
#'
#' @return Tibble with columns: page_title, wikitext, pageid
#'
#' @examples
#' \dontrun{
#' wiki_get_wikitext_by_title("Wikipedia:Verifiability")
#' }
#'
#' @export
wiki_get_wikitext_by_title <- function(page_title) {
  tryCatch({
    req <- request("https://en.wikipedia.org/w/api.php") |>
      req_url_query(
        action = "query",
        titles = page_title,
        prop = "revisions",
        rvprop = "content",
        rvslots = "main",
        format = "json"
      )

    resp <- req_perform(req)
    json <- resp_body_json(resp)

    # Extract page content
    pages <- json$query$pages
    page <- pages[[1]]

    if (!is.null(page$revisions)) {
      wikitext <- page$revisions[[1]]$slots$main$`*`
      pageid <- page$pageid
    } else {
      wikitext <- NA_character_
      pageid <- NA_integer_
    }

    tibble(
      page_title = page_title,
      pageid = pageid,
      wikitext = wikitext
    )
  }, error = function(e) {
    warning(sprintf("Error fetching wikitext for %s: %s", page_title, e$message))
    tibble(
      page_title = page_title,
      pageid = NA_integer_,
      wikitext = NA_character_
    )
  })
}
