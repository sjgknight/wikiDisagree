
#' Get section structure for a Wikipedia page
#'
#' Retrieves the section index and titles for a given Wikipedia page using
#' the MediaWiki parse API.
#'
#' @param pageid Numeric page ID from Wikipedia
#'
#' @return A tibble with columns: index (section number), line (section title)
#'   Returns a single row with NA values if no sections found
#'
#' @examples
#' \dontrun{
#' sections <- get_sections(67553694)
#' }
#'
#' @export
wiki_get_structure <- function(pageid) {
  req <- request("https://en.wikipedia.org/w/api.php") |>
    req_url_query(
      action = "parse",
      pageid = pageid,
      prop = "sections",
      format = "json"
    )

  resp <- req_perform(req)
  json <- resp_body_json(resp)

  if (!is.null(json$parse$sections)) {
    tibble(
      index = map_chr(json$parse$sections, "index"),
      line  = map_chr(json$parse$sections, "line")
    )
  } else {
    tibble(index = NA_character_, line = NA_character_)
  }
}
