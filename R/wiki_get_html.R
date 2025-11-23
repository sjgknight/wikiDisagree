
#' Get rendered HTML content from Wikipedia using parse API
#'
#' Fetches the rendered HTML content of a Wikipedia page or specific section
#' using the MediaWiki parse action endpoint. This function returns fully
#' rendered HTML as it would appear on Wikipedia, including formatting,
#' links, and other markup.
#'
#' @param pageid Numeric or integer page ID from Wikipedia. Each Wikipedia
#'   page has a unique pageid that can be found in the page URL or through
#'   search API results. If NA, the function returns NA_character_ immediately.
#' @param section_index Character or numeric section index (optional). Specifies
#'   which section of the page to retrieve. If NULL, NA, or empty string (""),
#'   fetches the entire page. Section indices can be obtained using the
#'   \code{get_sections()} function. Valid values are typically "0" for the
#'   lead section, "1", "2", etc. for subsequent sections.
#'
#' @return Character string containing rendered HTML content from Wikipedia.
#'   Returns NA_character_ if:
#'   \itemize{
#'     \item The pageid is NA
#'     \item The API response does not contain HTML content
#'     \item The page or section does not exist
#'   }
#'
#' @details
#' This function uses the Wikipedia API's "parse" action, which provides
#' fully rendered HTML content. This is more reliable and accurate than
#' attempting to convert raw wikitext to HTML manually.
#'
#' The function constructs a query to https://en.wikipedia.org/w/api.php with:
#' \itemize{
#'   \item action="parse" - Parses content and returns parser output
#'   \item format="json" - Returns results in JSON format
#'   \item pageid=[id] - Specifies the page to parse
#'   \item prop="text" - Returns the parsed wikitext as HTML
#'   \item section=[index] - (Optional) Specifies section to parse
#' }
#'
#' The returned HTML includes Wikipedia-specific styling and structure,
#' including internal links, references, and formatting. You may need to
#' post-process the HTML for display outside of Wikipedia.
#'
#' @section API Requirements:
#' This function requires the httr2 package for HTTP requests and respects
#' Wikipedia's API usage guidelines. The function makes synchronous requests
#' and does not implement rate limiting - users should implement their own
#' rate limiting when making multiple requests.
#'
#' @examples
#' \dontrun{
#' # Get entire page HTML
#' html <- get_html_from_api(67553694)
#'
#' # Get specific section HTML (section 2)
#' html_section <- get_html_from_api(67553694, section_index = "2")
#'
#' # Using with a dataframe of pageids
#' library(purrr)
#' library(dplyr)
#'
#' df <- data.frame(pageid = c(12345, 67890))
#' df <- df %>%
#'   mutate(html_content = map_chr(pageid, get_html_from_api))
#'
#' # Get HTML for specific sections
#' df <- data.frame(
#'   pageid = c(12345, 12345),
#'   section = c("1", "2")
#' )
#' df <- df %>%
#'   mutate(html_content = map2_chr(pageid, section, get_html_from_api))
#' }
#'
#' @seealso
#' \code{\link{get_page_content}} for getting raw wikitext instead of HTML
#' \code{\link{get_sections}} for retrieving section indices for a page
#'
#' @references
#' MediaWiki Parse API: https://www.mediawiki.org/wiki/API:Parse
#' Wikipedia API documentation: https://en.wikipedia.org/w/api.php
#'
#' @importFrom httr2 request req_url_query req_perform resp_body_json
#' @importFrom tibble tibble
#'
#' @export
wiki_get_html <- function(pageid, section_index = NULL) {
  # Early return if pageid is NA
  if (is.na(pageid)) return(NA_character_)

  # Build the query parameters list
  query_list <- list(
    action = "parse",
    format = "json",
    pageid = pageid,
    prop = "text"
  )

  # Add section parameter if provided and valid
  if (!is.null(section_index) && !is.na(section_index) && section_index != "") {
    query_list$section <- section_index
  }

  # Construct and perform the API request
  req <- request("https://en.wikipedia.org/w/api.php") |>
    req_url_query(!!!query_list)

  resp <- req_perform(req)
  json <- resp_body_json(resp)

  # Extract HTML from the JSON response
  # The asterisk (*) is the key name in the API response
  html <- json$parse$text$`*`

  # Return HTML if present, otherwise return NA
  if (!is.null(html)) html else NA_character_
}
