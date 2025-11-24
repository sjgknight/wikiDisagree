#' Resolve Wikipedia shortcut to its target page
#'
#' Follows redirects to find the actual policy page that a shortcut points to.
#'
#' @param shortcut Character string of Wikipedia shortcut (e.g., "WP:RS", "Wikipedia:MEDRS")
#'
#' @return Tibble with columns: shortcut, target_page, is_redirect
#'
#' @details
#' Uses the Wikipedia API to follow redirects. If the shortcut is not a redirect,
#' it returns itself as the target.
#'
#' @examples
#' \dontrun{
#' resolve_policy_redirect("WP:RS")
#' resolve_policy_redirect("Wikipedia:MEDRS")
#' }
#'
#' @export
resolve_policy_redirect <- function(shortcut) {
  # Normalize to full Wikipedia: format
  page_title <- if (startsWith(shortcut, "WP:")) {
    gsub("^WP:", "Wikipedia:", shortcut)
  } else {
    shortcut
  }

  tryCatch({
    req <- request("https://en.wikipedia.org/w/api.php") |>
      req_url_query(
        action = "query",
        titles = page_title,
        redirects = 1,
        format = "json"
      )

    resp <- req_perform(req)
    json <- resp_body_json(resp)

    # Check if there was a redirect
    if (!is.null(json$query$redirects)) {
      target <- json$query$redirects[[1]]$to
      is_redirect <- TRUE
    } else {
      target <- page_title
      is_redirect <- FALSE
    }

    tibble(
      shortcut = shortcut,
      target_page = target,
      is_redirect = is_redirect
    )
  }, error = function(e) {
    warning(sprintf("Error resolving %s: %s", shortcut, e$message))
    tibble(
      shortcut = shortcut,
      target_page = NA_character_,
      is_redirect = NA
    )
  })
}

