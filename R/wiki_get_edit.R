#' Get revision content or diff between revisions
#'
#' @param revid Single revision ID or vector of two revision IDs
#' @param mode Either "content", "diff", or "both" (default: "content")
#' @return List with revision content and/or diff information
wiki_get_edit <- function(revid, mode = "content") {

  if (length(revid) > 2) {
    stop("revid must be a single revision ID or a vector of two revision IDs")
  }

  # Single revision - get content
  if (length(revid) == 1 && mode %in% c("content", "both")) {
    response <- GET(
      "https://en.wikipedia.org/w/api.php",
      query = list(
        action = "query",
        revids = revid,
        prop = "revisions",
        rvprop = "content|timestamp|user|comment|size|ids",
        rvslots = "main",
        formatversion = "2",
        format = "json"
      )
    )

    data <- content(response, as = "parsed")
    page <- data$query$pages[[1]]
    rev <- page$revisions[[1]]

    content_result <- list(
      revid = rev$revid,
      parentid = rev$parentid %||% NA_integer_,
      timestamp = rev$timestamp,
      user = rev$user,
      comment = rev$comment %||% "",
      size = rev$size,
      content = rev$slots$main$content,
      page_title = page$title,
      pageid = page$pageid
    )
  } else {
    content_result <- NULL
  }

  # Two revisions or mode includes diff - get diff
  if (length(revid) == 2 || (length(revid) == 1 && mode %in% c("diff", "both"))) {

    # If only one revid provided, compare with its parent
    if (length(revid) == 1) {
      info_response <- GET(
        "https://en.wikipedia.org/w/api.php",
        query = list(
          action = "query",
          revids = revid,
          prop = "revisions",
          rvprop = "ids",
          formatversion = "2",
          format = "json"
        )
      )

      info_data <- content(info_response, as = "parsed")
      parentid <- info_data$query$pages[[1]]$revisions[[1]]$parentid

      if (is.null(parentid)) {
        warning("No parent revision found for diff comparison")
        diff_result <- NULL
      } else {
        from_revid <- parentid
        to_revid <- revid
      }
    } else {
      from_revid <- revid[1]
      to_revid <- revid[2]
    }

    if (!is.null(from_revid)) {
      diff_response <- GET(
        "https://en.wikipedia.org/w/api.php",
        query = list(
          action = "compare",
          fromrev = from_revid,
          torev = to_revid,
          prop = "diff|diffsize|rel|ids|title|user|comment|size",
          formatversion = "2",
          format = "json"
        )
      )

      diff_data <- content(diff_response, as = "parsed")$compare

      diff_result <- list(
        from_revid = diff_data$fromrevid,
        to_revid = diff_data$torevid,
        from_size = diff_data$fromsize,
        to_size = diff_data$tosize,
        diff_size = diff_data$tosize - diff_data$fromsize,
        from_user = diff_data$fromuser %||% NA_character_,
        to_user = diff_data$touser %||% NA_character_,
        to_comment = diff_data$tocomment %||% "",
        diff_html = diff_data$`*` %||% "",
        page_title = diff_data$totitle %||% diff_data$fromtitle
      )
    } else {
      diff_result <- NULL
    }
  } else {
    diff_result <- NULL
  }

  # Return based on mode
  if (mode == "content") {
    return(content_result)
  } else if (mode == "diff") {
    return(diff_result)
  } else {
    return(list(content = content_result, diff = diff_result))
  }
}
