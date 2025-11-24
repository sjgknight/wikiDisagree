library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(stringr)

#' Get article diffs around a talk page edit
#'
#' @param talk_pageid Page ID of the talk page
#' @param talk_revid Revision ID of the talk page edit (optional)
#' @param window_hours_back Hours to look back from talk edit (default 24)
#' @param window_hours_forward Hours to look forward from talk edit (default 24)
#' @param return_mode "closest", "all", or "window" - what revisions to return
#' @return List with talk page info, article revisions, and diffs
wiki_get_article_state <- function(talk_pageid,
                                   talk_revid = NULL,
                                   window_hours_back = 24,
                                   window_hours_forward = 24,
                                   return_mode = "closest") {

  # Step 1: Get talk page information
  talk_response <- GET(
    "https://en.wikipedia.org/w/api.php",
    query = list(
      action = "query",
      pageids = talk_pageid,
      prop = "info|revisions",
      rvprop = "timestamp|ids|user|comment",
      rvlimit = if (!is.null(talk_revid)) 1 else 1,
      rvstartid = talk_revid,
      formatversion = "2",
      format = "json"
    )
  )

  talk_data <- content(talk_response, as = "parsed")
  talk_page <- talk_data$query$pages[[1]]

  # Get the talk page title and determine article title
  talk_title <- talk_page$title

  # Remove "Talk:" prefix and subpage to get article title
  article_title <- str_remove(talk_title, "^Talk:") %>%
    str_split_1("/") %>%
    .[1]

  # Get timestamp of the talk edit
  talk_timestamp <- talk_page$revisions[[1]]$timestamp
  talk_time <- ymd_hms(talk_timestamp)

  # Step 2: Get article page revisions in window
  time_start <- talk_time - hours(window_hours_back)
  time_end <- talk_time + hours(window_hours_forward)

  article_response <- GET(
    "https://en.wikipedia.org/w/api.php",
    query = list(
      action = "query",
      titles = article_title,
      prop = "revisions|info",
      rvprop = "timestamp|ids|user|comment|size",
      rvlimit = "max",
      rvstart = format(time_end, "%Y-%m-%dT%H:%M:%SZ"),
      rvend = format(time_start, "%Y-%m-%dT%H:%M:%SZ"),
      rvdir = "older",
      formatversion = "2",
      format = "json"
    )
  )

  article_data <- content(article_response, as = "parsed")
  article_page <- article_data$query$pages[[1]]

  if (is.null(article_page$revisions) || length(article_page$revisions) == 0) {
    warning("No article revisions found in the specified time window")
    return(list(
      talk_page = list(
        pageid = talk_page$pageid,
        title = talk_title,
        revid = talk_revid,
        timestamp = talk_timestamp
      ),
      article_page = NULL,
      revisions = tibble(),
      diffs = list()
    ))
  }

  # Step 3: Process article revisions
  article_revs <- map_dfr(article_page$revisions, function(rev) {
    tibble(
      revid = rev$revid,
      parentid = rev$parentid %||% NA_integer_,
      timestamp = ymd_hms(rev$timestamp),
      user = rev$user,
      comment = rev$comment %||% "",
      size = rev$size
    )
  }) %>%
    mutate(
      time_diff_hours = as.numeric(difftime(timestamp, talk_time, units = "hours")),
      abs_time_diff = abs(time_diff_hours),
      is_before = time_diff_hours <= 0,
      is_after = time_diff_hours > 0
    ) %>%
    arrange(timestamp)

  # Find closest revision and surrounding revisions
  closest_rev <- article_revs %>% arrange(abs_time_diff) %>% slice(1)

  rev_before_talk <- article_revs %>%
    filter(is_before) %>%
    arrange(desc(timestamp)) %>%
    slice(1)

  rev_after_talk <- article_revs %>%
    filter(is_after) %>%
    arrange(timestamp) %>%
    slice(1)

  # Step 4: Get diffs based on return_mode
  diffs <- list()

  if (return_mode == "closest") {
    # Get diff of closest revision (compare with its parent)
    if (nrow(closest_rev) > 0 && !is.na(closest_rev$parentid)) {
      diffs$closest <- wiki_get_revision(closest_rev$revid, mode = "diff")
      diffs$closest$context <- closest_rev
    }

  } else if (return_mode == "window") {
    # Get diff spanning the talk edit (before -> after)
    if (nrow(rev_before_talk) > 0 && nrow(rev_after_talk) > 0) {
      diffs$across_talk <- wiki_get_revision(
        c(rev_before_talk$revid, rev_after_talk$revid),
        mode = "diff"
      )
      diffs$across_talk$context <- list(
        before = rev_before_talk,
        after = rev_after_talk
      )
    }

  } else if (return_mode == "all") {
    # Get all diffs in the window
    if (nrow(article_revs) > 1) {
      diffs$all <- map(1:(nrow(article_revs) - 1), function(i) {
        from_rev <- article_revs[i, ]
        to_rev <- article_revs[i + 1, ]

        diff <- wiki_get_revision(c(from_rev$revid, to_rev$revid), mode = "diff")
        diff$context <- list(from = from_rev, to = to_rev)

        Sys.sleep(0.1)  # Be nice to API
        diff
      })
    }

    # Also include the closest diff
    if (nrow(closest_rev) > 0 && !is.na(closest_rev$parentid)) {
      diffs$closest <- wiki_get_revision(closest_rev$revid, mode = "diff")
      diffs$closest$context <- closest_rev
    }

    # And the window diff
    if (nrow(rev_before_talk) > 0 && nrow(rev_after_talk) > 0) {
      diffs$across_talk <- wiki_get_revision(
        c(rev_before_talk$revid, rev_after_talk$revid),
        mode = "diff"
      )
      diffs$across_talk$context <- list(
        before = rev_before_talk,
        after = rev_after_talk
      )
    }
  }

  # Return results
  list(
    talk_page = list(
      pageid = talk_page$pageid,
      title = talk_title,
      revid = talk_revid %||% talk_page$revisions[[1]]$revid,
      timestamp = talk_timestamp,
      user = talk_page$revisions[[1]]$user,
      comment = talk_page$revisions[[1]]$comment %||% ""
    ),
    article_page = list(
      pageid = article_page$pageid,
      title = article_title,
      closest_revid = closest_rev$revid,
      closest_timestamp = closest_rev$timestamp,
      time_diff_hours = closest_rev$time_diff_hours
    ),
    revisions = article_revs,
    diffs = diffs,
    window = list(
      back_hours = window_hours_back,
      forward_hours = window_hours_forward,
      talk_time = talk_time
    )
  )
}
