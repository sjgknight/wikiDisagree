library(httr)
library(jsonlite)
library(purrr)
library(dplyr)

#' Get Wikipedia policy/guideline information
#'
#' @param shortcuts Character vector of Wikipedia shortcuts (e.g., "WP:MEDRS")
#' @return Tibble with policy information
get_policy_info <- function(shortcuts) {

  titles <- paste0("Wikipedia:", sub("^WP:", "", shortcuts))

  get_batch_info <- function(batch_titles, batch_shortcuts) {

    titles_param <- paste(batch_titles, collapse = "|")

    response <- GET(
      "https://en.wikipedia.org/w/api.php",
      query = list(
        action = "query",
        titles = titles_param,
        prop = "categories|pageprops|revisions",
        rvprop = "content",
        rvsection = "0",
        rvslots = "main",
        cllimit = "max",
        formatversion = "2",  # Modern format, easier to parse
        format = "json"
      )
    )

    data <- content(response, as = "parsed")

    if (is.null(data$query) || is.null(data$query$pages)) {
      return(tibble())
    }

    map2_dfr(data$query$pages, batch_shortcuts, function(page, shortcut) {

      if (!is.null(page$missing)) {
        return(tibble(
          shortcut = shortcut,
          page_title = page$title,
          exists = FALSE
        ))
      }

      # Extract categories
      cats <- if (!is.null(page$categories)) {
        map_chr(page$categories, "title") %>% str_remove("^Category:")
      } else {
        character(0)
      }

      policy_type <- case_when(
        any(str_detect(cats, "Wikipedia policies")) ~ "Policy",
        any(str_detect(cats, "Wikipedia guidelines")) ~ "Guideline",
        any(str_detect(cats, "Wikipedia essays")) ~ "Essay",
        any(str_detect(cats, "Category:Wikipedia")) ~ "Information",
        TRUE ~ "Unknown"
      )

      # Extract wikitext
      wikitext <- page$revisions[[1]]$slots$main$content %||% NA_character_

      # Extract nutshell
      nutshell <- NA_character_
      if (!is.na(wikitext)) {
        nut <- str_match(wikitext, "(?i)\\{\\{nutshell\\|([^}]+)\\}\\}")
        if (!is.na(nut[1,1])) {
          nutshell <- nut[1,2]
        }
      }

      tibble(
        shortcut = shortcut,
        page_title = page$title,
        pageid = page$pageid,
        policy_type = policy_type,
        short_description = page$pageprops$`wikibase-shortdesc` %||% NA_character_,
        nutshell = nutshell,
        categories = list(cats),
        exists = TRUE
      )
    })
  }

  shortcuts_split <- split(shortcuts, ceiling(seq_along(shortcuts) / 50))
  titles_split <- split(titles, ceiling(seq_along(titles) / 50))

  map2_dfr(titles_split, shortcuts_split, ~{
    result <- get_batch_info(.x, .y)
    Sys.sleep(0.1)
    result
  })
}

# Usage
shortcuts <- c("WP:MEDRS", "WP:V", "WP:RS", "WP:NPOV", "WP:BLP")
policy_info <- get_policy_info(shortcuts)
