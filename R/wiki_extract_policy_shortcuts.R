
#' Extract policy shortcuts from infobox or shortcut template
#'
#' Specifically looks for shortcuts listed in {{shortcut}} or {{redirect-multi}}
#' templates at the top of policy pages, which indicate the "official" shortcuts
#' for that page.
#'
#' @param wikitext Character string of Wikipedia wikitext
#'
#' @return Character vector of official shortcuts
#'
#' @details
#' Handles multiple template formats:
#' - {{shortcut|WP:RS|WP:IRS}}
#' - {{redirect-multi|3|WP:RS|WP:IRS|WP:RELIABILITY|other uses|...}}
#' - {{redirect|WP:RS}}
#'
#' For redirect-multi, the first parameter is the count of shortcuts to extract.
#'
#' @export
wiki_extract_policy_shortcuts <- function(wikitext) {
  if (is.na(wikitext) || wikitext == "") return(character(0))

  shortcuts <- character(0)

  # Pattern 1: [[WP:SOMETHING]] or [[Wikipedia:SOMETHING]]
  # wiki_links <- str_match_all(wikitext, "\\[\\[(WP:[A-Z0-9/_-]+|Wikipedia:[A-Za-z0-9_/ -]+)(?:\\||\\])")[[1]]
  # if (nrow(wiki_links) > 0) {
  #   shortcuts <- c(shortcuts, wiki_links[, 2])
  # }


  shortcutstring <- "\\{\\{\\s*[Ss]hortcut\\s*\\|([^}]+)\\}\\}|[Ss]hortcut\\s*=\\s*(WP:[A-Za-z0-9/_-]+|Wikipedia:[A-Za-z0-9/_-]+)"

  # Pattern 2: {{shortcut|WP:SOMETHING}} template
  shortcut_template <- str_match_all(wikitext, regex(shortcutstring, ignore_case = T))[[1]]

  # Combine captures
  shortcut_template <- c(shortcut_template[,2], shortcut_template[,3])
  shortcut_template <- shortcut_template[!is.na(shortcut_template)]

  shortcuts <- shortcut_template %>%
    str_split("[|=]") %>%
    unlist() %>%
    str_trim() %>%
    #str_remove_all(" ") %>%
    discard(~ .x == "") %>%
    keep(~ str_detect(.x, "^(WP:|Wikipedia:)"))

  shortcuts

  # if (nrow(shortcut_template) > 0) {
  #   # Split by | and extract WP: shortcuts
  #   template_content <- shortcut_template[, 2]
  #   for (content in template_content) {
  #     parts <- str_split(content, "\\|")[[1]]
  #     for (part in parts) {
  #       part_clean <- str_trim(part)
  #       part_clean <- str_remove_all(part_clean, " ")
  #       if (str_detect(part_clean, "^(WP:|Wikipedia:)")) {
  #         shortcuts <- c(shortcuts, part_clean)
  #       }
  #     }
  #   }
  # }

  # Pattern 3: Bare WP:SOMETHING in text (more conservative)
  # bare_shortcuts <- str_match_all(wikitext, "\\b(WP:[A-Z0-9/_-]+)\\b")[[1]]
  # if (nrow(bare_shortcuts) > 0) {
  #   shortcuts <- c(shortcuts, bare_shortcuts[, 2])
  # }

  # Clean and return unique
  # shortcuts <- unique(shortcuts)
  # shortcuts <- str_trim(shortcuts)
  # shortcuts <- stringr::str_remove_all(shortcuts, " ")
  # shortcuts[nchar(shortcuts) > 0]

}

