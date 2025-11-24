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
#' Handles multiple template formats that appear at the top of pages:
#' - {{subcat guideline|...|WP:SHORTCUT}}
#' - {{redirect-multi|count|WP:SC1|WP:SC2|...}}
#' - {{redirect|WP:SOMETHING}}
#' - {{supplement|...|shortcut=WP:SOMETHING}}
#' - {{guidance essay|WP:SOMETHING}}
#' - {{shortcut|WP:SOMETHING}} (only first occurrence before headings)
#'
#' Only returns the first shortcut found before any heading (==).
#' For redirect-multi, the first parameter is the count of shortcuts to extract.
#'
#' @export
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
#' Handles multiple template formats that appear at the top of pages:
#' - {{subcat guideline|...|WP:SHORTCUT}}
#' - {{redirect-multi|count|WP:SC1|WP:SC2|...}}
#' - {{redirect|WP:SOMETHING}}
#' - {{supplement|...|shortcut=WP:SOMETHING}}
#' - {{guidance essay|WP:SOMETHING}}
#' - {{shortcut|WP:SOMETHING}}
#'
#' Returns all shortcuts found before any heading (==).
#' For redirect-multi, extracts all shortcuts specified by the count parameter.
#'
#' @export
wiki_extract_self_shortcuts <- function(wikitext) {
  if (is.na(wikitext) || wikitext == "") return(character(0))
  shortcuts <- character(0)

  # Extract text before first heading (real or fake)
  first_real_heading <- str_locate(wikitext, "==|^==")[1, 1]
  first_fake_heading <- str_locate(wikitext, "\\{\\{fake heading")[1, 1]

  # Find whichever comes first
  first_heading_pos <- min(c(first_real_heading, first_fake_heading), na.rm = TRUE)

  if (is.finite(first_heading_pos)) {
    text_before_heading <- str_sub(wikitext, 1, first_heading_pos - 1)
  } else {
    text_before_heading <- wikitext  # No headings found
  }

  # Pattern 1: {{subcat guideline|...}} templates with last parameter as shortcut
  subcat_matches <- str_match_all(text_before_heading, "\\{\\{[Ss]ubcat [^}]+\\|([^|}]+)\\}\\}")[[1]]
  if (nrow(subcat_matches) > 0) {
    for (i in seq_len(nrow(subcat_matches))) {
      full_match <- subcat_matches[i, 1]
      params <- str_split(full_match, "\\|")[[1]]
      if (length(params) > 0) {
        last_param <- str_trim(str_remove(params[length(params)], "\\}\\}$"))
        if (str_detect(last_param, "^(WP:|Wikipedia:)")) {
          shortcuts <- c(shortcuts, last_param)
        }
      }
    }
  }

  # Pattern 2: {{redirect-multi|count|WP:SC1|WP:SC2|...}} templates (all shortcuts)
  redirect_multi_matches <- str_match_all(text_before_heading, "\\{\\{[Rr]edirect-multi\\|([^}]+)\\}\\}")[[1]]
  if (nrow(redirect_multi_matches) > 0) {
    for (i in seq_len(nrow(redirect_multi_matches))) {
      content <- redirect_multi_matches[i, 2]
      parts <- str_split(content, "\\|")[[1]] %>% str_trim()

      if (length(parts) > 0 && str_detect(parts[1], "^\\d+$")) {
        count <- as.integer(parts[1])
        if (length(parts) > count) {
          potential_shortcuts <- parts[2:(count + 1)]
          for (shortcut in potential_shortcuts) {
            if (str_detect(shortcut, "^(WP:|Wikipedia:)")) {
              shortcuts <- c(shortcuts, shortcut)
            }
          }
        }
      }
    }
  }

  # Pattern 3: {{redirect|WP:SOMETHING}} templates (all occurrences)
  redirect_matches <- str_match_all(text_before_heading, "\\{\\{[Rr]edirect\\|([^|}]+)")[[1]]
  if (nrow(redirect_matches) > 0) {
    for (i in seq_len(nrow(redirect_matches))) {
      shortcut <- str_trim(redirect_matches[i, 2])
      if (str_detect(shortcut, "^(WP:|Wikipedia:)")) {
        shortcuts <- c(shortcuts, shortcut)
      }
    }
  }

  # Pattern 4: {{supplement|...|shortcut=WP:SOMETHING}} (all occurrences)
  supplement_matches <- str_match_all(text_before_heading, "\\{\\{[Ss]upplement\\|[^}]*shortcut\\s*=\\s*([^|}]+)")[[1]]
  if (nrow(supplement_matches) > 0) {
    for (i in seq_len(nrow(supplement_matches))) {
      shortcut <- str_trim(supplement_matches[i, 2])
      if (str_detect(shortcut, "^(WP:|Wikipedia:)")) {
        shortcuts <- c(shortcuts, shortcut)
      }
    }
  }

  # Pattern 5: {{guidance essay|WP:SOMETHING}} (all occurrences)
  guidance_matches <- str_match_all(text_before_heading, "\\{\\{[Gg]uidance essay\\|([^|}]+)")[[1]]
  if (nrow(guidance_matches) > 0) {
    for (i in seq_len(nrow(guidance_matches))) {
      shortcut <- str_trim(guidance_matches[i, 2])
      if (str_detect(shortcut, "^(WP:|Wikipedia:)")) {
        shortcuts <- c(shortcuts, shortcut)
      }
    }
  }

  # Pattern 6: {{shortcut|WP:SOMETHING}} (all shortcuts from all occurrences)
  shortcut_matches <- wiki_extract_policy_shortcuts(text_before_heading)
  if (length(shortcut_matches) > 0) {
    shortcuts <- c(shortcuts, shortcut_matches)
  }
  # Pattern 7: {{Policy|WP:SHORTCUT1|WP:SHORTCUT2|...}}
  policy_matches <- str_match_all(text_before_heading, "\\{\\{[Pp]olicy\\|([^}]+)\\}\\}")[[1]]
  if (nrow(policy_matches) > 0) {
    for (i in seq_len(nrow(policy_matches))) {
      content <- policy_matches[i, 2]
      parts <- str_split(content, "\\|")[[1]] %>% str_trim()

      for (shortcut in parts) {
        if (str_detect(shortcut, "^(WP:|Wikipedia:)")) {
          shortcuts <- c(shortcuts, shortcut)
        }
      }
    }
  }

  # Pattern 8: {{Wikipedia subcat guideline|...|WP:SHORTCUT1|WP:SHORTCUT2|...}}
  wiki_subcat_matches <- str_match_all(text_before_heading, "\\{\\{[Ww]ikipedia subcat guideline\\|([^}]+)\\}\\}")[[1]]
  if (nrow(wiki_subcat_matches) > 0) {
    for (i in seq_len(nrow(wiki_subcat_matches))) {
      content <- wiki_subcat_matches[i, 2]
      parts <- str_split(content, "\\|")[[1]] %>% str_trim()

      for (shortcut in parts) {
        if (str_detect(shortcut, "^(WP:|Wikipedia:)")) {
          shortcuts <- c(shortcuts, shortcut)
        }
      }
    }
  }

  # Pattern 9: {{redirect2|WP:SHORTCUT1|WP:SHORTCUT2|text=...}}
  redirect2_matches <- str_match_all(text_before_heading, "\\{\\{[Rr]edirect2\\|([^}]+)\\}\\}")[[1]]
  if (nrow(redirect2_matches) > 0) {
    for (i in seq_len(nrow(redirect2_matches))) {
      content <- redirect2_matches[i, 2]
      # Split by | and extract only shortcuts (before any named parameters like text=)
      parts <- str_split(content, "\\|")[[1]] %>% str_trim()

      for (part in parts) {
        # Stop processing when we hit a named parameter (contains =)
        if (str_detect(part, "=")) break

        # Only add if it's a WP: or Wikipedia: shortcut
        if (str_detect(part, "^(WP:|Wikipedia:)")) {
          shortcuts <- c(shortcuts, part)
        }
      }
    }
  }

  # Return unique shortcuts
  unique(shortcuts)
}
