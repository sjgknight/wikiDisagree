library(httr2)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(tidyr)

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
#' get_wikitext_by_title("Wikipedia:Verifiability")
#' }
#'
#' @export
get_wikitext_by_title <- function(page_title) {
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

#' Build Wikipedia policy hierarchy from a set of shortcuts
#'
#' Creates a hierarchy showing parent-child relationships between Wikipedia
#' policies and their shortcuts.
#'
#' @param shortcuts Character vector of Wikipedia policy shortcuts (e.g., c("WP:RS", "WP:MEDRS"))
#' @param include_referenced Logical, whether to also fetch policies referenced
#'   in the wikitext of the input policies (default: TRUE)
#' @param max_depth Integer, maximum depth to traverse references (default: 2)
#'
#' @return List containing:
#'   - policies: Tibble of all policies with their wikitext
#'   - redirects: Tibble showing which shortcuts redirect where
#'   - hierarchy: Tibble showing parent-child relationships
#'   - shortcuts_map: Tibble mapping each policy to its official shortcuts
#'
#' @details
#' This function:
#' 1. Resolves all shortcuts to their target pages
#' 2. Fetches wikitext for all target pages
#' 3. Extracts official shortcuts from each page
#' 4. Optionally finds referenced policies in the wikitext
#' 5. Builds a hierarchy showing relationships
#'
#' @examples
#' \dontrun{
#' policies <- c("WP:RS", "WP:MEDRS", "WP:NPOV", "WP:V")
#' hierarchy <- build_policy_hierarchy(policies)
#'
#' # View the hierarchy
#' hierarchy$hierarchy
#'
#' # View which shortcuts map to which pages
#' hierarchy$redirects
#'
#' # View official shortcuts for each policy
#' hierarchy$shortcuts_map
#' }
#'
#' @export
build_policy_hierarchy <- function(shortcuts,
                                   include_referenced = TRUE,
                                   max_depth = 3) {

  message("Step 1: Resolving shortcuts to target pages...")

  # Resolve all shortcuts to their target pages
  redirects <- map_dfr(shortcuts, resolve_policy_redirect)

  # Get unique target pages
  target_pages <- unique(na.omit(redirects$target_page))

  message(sprintf("Found %d unique target pages from %d shortcuts",
                  length(target_pages), length(shortcuts)))

  # Fetch wikitext for all target pages
  message("Step 2: Fetching wikitext for all target pages...")

  policies <- map_dfr(target_pages, get_wikitext_by_title)

  # Extract official shortcuts from each policy page
  message("Step 3: Extracting official shortcuts from each policy...")

  policy_pages <- policies %>%
    mutate(
      shortcuts = map(wikitext, wiki_extract_policy_shortcuts),
      num_shortcuts = map_int(shortcuts, length),
      self_shortcuts = map(wikitext, wiki_extract_self_shortcuts)
    )

  # Extract referenced policies
  all_pages <- policy_pages
  referenced_policies <- tibble()

  if (include_referenced && max_depth > 0) {
    message(sprintf("Step 4: Extracting referenced policies (max depth: %d)...", max_depth))

    current_depth <- 1
    processed_pages <- target_pages

    while (current_depth <= max_depth) {
      message(sprintf("  Processing depth %d...", current_depth))

      # Extract all referenced shortcuts from current pages
      all_referenced <- policy_pages %>%
        filter(page_title %in% processed_pages) %>%
        mutate(referenced = map(wikitext, wiki_extract_policy_shortcuts)) %>%
        select(page_title, referenced) %>%
        unnest(referenced) %>%
        distinct()

      if (nrow(all_referenced) == 0) break

      # Resolve new references
      new_shortcuts <- setdiff(all_referenced$referenced, redirects$shortcut)

      if (length(new_shortcuts) == 0) break

      message(sprintf("    Found %d new policy references", length(new_shortcuts)))

      # Resolve new shortcuts
      new_redirects <- map_dfr(new_shortcuts, resolve_policy_redirect)
      redirects <- bind_rows(redirects, new_redirects)

      # Get new target pages
      new_targets <- setdiff(na.omit(new_redirects$target_page), all_pages$page_title)

      if (length(new_targets) == 0) break

      message(sprintf("    Fetching %d new policy pages", length(new_targets)))

      # Fetch wikitext for new pages
      new_policies <- map_dfr(new_targets, get_wikitext_by_title) %>%
        mutate(
          shortcuts = map(wikitext, wiki_extract_policy_shortcuts),
          num_shortcuts = map_int(shortcuts, length)
        )

      all_pages <- bind_rows(all_pages, new_policies)
      policy_pages <- all_pages
      processed_pages <- new_targets

      current_depth <- current_depth + 1
    }
  }

  # Build hierarchy
  message("Step 5: Building policy hierarchy...")

  # Create parent-child relationships
  # hierarchy <- policy_pages %>%
  #   select(page_title, wikitext) %>%
  #   mutate(
  #     referenced_policies = map(wikitext, wiki_extract_policy_shortcuts)
  #   ) %>%
  #   unnest(referenced_policies, keep_empty = TRUE) %>%
  #   left_join(
  #     redirects %>% select(shortcut, target_page),
  #     by = c("referenced_policies" = "shortcut")
  #   ) %>%
  #   filter(!is.na(target_page)) %>%
  #   select(
  #     parent = page_title,
  #     child_shortcut = referenced_policies,
  #     wikitext = wikitext
  #   ) %>%
  #   distinct()

  message("✓ Policy hierarchy built successfully!")
#
#   list(
#     policies = policy_pages,
#     redirects = redirects,
#     hierarchy = hierarchy
#     )

  policy_pages
}


#' Visualize policy hierarchy as a tree
#'
#' Creates a simple text-based tree visualization of policy relationships.
#'
#' @param hierarchy_result Result from build_policy_hierarchy()
#' @param root_policy Character string of root policy to start from (optional)
#'
#' @return Prints tree to console
#'
#' @export
print_policy_tree <- function(hierarchy_result, root_policy = NULL) {
  hierarchy <- hierarchy_result$hierarchy

  if (is.null(root_policy)) {
    # Find policies that are never children (potential roots)
    roots <- setdiff(hierarchy$parent, hierarchy$child_shortcut)
    if (length(roots) == 0) {
      roots <- unique(hierarchy$parent)[1]
    }
  } else {
    roots <- root_policy
  }

  print_node <- function(page, indent = "") {
    cat(indent, "├─ ", page, "\n", sep = "")

    children <- hierarchy %>%
      filter(parent == page) %>%
      pull(child_shortcut) %>%
      unique()

    if (length(children) > 0) {
      for (i in seq_along(children)) {
        new_indent <- paste0(indent, "│  ")
        if (i == length(children)) {
          new_indent <- paste0(indent, "   ")
        }
        print_node(children[i], new_indent)
      }
    }
  }

  for (root in roots) {
    cat("\n", root, "\n", sep = "")
    children <- hierarchy %>%
      filter(parent == root) %>%
      pull(child_shortcut) %>%
      unique()

    for (child in children) {
      print_node(child, "")
    }
  }
}

