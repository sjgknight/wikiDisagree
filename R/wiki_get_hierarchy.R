library(httr2)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(tidyr)


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

  policies <- map_dfr(target_pages, wiki_get_wikitext_by_title)

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
      new_policies <- map_dfr(new_targets, wiki_get_wikitext_by_title) %>%
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




