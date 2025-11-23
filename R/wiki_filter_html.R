library(rvest)
library(xml2)
library(purrr)
library(stringr)

#' Filter HTML sections that contain WP policy links (stringr method - FAST)
#'
#' Uses regex-based text splitting to extract complete sections. Faster than rvest but
#' less robust for malformed HTML. Good for well-structured Wikipedia HTML.
#'
#' @param html Character vector of HTML content
#' @param section_pattern Regex pattern to identify section starts (default matches mw-heading divs)
#' @param policy_pattern Regex pattern to identify policy links (default: "wp-policy-link")
#'
#' @return Character vector of filtered HTML containing only complete sections with policy links
#'
#' @details
#' This function:
#' 1. Splits HTML by section div pattern (keeping the delimiter)
#' 2. Each "section" includes the heading div and all content until the next heading
#' 3. Filters sections containing policy_pattern
#' 4. Reconstructs filtered HTML
#' 5. Much faster than rvest but assumes well-formed HTML
#'
#' @examples
#' \dontrun{
#' # Single HTML string
#' filtered <- filter_policy_sections_stringr(html_content)
#'
#' # In a dplyr pipeline
#' df <- df %>%
#'   mutate(content_filtered = filter_policy_sections_stringr(content_html))
#' }
#'
#' @export
wiki_filter_text_select <- function(html,
                             section_pattern = '<div class="[^"]*mw-heading[^"]*"',
                             policy_pattern = 'class="[^"]*wp-policy-link[^"]*"') {
  map_chr(html, function(single_html) {
    if (is.na(single_html) || single_html == "") return("")

    # Split by section pattern, keeping the delimiter as part of each section
    # Use positive lookahead to split BEFORE each section marker
    sections <- str_split(single_html, paste0("(?=", section_pattern, ")"))[[1]]

    # Remove empty sections
    sections <- sections[nchar(sections) > 0]

    if (length(sections) == 0) {
      # No sections found - check if entire HTML has policy links
      has_policy <- str_detect(single_html, policy_pattern)
      return(if (has_policy) single_html else "")
    }

    # Filter sections containing policy links
    sections_with_policy <- sections[str_detect(sections, policy_pattern)]

    if (length(sections_with_policy) == 0) {
      return("")
    }

    # Combine filtered sections
    paste(sections_with_policy, collapse = "\n\n")
  })
}



#' Filter HTML sections that contain WP policy links (rvest method - RECOMMENDED)
#'
#' Uses proper HTML parsing to extract complete sections (heading div + following content)
#' that contain at least one link with class="wp-policy-link". This is more robust
#' than regex-based text parsing.
#'
#' @param html Character vector of HTML content
#' @param section_class CSS class selector for section divs (default: "mw-heading")
#' @param policy_class CSS class for policy links (default: "wp-policy-link")
#'
#' @return Character vector of filtered HTML containing only sections with policy links
#'
#' @details
#' This function:
#' 1. Parses HTML properly using xml2/rvest
#' 2. Finds all section heading divs by class
#' 3. For each section, collects the heading + all following siblings until next section
#' 4. Checks if the complete section contains policy links
#' 5. Returns only complete sections with policy links
#' 6. Handles vectorized input for use in mutate()
#'
#' @examples
#' \dontrun{
#' # Single HTML string
#' filtered <- filter_policy_sections_rvest(html_content)
#'
#' # In a dplyr pipeline
#' df <- df %>%
#'   mutate(content_filtered = filter_policy_sections_rvest(content_html))
#' }
#'
#' @export
wiki_filter_html <- function(html,
                             section_class = "mw-heading",
                             policy_class = "wp-policy-link") {
  map_chr(html, function(single_html) {
    if (is.na(single_html) || single_html == "") return("")

    tryCatch({
      # Parse HTML
      doc <- read_html(single_html)

      # Find all section heading divs that contain the section_class
      section_headings <- html_elements(doc, xpath = sprintf("//div[contains(@class, '%s')]", section_class))

      if (length(section_headings) == 0) {
        # No sections found, return empty or original based on whether it has policy links
        has_policy <- length(html_elements(doc, sprintf("a.%s", policy_class))) > 0
        return(if (has_policy) single_html else "")
      }

      # For each section heading, collect it and all following siblings until the next section
      complete_sections <- map(section_headings, function(heading) {
        # Start with the heading itself
        section_nodes <- list(heading)

        # Get all following siblings
        current <- heading
        while (TRUE) {
          next_sibling <- xml_find_first(current, "./following-sibling::*[1]")

          # Stop if no more siblings or if we hit another section heading
          if (length(next_sibling) == 0 || inherits(next_sibling, "xml_missing")) break

          # Check if this sibling is another section heading
          sibling_class <- xml_attr(next_sibling, "class")
          if (!is.na(sibling_class) && grepl(section_class, sibling_class)) break

          # Add this sibling to our section
          section_nodes <- c(section_nodes, list(next_sibling))
          current <- next_sibling
        }

        section_nodes
      })

      # Filter sections that contain policy links
      sections_with_policy <- complete_sections[map_lgl(complete_sections, function(section_nodes) {
        # Check all nodes in this section for policy links
        any(map_lgl(section_nodes, function(node) {
          policy_links <- html_elements(node, sprintf("a.%s", policy_class))
          length(policy_links) > 0
        }))
      })]

      if (length(sections_with_policy) == 0) {
        return("")
      }

      # Convert filtered sections back to HTML
      filtered_html <- map_chr(sections_with_policy, function(section_nodes) {
        paste(map_chr(section_nodes, as.character), collapse = "\n")
      })

      paste(filtered_html, collapse = "\n\n")

    }, error = function(e) {
      warning(sprintf("Error parsing HTML: %s", e$message))
      return("")
    })
  })
}



#' Remove HTML sections that do NOT contain WP policy links (rvest method)
#'
#' Uses proper HTML parsing to remove complete sections (heading div + following content)
#' that do NOT contain policy links. Preserves all other content and HTML structure.
#'
#' @param html Character vector of HTML content
#' @param section_class CSS class selector for section divs (default: "mw-heading")
#' @param policy_class CSS class for policy links (default: "wp-policy-link")
#'
#' @return Character vector of HTML with non-policy sections removed
#'
#' @details
#' This function:
#' 1. Parses HTML properly using xml2/rvest
#' 2. Finds all section heading divs by class
#' 3. For each section, collects the heading + all following siblings until next section
#' 4. REMOVES sections that do NOT contain policy links
#' 5. Preserves all other HTML structure (wrapper divs, non-section content, etc.)
#' 6. Returns valid HTML with proper structure maintained
#'
#' @examples
#' \dontrun{
#' # Single HTML string
#' filtered <- remove_non_policy_sections_rvest(html_content)
#'
#' # In a dplyr pipeline
#' df <- df %>%
#'   mutate(content_filtered = remove_non_policy_sections_rvest(content_html))
#' }
#'
#' @export
wiki_filter_html <- function(html,
                                             section_class = "mw-heading",
                                             policy_class = "wp-policy-link") {
  map_chr(html, function(single_html) {
    if (is.na(single_html) || single_html == "") return("")

    tryCatch({
      # Parse HTML
      doc <- read_html(single_html)

      # Find all section heading divs that contain the section_class
      section_headings <- html_elements(doc, xpath = sprintf("//div[contains(@class, '%s')]", section_class))

      if (length(section_headings) == 0) {
        # No sections found, return original
        return(single_html)
      }

      # For each section heading, collect it and all following siblings until the next section
      # and check if it contains policy links
      for (heading in section_headings) {
        # Collect all nodes in this section
        section_nodes <- list(heading)
        nodes_to_check <- list(heading)

        # Get all following siblings until next section
        current <- heading
        while (TRUE) {
          next_sibling <- xml_find_first(current, "./following-sibling::*[1]")

          # Stop if no more siblings or if we hit another section heading
          if (length(next_sibling) == 0 || inherits(next_sibling, "xml_missing")) break

          # Check if this sibling is another section heading
          sibling_class <- xml_attr(next_sibling, "class")
          if (!is.na(sibling_class) && grepl(section_class, sibling_class)) break

          # Add this sibling to our section
          section_nodes <- c(section_nodes, list(next_sibling))
          nodes_to_check <- c(nodes_to_check, list(next_sibling))
          current <- next_sibling
        }

        # Check if this section contains policy links
        has_policy <- any(map_lgl(nodes_to_check, function(node) {
          policy_links <- html_elements(node, sprintf("a.%s", policy_class))
          length(policy_links) > 0
        }))

        # If section does NOT have policy links, remove all its nodes
        if (!has_policy) {
          for (node in section_nodes) {
            xml_remove(node)
          }
        }
      }

      # Return the modified HTML
      as.character(doc)

    }, error = function(e) {
      warning(sprintf("Error parsing HTML: %s", e$message))
      return(single_html)  # Return original on error
    })
  })
}


#' Remove HTML sections that do NOT contain WP policy links (stringr method - FAST)
#'
#' Uses regex-based text splitting to remove sections without policy links.
#' Faster than rvest and preserves HTML structure well for Wikipedia HTML.
#'
#' @param html Character vector of HTML content
#' @param section_pattern Regex pattern to identify section starts (default matches mw-heading divs)
#' @param policy_pattern Regex pattern to identify policy links (default: "wp-policy-link")
#'
#' @return Character vector of HTML with non-policy sections removed
#'
#' @details
#' This function:
#' 1. Splits HTML by section div pattern (keeping the delimiter)
#' 2. Each "section" includes the heading div and all content until the next heading
#' 3. REMOVES sections that do NOT contain policy_pattern
#' 4. Preserves HTML structure before first section and between kept sections
#' 5. Much faster than rvest
#'
#' @examples
#' \dontrun{
#' # Single HTML string
#' filtered <- remove_non_policy_sections_stringr(html_content)
#'
#' # In a dplyr pipeline
#' df <- df %>%
#'   mutate(content_filtered = remove_non_policy_sections_stringr(content_html))
#' }
#'
#' @export
wiki_filter_text <- function(html,
                                               section_pattern = '<div class="[^"]*mw-heading[^"]*"',
                                               policy_pattern = 'class="[^"]*wp-policy-link[^"]*"') {
  map_chr(html, function(single_html) {
    if (is.na(single_html) || single_html == "") return("")

    # Find the first section heading
    first_section <- str_locate(single_html, section_pattern)[1, "start"]

    # Extract content before first section (if any) - this gets kept
    preamble <- if (!is.na(first_section) && first_section > 1) {
      str_sub(single_html, 1, first_section - 1)
    } else {
      ""
    }

    # Extract the part with sections
    sections_part <- if (!is.na(first_section)) {
      str_sub(single_html, first_section, nchar(single_html))
    } else {
      single_html
    }

    # Split by section pattern, keeping the delimiter as part of each section
    sections <- str_split(sections_part, paste0("(?=", section_pattern, ")"))[[1]]

    # Remove empty sections
    sections <- sections[nchar(sections) > 0]

    if (length(sections) == 0) {
      # No sections found, return original
      return(single_html)
    }

    # Keep only sections containing policy links
    sections_with_policy <- sections[str_detect(sections, policy_pattern)]

    # Reconstruct HTML: preamble + filtered sections
    result_parts <- c()
    if (nchar(preamble) > 0) {
      result_parts <- c(result_parts, preamble)
    }
    if (length(sections_with_policy) > 0) {
      result_parts <- c(result_parts, paste(sections_with_policy, collapse = ""))
    }

    if (length(result_parts) == 0) {
      return(preamble)  # Return just preamble if no sections have policies
    }

    paste(result_parts, collapse = "")
  })
}

