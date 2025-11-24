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


#' Filter HTML to keep only sections containing Wikipedia policy links
#'
#' @param html Character vector of HTML content
#' @param policy_pattern Regex pattern to identify policy links (default: "wp-policy-link")
#' @param keep_wrapper Logical, whether to keep outer wrapper divs (default: TRUE)
#'
#' @return Character vector of HTML with non-policy sections removed
#'
#' @details
#' This function:
#' 1. Identifies top-level discussion sections (mw-heading divs)
#' 2. For each section, checks if it or its nested replies contain policy links
#' 3. Removes entire sections that don't contain any policy references
#' 4. Preserves HTML structure, including nested replies and formatting
#' 5. Keeps the outer wrapper div for proper rendering
#'
#' The function works by:
#' - Finding section boundaries using heading markers
#' - Including all nested content within each section
#' - Filtering based on policy link presence in the entire section tree
#'
#' @examples
#' \dontrun{
#' # Single HTML string
#' filtered <- wiki_filter_text(html_content)
#'
#' # In a dplyr pipeline
#' df <- df %>%
#'   mutate(content_filtered = wiki_filter_text(content_html))
#' }
#'
#' @export
wiki_filter_text <- function(html,
                             policy_pattern = 'wp-policy-link',
                             keep_wrapper = TRUE) {

  map_chr(html, function(single_html) {
    if (is.na(single_html) || single_html == "") return("")

    # Pattern to match top-level section headings
    # These are the main discussion threads
    heading_pattern <- '<div class="[^"]*mw-heading[^"]*mw-heading2[^"]*"[^>]*>'

    # Find all heading positions
    heading_starts <- str_locate_all(single_html, heading_pattern)[[1]]

    if (nrow(heading_starts) == 0) {
      # No sections found - check if whole content has policies
      if (str_detect(single_html, policy_pattern)) {
        return(single_html)
      } else {
        # Extract and return just the wrapper
        wrapper_match <- str_match(single_html, '^(<div[^>]*mw-parser-output[^>]*>).*')
        if (!is.na(wrapper_match[1, 2]) && keep_wrapper) {
          return(paste0(wrapper_match[1, 2], "</div>"))
        }
        return("")
      }
    }

    # Extract wrapper (everything before first heading)
    first_heading_start <- heading_starts[1, "start"]
    wrapper <- str_sub(single_html, 1, first_heading_start - 1)

    # Extract closing wrapper tags (everything after last section)
    # Look for closing </div> tags at the end
    closing_tags <- str_extract(single_html, "</div>\\s*(?:<!--[^>]*-->\\s*)?</div>\\s*$")
    if (is.na(closing_tags)) {
      closing_tags <- "" #"</div>"
    }

    # Define section boundaries
    section_boundaries <- c(
      heading_starts[, "start"],
      nchar(single_html) + 1  # End of document
    )

    # Extract each section and check for policy links
    sections_to_keep <- list()

    for (i in seq_len(nrow(heading_starts))) {
      section_start <- section_boundaries[i]
      section_end <- section_boundaries[i + 1] - 1

      section_content <- str_sub(single_html, section_start, section_end)

      # Keep section if it contains policy links
      if (str_detect(section_content, policy_pattern)) {
        sections_to_keep <- c(sections_to_keep, list(section_content))
      }
    }

    # Reconstruct HTML
    if (length(sections_to_keep) == 0) {
      # No sections with policies - return empty wrapper
      if (keep_wrapper) {
        return(paste0(wrapper, closing_tags))
      } else {
        return("")
      }
    }

    # Combine kept sections
    filtered_sections <- paste(sections_to_keep, collapse = "\n")

    # Return reconstructed HTML
    paste0(wrapper, filtered_sections, closing_tags)
  })
}

#' Filter Wikipedia talk page HTML to keep only sections with policy discussions
#'
#' @param html Character vector of HTML content from Wikipedia talk pages
#' @param policy_patterns Vector of patterns to match policies
#' @param preserve_wrapper Logical, whether to wrap output in content-box div
#' @param min_policy_mentions Minimum number of policy mentions to keep section (default: 1)
#'
#' @return Character vector of filtered HTML
#'
#' @export
wiki_filter_policy_sections <- function(html,
                                        policy_patterns = NULL,
                                        preserve_wrapper = TRUE,
                                        min_policy_mentions = 1) {

  # Default patterns - look for policies in multiple ways
  if (is.null(policy_patterns)) {
    policy_patterns <- c(
      'class="[^"]*wp-policy-link[^"]*"',  # Policy link class
      'data-policy="[^"]+"',                # Data attribute
      '\\bWP:[A-Z][A-Z0-9]+\\b',           # WP:SHORTCUT in text
      '\\bMEDRS\\b',                        # Common policy names in ANY context
      '\\bNOTNEWS\\b',
      '\\bNPOV\\b',
      '\\bVERIFY\\b',
      '\\b(?:WP:)?RS\\b'
    )
  }

  # Combine patterns
  combined_pattern <- paste(policy_patterns, collapse = "|")

  # Add content-box wrapper function
  add_wrapper <- function(content) {
    if (preserve_wrapper && nchar(content) > 0) {
      paste0(
        '<div class="content-box" style="max-height: 400px; overflow-y: auto; border: 1px solid #ddd; padding: 10px;">\n',
        content,
        '\n</div>'
      )
    } else {
      content
    }
  }

  map_chr(html, function(single_html) {
    if (is.na(single_html) || single_html == "") return(add_wrapper(""))

    # Remove existing content-box wrapper if present
    single_html <- str_replace(single_html, '^<div class="content-box"[^>]*>\\s*', '')
    single_html <- str_replace(single_html, '\\s*</div>\\s*$', '')

    # Find section markers
    section_pattern <- '<div class="mw-heading mw-heading2[^>]*ext-discussiontools-init-section[^>]*">'
    section_starts <- str_locate_all(single_html, section_pattern)[[1]]

    if (nrow(section_starts) == 0) {
      # No structured sections - check whole content
      policy_count <- str_count(single_html, combined_pattern)
      if (policy_count >= min_policy_mentions) {
        return(add_wrapper(single_html))
      } else {
        # Return just the mw-parser-output wrapper (empty)
        wrapper_match <- str_extract(single_html, '<div class="mw-content-ltr mw-parser-output"[^>]*>')
        if (!is.na(wrapper_match)) {
          return(add_wrapper(paste0(wrapper_match, "\n</div>")))
        }
        return(add_wrapper(""))
      }
    }

    # Find positions of all section starts
    section_starts <- section_starts[, "start"]

    # Extract everything before first section (wrapper + any preamble content)
    preamble <- str_sub(single_html, 1, section_starts[1] - 1)

    # Find the closing tags (comments + closing divs at end)
    # Be more careful to preserve NewPP comments and structure
    closing_pattern <- "<!-- \\nNewPP limit report.*$"
    closing_match <- str_locate(single_html, closing_pattern)

    if (!is.na(closing_match[1, "start"])) {
      closing_start <- closing_match[1, "start"]
      closing <- str_sub(single_html, closing_start, nchar(single_html))
    } else {
      # Fallback: look for final closing div
      closing_start <- nchar(single_html) + 1
      closing <- "\n</div>"
    }

    # Process each section
    sections_to_keep <- list()

    for (i in seq_along(section_starts)) {
      # Determine section boundaries
      section_start <- section_starts[i]
      section_end <- if (i < length(section_starts)) {
        section_starts[i + 1] - 1
      } else {
        closing_start - 1
      }

      # Extract section content (includes heading + all nested content)
      section_content <- str_sub(single_html, section_start, section_end)

      # Count policy mentions
      policy_count <- str_count(section_content, combined_pattern)

      # Keep if meets threshold
      if (policy_count >= min_policy_mentions) {
        sections_to_keep <- c(sections_to_keep, list(section_content))
      }
    }

    # Reconstruct HTML
    if (length(sections_to_keep) == 0) {
      # No sections kept - return just wrapper structure
      return(add_wrapper(paste0(preamble, closing)))
    }

    # Combine all parts
    result <- paste0(
      preamble,
      paste(sections_to_keep, collapse = "\n"),
      closing
    )

    result
  })
}

# # Usage examples
# filtered <- wiki_filter_policy_sections(df$content_html)
#
# # More strict filtering (require at least 2 policy mentions)
# filtered_strict <- wiki_filter_policy_sections(
#   df$content_html,
#   min_policy_mentions = 2
# )
#
# # In a pipeline
# df_filtered <- df %>%
#   mutate(
#     content_filtered = wiki_filter_policy_sections(content_html),
#     n_sections_removed = str_count(content_html, "mw-heading2") -
#       str_count(content_filtered, "mw-heading2")
#   )

# to test
# writeLines(filtered, "test_output.html")
# browseURL("test_output.html")
