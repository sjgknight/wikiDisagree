#' Extract section titles associated with policy shortcuts
#'
#' Parses Wikipedia wikitext to identify shortcuts (e.g., WP:SOMETHING) and
#' associates each shortcut with the section heading it appears under.
#'
#' @param wikitext Character string containing Wikipedia wikitext markup
#' @param page_title Character string of the page title to use for shortcuts
#'   that appear before any headings. Defaults to NA_character_.
#'
#' @return A tibble with three columns:
#'   \describe{
#'     \item{line_num}{Integer line number where the shortcut was found}
#'     \item{shortcuts}{Character string of the shortcut (e.g., "WP:RS")}
#'     \item{heading}{Character string of the section heading under which the
#'       shortcut appears. If the shortcut appears before any headings, this
#'       will be the page_title value.}
#'   }
#'
#' @details
#' The function works in several steps:
#'
#' 1. **Parse headings**: Identifies all MediaWiki section headings
#'    (e.g., == Section ==, === Subsection ===) and extracts their text
#'    and hierarchy level
#'
#' 2. **Identify shortcuts**: Finds shortcuts in two formats:
#'    - Template format: {{shortcut|WP:SOMETHING}}
#'    - Parameter format: shortcut=WP:SOMETHING
#'
#' 3. **Associate shortcuts with sections**: Links each shortcut to the most
#'    recent heading that appears above it in the wikitext. Shortcuts that
#'    appear before any headings are associated with the page_title.
#'
#' HTML tags within headings are stripped during processing.
#'
#' @section Shortcut patterns recognized:
#' - {{shortcut|WP:ABC}}
#' - {{Shortcut|WP:ABC|WP:XYZ}} (case-insensitive)
#' - shortcut=WP:ABC
#' - Shortcuts starting with "WP:" or "Wikipedia:"
#'
#' @examples
#' \dontrun{
#' wikitext <- "
#' {{shortcut|WP:PAGE}}
#' Page introduction text.
#'
#' == Section 1 ==
#' {{shortcut|WP:SEC1}}
#' Section content.
#'
#' === Subsection 1.1 ===
#' {{shortcut|WP:SUB11|WP:SUBSEC}}
#' Subsection content.
#' "
#'
#' result <- wiki_extract_title(wikitext, page_title = "Wikipedia:Example")
#' # Returns:
#' # line_num  shortcuts  heading
#' # 2         WP:PAGE    Wikipedia:Example
#' # 6         WP:SEC1    Section 1
#' # 10        WP:SUB11   Subsection 1.1
#' # 10        WP:SUBSEC  Subsection 1.1
#' }
#'
#' @importFrom stringr str_split str_detect str_match str_replace str_trim
#'   str_remove_all regex
#' @importFrom dplyr tibble filter mutate select rowwise ungroup
#' @importFrom purrr map
#' @importFrom tidyr unnest
#'
#' @export
wiki_extract_title <- function(wikitext, page_title = NA_character_) {
  library(stringr)
  library(dplyr)
  library(purrr)
  library(tidyr)

  # Split into lines
  lines <- str_split(wikitext, "\n", simplify = FALSE)[[1]]

  # Detect headings
  # Matches == Heading == , === Subheading === , etc.
  heading_df <- tibble(
    line_num = seq_along(lines),
    text = lines
  ) %>%
    # Headings start and end with ===...===
    filter(str_detect(text, "^=+.*=+$")) %>%

    mutate(
      # heading level = number of leading =
      heading_level = str_match(text, "^(=+)")[,2] %>% nchar(),

      # Remove all HTML tags inside line
      text_no_html = str_remove_all(text, "<[^>]+>"),

      # Extract title between the =...= markers
      heading_title = text_no_html %>%
        str_replace("^=+", "") %>%
        str_replace("=+$", "") %>%
        str_trim()
    ) %>%
    select(line_num, heading_level, heading_title)

  shortcutstring <- "\\{\\{\\s*[Ss]hortcut\\s*\\|([^}]+)\\}\\}|[Ss]hortcut\\s*=\\s*(WP:[A-Za-z0-9/_-]+|Wikipedia:[A-Za-z0-9/_-]+)"

  # Detect shortcuts
  shortcut_df <- tibble(
    line_num = seq_along(lines),
    text = lines
  ) %>%
    dplyr::filter(str_detect(text, regex(shortcutstring, ignore_case =T))) %>%
    mutate(
      shortcuts = map(text, extract_policy_shortcuts)
    ) %>%
    select(line_num, shortcuts) %>%
    unnest(shortcuts)


  # Join each shortcut to the most recent heading ABOVE it
  shortcut_df %>%
    rowwise() %>%
    mutate(
      heading = {
        idx <- which(heading_df$line_num < line_num)
        if (length(idx) == 0) page_title else heading_df$heading_title[max(idx)]
      }
    ) %>%
    ungroup()
}
