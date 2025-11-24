#' Extract categories and/or templates from Wikipedia wikitext
#'
#' @param wikitext Character string of Wikipedia wikitext
#' @param part Character string: "category", "template", or "both" (default: "category")
#'
#' @return Character vector of unique category/template names, or list if part = "both"
#'
#' @details
#' Extracts:
#' - Categories: [[Category:Name]] or [[Category:Name|Sort key]]
#' - Templates: {{Template name}} or {{Template:Name|param=value}}
#'
#' When part = "both", returns a list with two elements: $categories and $templates
#'
#' @examples
#' \dontrun{
#' wikitext <- "Text [[Category:Living people]] {{cite book|title=Example}}"
#'
#' # Extract categories only
#' wiki_extract_parts(wikitext, part = "category")
#' # Returns: c("Living people")
#'
#' # Extract templates only
#' wiki_extract_parts(wikitext, part = "template")
#' # Returns: c("cite book")
#'
#' # Extract both
#' wiki_extract_parts(wikitext, part = "both")
#' # Returns: list(categories = c("Living people"), templates = c("cite book"))
#' }
#'
#' @export
wiki_extract_parts <- function(wikitext, part = "category") {
  if (is.na(wikitext) || wikitext == "") {
    if (part == "both") {
      return(list(categories = character(0), templates = character(0)))
    }
    return(character(0))
  }

  # Validate part parameter
  part <- tolower(part)
  if (!part %in% c("category", "template", "both")) {
    stop("part must be 'category', 'template', or 'both'")
  }

  # Extract categories
  if (part %in% c("category", "both")) {
    cat_pattern <- "\\[\\[Category:([^\\]|]+)(\\|[^\\]]*)?\\]\\]"
    cat_matches <- gregexpr(cat_pattern, wikitext, perl = TRUE, ignore.case = TRUE)
    cat_results <- regmatches(wikitext, cat_matches)[[1]]

    if (length(cat_results) > 0) {
      categories <- gsub(cat_pattern, "\\1", cat_results, ignore.case = TRUE)
      categories <- unique(trimws(categories))
    } else {
      categories <- character(0)
    }
  }

  # Extract templates
  if (part %in% c("template", "both")) {
    # Pattern matches {{Template}} or {{Template:Name}} or {{Template|params}}
    # This is complex because templates can be nested
    template_pattern <- "\\{\\{\\s*([Tt]emplate:)?([^{}|]+)"

    temp_matches <- gregexpr(template_pattern, wikitext, perl = TRUE)
    temp_results <- regmatches(wikitext, temp_matches)[[1]]

    if (length(temp_results) > 0) {
      # Extract template names (remove {{ and Template: prefix)
      templates <- gsub("^\\{\\{\\s*([Tt]emplate:)?", "", temp_results)
      # Remove any trailing spaces or parameters
      templates <- trimws(templates)
      # Get unique templates
      templates <- unique(templates)
    } else {
      templates <- character(0)
    }
  }

  # Return based on part parameter
  if (part == "category") {
    return(categories)
  } else if (part == "template") {
    return(templates)
  } else {  # both
    return(list(
      categories = categories,
      templates = templates
    ))
  }
}
