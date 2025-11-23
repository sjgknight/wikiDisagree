
#' Mark Wikipedia policy links in HTML with custom attributes
#'
#' Adds a custom class and `data-policy` attribute to Wikipedia policy links in HTML
#' content. This allows targeted CSS styling and filtering, e.g., in `reactable` tables.
#'
#' The function handles links in the following forms:
#' - `/wiki/WP:POLICY`
#' - `/wiki/Wikipedia:POLICY`
#'
#' Existing classes are preserved and `wp-policy-link` is appended if not already present.
#' The `data-policy` attribute is added only if it does not already exist.
#'
#' @param html Character string or vector of HTML content containing `<a>` links.
#' @param policy_terms Character vector of policy terms to mark
#'   (e.g., `c("WP:MEDRS", "Wikipedia:NPOV")`).
#'
#' @return Character vector of modified HTML strings with policy links marked.
#'
#' @examples
#' \dontrun{
#' html <- '<a href="/wiki/WP:MEDRS">MEDRS</a>'
#' policies <- c("WP:MEDRS", "WP:NPOV")
#' mark_policy_links(html, policies)
#'
#' html2 <- '<a href="/wiki/Wikipedia:RS" class="mw-redirect">WP:RS</a>'
#' mark_policy_links(html2, c("RS"))
#' }
#'
#' @export
wikipolicy_mark <- function(html, policy_terms) {
  # Extract policy names without prefixes
  policy_names <- unique(gsub("^(WP:|Wikipedia:)", "", policy_terms))

  vapply(html, function(single_html) {
    if (is.na(single_html) || single_html == "") return(single_html)

    # Parse HTML safely
    doc <- xml2::read_html(single_html)

    # Select all <a> links
    links <- rvest::html_elements(doc, "a")

    for (link in links) {
      href <- xml2::xml_attr(link, "href")
      if (is.na(href)) next

      for (policy_name in policy_names) {
        pattern <- paste0("/wiki/(WP:|Wikipedia:)", policy_name, "$")
        if (grepl(pattern, href)) {
          # Handle class attribute
          old_class <- xml2::xml_attr(link, "class")
          new_class <- if (!is.na(old_class)) {
            if (grepl("wp-policy-link", old_class)) old_class else paste(old_class, "wp-policy-link")
          } else {
            "wp-policy-link"
          }
          xml2::xml_set_attr(link, "class", new_class)

          # Handle data-policy attribute
          if (is.na(xml2::xml_attr(link, "data-policy"))) {
            xml2::xml_set_attr(link, "data-policy", policy_name)
          }
        }
      }
    }

    # Return HTML as string (cleaned)
    paste(xml2::xml_children(xml2::xml_find_first(doc, "//body")), collapse = "")

  }, FUN.VALUE = character(1))
}


wikipolicy_highlight_terms <- function(text, terms) {
  for (term in terms) {
    # Replace term with <mark>term</mark>
    text <- str_replace_all(text, fixed(term), paste0("<mark>", term, "</mark>"))
  }
  text
}




