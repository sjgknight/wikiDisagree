wikitext_to_markdown <- function(text) {

  md <- text

  # Headings
  md <- gsub("^====== (.*?) ======$", "###### \\1",  md, perl = TRUE)
  md <- gsub("^===== (.*?) =====$",   "##### \\1",   md, perl = TRUE)
  md <- gsub("^==== (.*?) ====$",     "#### \\1",    md, perl = TRUE)
  md <- gsub("^=== (.*?) ===$",       "### \\1",     md, perl = TRUE)
  md <- gsub("^== (.*?) ==$",         "## \\1",      md, perl = TRUE)

  # Bold & Italic
  md <- gsub("'''(.*?)'''", "**\\1**", md, perl = TRUE)
  md <- gsub("''(.*?)''",   "*\\1*",   md, perl = TRUE)

  # Links [[Page|Text]] → [Text](Page)
  md <- gsub("\\[\\[([^\\]|]+)\\|([^\\]]+)\\]\\]", "[\\2](\\1)", md, perl = TRUE)

  # Links [[Page]] → [Page](Page)
  md <- gsub("\\[\\[([^\\]]+)\\]\\]", "[\\1](\\1)", md, perl = TRUE)

  # Lists
  md <- gsub("^\\* ", "- ", md, perl = TRUE)
  md <- gsub("^# ", "1. ", md, perl = TRUE)

  # Remove common wiki templates (optional)
  md <- gsub("\\{\\{[^}]+\\}\\}", "", md, perl = TRUE)

  # Trim excess whitespace
  md <- trimws(md)

  md
}

wikitext_to_markdown_links <- function(text) {

  md <- text

  # 1. Convert internal wiki links [[Page]] or [[Page|Label]] → [Label](https://en.wikipedia.org/wiki/Page)
  # [[Talk:Hello world]] → [Talk:Hello world](https://en.wikipedia.org/wiki/Talk:Hello world)
  md <- gsub(
    "\\[\\[([^\\]|]+)\\|([^\\]]+)\\]\\]",
    "[\\2](https://en.wikipedia.org/wiki/\\1)",
    md,
    perl = TRUE
  )

  md <- gsub(
    "\\[\\[([^\\]]+)\\]\\]",
    "[\\1](https://en.wikipedia.org/wiki/\\1)",
    md,
    perl = TRUE
  )

  # 2. Leave full external links [https://...] intact for commonmark to process
  # No change needed — commonmark already renders [https://...] as clickable links

  md
}


wikitext_to_html <- function(text) {

  md <- wikitext_to_markdown_links(text)
  md <- wikitext_to_markdown(md)

  # Fix wiki-style links

  html <- commonmark::markdown_html(md)

  return(html)

}
