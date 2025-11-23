

#' Generate CSS stylesheet for policy highlighting
#'
#' Creates a CSS string with styling rules for highlighted policy links.
#' This can be included in HTML output to style the policy-highlight class.
#'
#' @param bg_color Character string for background color (default: "yellow")
#' @param text_color Character string for text color (default: "inherit")
#' @param font_weight Character string for font weight (default: "bold")
#' @param padding Character string for padding (default: "2px 4px")
#' @param border_radius Character string for border radius (default: "3px")
#' @param additional_styles Named list of additional CSS properties
#'
#' @return Character string containing CSS rules wrapped in <style> tags
#'
#' @examples
#' \dontrun{
#' # Default styling
#' css <- generate_policy_css()
#'
#' # Custom styling
#' css <- generate_policy_css(
#'   bg_color = "#ffeb3b",
#'   text_color = "#000",
#'   additional_styles = list("text-decoration" = "underline")
#' )
#'
#' # Include in your HTML output
#' cat(css)
#' }
#'
#' @export
generate_policy_css <- function(
    bg_color = "yellow",
    text_color = "inherit",
    font_weight = "bold",
    padding = "2px 4px",
    border_radius = "3px",
    additional_styles = NULL
) {

  base_styles <- c(
    sprintf("  background-color: %s;", bg_color),
    sprintf("  color: %s;", text_color),
    sprintf("  font-weight: %s;", font_weight),
    sprintf("  padding: %s;", padding),
    sprintf("  border-radius: %s;", border_radius)
  )

  # Add any additional styles
  if (!is.null(additional_styles)) {
    extra_styles <- sprintf("  %s: %s;", names(additional_styles), additional_styles)
    base_styles <- c(base_styles, extra_styles)
  }

  css <- paste0(
    "<style>\n",
    ".wp-policy-link {\n",
    paste(base_styles, collapse = "\n"),
    "\n}\n",
    "\n",
    "/* Optional: hover effect */\n",
    ".wp-policy-link:hover {\n",
    "  filter: brightness(0.95);\n",
    "  text-decoration: underline;\n",
    "}\n",
    "</style>"
  )

  return(css)
}


#' Add highlighting CSS to reactable
#'
#' Convenience function to inject policy highlighting CSS into a reactable
#' HTML widget or include it in an R Markdown document.
#'
#' @param ... Arguments passed to generate_policy_css()
#'
#' @return HTML dependency or HTML string that can be included in output
#'
#' @examples
#' \dontrun{
#' # In an R Markdown document or Shiny app:
#' library(htmltools)
#'
#' # Create your reactable
#' my_table <- reactable(data, ...)
#'
#' # Combine with CSS
#' browsable(tagList(
#'   HTML(generate_policy_css()),
#'   my_table
#' ))
#' }
#'
#' @export
add_policy_css_to_output <- function(...) {
  css_string <- generate_policy_css(...)
  htmltools::HTML(css_string)
}

# ------------------------------------------------------------------------------
# Create Interactive Table with Policy Filtering
# ------------------------------------------------------------------------------
create_interactive_table <- function(df, displaytext = content_html) {
  # Prepare data
  df_display <- df %>%
    mutate(
      wp_policies_str = map_chr(wp_policies, ~ {
        # .x is the character vector of policies for this row
        links <- sprintf(
          '<a href="https://wikipedia.org/wiki/%s" target="_blank">%s</a>',
          .x, .x
        )
        paste(links, collapse = ", ")
      }),
      row_collapsed = FALSE  # Track collapse state
    ) %>%
    select(title, pageid, timestamp, wp_policies_str, wp_policies, ensym(displaytext))

  reactable(
    df_display,
    columns = list(
      pageid = colDef(
        name = "Page ID",
        width = 50
      ),
      title = colDef(
        name = "Title",
        width = 100,
        cell = function(value, index) {
          pageid <- df_display$pageid[index]
          sprintf('<a href="https://en.wikipedia.org/?curid=%s" target="_blank">%s</a>',
                  pageid, value)
        },
        html = TRUE
      ),
      timestamp = colDef(
        name = "Date",
        width = 50,
        format = colFormat(datetime = TRUE)
      ),
      wp_policies_str = colDef(
        name = "WP Policies",
        width = 150,
        html = TRUE,
        filterable = TRUE,
        filterMethod = JS("
          function(rows, columnId, filterValue) {
            return rows.filter(function(row) {
              return row.values[columnId].toLowerCase().includes(filterValue.toLowerCase())
            })
          }
        ")
      ),
      wp_policies = colDef(show = FALSE),
      content_html = colDef(
        name = "Content",
        html = TRUE,
        minWidth = 800,
        cell = function(value, index) {
          # Create collapsible content
          sprintf('
            <div class="content-container">
              <button onclick="this.nextElementSibling.classList.toggle(\'collapsed\')"
                      style="margin-bottom: 10px; padding: 5px 10px; cursor: pointer;">
                Toggle Content
              </button>
              <div class="content-box" style="max-height: 400px; overflow-y: auto; border: 1px solid #ddd; padding: 10px;">
                %s
              </div>
            </div>
            <style>
              .collapsed { display: none; }
            </style>
          ', value)
        }
      )
    ),
    # Add CSS styles here for the whole table
    theme = reactableTheme(
      style = list(
        ".wp-policy-link" = list(
          backgroundColor = "yellow",
          fontWeight = "bold",
          padding = "2px 4px",
          borderRadius = "3px"
        ))),
    searchable = TRUE,
    filterable = TRUE,
    pagination = TRUE,
    defaultPageSize = 250,
    highlight = TRUE,
    compact = TRUE,
    defaultColDef = colDef(
      align = "left",
      headerStyle = list(background = "#f7f7f8")
    )
  )
}
#
# browsable(tagList(
#   HTML(generate_policy_css()),    # CSS styles
#   create_interactive_table(results_with_content)   # The reactable widget
# ))

