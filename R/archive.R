# get_html_from_wikitext <- function(wikitext) {
#   if (is.na(wikitext) || wikitext == "") return(NA_character_)
#
#   encoded <- URLencode(wikitext, reserved = TRUE)
#
#   req <- request("https://en.wikipedia.org/w/api.php") |>
#     req_url_query(
#       action = "parse",
#       format = "json",
#       text = encoded,
#       prop = "text"
#     )
#
#   resp <- req_perform(req)
#   data <- resp_body_json(resp, simplifyVector = TRUE)
#
#   # Extract HTML if present
#   if (!is.null(data$parse$text$`*`)) {
#     return(data$parse$text$`*`)
#   } else {
#     return(NA_character_)
#   }
# }
#
# get_html_from_pageid <- function(pageid, section_index = NULL) {
#   if (is.na(pageid)) return(NA_character_)
#
#   query_list <- list(
#     action = "parse",
#     format = "json",
#     pageid = pageid,
#     prop = "text"
#   )
#
#   if (!is.null(section_index)) {
#     query_list$section <- section_index
#   }
#
#
#   req <- request("https://en.wikipedia.org/w/api.php") |>
#     req_url_query(!!!query_list)
#
#   resp <- req_perform(req)
#   json <- resp_body_json(resp)
#
#   html <- json$parse$text$`*`
#   if (!is.null(html)) html else NA_character_
# }
