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
