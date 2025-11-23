#' Create summary statistics of Wikipedia policy usage
#'
#' Generates a frequency table showing how often each Wikipedia policy is
#' referenced across the dataset. Unnests the list column of policies and
#' counts occurrences of each unique policy reference.
#'
#' @param df A dataframe containing a list column named 'wp_policies' with
#'   policy references extracted from Wikipedia pages. Each element should be
#'   a character vector of policy names (e.g., "WP:MEDRS", "WP:NPOV").
#'
#' @return A tibble with columns:
#'   \itemize{
#'     \item Policy: Character string of the policy name (e.g., "WP:MEDRS")
#'     \item Count: Integer count of how many times this policy appears
#'     \item Percentage: Numeric percentage of total policy references
#'     \item n_edits: Integer count of unique page edits containing this policy
#'   }
#'   Sorted by Count in descending order (most frequent policies first).
#'   Returns an empty tibble if no policies are found.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Filters out rows where wp_policies is empty or NULL
#'   \item Unnests the wp_policies list column to create one row per policy reference
#'   \item Counts occurrences of each policy
#'   \item Calculates percentage of total references
#'   \item Counts unique pages containing each policy
#'   \item Sorts by frequency (descending)
#' }
#'
#' @examples
#' \dontrun{
#' # Assuming results_with_content has a wp_policies column
#' summary <- create_policy_summary(results_with_content)
#'
#' # View top 10 most common policies
#' head(summary, 10)
#'
#' # Filter for specific policies
#' summary %>% filter(stringr::str_detect(Policy, "MED"))
#' }
#'
#' @importFrom dplyr select filter mutate count arrange desc n_distinct group_by summarise
#' @importFrom tidyr unnest
#' @importFrom tibble tibble
#'
#' @export
create_policy_summary <- function(df, policy_col = "wp_policies") {
  # Check if wp_policies column exists
  if (!policy_col %in% names(df)) {
    stop("Dataframe must contain a 'wp_policies' column")
  }

  # Store total number of edits for percentage calculation
  total_edits <- nrow(df)

  # Filter out rows with no policies (empty list or NULL)
  df_with_policies <- df %>%
    filter(lengths(.data[[policy_col]]) > 0)

  # Check if any policies were found
  if (nrow(df_with_policies) == 0) {
    warning("No policies found in the dataset")
    return(tibble(
      Policy = character(0),
      n_mentions = integer(0),
      n_edits = integer(0),
      pct_edits = numeric(0)
    ))
  }

  # Unnest policies and count occurrences
  policy_counts <- df_with_policies %>%
    select(pageid, all_of(policy_col)) %>%
    unnest(all_of(policy_col)) %>%
    rename(policy_temp = all_of(policy_col)) %>%
    group_by(policy_temp) %>%
    summarise(
      n_mentions = n(),           # Total times this policy was mentioned
      n_edits = n_distinct(pageid), # Number of unique edits mentioning this policy
      .groups = "drop"
    ) %>%
    mutate(
      pct_edits = round(n_edits / total_edits * 100, 2)  # % of all edits that mention this policy
    ) %>%
    rename(Policy = policy_temp) %>%
    arrange(desc(n_mentions))

  return(policy_counts)
}


#' Create detailed policy summary with article breakdown
#'
#' Generates an extended summary showing policy usage broken down by article
#' and providing additional context about where policies appear.
#'
#' @param df A dataframe containing columns: title (page title), pageid,
#'   wp_policies (list column of policy references), and optionally timestamp
#'
#' @return A list containing three tibbles:
#'   \itemize{
#'     \item overall: Overall policy frequency summary
#'     \item by_article: Policy counts broken down by article
#'     \item policy_cooccurrence: Matrix showing how often policies appear together
#'   }
#'
#' @examples
#' \dontrun{
#' detailed_summary <- create_detailed_policy_summary(results_with_content)
#' detailed_summary$overall
#' detailed_summary$by_article
#' }
#'
#' @export
create_detailed_policy_summary <- function(df) {
  # Overall summary
  overall <- create_policy_summary(df)

  # Extract article name from title (e.g., "Talk:COVID-19" -> "COVID-19")
  df_with_article <- df %>%
    mutate(article = stringr::str_extract(title, "(?<=Talk:)[^/]+"))

  # By article breakdown
  by_article <- df_with_article %>%
    filter(lengths(wp_policies) > 0) %>%
    select(article, pageid, wp_policies) %>%
    unnest(wp_policies) %>%
    group_by(article, wp_policies) %>%
    summarise(
      Count = n(),
      n_edits = n_distinct(pageid),
      .groups = "drop"
    ) %>%
    rename(Policy = wp_policies) %>%
    arrange(article, desc(Count))

  # Policy co-occurrence (which policies appear together)
  policy_pairs <- df_with_article %>%
    filter(lengths(wp_policies) >= 2) %>%
    select(pageid, wp_policies) %>%
    rowwise() %>%
    mutate(
      policy_pairs = list(
        combn(sort(wp_policies), 2, simplify = FALSE)
      )
    ) %>%
    unnest(policy_pairs) %>%
    mutate(
      policy1 = sapply(policy_pairs, `[`, 1),
      policy2 = sapply(policy_pairs, `[`, 2)
    ) %>%
    select(pageid, policy1, policy2) %>%
    group_by(policy1, policy2) %>%
    summarise(
      cooccurrence_count = n(),
      n_edits = n_distinct(pageid),
      .groups = "drop"
    ) %>%
    arrange(desc(cooccurrence_count))

  # Return list of summaries
  list(
    overall = overall,
    by_article = by_article,
    policy_cooccurrence = policy_pairs
  )
}


#' Filter policy summary to core policies only
#'
#' Filters a policy summary table to include only policies from a predefined
#' list of core policies (typically those you searched for).
#'
#' @param summary_df A tibble from create_policy_summary() with a Policy column
#' @param core_policies Character vector of core policy names to retain
#'
#' @return A filtered tibble containing only the specified core policies
#'
#' @examples
#' \dontrun{
#' summary <- create_policy_summary(results_with_content)
#' core_summary <- filter_core_policies(summary, policies_core)
#' }
#'
#' @export
filter_core_policies <- function(summary_df, core_policies) {
  # Create variations with both WP: and Wikipedia: prefixes
  core_with_prefix <- unique(c(
    paste0("WP:", core_policies),
    paste0("Wikipedia:", core_policies),
    core_policies
  ))

  summary_df %>%
    filter(Policy %in% core_with_prefix)
}
