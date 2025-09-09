# -----------------------------------------------------------------------------
# FILE: R/TG_jung_table.R
# STATUS: NEW
# -----------------------------------------------------------------------------

#' Create a descriptive table showing Jungian-style classifications
#'
#' This function takes a dataset with four Jungian dimensions (Extroversion,
#' Emotionality, Agreeableness, Conscientiousness) and classifies each individual
#' into a four-letter type based on their percentile scores.
#'
#' The four dimensions are:
#' \itemize{
#'   \item Extraversion (E) vs. Introversion (I)
#'   \item Emotionality (F) vs. Thinking (T)
#'   \item Agreeableness (A) vs. Disagreeableness (D)
#'   \item Conscientiousness (J) vs. Perceiving (P)
#' }
#'
#' @param dataset A data frame containing the data.
#' @param name The name of the column containing unique identifiers (e.g., "name").
#' @param extraversion_col The column name for Extraversion percentile scores.
#' @param emotionality_col The column name for Emotionality percentile scores.
#' @param agreeableness_col The column name for Agreeableness percentile scores.
#' @param conscientiousness_col The column name for Conscientiousness percentile scores.
#' @param percentile_threshold The threshold to determine the classification.
#'   A score above this threshold is classified as the first trait (e.g., E),
#'   and below is the second (e.g., I). Defaults to `50`.
#'
#' @return A data frame with each individual's name and their four-letter
#'   Jungian-style classification.
#' @export
TG_jung_table <- function(
    dataset,
    name = "name",
    extraversion_col = "Extroversion",
    emotionality_col = "Emotionality",
    agreeableness_col = "Agreeableness",
    conscientiousness_col = "Conscientiousness",
    percentile_threshold = 50
) {

  # Check if required columns exist in the dataset
  required_cols <- c(name, extraversion_col, emotionality_col, agreeableness_col, conscientiousness_col)
  if (!all(required_cols %in% names(dataset))) {
    missing_cols <- required_cols[!(required_cols %in% names(dataset))]
    stop(paste("The following required columns are missing from the dataset:", paste(missing_cols, collapse = ", ")))
  }

  jung_data <- dataset %>%
    dplyr::select(!!rlang::sym(name), !!rlang::sym(extraversion_col), !!rlang::sym(emotionality_col), !!rlang::sym(agreeableness_col), !!rlang::sym(conscientiousness_col)) %>%
    dplyr::rename(
      id = !!rlang::sym(name),
      extroversion = !!rlang::sym(extraversion_col),
      emotionality = !!rlang::sym(emotionality_col),
      agreeableness = !!rlang::sym(agreeableness_col),
      conscientiousness = !!rlang::sym(conscientiousness_col)
    )

  # Classify individuals based on percentile threshold
  jung_classified <- jung_data %>%
    dplyr::mutate(
      ei_type = ifelse(extroversion > percentile_threshold, "E", "I"),
      tf_type = ifelse(emotionality > percentile_threshold, "F", "T"),
      ad_type = ifelse(agreeableness > percentile_threshold, "A", "D"),
      jp_type = ifelse(conscientiousness > percentile_threshold, "J", "P")
    ) %>%
    dplyr::mutate(
      jung_type = paste0(ei_type, tf_type, ad_type, jp_type)
    ) %>%
    dplyr::select(id, jung_type)

  return(jung_classified)
}


***

  ### Visualizations

  Beyond just the table, there are some great ways to visualize this data that would complement your existing plots.

1.  **Network Graph**: You could use a network graph to show how people with similar Jungian types are connected within a group. This would be a powerful way to see clusters.
2.  **Heatmap**: A heatmap showing the percentage of each type within different teams or groups would be a fantastic way to see the distribution of personality types across an organization.
3.  **Bar Chart**: A simple bar chart could show the percentage of each of the 16 possible Jungian types in your data set. This would be a great overview of the personality diversity in your group.






