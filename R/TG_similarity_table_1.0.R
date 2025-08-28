#' Create a Psychological Similarity Table
#'
#' Computes and presents the psychological similarity between individuals as a
#' correlation matrix. Optionally, it can return a beautifully formatted table
#' using the `kableExtra` package.
#'
#' @param dataset A data frame (or tibble) containing the data to plot.
#' @param columns A character vector of column names to use for the similarity
#'     calculation (traits/opinions measured for each person).
#' @param name The name of the column containing unique identifiers (e.g., full names).
#'     Defaults to `"names"`.
#' @param format The desired output format. Can be `"matrix"` (the default) to
#'     return a raw correlation matrix, or `"html"` to return a formatted
#'     `knitr_kable` object.
#' @param threshold Numeric. The absolute correlation value above which numbers will be
#'     bolded and colored. Defaults to `0.5`.
#'
#' @return A correlation matrix (`matrix`) or a formatted table (`knitr_kable`).
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'    names = c("Ada Lovelace", "Grace Hopper", "Marie"),
#'    favourite_color = c("#1f77b4", "#ff7f0e", "#2ca02c"),
#'    TraitA = c(3, 4, 5),
#'    TraitB = c(1, 2, 1),
#'    TraitC = c(5, 3, 4)
#' )
#'
#' # Get a raw correlation matrix (default)
#' TG_similarity_table(
#'    dataset = df,
#'    columns = c("TraitA", "TraitB", "TraitC"),
#'    name = "names"
#' )
#'
#' # Get a formatted HTML table with a different threshold
#' TG_similarity_table(
#'    dataset = df,
#'    columns = c("TraitA", "TraitB", "TraitC"),
#'    name = "names",
#'    format = "html",
#'    threshold = 0.7
#' )
#' }
#'
#' @export
TG_similarity_table <- function(dataset,
                                columns,
                                name = "names",
                                format = "matrix",
                                threshold = 0.5) {

  # ---- 0) Validate inputs ----------------------------------------------------
  if (!is.data.frame(dataset)) {
    rlang::abort("`dataset` must be a data.frame or tibble.")
  }
  if (!is.character(columns) || length(columns) == 0) {
    rlang::abort("`columns` must be a non-empty character vector of column names.")
  }
  missing_cols <- setdiff(columns, colnames(dataset))
  if (length(missing_cols) > 0) {
    rlang::abort(paste0("These `columns` are missing in `dataset`: ",
                        paste(missing_cols, collapse = ", ")))
  }
  if (!name %in% colnames(dataset)) {
    rlang::abort(paste0("`name` column '", name, "' not found in `dataset`."))
  }
  if (!is.numeric(threshold) || length(threshold) != 1 || threshold < 0 || threshold > 1) {
    rlang::abort("`threshold` must be a single numeric value between 0 and 1.")
  }

  # ---- 1) Prepare & scale the data ------------------------------------------
  similarity_data <- dataset %>%
    dplyr::select(dplyr::all_of(columns))

  n_people <- nrow(similarity_data)
  if (n_people < 2) {
    message("`dataset` has fewer than 2 rows; cannot compute correlations.")
    if (format == "html") {
      return(knitr::kable(
        data.frame(
          Person = character(),
          Correlation = numeric()
        ),
        format = "html",
        caption = "Psychological Similarity Between Individuals"
      ))
    } else {
      return(matrix(NA, 0, 0))
    }
  }

  scaled_data <- base::suppressWarnings(base::scale(similarity_data))
  rownames(scaled_data) <- dataset[[name]]

  # ---- 2) Correlation matrix ------------------------------------------------
  correlation_matrix <- stats::cor(t(scaled_data), use = "pairwise.complete.obs")

  # ---- 3) Return formatted table or raw matrix ------------------------------
  if (format == "html") {
    # Round the matrix
    formatted_matrix <- round(correlation_matrix, 2)

    # Create the style matrices based on the rounded values
    cell_colors <- ifelse(abs(formatted_matrix) > threshold,
                          ifelse(formatted_matrix > 0, "seagreen4", "firebrick3"),
                          "black"
    )
    cell_bold <- abs(formatted_matrix) > threshold

    # Set the diagonal values to control their appearance
    diag(formatted_matrix) <- ""
    diag(cell_colors) <- "transparent"
    diag(cell_bold) <- FALSE

    # Convert to a tibble with row names and then format each cell individually
    formatted_table <- formatted_matrix %>%
      tibble::as_tibble(rownames = " ")

    # Apply cell-by-cell styling by iterating over columns
    for (i in 2:ncol(formatted_table)) {
      formatted_table[[i]] <- kableExtra::cell_spec(
        formatted_table[[i]],
        bold = cell_bold[, i - 1],
        color = cell_colors[, i - 1]
      )
    }

    # Now, pass the fully styled tibble to kable
    formatted_table <- formatted_table %>%
      knitr::kable(
        format = "html",
        caption = "Psychological Similarity Between Individuals",
        escape = FALSE # Crucial for rendering HTML styling
      ) %>%
      kableExtra::kable_styling(
        bootstrap_options = c("striped", "hover", "condensed", "responsive"),
        full_width = T,
        font_size = 12
      )

    return(formatted_table)
  } else {
    return(correlation_matrix)
  }
}
