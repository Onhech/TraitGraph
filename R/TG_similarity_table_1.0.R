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



# -----------------------------------------------------------------------------
# FILE: R/TG_similarity_plot.R
# -----------------------------------------------------------------------------

#' Create a Psychological Similarity Heatmap
#'
#' This function visualizes psychological similarity as a ggplot2 heatmap. It
#' calculates the correlation between individuals and displays it on a 0-100%
#' similarity scale.
#'
#' @param dataset A data frame containing the data to plot.
#' @param columns A character vector of column names to use for the similarity calculation.
#' @param name The name of the column containing unique identifiers. Defaults to "name".
#' @param low_color The color for low similarity (dissimilar profiles).
#' @param mid_color The color for neutral similarity (50%).
#' @param high_color The color for high similarity (similar profiles).
#' @param show_values A logical value. If TRUE, the similarity percentage is displayed on each tile.
#' @param title A string for the plot's main title.
#' @param legend_width A numeric value specifying the width of the legend color bar in cm.
#' @param legend_height A numeric value specifying the height of the legend color bar in cm.
#' @param show_legend A logical value. If TRUE, the legend is displayed.
#'
#' @return A ggplot object.
#' @importFrom tidyr pivot_longer
#' @importFrom tibble as_tibble rownames_to_column
#' @export
TG_similarity_plot <- function(dataset,
                               columns,
                               name = "name",
                               low_color = "#E57373",  # Muted Red
                               mid_color = "white",
                               high_color = "#81C784", # Muted Green
                               show_values = TRUE,
                               title = "Psychological Similarity Between Individuals",
                               legend_width = 1,
                               legend_height = 10,
                               show_legend = TRUE) {

  # ---- 1) Prepare & scale the data (reusing logic from table function) ----
  similarity_data <- dataset %>%
    dplyr::select(dplyr::all_of(columns))
  scaled_data <- base::scale(similarity_data)
  rownames(scaled_data) <- dataset[[name]]

  # ---- 2) Get correlation matrix and convert to 0-100% scale ----
  correlation_matrix <- stats::cor(t(scaled_data), use = "pairwise.complete.obs")
  similarity_matrix <- (correlation_matrix + 1) / 2 * 100

  # ---- 3) Clean matrix for plotting ----
  # Set upper triangle and diagonal to NA to hide them
  similarity_matrix[upper.tri(similarity_matrix)] <- NA
  diag(similarity_matrix) <- NA

  # ---- 4) Tidy the data for ggplot2 ----
  tidy_similarity <- similarity_matrix %>%
    tibble::as_tibble(rownames = "Person1") %>%
    tidyr::pivot_longer(
      cols = -Person1,
      names_to = "Person2",
      values_to = "Similarity"
    ) %>%
    # Ensure factors are ordered correctly for the plot axes
    dplyr::mutate(
      Person1 = factor(Person1, levels = rev(rownames(similarity_matrix))),
      Person2 = factor(Person2, levels = colnames(similarity_matrix))
    ) %>%
    # Remove the NA values so they don't create empty plot areas
    dplyr::filter(!is.na(Similarity))


  # ---- 5) Build the ggplot heatmap ----
  p <- ggplot2::ggplot(tidy_similarity, ggplot2::aes(x = Person2, y = Person1, fill = Similarity)) +
    ggplot2::geom_tile(color = "white", size = 0.5) + # Add a white border for clarity
    ggplot2::scale_fill_gradient2(
      low = low_color,
      mid = mid_color,
      high = high_color,
      midpoint = 50,
      limits = c(0, 100),
      name = "Similarity (%)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = if(show_legend) "right" else "none",
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    ) +
    ggplot2::labs(x = NULL, y = NULL, title = title) +
    ggplot2::coord_fixed() # Ensure tiles are square

  # Add guides layer only if the legend is shown
  if (show_legend) {
    p <- p + ggplot2::guides(fill = ggplot2::guide_colorbar(
      barwidth = ggplot2::unit(legend_width, "cm"),
      barheight = ggplot2::unit(legend_height, "cm")
    ))
  }

  # Optionally add the numeric values on top of the tiles
  if (show_values) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = paste0(round(Similarity, 0), "%")),
      color = "black",
      size = 3
    )
  }

  return(p)
}


