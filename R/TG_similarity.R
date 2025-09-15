# --- Psychological Similarity Function Suite ---
# Provides functions to calculate, visualize, and tabulate
# psychological similarity within a group.

# Required libraries:
# dplyr, igraph, ggraph, scales, knitr, kableExtra, tidyr, tibble, rlang, ggrepel

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# INTERNAL HELPER FUNCTION
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Calculate a Psychological Correlation Matrix
#'
#' @description
#' Internal function that validates input, prepares the dataset, scales
#' the variables, and calculates the correlation matrix between individuals.
#'
#' @param dataset A `data.frame` or `tibble` containing individual-level scores.
#' @param columns A character vector of column names to include in the calculation.
#' @param name_col A string giving the column name containing individual names.
#'
#' @return A numeric correlation matrix with individuals as row and column names.
#' @keywords internal
.calculate_similarity_matrix <- function(dataset, columns, name_col = "names") {
  if (!is.data.frame(dataset)) {
    rlang::abort("`dataset` must be a data.frame or tibble.")
  }
  if (!is.character(columns) || length(columns) == 0) {
    rlang::abort("`columns` must be a non-empty character vector of column names.")
  }
  missing_cols <- setdiff(c(columns, name_col), colnames(dataset))
  if (length(missing_cols) > 0) {
    rlang::abort(paste0("Columns not found in `dataset`: ", paste(missing_cols, collapse = ", ")))
  }

  similarity_data <- dataset %>% dplyr::select(dplyr::all_of(columns))

  if (nrow(similarity_data) < 2) {
    rlang::warn("Cannot calculate similarity with fewer than 2 individuals.")
    return(matrix(
      NA, nrow = nrow(dataset), ncol = nrow(dataset),
      dimnames = list(dataset[[name_col]], dataset[[name_col]])
    ))
  }

  scaled_data <- base::scale(similarity_data)
  rownames(scaled_data) <- dataset[[name_col]]

  stats::cor(t(scaled_data), use = "pairwise.complete.obs")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# USER-FACING FUNCTION 1: SIMILARITY PLOT
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a Psychological Similarity Network Graph
#'
#' @description
#' Generates a force-directed network graph of psychological similarity.
#'
#' @param dataset A `data.frame` or `tibble`.
#' @param columns A character vector of column names to use.
#' @param name_col The column with individual names (default: "names").
#' @param color_col The column with hex color codes for nodes (default: NULL, uses 'skyblue').
#' @param use_initials Logical. If TRUE, label nodes with initials (default: FALSE).
#' @param connection_threshold Numeric [0,1]. Edges with absolute correlation below this are not shown.
#' @param min_node_size Minimum node size for plotting (default: 8).
#' @param max_node_size Maximum node size for plotting (default: 20).
#' @param title String for the plot title.
#' @param subtitle String for the plot subtitle.
#' @param save_plot Logical. If `TRUE`, save the plot to `output_path`.
#' @param show_plot Logical. If `TRUE`, print the plot to the active device.
#' @param show_legend Logical. If `TRUE`, display the color legend.
#' @param output_path File path for saving the plot.
#' @param edge_width_power Numeric. Power to which edge weights are raised for scaling width. Defaults to 1.
#' @param zoom_out_factor Numeric scaling for plot limits to prevent clipping (default: 1.2 for a 20% margin).
#'
#' @return A `ggplot` object representing the similarity network (returned invisibly).
#' @export
#'
#' @examples
#' sample_data <- data.frame(
#'   names = c("Ada Lovelace", "Grace Hopper", "Marie Curie"),
#'   favourite_color = c("#1f77b4", "#ff7f0e", "#2ca02c"),
#'   trait1 = c(5, 1, 4),
#'   trait2 = c(5, 2, 5),
#'   trait3 = c(5, 1, 4)
#' )
#' TG_similarity_network(
#'   dataset = sample_data,
#'   columns = c("trait1", "trait2", "trait3"),
#'   name_col = "names",
#'   color_col = "favourite_color",
#'   connection_threshold = 0.9, # Hide weaker connections
#'   use_initials = TRUE,
#'   save_plot = FALSE
#' )
TG_similarity_network <- function(dataset, columns, name_col = "names",
                                  color_col = NULL, use_initials = FALSE,
                                  connection_threshold = 0,
                                  min_node_size = 8, max_node_size = 20,
                                  title = "Psychological Similarity Network",
                                  subtitle = "Node size reflects average similarity to all others.",
                                  save_plot = TRUE, show_plot = TRUE, show_legend = TRUE,
                                  output_path = "similarity_network.png",
                                  edge_width_power = 1,
                                  zoom_out_factor = 1.2) {

  # --- Internal Helpers ---
  .get_initials <- function(names_vec) {
    vapply(names_vec, function(x) {
      if (is.na(x) || !nzchar(x)) return("?")
      parts <- unlist(strsplit(trimws(x), "\\s+"))
      if (length(parts) == 1) substr(parts[1], 1, 1)
      else paste0(substr(parts[1], 1, 1), substr(parts[length(parts)], 1, 1))
    }, character(1))
  }

  is_color_light <- function(hex_color) {
    rgb <- grDevices::col2rgb(hex_color)
    brightness <- (0.299 * rgb[1,] + 0.587 * rgb[2,] + 0.114 * rgb[3,]) / 255
    brightness > 0.5
  }

  correlation_matrix <- .calculate_similarity_matrix(dataset, columns, name_col = name_col)

  node_labels <- if (use_initials) .get_initials(dataset[[name_col]]) else as.character(dataset[[name_col]])
  rownames(correlation_matrix) <- node_labels
  colnames(correlation_matrix) <- node_labels

  # --- Node Size Calculation (based on ALL connections) ---
  calc_matrix <- correlation_matrix
  diag(calc_matrix) <- NA # Exclude self-similarity for a true average
  avg_similarity <- rowMeans(calc_matrix, na.rm = TRUE)

  rescaled_size <- if (length(unique(avg_similarity)) > 1) {
    scales::rescale(avg_similarity, to = c(min_node_size, max_node_size))
  } else {
    rep((min_node_size + max_node_size) / 2, length(avg_similarity))
  }

  # --- Graph Creation (using full matrix for layout) ---
  diag(correlation_matrix) <- 0 # Set diag to 0 for graph creation
  graph <- igraph::graph_from_adjacency_matrix(correlation_matrix, weighted = TRUE, mode = "undirected")
  igraph::V(graph)$node_size <- rescaled_size

  # Set node and text colors as vertex attributes for reliable mapping
  if (!is.null(color_col) && color_col %in% colnames(dataset)) {
    igraph::V(graph)$node_color <- dataset[[color_col]]
    is_light <- is_color_light(igraph::V(graph)$node_color)
    igraph::V(graph)$text_color <- ifelse(is_light, "black", "white")
  } else {
    igraph::V(graph)$node_color <- "skyblue"
    igraph::V(graph)$text_color <- "black"
  }

  layout <- ggraph::create_layout(graph, layout = 'fr', weights = abs(igraph::E(graph)$weight))

  # Center the layout
  if (nrow(layout) > 0) {
    layout$x <- layout$x - mean(range(layout$x, na.rm = TRUE))
    layout$y <- layout$y - mean(range(layout$y, na.rm = TRUE))
  }

  p <- ggraph::ggraph(layout) +
    ggraph::geom_edge_link(
      aes(
        width = abs(weight)^edge_width_power,
        color = weight,
        alpha = ifelse(abs(weight) > connection_threshold, 0.7, 0)
      )
    ) +
    ggraph::scale_edge_alpha_identity(guide = "none") + # Interpret alpha directly for visibility
    ggraph::scale_edge_width(range = c(0.2, 3), guide = "none") +
    ggraph::scale_edge_color_gradient2(
      low = "firebrick", mid = "lightgray", high = "seagreen",
      midpoint = 0, name = "Similarity", labels = scales::percent
    ) +
    ggraph::geom_node_point(aes(size = node_size, color = node_color), shape = 19) +
    ggplot2::scale_size_identity(guide = "none") +
    ggplot2::scale_color_identity(guide = "none") +
    ggraph::geom_node_text(aes(label = name, color = I(text_color))) +
    ggplot2::expand_limits(
      x = c(min(layout$x, -1, na.rm = TRUE) * zoom_out_factor, max(layout$x, 1, na.rm = TRUE) * zoom_out_factor),
      y = c(min(layout$y, -1, na.rm = TRUE) * zoom_out_factor, max(layout$y, 1, na.rm = TRUE) * zoom_out_factor)
    ) +
    ggraph::theme_graph(base_family = 'sans') +
    ggplot2::theme(
      legend.position = if (show_legend) "right" else "none",
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    ) +
    ggplot2::labs(title = title, subtitle = subtitle)

  if (save_plot) {
    ggplot2::ggsave(filename = output_path, plot = p, width = 10, height = 7, dpi = 300)
    message("Plot saved to: ", output_path)
  }

  if (show_plot) {
    print(p)
  }

  invisible(p)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# USER-FACING FUNCTION 2: HEATMAP PLOT
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a Psychological Similarity Heatmap
#'
#' @description
#' Produces a heatmap of similarity scores between individuals.
#'
#' @param dataset A `data.frame` or `tibble`.
#' @param columns A character vector of column names.
#' @param name_col Column with individual names (default: "names").
#' @param low_color Color for low similarity.
#' @param mid_color Color for mid-range similarity.
#' @param high_color Color for high similarity.
#' @param show_values Logical. If TRUE, show percentage values on tiles.
#' @param show_legend Logical. If TRUE, display the color legend.
#' @param title String for the plot title.
#' @param save_plot Logical. If `TRUE`, save the plot to `output_path`.
#' @param show_plot Logical. If `TRUE`, print the plot to the active device.
#' @param output_path File path for saving the plot.
#'
#' @return A `ggplot` object representing the similarity heatmap (returned invisibly).
#' @export
#'
#' @examples
#' TG_similarity_heatmap(
#'   sample_data,
#'   columns = c("trait1", "trait2", "trait3"),
#'   name_col = "names",
#'   save_plot = FALSE
#' )
TG_similarity_heatmap <- function(dataset, columns, name_col = "names",
                                  low_color = "#E57373", mid_color = "white", high_color = "#81C784",
                                  show_values = TRUE, show_legend = TRUE,
                                  title = "Psychological Similarity Between Individuals",
                                  save_plot = TRUE, show_plot = TRUE,
                                  output_path = "similarity_heatmap.png") {
  correlation_matrix <- .calculate_similarity_matrix(dataset, columns, name_col = name_col)

  similarity_matrix <- (correlation_matrix + 1) / 2 * 100
  similarity_matrix[upper.tri(similarity_matrix)] <- NA
  diag(similarity_matrix) <- NA

  tidy_similarity <- similarity_matrix %>%
    tibble::as_tibble(rownames = "Person1") %>%
    tidyr::pivot_longer(cols = -Person1, names_to = "Person2", values_to = "Similarity") %>%
    dplyr::mutate(
      Person1 = factor(Person1, levels = rev(rownames(similarity_matrix))),
      Person2 = factor(Person2, levels = colnames(similarity_matrix))
    ) %>%
    dplyr::filter(!is.na(Similarity))

  p <- ggplot2::ggplot(tidy_similarity, ggplot2::aes(x = Person2, y = Person1, fill = Similarity)) +
    ggplot2::geom_tile(color = "white", size = 0.5) +
    ggplot2::scale_fill_gradient2(
      low = low_color, mid = mid_color, high = high_color,
      midpoint = 50, limits = c(0, 100), name = "Similarity (%)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
      axis.ticks = ggplot2::element_blank(), panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      legend.position = if(show_legend) "right" else "none"
    ) +
    ggplot2::labs(x = NULL, y = NULL, title = title) +
    ggplot2::coord_fixed()

  if (show_values) {
    p <- p + ggplot2::geom_text(aes(label = paste0(round(Similarity, 0), "%")), color = "black", size = 3)
  }

  if (save_plot) {
    ggplot2::ggsave(filename = output_path, plot = p, width = 8, height = 7, dpi = 300)
    message("Plot saved to: ", output_path)
  }

  if (show_plot) {
    print(p)
  }

  invisible(p)
}





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# USER-FACING FUNCTION 3: SIMILARITY TABLE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a Psychological Similarity Table
#'
#' @description
#' Generates a similarity matrix, a formatted HTML table, or summary statistics.
#' The matrix and HTML formats only show the lower triangle to avoid redundancy.
#'
#' @param dataset A `data.frame` or `tibble`.
#' @param columns A character vector of column names.
#' @param name_col Column with individual names (default: "names").
#' @param format Output format: "matrix", "html", "summary", or "ranked_summary".
#' @param digits Number of decimal places for rounding (default: 2).
#' @param caption Caption for the HTML table.
#' @param threshold For "html" format, the absolute correlation to bold/color cells.
#'
#' @return A matrix, HTML table, or tibble, depending on `format`.
#' @export
#'
#' @examples
#' TG_similarity_table(sample_data, columns = c("trait1", "trait2", "trait3"), format = "html", threshold = 0.8)
TG_similarity_table <- function(dataset, columns, name_col = "names",
                                format = "matrix", digits = 2,
                                caption = "Psychological Similarity (Correlation)",
                                threshold = 0.5) {
  correlation_matrix <- .calculate_similarity_matrix(dataset, columns, name_col = name_col)

  if (format == "html") {
    # Create style matrices first from the original data
    cell_colors <- ifelse(abs(correlation_matrix) > threshold,
                          ifelse(correlation_matrix > 0, "seagreen", "firebrick"),
                          "black")
    cell_bold <- abs(correlation_matrix) > threshold

    # Now round the matrix for display
    display_matrix <- round(correlation_matrix, digits)

    # Blank out the upper triangle and diagonal for a clean look
    display_matrix[upper.tri(display_matrix)] <- ""
    diag(display_matrix) <- ""

    formatted_table <- display_matrix %>%
      tibble::as_tibble(rownames = " ")

    for (i in 2:ncol(formatted_table)) {
      # Apply styling; kableExtra handles empty strings gracefully
      formatted_table[[i]] <- kableExtra::cell_spec(
        formatted_table[[i]],
        bold = cell_bold[, i - 1],
        color = cell_colors[, i - 1]
      )
    }

    return(
      knitr::kable(formatted_table, format = "html", caption = caption, escape = FALSE) %>%
        kableExtra::kable_styling(full_width = TRUE, bootstrap_options = c("hover", "condensed"))
    )

  } else if (format == "summary") {
    summary_matrix <- correlation_matrix
    diag(summary_matrix) <- NA # Exclude self-similarity from the mean calculation
    avg_similarity <- rowMeans(summary_matrix, na.rm = TRUE)
    return(tibble::tibble(
      Individual = rownames(correlation_matrix),
      AvgSimilarityToOthers = round(avg_similarity, digits)
    ))
  } else if (format == "ranked_summary") {
    summary_matrix <- correlation_matrix
    diag(summary_matrix) <- NA # Exclude self-similarity
    avg_similarity <- rowMeans(summary_matrix, na.rm = TRUE)
    return(
      tibble::tibble(
        Individual = rownames(correlation_matrix),
        AvgSimilarityToOthers = round(avg_similarity, digits)
      ) %>%
        dplyr::arrange(dplyr::desc(AvgSimilarityToOthers))
    )
  } else { # This is the "matrix" format case
    # For matrix format, show only lower triangle by setting others to NA
    matrix_to_return <- round(correlation_matrix, digits)
    matrix_to_return[upper.tri(matrix_to_return)] <- NA
    diag(matrix_to_return) <- NA
    return(matrix_to_return)
  }
}

