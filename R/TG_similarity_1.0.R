# -----------------------------------------------------------------------------
# FILE: R/TG_similarity.R
# STATUS: REVISED
# -----------------------------------------------------------------------------

#' Create a Psychological Similarity Network Graph
#'
#' Visualize psychological similarity between individuals in a group as a
#' network graph. Similarity is computed as the correlation between people
#' across the selected trait columns. Node size represents the average similarity
#' of a person to their connections, with larger nodes indicating higher average
#' similarity. Thicker/greener edges indicate stronger positive similarity, and
#' redder edges indicate dissimilarity.
#'
#' @param dataset A data frame (or tibble) containing the data to plot.
#' @param columns A character vector of column names to use for the similarity
#'   calculation (traits/opinions measured for each person).
#' @param name The name of the column containing unique identifiers (e.g., full names).
#'   Defaults to `"names"`.
#' @param color The name of the column containing hex color codes (e.g., `#AABBCC`)
#'   used to color nodes. Defaults to `"favourite_color"`.
#' @param use_initials Logical. If `TRUE`, nodes are labeled with initials
#'   (first + last initial when available; falls back to first letter for single names).
#' @param connection_threshold Numeric in `[0, 1]`. Only edges with absolute
#'   correlation above this value are drawn. Defaults to `0.3`.
#' @param min_node_size The minimum size for a node, corresponding to the lowest
#'   possible average similarity (-1). Defaults to `8`.
#' @param max_node_size The maximum size for a node, corresponding to the highest
#'   possible average similarity (+1). Defaults to `25`.
#' @param name_size_mod Numeric value added to the base label font size (6).
#' @param zoom_out_factor Numeric scaling for plot limits (prevents clipping).
#'   Defaults to `1.2` (20% margin).
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#' @param output_path File path for saving the plot (e.g., `"similarity_plot.jpg"`).
#' @param output_width Width (inches) for the saved image.
#' @param output_height Height (inches) for the saved image.
#' @param output_dpi DPI for the saved image.
#' @param save_plot Logical. If `TRUE`, save the plot to `output_path`.
#' @param show_plot Logical. If `TRUE`, print the plot.
#'
#' @return A `ggraph` plot object (returned invisibly).
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   names = c("Ada Lovelace", "Grace Hopper", "Marie Curie"),
#'   favourite_color = c("#1f77b4", "#ff7f0e", "#2ca02c"),
#'   TraitA = c(5, 1, 4),
#'   TraitB = c(5, 2, 5),
#'   TraitC = c(5, 1, 4)
#' )
#'
#' TG_similarity(
#'   dataset = df,
#'   columns = c("TraitA", "TraitB", "TraitC"),
#'   connection_threshold = 0.1,
#'   save_plot = FALSE
#' )
#' }
#'
#' @export
TG_similarity <- function(dataset,
                          columns,
                          name = "names",
                          color = "favourite_color",
                          use_initials = TRUE,
                          connection_threshold = 0.3,
                          min_node_size = 8,
                          max_node_size = 25,
                          name_size_mod = 0,
                          zoom_out_factor = 1.2,
                          title = "Psychological Similarity Network",
                          subtitle = "Node size reflects average similarity to others.",
                          output_path = "similarity_plot.jpg",
                          output_width = 12,
                          output_height = 8,
                          output_dpi = 300,
                          save_plot = TRUE,
                          show_plot = TRUE) {

  # ---- Helper: Extract initials ---------------------------------------------
  .tg_initials <- function(names_vec) {
    vapply(names_vec, function(x) {
      if (is.na(x) || !nzchar(x)) return("?")
      parts <- unlist(strsplit(trimws(x), "\\s+"))
      if (length(parts) == 1) {
        substr(parts[1], 1, 1)
      } else {
        paste0(substr(parts[1], 1, 1), substr(parts[length(parts)], 1, 1))
      }
    }, character(1))
  }

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
  if (!color %in% colnames(dataset)) {
    dataset[[color]] <- rep("#7F7F7F", nrow(dataset))
  }
  if (!is.numeric(connection_threshold) ||
      length(connection_threshold) != 1 ||
      is.na(connection_threshold) ||
      connection_threshold < 0 || connection_threshold > 1) {
    rlang::abort("`connection_threshold` must be a single numeric value in [0, 1].")
  }
  if (!is.numeric(min_node_size) || !is.numeric(max_node_size) || min_node_size > max_node_size) {
    rlang::abort("`min_node_size` and `max_node_size` must be numeric, with min <= max.")
  }


  # ---- 1) Prepare & scale the data ------------------------------------------
  similarity_data <- dataset %>%
    dplyr::select(dplyr::all_of(columns))

  n_people <- nrow(similarity_data)
  if (n_people == 0) {
    rlang::abort("`dataset` has 0 rows; nothing to plot.")
  }

  scaled_data <- base::suppressWarnings(base::scale(similarity_data))
  if (n_people == 1) {
    scaled_data[is.na(scaled_data)] <- 0
  }

  node_labels <- if (use_initials) {
    .tg_initials(dataset[[name]])
  } else {
    as.character(dataset[[name]])
  }
  rownames(scaled_data) <- node_labels

  # ---- 2) Correlation matrix & graph ----------------------------------------
  if (n_people <= 1) {
    graph <- igraph::make_empty_graph(n = n_people)
    if (n_people == 1) igraph::V(graph)$name <- node_labels
  } else {
    correlation_matrix <- stats::cor(t(scaled_data), use = "pairwise.complete.obs")
    diag(correlation_matrix) <- 0
    graph <- igraph::graph_from_adjacency_matrix(
      correlation_matrix,
      weighted = TRUE,
      mode = "undirected"
    )
  }

  # ---- 3) Node attributes ----------------------------------------------------
  # Helper to determine text color based on background
  is_color_light <- function(hex_color) {
    rgb <- grDevices::col2rgb(hex_color)
    brightness <- (0.299 * rgb[1,] + 0.587 * rgb[2,] + 0.114 * rgb[3,]) / 255
    brightness > 0.5
  }

  if (n_people > 0) {
    node_colors <- as.character(dataset[[color]])
    text_colors <- ifelse(is_color_light(node_colors), "black", "white")
    igraph::V(graph)$node_color <- node_colors
    igraph::V(graph)$text_color <- text_colors
  }


  # ---- 4) Node Size Calculation (Average Similarity) ------------------------
  if (n_people <= 1) {
    avg_similarity <- 0
  } else {
    # Calculate degree, considering only connections above the threshold
    threshold_graph <- igraph::delete_edges(graph, which(abs(igraph::E(graph)$weight) < connection_threshold))
    node_degrees <- igraph::degree(threshold_graph)

    # Use a temporary weights vector for calculation to avoid modifying the graph object
    temp_weights <- igraph::E(graph)$weight
    temp_weights[abs(temp_weights) < connection_threshold] <- 0
    sum_of_weights <- igraph::strength(graph, weights = temp_weights)

    # Calculate average similarity, avoiding division by zero
    avg_similarity <- ifelse(node_degrees > 0, sum_of_weights / node_degrees, 0)
  }

  # Linearly scale the [-1, 1] similarity score to [0, 1] for size mapping.
  node_size_metric <- (avg_similarity + 1) / 2

  if (n_people > 0) {
    igraph::V(graph)$avg_similarity <- avg_similarity
    igraph::V(graph)$size_metric <- node_size_metric
  }


  # ---- 5) Layout -------------------------------------------------------------
  if (n_people == 1) {
    layout <- ggraph::create_layout(graph, layout = "manual", x = 0, y = 0)
  } else if (n_people > 1){
    # The "fr" layout requires positive weights. We ensure this by taking the
    # absolute value and replacing any zeros with a very small number.
    layout_weights <- abs(igraph::E(graph)$weight)
    layout_weights[layout_weights == 0] <- 1e-6

    layout <- ggraph::create_layout(
      graph,
      layout  = "fr",
      weights = layout_weights
    )
    # Center the layout
    layout$x <- layout$x - mean(range(layout$x))
    layout$y <- layout$y - mean(range(layout$y))
  } else { # 0 people
    layout <- data.frame(x=numeric(), y=numeric())
  }


  # ---- 6) Plot ---------------------------------------------------------------
  p <- ggraph::ggraph(layout) +
    ggraph::geom_edge_link(
      ggplot2::aes(
        width = abs(weight)^2,
        color = weight,
        alpha = ifelse(abs(weight) > connection_threshold, 0.7, 0)
      ),
      show.legend = FALSE
    ) +
    ggraph::scale_edge_alpha_identity() +
    ggraph::scale_edge_width(range = c(0.5, 4), guide = "none") +
    ggraph::scale_edge_color_gradient2(
      low = "firebrick3",
      mid = "lightgray",
      high = "seagreen4",
      midpoint = 0,
      guide = "none"
    ) +
    ggraph::geom_node_point(
      ggplot2::aes(size = size_metric, color = I(node_color))
    ) +
    ggplot2::scale_size_continuous(range = c(min_node_size, max_node_size), guide = "none") +
    ggraph::geom_node_text(
      ggplot2::aes(label = name, color = I(text_color)),
      size = 6 + name_size_mod,
      repel = FALSE
    ) +
    ggplot2::coord_fixed() +
    ggplot2::expand_limits(
      x = c(min(layout$x, -1) * zoom_out_factor, max(layout$x, 1) * zoom_out_factor),
      y = c(min(layout$y, -1) * zoom_out_factor, max(layout$y, 1) * zoom_out_factor)
    ) +
    ggraph::theme_graph(base_family = "sans") +
    ggplot2::theme(
      legend.position = "none",
      plot.margin = ggplot2::margin(20, 20, 20, 20),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 16),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12, color = "gray30")
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle
    )

  if (save_plot) {
    ggplot2::ggsave(
      filename = output_path,
      plot     = p,
      dpi      = output_dpi,
      width    = output_width,
      height   = output_height,
      units    = "in"
    )
    message("Plot saved to: ", output_path)
  }

  if (show_plot) {
    print(p)
  }

  invisible(p)
}

