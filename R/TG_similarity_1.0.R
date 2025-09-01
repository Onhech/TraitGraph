# -----------------------------------------------------------------------------
# FILE: R/TG_similarity.R
# STATUS: STABLE
# -----------------------------------------------------------------------------

#' Create a Psychological Similarity Network Graph
#'
#' Visualize psychological similarity between individuals in a group as a
#' network graph. Similarity is computed as the correlation between people
#' across the selected trait columns; thicker/greener edges indicate stronger
#' positive similarity, and redder edges indicate dissimilarity.
#'
#' @param dataset A data frame (or tibble) containing the data to plot.
#' @param columns A character vector of column names to use for the similarity
#'     calculation (traits/opinions measured for each person).
#' @param name The name of the column containing unique identifiers (e.g., full names).
#'     Defaults to `"names"`.
#' @param color The name of the column containing hex color codes (e.g., `#AABBCC`)
#'     used to color nodes. Defaults to `"favourite_color"`.
#' @param use_initials Logical. If `TRUE`, nodes are labeled with initials
#'     (first + last initial when available; falls back to first letter for single names).
#' @param connection_threshold Numeric in `[0, 1]`. Only edges with absolute
#'     correlation above this value are drawn. Defaults to `0.3`.
#' @param name_size_mod Numeric value added to the base label font size (6).
#' @param zoom_out_factor Numeric scaling for plot limits (prevents clipping).
#'     Defaults to `1.2` (20% margin).
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
#'    names = c("Ada Lovelace", "Grace Hopper", "Marie"),
#'    favourite_color = c("#1f77b4", "#ff7f0e", "#2ca02c"),
#'    TraitA = c(3, 4, 5),
#'    TraitB = c(1, 2, 1),
#'    TraitC = c(5, 3, 4)
#' )
#'
#' TG_similarity(
#'    dataset = df,
#'    columns = c("TraitA", "TraitB", "TraitC"),
#'    name = "names",
#'    color = "favourite_color",
#'    use_initials = TRUE,
#'    connection_threshold = 0.3,
#'    save_plot = FALSE
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
                          name_size_mod = 0,
                          zoom_out_factor = 1.2,
                          title = "Psychological Similarity",
                          subtitle = NULL, # The subtitle parameter is now set to NULL by default
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

  # Build node labels: initials or full
  node_labels <- if (use_initials) {
    .tg_initials(dataset[[name]])
  } else {
    as.character(dataset[[name]])
  }

  rownames(scaled_data) <- node_labels

  # ---- 2) Correlation matrix & graph ----------------------------------------
  if (n_people == 1) {
    graph <- igraph::make_empty_graph(n = 1)
    igraph::V(graph)$name <- node_labels
    correlation_matrix <- matrix(0, nrow = 1, ncol = 1,
                                 dimnames = list(node_labels, node_labels))
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
  net_sim <- if (n_people == 1) 0 else igraph::strength(graph, weights = igraph::E(graph)$weight)
  node_colors <- as.character(dataset[[color]])
  # is_color_light helper is not in the original code, but is needed for text color
  is_color_light <- function(hex_color) {
    rgb <- grDevices::col2rgb(hex_color)
    brightness <- (0.299 * rgb[1,] + 0.587 * rgb[2,] + 0.114 * rgb[3,]) / 255
    brightness > 0.5
  }
  text_is_light <- is_color_light(node_colors)
  text_colors <- ifelse(text_is_light, "black", "white")

  igraph::V(graph)$net_similarity <- net_sim
  igraph::V(graph)$node_color    <- node_colors
  igraph::V(graph)$text_color    <- text_colors

  # ---- 4) Layout -------------------------------------------------------------
  if (n_people == 1) {
    layout <- ggraph::create_layout(graph, layout = "manual", x = 0, y = 0)
  } else {
    layout <- ggraph::create_layout(
      graph,
      layout  = "fr",
      weights = abs(igraph::E(graph)$weight)
    )
  }

  layout$x <- layout$x - mean(range(layout$x))
  layout$y <- layout$y - mean(range(layout$y))

  # ---- 5) Plot ---------------------------------------------------------------
  p <- ggraph::ggraph(layout) +
    ggraph::geom_edge_link(
      ggplot2::aes(
        width  = abs(weight)^2,
        color  = weight,
        alpha  = ifelse(abs(weight) > connection_threshold, 0.7, 0)
      ),
      show.legend = FALSE # This removes the legend
    ) +
    ggraph::scale_edge_alpha_identity() +
    ggraph::scale_edge_width(range = c(0.5, 4), guide = "none") +
    ggraph::scale_edge_color_gradient2(
      low = "firebrick3",
      mid = "lightgray",
      high = "seagreen4",
      midpoint = 0,
      guide = "none" # This also removes the legend
    ) +
    ggraph::geom_node_point(
      ggplot2::aes(size = net_similarity, color = node_color)
    ) +
    ggplot2::scale_color_identity(guide = "none") +
    ggplot2::scale_size_continuous(range = c(10, 25), guide = "none") +
    ggraph::geom_node_text(
      ggplot2::aes(label = name, color = text_color),
      size = 6 + name_size_mod
    ) +
    ggplot2::scale_colour_identity(guide = "none") +
    ggplot2::expand_limits(
      x = c(-zoom_out_factor, zoom_out_factor),
      y = c(-zoom_out_factor, zoom_out_factor)
    ) +
    ggraph::theme_graph(base_family = "sans") +
    ggplot2::theme(
      legend.position = "none", # And this final line ensures it's gone
      plot.margin = ggplot2::margin(20, 20, 20, 20),
      plot.title = ggplot2::element_text(hjust = 0.5), # This centers the title
      plot.subtitle = ggplot2::element_blank() # This removes the subtitle
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle # The subtitle argument is now passed as NULL
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
