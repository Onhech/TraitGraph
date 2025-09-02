# -----------------------------------------------------------------------------
# FILE: R/TG_votes.R
# -----------------------------------------------------------------------------

#' Create and Save a Circular Bar Chart for Votes/Opinion Data
#'
#' @param dataset A data frame containing the data to plot.
#' @param column_name The name of the column containing the numeric values to plot.
#' @param title A string for the plot's main title. If not provided, the name
#'   of the `column_name` will be used.
#' @param title_face The font style of the title (e.g., `plain`, `bold`, or `bold.italic`). Defaalts to `bold`.
#' @param color The color of the tile.
#' @param name The name of the column containing unique identifiers. Defaults to "names".
#' @param color The name of the column containing hex color codes. Defaults to "favourite_color".
#' @param sort_order The order to sort the results. Can be "desc" or "asc".
#' @param minor_ticks The number of minor y-axis ticks you want to display. Defaults to none.
#' @param plot_zoom_mod A numeric value to add or subtract from the default outer plot boundary.
#' @param name_size_mod A numeric value to add or subtract from the name label font size.
#' @param name_position_mod A numeric value to modify the vertical position of the individual names.
#' @param title_size_mod A numeric value to add or subtract from the dynamic title font size.
#' @param title_vjust_mod A numeric value to add or subtract from the dynamic title vertical adjustment.
#' @param output_path The full path where the plot will be saved.
#' @param output_width The width of the saved image in inches.
#' @param output_height The height of the saved image in inches.
#' @param output_dpi The resolution (dots per inch) for the saved image.
#' @param save_plot A logical value. If TRUE, the plot is saved to disk.
#' @param show_plot A logical value. If TRUE, the plot is displayed.
#'
#' @return Invisibly returns the ggplot object.
#' @export
TG_votes <- function(dataset,
                     column_name,
                     title = NULL,
                     title_face = "bold",
                     title_color = "black",
                     name = "name",
                     color = "favourite_color",
                     sort_order = "desc",
                     minor_ticks = 0,
                     plot_zoom_mod = 0,
                     name_size_mod = 0,
                     name_position_mod = 1,
                     title_size_mod = 0,
                     title_vjust_mod = 0,
                     output_path = "votes_plot.jpg",
                     output_width = 7,
                     output_height = 6,
                     output_dpi = 300,
                     save_plot = TRUE,
                     show_plot = TRUE) {


  if (is.null(title)) { title <- column_name }
  if (!sort_order %in% c("desc", "asc")) { stop("`sort_order` must be either 'desc' or 'asc'.") }

  plot_data <- dataset %>%
    dplyr::rename(id = !!rlang::sym(name), value = !!rlang::sym(column_name), color = !!rlang::sym(color)) %>%
    dplyr::filter(value > 0)

  if (sort_order == "desc") {
    plot_data <- plot_data %>% dplyr::arrange(dplyr::desc(value))
  } else {
    plot_data <- plot_data %>% dplyr::arrange(value)
  }

  plot_data <- plot_data %>%
    dplyr::mutate(
      id = factor(id, levels = id),
      is_light = is_color_light(color),
      dark_color = sapply(color, darken_color),
      border_color = ifelse(is_light, dark_color, NA_character_)
    )

  if (nrow(plot_data) == 0) { message("No data with scores greater than 0 to plot."); return(invisible(NULL)) }

  max_score <- max(plot_data$value)
  title_params <- get_dynamic_title(title)
  final_title_size <- title_params$size + title_size_mod
  final_title_vjust <- title_params$vjust + title_vjust_mod
  column_width <- dplyr::case_when(nrow(plot_data) <= 8 ~ 0.98 - (nrow(plot_data) * 0.01), TRUE ~ 0.90)
  final_y_outer_limit <- ((max_score + 1) * 1.7) + plot_zoom_mod

  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_bar(ggplot2::aes(x = id, y = value, fill = color), width = column_width, stat = "identity", alpha = 0.85, color = ggplot2::alpha(plot_data$border_color, 0.75), size = 0.2) +
    ggplot2::geom_hline(yintercept = max_score + 1, color = "black", size = 0.6, alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0, color = "black", size = 0.6, alpha = 0.5) +
    ggplot2::geom_hline(yintercept = seq(from = 0, to = max_score, by = max_score/(1 + minor_ticks)), color = "black", size = 0.1, alpha = 0.25, linetype = 'dashed') +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_y_continuous(limits = c(-0.4 * (max_score + 1), final_y_outer_limit)) +
    ggplot2::theme_void() +
    ggplot2::geom_label(ggplot2::aes(x = id, y = pmax(value - (max_score * 0.15), (max_score * 0.1)), label = round(value,0)),
                        size = 3, fontface = "plain", fill = "white", alpha = 0.99,
                        color = plot_data$dark_color, label.size = 0.2, show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(x = id, y = (max_score * 1.25) * name_position_mod, label = id),
                       size = 4 + name_size_mod, color = plot_data$dark_color, angle = 0, lineheight = 0.8,
                       hjust = dplyr::case_when(
                         plot_data$id == plot_data$id[1] ~ 0.5,
                         c(FALSE, seq_along(plot_data$id[-1]) == ceiling(length(plot_data$id[-1]) / 2)) ~ 0.5,
                         seq_along(plot_data$id) <= length(plot_data$id) / 2 ~ 0,
                         TRUE ~ 1
                       )) +
    ggplot2::theme(
      plot.margin = ggplot2::unit(c(-3, -4, -3.5, -4), "cm"),
      plot.title = ggplot2::element_text(hjust = 0.5, vjust = final_title_vjust, size = final_title_size, face = title_face, color = title_color)
    ) +
    ggplot2::coord_polar(start = -pi / (nrow(plot_data))) +
    ggplot2::ggtitle(title_params$text)

  if (save_plot) { ggplot2::ggsave(filename = output_path, plot = p, dpi = output_dpi, width = output_width, height = output_height, units = "in"); message("Plot saved to: ", output_path) }
  if (show_plot) { print(p) }
  return(invisible(p))
}
