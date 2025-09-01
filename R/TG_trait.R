# -----------------------------------------------------------------------------
# FILE: R/TG_trait.R
# STATUS: FINAL - Corrected
# -----------------------------------------------------------------------------

#' Create a Circular Percentile Bar Chart
#'
#' This function generates a polar bar chart to visualize percentile scores for
#' individuals within a group. It automatically calculates and adds a group
#' average comparison to the plot.
#'
#' @param dataset A data frame containing the data to plot.
#' @param column_name The name of the column to use for the plot values.
#' @param title A string for the plot's main title. Defaults to the `column_name`.
#' @param name The name of the column containing unique identifiers.
#' @param color The name of the column containing hex color codes.
#' @param group_average_label A string used to label the group average bar. Defaults to "Group\\nAverage".
#' @param group_average_position_mod A numeric value to modify the vertical position of the group average label.
#' @param plot_zoom_mod A numeric value to add/subtract from the outer plot boundary.
#' @param inner_hole_size_mod A positive numeric value to reduce the inner hole size.
#' @param margin_y_mod A numeric value (in cm) to add/subtract from top/bottom margins.
#' @param margin_x_mod A numeric value (in cm) to add/subtract from left/right margins.
#' @param title_size_mod A numeric value to add/subtract from the title font size.
#' @param title_vjust_mod A numeric value to add/subtract from the title vertical adjustment.
#' @param name_size_mod A numeric value to add/subtract from the name label font size.
#' @param name_position_mod A numeric value to modify the vertical position of the individual names.
#' @param output_path The full path where the plot will be saved.
#' @param output_width The width of the saved image in inches.
#' @param output_height The height of the saved image in inches.
#' @param output_dpi The resolution (dots per inch) for the saved image.
#' @param save_plot A logical value. If TRUE, the plot is saved to disk.
#' @param show_plot A logical value. If TRUE, the plot is displayed.
#'
#' @return Invisibly returns the ggplot object.
#' @export
TG_trait <- function(
    dataset,
    column_name,
    title = column_name,
    name = "name",
    color = "favourite_color",
    group_average_label = "Group\nAverage",
    group_average_position_mod = 1,
    plot_zoom_mod = 0,
    inner_hole_size_mod = 0,
    margin_y_mod = 0,
    margin_x_mod = 0,
    title_size_mod = 0,
    title_vjust_mod = 0,
    name_size_mod = 0,
    name_position_mod = 1,
    output_path = "trait_plot.jpg",
    output_width = 7,
    output_height = 5,
    output_dpi = 300,
    save_plot = FALSE,
    show_plot = TRUE) {

  plot_data <- dataset %>%
    dplyr::rename(
      id    = !!rlang::sym(name),
      value = !!rlang::sym(column_name),
      color = !!rlang::sym(color)
    )

  group_avg <- round(mean(plot_data$value, na.rm = TRUE), 0)
  average_row <- tibble::tibble(id = group_average_label, value = group_avg, color = "black")
  plot_data <- dplyr::bind_rows(average_row, plot_data)

  plot_data <- plot_data %>%
    dplyr::mutate(
      value = ifelse(value >= 99, value, round(value, 0)),
      id = factor(id, levels = id),
      is_light = is_color_light(color),
      dark_color = sapply(color, darken_color),
      border_color = ifelse(is_light, dark_color, NA_character_)
    )

  title_params <- get_dynamic_title(title)
  final_title_size <- 8 + title_params$size + title_size_mod
  final_title_vjust <- 17.5 + title_params$vjust + title_vjust_mod
  final_y_outer_limit <- 135 + plot_zoom_mod
  final_y_inner_limit <- -40 + inner_hole_size_mod

  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_hline(yintercept = c(25, 75), color = "black", size = 0.2, alpha = 0.4, lty = 'dashed') +
    ggplot2::geom_hline(yintercept = 50, color = "black", size = 0.6, alpha = 0.5) +
    ggplot2::geom_hline(yintercept = c(100), color = "black", size = 0.6, alpha = .5) +
    ggplot2::geom_bar(ggplot2::aes(x = id, y = value, fill = color), stat = "identity", alpha = 0.85, color = ggplot2::alpha(plot_data$border_color, 0.75), size = 0.2) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_y_continuous(limits = c(final_y_inner_limit, final_y_outer_limit)) +
    ggplot2::theme_void() +
    ggplot2::geom_label(
      data = plot_data,
      ggplot2::aes(x = id, y = pmax(value - 18, 12), label = paste0(value, "%")),
      size = ifelse(plot_data$id == group_average_label, 3.2, 3),
      fontface = ifelse(plot_data$id == group_average_label, "bold", "plain"),
      fill = "white", alpha = 0.99, color = plot_data$dark_color,
      label.size = 0.2, show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = plot_data,
      ggplot2::aes(x = id, y = ifelse(id == group_average_label, 120 * group_average_position_mod, 113 * name_position_mod), label = id, fontface = ifelse(id == group_average_label, "bold", "plain")),
      size = (ifelse(plot_data$id == group_average_label, 5, 4)) + name_size_mod,
      color = plot_data$dark_color, angle = 0, lineheight = 0.8,
      hjust = dplyr::case_when(
        plot_data$id == group_average_label ~ 0.5,
        c(FALSE, seq_along(plot_data$id[-1]) == ceiling(length(plot_data$id[-1]) / 2)) ~ 0.5,
        seq_along(plot_data$id) <= length(plot_data$id) / 2 ~ 0,
        TRUE ~ 1
      )
    ) +
    ggplot2::theme(
      plot.margin = ggplot2::unit(c(-1 + margin_y_mod, -1 + margin_x_mod, -1 + margin_y_mod, -1 + margin_x_mod), "cm"),
      plot.title = ggplot2::element_text(hjust = 0.5, vjust = final_title_vjust, size = final_title_size, face = "bold")
    ) +
    ggplot2::coord_polar(start = -pi / (nrow(plot_data))) +
    ggplot2::ggtitle(title_params$text)

  if (save_plot) {
    ggplot2::ggsave(filename = output_path, plot = p, dpi = output_dpi, width = output_width, height = output_height, units = "in")
    message("Plot saved to: ", output_path)
  }
  if (show_plot) {
    print(p)
  }
  return(invisible(p))
}
