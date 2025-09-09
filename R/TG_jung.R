# -----------------------------------------------------------------------------
# FILE: R/TG_jung.R
# STATUS: FINAL - UPDATED
# -----------------------------------------------------------------------------

#' Create a Circular Dichotomy Bar Chart (Jungian Style)
#'
#' This function generates a polar bar chart with background color bands and
#' curved text labels, ideal for visualizing dichotomous traits.
#'
#' @param dataset A data frame containing the data to plot.
#' @param column_name The name of the column containing the percentile scores to plot.
#' @param title A string for the plot's main title. Defaults to the `column_name`.
#' @param title_face The font style of the title (e.g., `plain`, `bold`, or `bold.italic`). Defaults to `bold`.
#' @param color The color of the tile.
#' @param label_top The text for the curved label at the top of the chart (100% mark).
#' @param label_bottom The text for the curved label at the bottom of the chart (0% mark).
#' @param name The name of the column containing unique identifiers. Defaults to "name".
#' @param color The name of the column containing hex color codes. Defaults to "favourite_color".
#' @param group_average_label A string for the group average bar's label. Defaults to "Group Average".
#' @param plot_zoom_mod A numeric value that is a percentage modifier for the plot zoom (size * mod). Default is `1`.
#' @param name_size_mod A numeric value that is a percentage modifier for the name sizes (size * mod). Default is `1`.
#' @param title_size_mod A numeric value to add or subtract from the dynamic title font size.
#' @param title_vjust_mod A numeric value to add or subtract from the dynamic title vertical adjustment.
#' @param color_bars A vector of five hex color codes to define the colors of the background bands. Default = `c("#B1C090", "#B1C090", "#B3C0CD", "#B5BEDF", "#B5BEDF")`.
#' @param color_bars_opacity A vector of five opacity (alpha) numbers to determine bar color solidity. Default = `c(0.6, 0.4, 0.2, 0.4, 0.6)`.
#' @param callout_size_mod A numeric value that is a percentage modifier for the callout sizes (size * mod). Default is `1`.
#' @param callout_text_color Toggles the callout background. Defaults to `TRUE`, `FALSE` removes callout background.
#' @param callout_text_face The font style of the callout (e.g., `plain`, `bold`, or `bold.italic`). Defaults to `bold`.
#' @param output_path The full path where the plot will be saved.
#' @param output_width The width of the saved image in inches.
#' @param output_height The height of the saved image in inches.
#' @param output_dpi The resolution (dots per inch) for the saved image.
#' @param save_plot A logical value. If TRUE, the plot is saved to disk.
#' @param show_plot A logical value. If TRUE, the plot is displayed.
#'
#' @importFrom dplyr %>%
#' @importFrom geomtextpath geom_textpath
#' @return Invisibly returns the ggplot object.
#' @export
TG_jung <- function(
    dataset,
    column_name,
    title = column_name,
    title_face = "bold",
    title_color = "black",
    label_top = "Trait A",
    label_bottom = "Trait B",
    name = "name",
    color = "favourite_color",
    group_average_label = "Group\nAverage",
    plot_zoom_mod = 1,
    name_size_mod = 1,
    title_size_mod = 0,
    title_vjust_mod = 1,
    color_bars = c("#B5BEDF", "#B5BEDF", "#B3C0CD","#B1C090", "#B1C090" ),
    color_bars_opacity = c(0.6, 0.4, 0.2, 0.4, 0.6),
    callout_size_mod = 1,
    callout_text_color = "dark_color",
    callout_text_face = "plain",
    callout_background = T,
    output_path = "jung_plot.jpg",
    output_width = 7,
    output_height = 6,
    output_dpi = 300,
    save_plot = TRUE,
    show_plot = TRUE) {

  plot_data <- dataset %>%
    dplyr::rename(id = !!rlang::sym(name), value = !!rlang::sym(column_name), color = !!rlang::sym(color))

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

  color_bands <- data.frame(
    color = color_bars,
    ystart = seq(0, 80, by = 20),
    ystop = seq(20, 100, by = 20),
    opacity = rev(color_bars_opacity)
  )

  plot_data <- plot_data %>%
    dplyr::mutate(
      callout_color_final = if (!is.null(callout_text_color) && callout_text_color == "dark_color") {
        dark_color
      } else {
        ifelse(
          value > 80,
          darken_color(color_bands$color[5], factor = .55),
            ifelse(
              value > 55,
              darken_color(color_bands$color[4], factor = .65),
                ifelse(
                  value > 45,
                  darken_color(color_bands$color[3], factor = .9),
                  ifelse(
                    value > 30,
                    darken_color(color_bands$color[2], factor = .65),
                      darken_color(color_bands$color[1], factor = .55)
                    ))))
      }
    )


  title_params <- get_dynamic_title(title)
  final_title_size <- title_params$size + title_size_mod
  final_title_vjust <- (title_params$vjust + 24) * title_vjust_mod

  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_rect(data = color_bands, ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = ystart, ymax = ystop, fill = color, alpha = opacity)) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::geom_hline(yintercept = 50, color = "black", size = 0.7, alpha = 0.5, lty = 'dashed') +
    ggplot2::geom_hline(yintercept = c(0, 100), color = "black", size = 0.6, alpha = 0.5) +
    ggplot2::geom_vline(xintercept = 1:nrow(plot_data), color = "white", size = 1, alpha = 0.25) +
    ggplot2::geom_point(ggplot2::aes(x = id, y = value), color = plot_data$color, shape = 1, size = 0, alpha = 0) +
    geomtextpath::geom_textpath(ggplot2::aes(x = 1, y = 91, label = label_top), hjust = 0.5, vjust = 0.5, color = "white", size = 6) +
    geomtextpath::geom_textpath(ggplot2::aes(x = 1, y = 11, label = label_bottom), hjust = 0.5, vjust = 0.5, color = "white", size = 6) +
    geomtextpath::geom_textpath(ggplot2::aes(x = nrow(plot_data) / 2 + 1, y = 91, label = label_top), hjust = 0.5, vjust = 0.5, color = "white", size = 6) +
    geomtextpath::geom_textpath(ggplot2::aes(x = nrow(plot_data) / 2 + 1, y = 11, label = label_bottom), hjust = 0.5, vjust = 0.5, color = "white", size = 6) +
    ggplot2::geom_label(
      ggplot2::aes(x = id, y = pmin(pmax(value, 7), 88), label = paste0(value, "%")),
      size = ifelse(plot_data$id == group_average_label, 6, 5) * callout_size_mod,
      fontface = ifelse(plot_data$id == group_average_label, "bold", callout_text_face),
      fill = "white", alpha = callout_background, color = plot_data$callout_color_final,
      label.size = 0.5 * callout_background, label.r = ggplot2::unit(8, "pt"), show.legend = FALSE
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = ggplot2::unit(c((0.3 + plot_zoom_mod), 0, (0.3 * plot_zoom_mod), 0), "cm"),
      plot.title = ggplot2::element_text(hjust = 0.5, vjust = final_title_vjust, size = final_title_size, face = title_face, color = title_color),
      axis.text.x = ggplot2::element_text(color = plot_data$dark_color, size = 15 * name_size_mod, face = ifelse(plot_data$id == group_average_label, "bold", "plain"), margin = ggplot2::margin(t = 12, unit = "pt"))
    ) +
    ggplot2:::coord_radial(start = -pi / (nrow(plot_data) + ((nrow(plot_data) * -0.1743) + 0.2101)), inner.radius = 0.25) +
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
