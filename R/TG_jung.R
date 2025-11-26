# -----------------------------------------------------------------------------
# FILE: R/TG_jung.R
# DESCRIPTION: Polar dichotomy bars for Jungian preference pairs with curved labels.
# AUTHOR: Lynden Jensen
# LAST EDIT: 2025-02-09
# DEPENDS: ggplot2, dplyr, scales, stringr, rlang
# STATUS: ACTIVE
# -----------------------------------------------------------------------------

#' Create a Circular Dichotomy Bar Chart (Jungian Style)
#'
#' @description
#' Generates a polar bar chart with background color bands and curved text labels,
#' ideal for visualizing dichotomous Jungian preference pairs.
#'
#' @param dataset A data frame containing the data to plot.
#' @param column_name The name of the column containing the percentile scores to plot.
#' @param title A string for the plot's main title. Defaults to the `column_name`.
#' @param show_title Logical: if TRUE (default), include the generated title in the plot.
#' @param title_face The font style of the title (e.g., `plain`, `bold`, or `bold.italic`). Defaults to `bold`.
#' @param color The color of the tile.
#' @param label_top The text for the curved label at the top of the chart (100% mark).
#' @param label_bottom The text for the curved label at the bottom of the chart (0% mark).
#' @param label_position_min The minimum vertical position (0-100) for a callout label. Defaults to 12.
#' @param label_position_max The maximum vertical position (0-100) for a callout label. Defaults to 83.
#' @param callout_background_style A string: "solid" for a standard white box, "gradient" for a faded sunburst effect, or "none" for a transparent background. Defaults to `"solid"`.
#' @param callout_bg_fill The background color of the callout label. Use `NA` for a transparent background. Defaults to `"white"`.
#' @param callout_bg_alpha The opacity of the callout background (0-1). Defaults to `1` (fully opaque).
#' @param callout_border_color The color of the callout border. Use `NA` for no border. Defaults to `"black"`.
#' @param callout_border_size The thickness of the callout border. Use `0` for no border. Defaults to `0.5`.
#' @param name The name of the column containing unique identifiers. Defaults to "name".
#' @param color The name of the column containing hex color codes. Defaults to "favourite_color".
#' @param group_average_label A string for the group average bar's label. Defaults to "Group Average".
#' @param plot_zoom_mod A numeric value that is a percentage modifier for the plot zoom (size * mod). Default is `1`.
#' @param name_size_mod A numeric value that is a percentage modifier for the name sizes (size * mod). Default is `1`.
#' @param title_size_mod A numeric value to add or subtract from the dynamic title font size.
#' @param title_vjust_mod A numeric value to add or subtract from the dynamic title vertical adjustment.
#' @param color_bars Either:
#'   - A string naming one of the built-in palettes (i.e.,  `sunset`, `coastal`, `autumn`, `twilight`, `regal`, `cyber`, `blaze`, `earth`, `vaporwave`, `oceanic`, `vibrant`, `volcano`, `forest`), OR
#'   - A vector of exactly five hex color codes.
#'   Defaults to `"sunset"`.
#' @param color_bars_opacity A vector of five opacity (alpha) numbers to determine bar color solidity. Default = `c(0.6, 0.4, 0.2, 0.4, 0.6)`.
#' @param point_shape Change the size of the points, defaults = 6pt.
#' @param point_shape The shape of the data points (e.g., 16 for a solid circle, 21 for a circle with a border, 22 for a square). Defaults to 16.
#' @param point_border_color The color of the border around the data points. Only applies to shapes 21-25. Defaults to NA (no border).
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
    show_title = TRUE,
    title_face = "bold",
    title_color = "black",
    label_top = "Trait A",
    label_bottom = "Trait B",
    label_position_min = 12,
    label_position_max = 83,
    name = "name",
    color = "favourite_color",
    group_average_label = "Group\nAverage",
    plot_zoom_mod = 1,
    name_size_mod = 1,
    title_size_mod = 0,
    title_vjust_mod = 1,
    color_bars = "sunset",
    color_bars_opacity = c(0.6, 0.4, 0.2, 0.4, 0.6),
    show_points = TRUE,
    point_size = 6,
    point_shape = 16,
    point_border_color = NA,
    callout_size_mod = 1,
    callout_text_color = "dark_color",
    callout_text_face = "plain",
    callout_background_style = "solid",
    callout_bg_fill = "white",
    callout_bg_alpha = .5,
    callout_border_color = "black",
    callout_border_size = 0.0,
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

  # Determine color palette
  #  `sunset`, `coastal`, `autumn`, `twilight`, `regal`, `cyber`, `blaze`, `earth`, `vaporwave`, `oceanic`, `vibrant`, `volcano`, `forest`
  # --- Define palette presets ---
  color_presets <- list(
    # 🌟 Top 4 — visually appealing & maximally distinct
    sunset    = c("#FBC34B", "#FBC34B", "#F7826E", "#F14293", "#F14293"),
    coastal   = c("#1871C2", "#1871C2", "#32826A", "#4B9311", "#4B9311"),
    twilight  = c("#7F0D0D", "#7F0D0D", "#430F5A", "#0611A7", "#0611A7"),
    solar     = c("#FFD700", "#FFD700", "#FF8C00", "#D72638", "#D72638"),
    # 🟢 Second tier — good palettes but closer in tone to top picks
    aurora    = c("#E0F7FA", "#E0F7FA", "#80DEEA", "#00796B", "#00796B"),
    forest    = c("#004225", "#004225", "#2C6E49", "#7BA05B", "#7BA05B"),
    regal     = c("#087F8C", "#087F8C", "#629064", "#BCA136", "#BCA136"),
    vibrant   = c("#FBEB5A", "#FBEB5A", "#4CE259", "#35D8A3", "#35D8A3"),
    # 🔵 Third tier — good but visually closer to something above
    oceanic   = c("#005C7A", "#005C7A", "#3E92B1", "#78C0A8", "#78C0A8"),
    blaze     = c("#F97316", "#F97316", "#C4407B", "#6D28D9", "#6D28D9"),
    autumn    = c("#3D5941", "#3D5941", "#845837", "#CA562C", "#CA562C"),
    # 🟣 Lower tier — niche or redundant palettes
    vaporwave = c("#9e0142", "#9e0142", "#7E2872", "#5e4fa2", "#5e4fa2"),
    cyber     = c("#64748B", "#64748B", "#9F4A8F", "#D9006C", "#D9006C"),
    volcano   = c("#D91E18", "#D91E18", "#A31621", "#421C52", "#421C52"),
    neonnight = c("#39FF14", "#39FF14", "#00E5FF", "#FF00FF", "#FF00FF"),
    earth     = c("#E5E0D8", "#E5E0D8", "#988681", "#4A2C2A", "#4A2C2A")
  )

  # --- Resolve chosen color bars ---
  color_bars_final <- if (is.character(color_bars) && length(color_bars) == 1) {
    if (color_bars %in% names(color_presets)) {
      color_presets[[color_bars]]
    } else {
      warning(sprintf(
        "Palette '%s' not found. Defaulting to 'sunset'. Valid options: %s",
        color_bars,
        paste(names(color_presets), collapse = ", ")
      ))
      color_presets[["sunset"]]
    }
  } else if (is.vector(color_bars) && length(color_bars) == 5) {
    color_bars
  } else {
    warning("Invalid color_bars provided. Defaulting to 'sunset'.")
    color_presets[["sunset"]]
  }

  # --- Build color bands for background ---
  color_bands <- data.frame(
    color = color_bars_final,
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
  plot_title_text <- if (isTRUE(show_title)) title_params$text else NULL
  final_title_size <- title_params$size + title_size_mod
  final_title_vjust <- (title_params$vjust + 24) * title_vjust_mod
  title_element <- if (isTRUE(show_title)) {
    ggplot2::element_text(hjust = 0.5, vjust = final_title_vjust, size = final_title_size, face = title_face, color = title_color)
  } else {
    ggplot2::element_blank()
  }

  # --- Build the plot layers ---
  p <- ggplot2::ggplot(plot_data) +
    # Background color bands and lines
    ggplot2::geom_rect(data = color_bands, ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = ystart, ymax = ystop, fill = color, alpha = opacity)) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::geom_hline(yintercept = 50, color = "black", size = 0.7, alpha = 0.5, lty = 'dashed') +
    ggplot2::geom_hline(yintercept = c(0, 100), color = "black", size = 0.6, alpha = 0.5) +
    ggplot2::geom_vline(xintercept = 1:nrow(plot_data), color = "white", size = 1, alpha = 0.25) +
    # Invisible points for structure
    ggplot2::geom_point(ggplot2::aes(x = id, y = value), alpha = 0) +
    # Curved text labels
    geomtextpath::geom_textpath(ggplot2::aes(x = 1, y = 91, label = label_top), hjust = 0.5, vjust = 0.5, color = "white", size = 6) +
    geomtextpath::geom_textpath(ggplot2::aes(x = 1, y = 11, label = label_bottom), hjust = 0.5, vjust = 0.5, color = "white", size = 6) +
    geomtextpath::geom_textpath(ggplot2::aes(x = nrow(plot_data) / 2 + 1, y = 91, label = label_top), hjust = 0.5, vjust = 0.5, color = "white", size = 6) +
    geomtextpath::geom_textpath(ggplot2::aes(x = nrow(plot_data) / 2 + 1, y = 11, label = label_bottom), hjust = 0.5, vjust = 0.5, color = "white", size = 6)

  # --- Conditionally Add the VISIBLE data points ---
  if (show_points) {
    # This logic now handles both filled and solid shapes correctly
    if (point_shape %in% 21:25) {
      # For shapes with a fill and a border (like 21)
      p <- p + ggplot2::geom_point(
        ggplot2::aes(x = id, y = value, fill = color),
        size = point_size,
        shape = point_shape,
        color = point_border_color, # Border color
        stroke = 1.2
      )
    } else {
      # For solid shapes (like 16, 17, 18)
      p <- p + ggplot2::geom_point(
        ggplot2::aes(x = id, y = value, color = color), # Use color aesthetic for solid shapes
        size = point_size,
        shape = point_shape
      )
    }
  }

  # --- Conditionally add the label background ---
  if (callout_background_style == "gradient") {
    # Use lapply to add multiple geom_point layers for the sunburst effect
    gradient_layers <- lapply(seq(19, 2, by = -.8), function(s) {
      ggplot2::geom_point(
        data = plot_data,
        ggplot2::aes(x = id, y = pmin(pmax(value, label_position_min), label_position_max)),
        size = s * callout_size_mod,
        color = "white",
        alpha = 0.030, # Each layer is very faint
        shape = 16   # Solid circle
      )
    })
    p <- p + gradient_layers
  }

  # --- Add the final text label on top of everything ---
  p <- p +
    ggplot2::geom_label(
      ggplot2::aes(x = id, y = pmin(pmax(value, label_position_min), label_position_max), label = paste0(value, "%")),
      size = ifelse(plot_data$id == group_average_label, 6, 5) * callout_size_mod,
      fontface = ifelse(plot_data$id == group_average_label, "bold", callout_text_face),
      color = plot_data$callout_color_final,
      fill = callout_bg_fill,
      alpha = callout_bg_alpha,
      label.color = callout_border_color,
      label.size = callout_border_size,
      label.r = ggplot2::unit(8, "pt"),
      show.legend = FALSE
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = ggplot2::unit(c((0.3 + plot_zoom_mod), 0, (0.3 * plot_zoom_mod), 0), "cm"),
      plot.title = title_element,
      axis.text.x = ggplot2::element_text(color = plot_data$dark_color, size = 15 * name_size_mod, face = ifelse(plot_data$id == group_average_label, "bold", "plain"), margin = ggplot2::margin(t = 12, unit = "pt"))
    ) +
    ggplot2:::coord_radial(start = -pi / (nrow(plot_data) + ((nrow(plot_data) * -0.1743) + 0.2101)), inner.radius = 0.25) +
    ggplot2::ggtitle(plot_title_text)

  if (save_plot) {
    ggplot2::ggsave(filename = output_path, plot = p, dpi = output_dpi, width = output_width, height = output_height, units = "in")
    message("Plot saved to: ", output_path)
  }
  if (show_plot) {
    print(p)
  }
  return(invisible(p))
}
