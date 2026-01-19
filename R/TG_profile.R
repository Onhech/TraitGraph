# ----------------------------------------------------------------------------
# FILE: R/TG_profile.R
# DESCRIPTION: Circular percentile bar chart for a single profile (no group avg).
# AUTHOR: Lynden Jensen
# LAST EDIT: 2025-02-09
# DEPENDS: ggplot2, dplyr, tibble, rlang, scales
# STATUS: ACTIVE
# ----------------------------------------------------------------------------

#' Create a Circular Profile Chart
#'
#' @description
#' Generates a polar bar chart for a single person's psychological profile, where
#' each row represents a trait and colors are derived from each trait's own
#' low/high palette.
#'
#' @param dataset A data frame containing trait rows for a single profile.
#' @param value_column The column name containing percentile values.
#' @param trait_column The column name containing trait labels. Defaults to "name".
#' @param low_color_column Column name for each trait's low color (hex). Defaults to "Low_Color".
#' @param mid_color_column Optional column name for each trait's midpoint color (hex). Defaults to "Mid_Color".
#' @param high_color_column Column name for each trait's high color (hex). Defaults to "High_Color".
#' @param palette Optional fallback palette list with `high`, `low`, and optional `mid` colors.
#'   Used when any row is missing valid colors. Defaults to grayscale fallback.
#' @param title A string for the plot's main title.
#' @param show_title Logical: if TRUE (default), include the title in the plot.
#' @param color_opacity Numeric between 0 and 1 controlling bar transparency. Defaults to 0.95.
#' @param gradient_lighten Logical; if TRUE, lightens colors toward white near the midpoint (50). Defaults to FALSE.
#' @param gradient_lighten_max Numeric between 0 and 1; maximum lightening toward white when `gradient_lighten = TRUE`. Defaults to 0.75.
#' @param gradient_lighten_power Numeric >= 0 controlling the curve of lightening; values > 1 make lightening drop off faster as scores move away from 50.
#' @param label_radius_base Numeric y coordinate used for name labels.
#' @param label_nudge_bottom Logical; if TRUE (default) slightly offsets labels near the 6 o'clock position.
#' @param title_size_mod A numeric value to add/subtract from the title font size.
#' @param title_vjust_mod A numeric value to add/subtract from the title vertical adjustment.
#' @param name_size_mod A numeric value to add/subtract from the name label font size.
#' @param plot_zoom_mod A numeric value to add/subtract from the outer plot boundary.
#' @param inner_hole_size_mod A positive numeric value to reduce the inner hole size.
#' @param margin_y_mod A numeric value (in cm) to add/subtract from top/bottom margins.
#' @param margin_x_mod A numeric value (in cm) to add/subtract from left/right margins.
#' @param output_path The full path where the plot will be saved.
#' @param output_width The width of the saved image in inches.
#' @param output_height The height of the saved image in inches.
#' @param output_dpi The resolution (dots per inch) for the saved image.
#' @param save_plot A logical value. If TRUE, the plot is saved to disk.
#' @param show_plot A logical value. If TRUE, the plot is displayed.
#'
#' @return Invisibly returns the ggplot object.
#' @export
#'
#' @examples
#' \dontrun{
#' TG_profile(
#'   dataset = group_score,
#'   value_column = "group_score",
#'   trait_column = "name",
#'   low_color_column = "Low_Color",
#'   high_color_column = "High_Color",
#'   title = "Group Psychological Profile",
#'   show_title = FALSE
#' )
#' }
TG_profile <- function(
    dataset,
    value_column,
    trait_column = "name",
    low_color_column = "Low_Color",
    mid_color_column = "Mid_Color",
    high_color_column = "High_Color",
    palette = NULL,
    title = NULL,
    show_title = TRUE,
    color_opacity = 0.95,
    gradient_lighten = FALSE,
    gradient_lighten_max = 0.75,
    gradient_lighten_power = 2,
    label_radius_base = 110,
    label_nudge_bottom = FALSE,
    title_size_mod = 0,
    title_vjust_mod = 0,
    name_size_mod = 0,
    plot_zoom_mod = 0,
    inner_hole_size_mod = 0,
    margin_y_mod = 0,
    margin_x_mod = 0,
    output_path = "profile_plot.jpg",
    output_width = 7,
    output_height = 5,
    output_dpi = 300,
    save_plot = FALSE,
    show_plot = TRUE
) {

  lighten_toward_white <- function(hex, amount) {
    if (is.na(hex)) return(NA_character_)
    amount <- pmax(0, pmin(amount, 1))
    rgb_matrix <- grDevices::col2rgb(hex)
    adjusted_rgb <- rgb_matrix + (255 - rgb_matrix) * amount
    grDevices::rgb(adjusted_rgb[1], adjusted_rgb[2], adjusted_rgb[3], maxColorValue = 255)
  }

  compute_profile_color <- function(value, high_hex, low_hex, mid_hex = NA_character_) {
    if (is.null(mid_hex)) {
      mid_hex <- NA_character_
    }
    if (is.na(value) || is.na(high_hex) || is.na(low_hex)) return(NA_character_)
    clamped <- pmin(pmax(value, 0), 100)
    colors <- c(low_hex, high_hex)
    if (!is.na(mid_hex) && nzchar(mid_hex)) {
      colors <- c(low_hex, mid_hex, high_hex)
    }
    ramp <- grDevices::colorRamp(colors)
    rgb_values <- ramp(clamped / 100)
    base <- grDevices::rgb(rgb_values[1], rgb_values[2], rgb_values[3], maxColorValue = 255)
    if (!isTRUE(gradient_lighten)) return(base)
    intensity <- abs(clamped - 50) / 50
    intensity <- pmax(0, pmin(intensity, 1))
    lightening <- gradient_lighten_max * ((1 - intensity) ^ gradient_lighten_power)
    lighten_toward_white(base, lightening)
  }

  gradient_lighten_max <- pmax(0, pmin(gradient_lighten_max, 1))
  gradient_lighten_power <- pmax(0, gradient_lighten_power)

  if (is.null(mid_color_column) || !(mid_color_column %in% names(dataset))) {
    mid_color_column <- "Mid_Color"
    if (!(mid_color_column %in% names(dataset))) {
      dataset[[mid_color_column]] <- NA_character_
    }
  }

  # Normalize fallback palette once; row-level validation handled below.
  fallback_palette <- tg_normalize_palette(palette, context = "TG_profile palette", allow_mid_na = TRUE, warn_on_fallback = FALSE)

  plot_data <- dataset %>%
    dplyr::rename(
      id = !!rlang::sym(trait_column),
      value = !!rlang::sym(value_column),
      low_color = !!rlang::sym(low_color_column),
      mid_color = !!rlang::sym(mid_color_column),
      high_color = !!rlang::sym(high_color_column)
    ) %>%
    dplyr::mutate(
      id = as.character(id),
      value = round(as.numeric(value), 0)
    )

  high_ok <- vapply(plot_data$high_color, tg_color_valid, logical(1))
  low_ok <- vapply(plot_data$low_color, tg_color_valid, logical(1))
  mid_missing <- is.na(plot_data$mid_color) | !nzchar(trimws(plot_data$mid_color))
  mid_ok <- mid_missing | vapply(plot_data$mid_color, tg_color_valid, logical(1))
  if (any(!(high_ok & low_ok & mid_ok))) {
    warning("TraitGraph: using grayscale fallback palette in TG_profile palette.")
  }

  # Apply fallback colors for any invalid rows while preserving optional mids.
  plot_data$high_color <- ifelse(high_ok, plot_data$high_color, fallback_palette$high)
  plot_data$low_color <- ifelse(low_ok, plot_data$low_color, fallback_palette$low)
  plot_data$mid_color <- ifelse(mid_missing, NA_character_, ifelse(mid_ok, plot_data$mid_color, fallback_palette$mid))

  plot_data <- plot_data %>%
    dplyr::mutate(
      fill_color = mapply(compute_profile_color, value, high_color, low_color, mid_color),
      is_light = is_color_light(fill_color),
      dark_color = sapply(fill_color, darken_color),
      label_color = ifelse(is_light, sapply(fill_color, darken_color), fill_color),
      border_color = ifelse(is_light, dark_color, NA_character_)
    )

  # Preserve input order
  plot_data$id <- factor(plot_data$id, levels = plot_data$id)

  n_labels <- nrow(plot_data)
  angles_deg <- ((seq_len(n_labels) - 0.5) / n_labels) * 360 - (180 / n_labels)
  # Keep label radius stable across different label counts.
  base_radius <- label_radius_base + (8 - n_labels) * 2
  base_radius <- max(100, base_radius)
  radius_offsets <- rep(0, n_labels)
  if (isTRUE(label_nudge_bottom)) {
    bottom_mask <- angles_deg > 135 & angles_deg < 225
    if (any(bottom_mask)) {
      idx <- which(bottom_mask)
      radius_offsets[idx] <- abs(rank(angles_deg[idx]) - (length(idx) + 1) / 2) * 3
    }
  }
  plot_data$label_radius <- base_radius + radius_offsets

  label_index <- seq_len(n_labels)
  right_end <- if (n_labels %% 2 == 0) (n_labels / 2) else (floor(n_labels / 2) + 1)
  center_indices <- if (n_labels %% 2 == 0) c(1, n_labels / 2 + 1) else c(1)
  plot_data$label_hjust <- dplyr::case_when(
    label_index %in% center_indices ~ 0.5,
    label_index <= right_end ~ 0,
    TRUE ~ 1
  )

  plot_title_text <- if (isTRUE(show_title)) title else NULL
  title_element <- if (isTRUE(show_title)) {
    ggplot2::element_text(hjust = 0.5, vjust = 16 + title_vjust_mod, size = 8 + title_size_mod, face = "bold")
  } else {
    ggplot2::element_blank()
  }

  final_y_outer_limit <- 135 + plot_zoom_mod
  final_y_inner_limit <- -40 + inner_hole_size_mod

  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_hline(yintercept = c(25, 75), color = "black", size = 0.2, alpha = 0.4, lty = "dashed") +
    ggplot2::geom_hline(yintercept = 50, color = "black", size = 0.6, alpha = 0.5) +
    ggplot2::geom_hline(yintercept = c(100), color = "black", size = 0.6, alpha = 0.5) +
    ggplot2::geom_bar(
      ggplot2::aes(x = id, y = value, fill = fill_color),
      stat = "identity",
      alpha = color_opacity,
      color = ggplot2::alpha(plot_data$border_color, 0.75),
      linewidth = 0.2
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_y_continuous(limits = c(final_y_inner_limit, final_y_outer_limit)) +
    ggplot2::theme_void() +
    ggplot2::geom_label(
      data = plot_data,
      ggplot2::aes(x = id, y = pmax(value - 18, 12), label = paste0(value, "%")),
      size = 3,
      fontface = "plain",
      fill = "white",
      alpha = 0.99,
      color = plot_data$label_color,
      label.size = 0.2,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = plot_data,
      ggplot2::aes(x = id, y = label_radius, label = id),
      size = 4 + name_size_mod,
      color = plot_data$label_color,
      angle = 0,
      lineheight = 0.8,
      hjust = plot_data$label_hjust
    ) +
    ggplot2::theme(
      plot.margin = ggplot2::unit(c(-1 + margin_y_mod, -1 + margin_x_mod, -1 + margin_y_mod, -1 + margin_x_mod), "cm"),
      plot.title = title_element
    ) +
    ggplot2::coord_polar(start = -pi / (nrow(plot_data))) +
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
