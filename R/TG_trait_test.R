
# -----------------------------------------------------------------------------
# FILE: R/TG_trait_test.R
# DESCRIPTION: Experimental version of TG_trait with conditional inside/outside value labels.
# AUTHOR: Lynden Jensen
# LAST EDIT: 2025-02-09
# DEPENDS: ggplot2, dplyr, tibble, rlang, scales
# STATUS: ACTIVE
# -----------------------------------------------------------------------------

#' Create a Circular Percentile Bar Chart (Experimental Labels)
#'
#' @description
#' Generates a polar bar chart of percentile scores for individuals, adds a group
#' average slice, supports favorite/midpoint coloring, and reorders labels to
#' reduce clipping while keeping label offsets fixed. This experimental version
#' uses conditional value labels that appear inside bars when possible and above
#' bars when not, to avoid boxed callouts.
#'
#' @param dataset A data frame containing the data to plot.
#' @param column_name The name of the column to use for the plot values.
#' @param title A string for the plot's main title. Defaults to the `column_name`.
#' @param show_title Logical: if TRUE (default), include the generated title in the plot.
#' @param name The name of the column containing unique identifiers.
#' @param color The name of the column containing hex color codes.
#' @param palette Optional list with `high`, `low`, and optional `mid` colors used
#'   for gradient or midpoint modes. When set, overrides `midpoint_colors`.
#' @param color_mode A string specifying how bar colors are chosen: `"favorite"` (default) uses the provided color column, `"midpoint"` applies trait-specific midpoint shading with optional lightening, and `"gradient"` simply interpolates between the low/high palette.
#' @param midpoint_colors Optional list with `high`, `low`, and optional `mid` hex codes used when `color_mode = "midpoint"` or `"gradient"`. Defaults to `list(high = "#00A878", low = "#3B6DD8")`.
#' @param midpoint_lighten Logical. If TRUE, midpoint mode lightens colors toward white as scores approach 50. Defaults to FALSE.
#' @param midpoint_lighten_max Numeric between 0 and 1; maximum lightening toward white when `midpoint_lighten = TRUE`. Defaults to 0.3.
#' @param midpoint_lighten_power Numeric >= 0 controlling the curve of lightening; values > 1 make lightening drop off faster as scores move away from 50.
#' @param midpoint_label_color Choose `"base"` (default) to color labels with the prototypical high/low colors, or `"shade"` to color labels with the lightened/darkened bar color (darkened for readability) when `color_mode = "midpoint"`.
#' @param color_opacity Numeric between 0 and 1 controlling the transparency of the bars. Defaults to 0.85.
#' @param group_average_label A string used to label the group average bar. Defaults to "Group\\nAverage".
#' @param group_average_color Optional hex color for the group average slice. If NULL (default), the color follows the same mode logic as the other bars.
#' @param group_average_label_fontface Font face for the group average label (e.g., "plain", "bold", "italic"). Defaults to the same as other labels.
#' @param group_average_label_color Optional hex color for the group average label. If NULL (default), the label follows the same color logic as other labels.
#' @param order_mode How to order participant labels around the circle: `"hybrid"` (default; manual slots for n > 5, else risk-based), `"risk"` (always longest-to-safest), or `"custom"` (use `order_column`).
#' @param order_column Name of a column that supplies a custom order when `order_mode = "custom"`.
#' @param random_seed Optional integer seed to make the randomized ordering reproducible (when `order_mode` uses randomness).
#' @param center_middle_label Logical; if TRUE (default), center-align the middle participant label to reduce overlap.
#' @param plot_zoom_mod A numeric value to add/subtract from the outer plot boundary.
#' @param inner_hole_size_mod A positive numeric value to reduce the inner hole size.
#' @param margin_y_mod A numeric value (in cm) to add/subtract from top/bottom margins.
#' @param margin_x_mod A numeric value (in cm) to add/subtract from left/right margins.
#' @param label_radius_base Numeric y coordinate used for name labels; higher values push labels further from the plot edge.
#' @param label_nudge_bottom Logical; if TRUE (default) slightly offsets labels near the 6 o'clock position to prevent overlap.
#' @param title_size_mod A numeric value to add/subtract from the title font size.
#' @param title_vjust_mod A numeric value to add/subtract from the title vertical adjustment.
#' @param name_size_mod A numeric value to add/subtract from the name label font size.
#' @param callout_inside_threshold Numeric; value labels are placed inside bars when
#'   the bar value is at or above this threshold. Defaults to 20.
#' @param callout_inside_offset Numeric; vertical offset used when labels are placed
#'   inside bars. Defaults to 18 (mirrors the original label placement).
#' @param callout_inside_min_y Numeric; minimum y position for inside labels to avoid
#'   crowding the hole. Defaults to 12.
#' @param callout_outside_nudge Numeric; vertical offset added to the bar value when
#'   labels are placed above bars. Defaults to 4.
#' @param callout_text_size Numeric; text size for value labels. Defaults to 3.
#' @param callout_text_size_group Numeric; text size for the group average label.
#'   Defaults to 3.2.
#' @param callout_text_face Font face for value labels (e.g., "plain", "bold",
#'   "italic", "bold.italic"). Defaults to "plain".
#' @param callout_style Label style toggle: \code{"inside_outside"} (default) uses
#'   unboxed labels with inside/above placement, while \code{"boxed"} uses the
#'   original boxed callouts with bar-color text.
#' @param output_path The full path where the plot will be saved.
#' @param output_width The width of the saved image in inches.
#' @param output_height The height of the saved image in inches.
#' @param output_dpi The resolution (dots per inch) for the saved image.
#' @param save_plot A logical value. If TRUE, the plot is saved to disk.
#' @param show_plot A logical value. If TRUE, the plot is displayed.
#' @param verbose Logical; if TRUE, emit a concise save message. Defaults to FALSE.
#'
#' @return Invisibly returns the ggplot object.
#' @export
TG_trait_test <- function(
    dataset,
    column_name,
    title = column_name,
    show_title = TRUE,
    name = "name",
    color = "favourite_color",
    palette = NULL,
    color_mode = c("gradient", "favorite", "midpoint"),
    midpoint_colors = list(high = "#00A878", low = "#3B6DD8"),
    midpoint_lighten = FALSE,
    midpoint_lighten_max = 0.75,
    midpoint_lighten_power = 2,
    midpoint_label_color = c("shade", "base"),
    group_average_label = "Group\nAverage",
    group_average_color = NULL,
    group_average_label_fontface = NULL,
    group_average_label_color = NULL,
    order_mode = c("hybrid", "risk", "custom"),
    order_column = NULL,
    random_seed = 12345,
    center_middle_label = TRUE,
    plot_zoom_mod = 0,
    inner_hole_size_mod = 0,
    margin_y_mod = 0,
    margin_x_mod = 0,
    label_radius_base = 110,
    label_nudge_bottom = FALSE,
    title_size_mod = 0,
    title_vjust_mod = 0,
    name_size_mod = 0,
    callout_inside_threshold = 20,
    callout_inside_offset = 18,
    callout_inside_min_y = 12,
    callout_outside_nudge = 4,
    callout_text_size = 3,
    callout_text_size_group = 3.2,
    callout_text_face = "plain",
    callout_style = c("inside_outside", "boxed"),
    output_path = "trait_plot.jpg",
    output_width = 7,
    color_opacity = 0.95,
    output_height = 5,
    output_dpi = 300,
    save_plot = FALSE,
    show_plot = TRUE,
    verbose = FALSE) {

  color_mode <- match.arg(color_mode)
  midpoint_label_color <- match.arg(midpoint_label_color)
  order_mode <- match.arg(order_mode)
  default_midpoint_colors <- list(high = "#00A878", low = "#3B6DD8", mid = NA_character_)
  palette_input <- if (!is.null(palette)) palette else utils::modifyList(default_midpoint_colors, midpoint_colors)
  # Normalize palette input with a grayscale fallback for missing/invalid colors.
  midpoint_colors <- tg_normalize_palette(palette_input, context = "TG_trait_test palette", allow_mid_na = TRUE, warn_on_fallback = TRUE)
  callout_style <- match.arg(callout_style)
  midpoint_lighten_max <- pmax(0, pmin(midpoint_lighten_max, 1))
  midpoint_lighten_power <- pmax(0, midpoint_lighten_power)
  if (!is.null(random_seed)) {
    old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) .Random.seed else NULL
    on.exit({
      if (!is.null(old_seed)) .Random.seed <<- old_seed
    }, add = TRUE)
    set.seed(random_seed)
  }

  lighten_toward_white <- function(hex, amount) {
    amount <- rep(amount, length.out = length(hex))
    result <- rep(NA_character_, length(hex))
    valid <- !is.na(hex)
    if (any(valid)) {
      rgb_matrix <- grDevices::col2rgb(hex[valid])
      clean_amount <- pmax(0, pmin(amount[valid], 1))
      adjusted_rgb <- rgb_matrix + sweep(255 - rgb_matrix, 2, clean_amount, `*`)
      result[valid] <- grDevices::rgb(
        adjusted_rgb[1, ],
        adjusted_rgb[2, ],
        adjusted_rgb[3, ],
        maxColorValue = 255
      )
    }
    result
  }

  compute_gradient_colors <- function(values, high_hex, low_hex, mid_hex = NA_character_) {
    if (is.null(mid_hex)) {
      mid_hex <- NA_character_
    }
    clamped <- pmin(pmax(values, 0), 100)
    gradient <- rep(NA_character_, length(clamped))
    valid <- !is.na(clamped) & !is.na(high_hex) & !is.na(low_hex)
    if (!any(valid)) {
      return(gradient)
    }
    colors <- c(low_hex, high_hex)
    if (!is.na(mid_hex) && nzchar(mid_hex)) {
      colors <- c(low_hex, mid_hex, high_hex)
    }
    ramp <- grDevices::colorRamp(colors)
    rgb_values <- ramp(clamped[valid] / 100)
    gradient[valid] <- grDevices::rgb(
      rgb_values[, 1],
      rgb_values[, 2],
      rgb_values[, 3],
      maxColorValue = 255
    )
    gradient
  }

  compute_midpoint_color <- function(values, high_hex, low_hex, mid_hex = NA_character_) {
    colors <- compute_gradient_colors(values, high_hex, low_hex, mid_hex)
    if (!midpoint_lighten) {
      return(colors)
    }
    clamped <- pmin(pmax(values, 0), 100)
    valid <- !is.na(colors) & !is.na(clamped)
    if (!any(valid)) {
      return(colors)
    }
    intensity <- abs(clamped[valid] - 50) / 50
    intensity <- pmax(0, pmin(intensity, 1))
    lightening <- midpoint_lighten_max * ((1 - intensity) ^ midpoint_lighten_power)
    colors[valid] <- lighten_toward_white(colors[valid], lightening)
    colors
  }

  average_hex_colors <- function(hex) {
    hex <- hex[!is.na(hex)]
    if (length(hex) == 0) {
      return(NA_character_)
    }
    rgb_matrix <- grDevices::col2rgb(hex)
    avg <- rowMeans(rgb_matrix)
    grDevices::rgb(avg[1], avg[2], avg[3], maxColorValue = 255)
  }

  plot_data <- dataset %>%
    dplyr::rename(
      id    = !!rlang::sym(name),
      value = !!rlang::sym(column_name),
      color = !!rlang::sym(color)
    )

  group_avg <- round(mean(plot_data$value, na.rm = TRUE), 0)
  group_average_color <- if (is.null(group_average_color) || is.na(group_average_color) || group_average_color == "") {
    NA_character_
  } else {
    group_average_color
  }
  average_row <- tibble::tibble(id = group_average_label, value = group_avg, color = group_average_color)
  plot_data <- dplyr::bind_rows(average_row, plot_data)

  plot_data <- plot_data %>%
    dplyr::mutate(
      value = ifelse(value >= 99, value, round(value, 0)),
      id = as.character(id)
    )

  # Reorder participants to reduce side-edge clipping. Preserve randomness, then steer long names
  # toward safer angles and short names toward riskier angles. Modes:
  # - hybrid (default): manual slots for n > 5, else risk-based
  # - risk: always longest-to-safest
  # - custom: use an order column from the data
  is_group_average <- plot_data$id == group_average_label
  participant_ids <- plot_data$id[!is_group_average]
  n_participants <- length(participant_ids)
  if (n_participants > 1) {
    if (order_mode == "custom" && !is.null(order_column) && order_column %in% names(dataset)) {
      custom_order <- dataset %>%
        dplyr::select(dplyr::all_of(c(name, order_column))) %>%
        dplyr::rename(id = !!rlang::sym(name), .order = !!rlang::sym(order_column)) %>%
        dplyr::filter(id %in% participant_ids) %>%
        dplyr::arrange(.order, id)
      placed <- custom_order$id
      final_levels <- c(group_average_label, placed)
    } else {
      participant_ids <- sample(participant_ids) # randomize base order each call
      start_angle <- -pi / (n_participants + 1)  # match coord_polar start
      angles <- ((seq_len(n_participants) - 0.5) / n_participants) * 2 * pi + start_angle
      # edge_risk: sin(angle) is 0 at top/bottom and ±1 at left/right; abs() makes it a risk score
      # so higher values mean more likely to clip horizontally, lower values are safer positions.
      edge_risk <- abs(sin(angles))

      use_hybrid <- order_mode == "hybrid" && n_participants > 5
      if (use_hybrid) {
        # Manual placement for larger groups:
        # longest -> position 1; second-longest -> position n;
        # shortest -> highest-risk open slot; second-shortest -> second highest-risk open slot;
        # remaining names keep their (shuffled) order in remaining slots.
        name_len <- nchar(participant_ids)
        long_order <- order(-name_len, seq_along(name_len)) # longest first
        short_order <- order(name_len, seq_along(name_len)) # shortest first

        placed <- rep(NA_character_, n_participants)

        longest_name <- participant_ids[long_order[1]]
        placed[1] <- longest_name
        if (n_participants >= 2) {
          second_long_name <- participant_ids[long_order[min(2, n_participants)]]
          placed[n_participants] <- second_long_name
        }

        used_names <- placed[!is.na(placed)]
        remaining_idx <- setdiff(seq_along(participant_ids), long_order[1:min(2, n_participants)])
        remaining_names <- participant_ids[remaining_idx]
        remaining_lens <- name_len[remaining_idx]

        short_idx_order <- order(remaining_lens, seq_along(remaining_lens)) # shortest among remaining
        risk_order <- order(edge_risk, decreasing = TRUE, seq_along(edge_risk)) # highest risk first

        # Place shortest then second-shortest into highest-risk available slots (not 1 or n)
        placed_short <- 0
        for (risk_slot in risk_order) {
          if (risk_slot %in% c(1, n_participants)) next
          if (placed_short >= min(2, length(short_idx_order))) break
          candidate_name <- remaining_names[short_idx_order[placed_short + 1]]
          if (!candidate_name %in% used_names) {
            placed[risk_slot] <- candidate_name
            used_names <- c(used_names, candidate_name)
            placed_short <- placed_short + 1
          }
        }

        # Fill remaining slots with remaining names in their current (shuffled) order
        fill_names <- participant_ids[!(participant_ids %in% used_names)]
        empty_slots <- which(is.na(placed))
        if (length(empty_slots) > 0 && length(fill_names) > 0) {
          placed[empty_slots] <- fill_names[seq_len(min(length(empty_slots), length(fill_names)))]
        }

        final_levels <- c(group_average_label, placed)
      } else {
        # Pure risk-based longest-to-safest placement
        name_rank <- order(-nchar(participant_ids), seq_along(participant_ids)) # longest first
        angle_rank <- order(edge_risk, seq_along(edge_risk))                   # safest angles first
        placed <- participant_ids
        placed[angle_rank] <- participant_ids[name_rank]
        final_levels <- c(group_average_label, placed)
      }
    }
  } else {
    final_levels <- plot_data$id
  }

  plot_data <- plot_data %>%
    dplyr::mutate(id = factor(id, levels = final_levels)) %>%
    dplyr::arrange(id)

  group_average_fill <- group_average_color
  if (is.na(group_average_fill)) {
    if (color_mode == "favorite") {
      group_average_fill <- average_hex_colors(plot_data$color[plot_data$id != group_average_label])
    } else if (color_mode == "midpoint") {
      group_average_fill <- compute_midpoint_color(group_avg, midpoint_colors$high, midpoint_colors$low, midpoint_colors$mid)
    } else {
      group_average_fill <- compute_gradient_colors(group_avg, midpoint_colors$high, midpoint_colors$low, midpoint_colors$mid)
    }
  }

  if (color_mode %in% c("midpoint", "gradient")) {
    gradient_colors <- compute_gradient_colors(plot_data$value, midpoint_colors$high, midpoint_colors$low, midpoint_colors$mid)
    plot_data <- plot_data %>%
      dplyr::mutate(
        base_mid_color = dplyr::case_when(
          id == group_average_label ~ group_average_fill,
          color_mode == "midpoint" & value >= 50 ~ midpoint_colors$high,
          color_mode == "midpoint" ~ midpoint_colors$low,
          TRUE ~ gradient_colors
        ),
        fill_color = dplyr::if_else(
          id == group_average_label,
          group_average_fill,
          if (color_mode == "midpoint") {
            compute_midpoint_color(value, midpoint_colors$high, midpoint_colors$low, midpoint_colors$mid)
          } else {
            gradient_colors
          }
        )
      )
  } else {
    plot_data <- plot_data %>%
      dplyr::mutate(
        fill_color = dplyr::if_else(id == group_average_label, group_average_fill, color),
        base_mid_color = fill_color
      )
  }

  plot_data <- plot_data %>%
    dplyr::mutate(
      is_light = is_color_light(fill_color),
      dark_color = sapply(fill_color, darken_color),
      # Use the base shade unless it's light; then darken for readability.
      label_base = dplyr::case_when(
        color_mode == "midpoint" & midpoint_label_color == "base"  ~ base_mid_color,
        color_mode == "midpoint" & midpoint_label_color == "shade" ~ fill_color,
        TRUE ~ fill_color
      ),
      label_color = ifelse(is_light, sapply(label_base, darken_color), label_base),
      border_color = ifelse(is_light, dark_color, NA_character_)
    )

  group_average_label_fontface <- if (is.null(group_average_label_fontface)) {
    "plain"
  } else {
    group_average_label_fontface
  }
  group_average_label_color <- if (is.null(group_average_label_color) || is.na(group_average_label_color) || group_average_label_color == "") {
    NA_character_
  } else {
    group_average_label_color
  }
  plot_data <- plot_data %>%
    dplyr::mutate(
      label_color = ifelse(id == group_average_label & !is.na(group_average_label_color), group_average_label_color, label_color),
      label_fontface = ifelse(id == group_average_label, group_average_label_fontface, "plain")
    )

  # --- Label radius adjustments ---
  n_labels <- nrow(plot_data)
  angles_deg <- ((seq_len(n_labels) - 0.5) / n_labels) * 360 - (180 / n_labels)
  base_radius <- ifelse(plot_data$id == group_average_label, 130, label_radius_base)
  radius_offsets <- rep(0, n_labels)
  if (isTRUE(label_nudge_bottom)) {
    bottom_mask <- angles_deg > 135 & angles_deg < 225 & plot_data$id != group_average_label
    if (any(bottom_mask)) {
      idx <- which(bottom_mask)
      radius_offsets[idx] <- abs(rank(angles_deg[idx]) - (length(idx) + 1) / 2) * 3
    }
  }
  plot_data$label_radius <- base_radius + radius_offsets

  # --- Conditional value label placement (inside vs above bar) ---
  callout_inside_threshold <- pmax(0, pmin(callout_inside_threshold, 100))
  callout_inside_offset <- pmax(0, callout_inside_offset)
  callout_inside_min_y <- pmax(0, callout_inside_min_y)
  callout_outside_nudge <- pmax(0, callout_outside_nudge)
  if (is.null(callout_text_face) || is.na(callout_text_face) || callout_text_face == "") {
    callout_text_face <- "plain"
  }
  plot_data <- plot_data %>%
    dplyr::mutate(
      callout_position = ifelse(value >= callout_inside_threshold, "inside", "outside"),
      callout_y = ifelse(
        callout_position == "inside",
        pmax(value - callout_inside_offset, callout_inside_min_y),
        value + callout_outside_nudge
      ),
      callout_color = ifelse(callout_position == "inside", "white", fill_color),
      callout_size = ifelse(id == group_average_label, callout_text_size_group, callout_text_size)
    )

  title_params <- get_dynamic_title(title)
  final_title_size <- 8 + title_params$size + title_size_mod
  final_title_vjust <- 16 + title_params$vjust + title_vjust_mod
  final_y_outer_limit <- 135 + plot_zoom_mod
  final_y_inner_limit <- -40 + inner_hole_size_mod
  plot_title_text <- if (isTRUE(show_title)) title_params$text else NULL
  title_element <- if (isTRUE(show_title)) {
    ggplot2::element_text(hjust = 0.5, vjust = final_title_vjust, size = final_title_size, face = "bold")
  } else {
    ggplot2::element_blank()
  }

  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_hline(yintercept = c(25, 75), color = "black", size = 0.2, alpha = 0.4, lty = 'dashed') +
    ggplot2::geom_hline(yintercept = 50, color = "black", size = 0.6, alpha = 0.5) +
    ggplot2::geom_hline(yintercept = c(100), color = "black", size = 0.6, alpha = .5) +
    ggplot2::geom_bar(ggplot2::aes(x = id, y = value, fill = fill_color), stat = "identity", alpha = color_opacity, color = ggplot2::alpha(plot_data$border_color, 0.75), linewidth = 0.2) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_y_continuous(limits = c(final_y_inner_limit, final_y_outer_limit)) +
    ggplot2::theme_void() +
    ggplot2::geom_text(
      data = plot_data,
      ggplot2::aes(x = id, y = label_radius, label = id, fontface = label_fontface),
      size = (ifelse(plot_data$id == group_average_label, 5, 4)) + name_size_mod,
      color = plot_data$label_color, angle = 0, lineheight = 0.8,
      hjust = dplyr::case_when(
        plot_data$id == group_average_label ~ 0.5,
        center_middle_label & c(FALSE, seq_along(plot_data$id[-1]) == ceiling(length(plot_data$id[-1]) / 2)) ~ 0.5,
        seq_along(plot_data$id) <= length(plot_data$id) / 2 ~ 0,
        TRUE ~ 1
      )
    ) +
    ggplot2::theme(
      plot.margin = ggplot2::unit(c(-1 + margin_y_mod, -1 + margin_x_mod, -1 + margin_y_mod, -1 + margin_x_mod), "cm"),
      plot.title = title_element
    ) +
    ggplot2::coord_polar(start = -pi / (nrow(plot_data))) +
    ggplot2::ggtitle(plot_title_text)

  if (callout_style == "boxed") {
    p <- p + ggplot2::geom_label(
      data = plot_data,
      ggplot2::aes(x = id, y = pmax(value - callout_inside_offset, callout_inside_min_y), label = paste0(value, "%")),
      size = ifelse(plot_data$id == group_average_label, callout_text_size_group, callout_text_size),
      fontface = callout_text_face,
      fill = "white",
      alpha = 0.99,
      color = plot_data$label_color,
      label.size = 0.2,
      show.legend = FALSE
    )
  } else {
    p <- p + ggplot2::geom_text(
      data = plot_data,
      ggplot2::aes(x = id, y = callout_y, label = paste0(value, "%")),
      size = plot_data$callout_size,
      fontface = callout_text_face,
      color = plot_data$callout_color,
      show.legend = FALSE
    )
  }

  if (save_plot) {
    ggplot2::ggsave(filename = output_path, plot = p, dpi = output_dpi, width = output_width, height = output_height, units = "in")
    tg_log_plot_saved(output_path, verbose)
  }
  if (show_plot) {
    print(p)
  }
  return(invisible(p))
}
