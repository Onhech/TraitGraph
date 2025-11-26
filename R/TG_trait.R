
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
#' @param show_title Logical: if TRUE (default), include the generated title in the plot.
#' @param name The name of the column containing unique identifiers.
#' @param color The name of the column containing hex color codes.
#' @param color_mode A string specifying how bar colors are chosen: `"favorite"` (default) uses the provided color column, `"midpoint"` applies trait-specific midpoint shading.
#' @param midpoint_colors Optional list with `high` and `low` hex codes used when `color_mode = "midpoint"`. Defaults to `list(high = "#00A878", low = "#3B6DD8")`.
#' @param midpoint_lighten Logical. If TRUE (default), midpoint mode lightens colors toward white as scores approach 50.
#' @param midpoint_lighten_max Numeric between 0 and 1; maximum lightening toward white when `midpoint_lighten = TRUE`. Defaults to 0.3.
#' @param midpoint_lighten_power Numeric >= 0 controlling the curve of lightening; values > 1 make lightening drop off faster as scores move away from 50.
#' @param midpoint_label_color Choose `"base"` (default) to color labels with the prototypical high/low colors, or `"shade"` to color labels with the lightened/darkened bar color (darkened for readability) when `color_mode = "midpoint"`.
#' @param group_average_label A string used to label the group average bar. Defaults to "Group\\nAverage".
#' @param group_average_color Hex color for the group average slice and text. Defaults to "black".
#' @param plot_zoom_mod A numeric value to add/subtract from the outer plot boundary.
#' @param inner_hole_size_mod A positive numeric value to reduce the inner hole size.
#' @param margin_y_mod A numeric value (in cm) to add/subtract from top/bottom margins.
#' @param margin_x_mod A numeric value (in cm) to add/subtract from left/right margins.
#' @param title_size_mod A numeric value to add/subtract from the title font size.
#' @param title_vjust_mod A numeric value to add/subtract from the title vertical adjustment.
#' @param name_size_mod A numeric value to add/subtract from the name label font size.
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
    show_title = TRUE,
    name = "name",
    color = "favourite_color",
    color_mode = c("favorite", "midpoint"),
    midpoint_colors = list(high = "#00A878", low = "#3B6DD8"),
    midpoint_lighten = TRUE,
    midpoint_lighten_max = 0.75,
    midpoint_lighten_power = 2,
    midpoint_label_color = c("base", "shade"),
    group_average_label = "Group\nAverage",
    group_average_color = "#0F2240",
    plot_zoom_mod = 0,
    inner_hole_size_mod = 0,
    margin_y_mod = 0,
    margin_x_mod = 0,
    title_size_mod = 0,
    title_vjust_mod = 0,
    name_size_mod = 0,
    output_path = "trait_plot.jpg",
    output_width = 7,
    output_height = 5,
    output_dpi = 300,
    save_plot = FALSE,
    show_plot = TRUE) {

  color_mode <- match.arg(color_mode)
  midpoint_label_color <- match.arg(midpoint_label_color)
  default_midpoint_colors <- list(high = "#00A878", low = "#3B6DD8")
  midpoint_colors <- utils::modifyList(default_midpoint_colors, midpoint_colors)
  midpoint_lighten_max <- pmax(0, pmin(midpoint_lighten_max, 1))
  midpoint_lighten_power <- pmax(0, midpoint_lighten_power)

  lighten_toward_white <- function(hex, amount) {
    rgb_matrix <- grDevices::col2rgb(hex)
    amount <- pmax(0, pmin(amount, 1))
    if (length(amount) == 1) {
      amount <- rep(amount, ncol(rgb_matrix))
    }
    adjusted_rgb <- rgb_matrix + sweep(255 - rgb_matrix, 2, amount, `*`)
    grDevices::rgb(adjusted_rgb[1, ], adjusted_rgb[2, ], adjusted_rgb[3, ], maxColorValue = 255)
  }

  compute_midpoint_color <- function(values, high_hex, low_hex) {
    vapply(values, function(v) {
      if (is.na(v)) return(NA_character_)
      base_color <- if (v >= 50) high_hex else low_hex
      intensity <- abs(v - 50) / 50
      intensity <- pmax(0, pmin(intensity, 1))
      if (!midpoint_lighten) return(base_color)
      lightening <- midpoint_lighten_max * ((1 - intensity) ^ midpoint_lighten_power)
      lighten_toward_white(base_color, lightening)
    }, character(1))
  }

  plot_data <- dataset %>%
    dplyr::rename(
      id    = !!rlang::sym(name),
      value = !!rlang::sym(column_name),
      color = !!rlang::sym(color)
    )

  group_avg <- round(mean(plot_data$value, na.rm = TRUE), 0)
  average_row <- tibble::tibble(id = group_average_label, value = group_avg, color = group_average_color)
  plot_data <- dplyr::bind_rows(average_row, plot_data)

  plot_data <- plot_data %>%
    dplyr::mutate(
      value = ifelse(value >= 99, value, round(value, 0)),
      id = as.character(id)
    )

  # Reorder participants to reduce side-edge clipping. Preserve randomness, then steer long names
  # toward safer angles and short names toward riskier angles. For larger groups we use a
  # manual placement to avoid a boxy look.
  is_group_average <- plot_data$id == group_average_label
  participant_ids <- plot_data$id[!is_group_average]
  n_participants <- length(participant_ids)
  if (n_participants > 1) {
    participant_ids <- sample(participant_ids) # randomize base order each call
    start_angle <- -pi / (n_participants + 1)  # match coord_polar start
    angles <- ((seq_len(n_participants) - 0.5) / n_participants) * 2 * pi + start_angle
    # edge_risk: sin(angle) is 0 at top/bottom and ±1 at left/right; abs() makes it a risk score
    # so higher values mean more likely to clip horizontally, lower values are safer positions.
    edge_risk <- abs(sin(angles))

    if (n_participants > 5) {
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
      name_rank <- order(-nchar(participant_ids), seq_along(participant_ids)) # longest first
      angle_rank <- order(edge_risk, seq_along(edge_risk))                   # safest angles first
      placed <- participant_ids
      placed[angle_rank] <- participant_ids[name_rank]
      final_levels <- c(group_average_label, placed)
    }
  } else {
    final_levels <- plot_data$id
  }

  plot_data <- plot_data %>%
    dplyr::mutate(id = factor(id, levels = final_levels)) %>%
    dplyr::arrange(id)

  if (color_mode == "midpoint") {
    plot_data <- plot_data %>%
      dplyr::mutate(
        base_mid_color = dplyr::if_else(
          id == group_average_label,
          group_average_color,
          dplyr::if_else(value >= 50, midpoint_colors$high, midpoint_colors$low)
        ),
        fill_color = dplyr::if_else(
          id == group_average_label,
          group_average_color,
          compute_midpoint_color(value, midpoint_colors$high, midpoint_colors$low)
        )
      )
  } else {
    plot_data <- plot_data %>%
      dplyr::mutate(
        fill_color = color,
        base_mid_color = fill_color
      )
  }

  plot_data <- plot_data %>%
    dplyr::mutate(
      is_light = is_color_light(fill_color),
      dark_color = sapply(fill_color, darken_color),
      label_color = dplyr::case_when(
        color_mode == "midpoint" & midpoint_label_color == "base"  ~ sapply(base_mid_color, darken_color),
        color_mode == "midpoint" & midpoint_label_color == "shade" ~ sapply(fill_color, darken_color),
        TRUE ~ dark_color
      ),
      border_color = ifelse(is_light, dark_color, NA_character_)
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
    ggplot2::geom_bar(ggplot2::aes(x = id, y = value, fill = fill_color), stat = "identity", alpha = 0.85, color = ggplot2::alpha(plot_data$border_color, 0.75), linewidth = 0.2) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_y_continuous(limits = c(final_y_inner_limit, final_y_outer_limit)) +
    ggplot2::theme_void() +
    ggplot2::geom_label(
      data = plot_data,
      ggplot2::aes(x = id, y = pmax(value - 18, 12), label = paste0(value, "%")),
      size = ifelse(plot_data$id == group_average_label, 3.2, 3),
      fontface = ifelse(plot_data$id == group_average_label, "bold", "plain"),
      fill = "white", alpha = 0.99, color = plot_data$label_color,
      label.size = 0.2, show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = plot_data,
      ggplot2::aes(x = id, y = ifelse(id == group_average_label, 130, 110), label = id, fontface = ifelse(id == group_average_label, "bold", "plain")),
      size = (ifelse(plot_data$id == group_average_label, 5, 4)) + name_size_mod,
      color = plot_data$label_color, angle = 0, lineheight = 0.8,
      hjust = dplyr::case_when(
        plot_data$id == group_average_label ~ 0.5,
        c(FALSE, seq_along(plot_data$id[-1]) == ceiling(length(plot_data$id[-1]) / 2)) ~ 0.5,
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

  if (save_plot) {
    ggplot2::ggsave(filename = output_path, plot = p, dpi = output_dpi, width = output_width, height = output_height, units = "in")
    message("Plot saved to: ", output_path)
  }
  if (show_plot) {
    print(p)
  }
  return(invisible(p))
}
