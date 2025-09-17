# --- Doughnut Chart for Votes/Opinion Data ---
# Creates a doughnut chart to visualize proportions, building on the logic
# refined in the testing script.

# Required libraries: dplyr, ggplot2, rlang

#' Create and Save a Doughnut Chart for Votes/Opinion Data
#'
#' @description
#' This function takes a dataset and creates a doughnut chart to represent the
#' proportional breakdown of a numeric column. It includes robust logic for
#' label placement and coloring to ensure readability.
#'
#' @param dataset A data frame containing the data to plot.
#' @param column_name The name of the column containing the numeric values.
#' @param name_col The name of the column containing unique identifiers (e.g., names). Defaults to "names".
#' @param color_col The name of the column containing hex color codes. Defaults to "favourite_color".
#' @param title A string for the plot's main title. If NULL, `column_name` is used.
#' @param show_title Logical. If TRUE (default), the title is displayed. Set to FALSE to hide.
#' @param title_face The font style of the title (e.g., `plain`, `bold`). Defaults to `bold`.
#' @param title_color The color of the title text.
#' @param sort_order The order to sort the results (`desc` or `asc`). Defaults to "desc".
#' @param inner_label_threshold A numeric value (0-100). Percentage labels for slices
#'   smaller than this value will be hidden or replaced. Defaults to 4.
#' @param use_indicator Logical. If TRUE, an asterisk is used for slices smaller than `inner_label_threshold`
#'   and a conditional footnote is added. Defaults to TRUE.
#' @param indicator_text A string for the footnote. Use `{threshold}` as a placeholder for the value of
#'   `inner_label_threshold`. Defaults to "* <{threshold}%".
#' @param hole_size Numeric value between 0 and 1 to control the size of the doughnut's hole. Defaults to 0.5.
#' @param name_size_mod A numeric value to add or subtract from the label font size.
#' @param title_size_mod A numeric value to add or subtract from the title font size.
#' @param title_vjust_mod A numeric value to add or subtract from the title's vertical adjustment.
#' @param output_path The file path for saving the plot.
#' @param output_width The width of the saved image in inches.
#' @param output_height The height of the saved image in inches.
#' @param output_dpi The resolution for the saved image.
#' @param save_plot Logical. If TRUE, the plot is saved.
#' @param show_plot Logical. If TRUE, the plot is displayed.
#'
#' @return Invisibly returns the ggplot object.
#' @export
TG_doughnut_chart <- function(dataset,
                              column_name,
                              name_col = "name",
                              color_col = "favourite_color",
                              title = NULL,
                              show_title = TRUE,
                              title_face = "bold",
                              title_color = "black",
                              sort_order = "desc",
                              inner_label_threshold = 4,
                              use_indicator = TRUE,
                              indicator_text = "* <{threshold}%",
                              hole_size = 0.5,
                              name_size_mod = 0,
                              title_size_mod = 0,
                              title_vjust_mod = 0,
                              output_path = "doughnut_plot.png",
                              output_width = 7,
                              output_height = 7,
                              output_dpi = 300,
                              save_plot = TRUE,
                              show_plot = TRUE) {

  # --- 1. Input Validation & Data Prep ---
  if (!show_title) { title <- NULL }
  else if (is.null(title)) { title <- column_name }

  if (!sort_order %in% c("desc", "asc")) { stop("`sort_order` must be either 'desc' or 'asc'.") }

  plot_data <- dataset %>%
    dplyr::rename(
      id = !!rlang::sym(name_col),
      value = !!rlang::sym(column_name),
      color = !!rlang::sym(color_col)
    ) %>%
    dplyr::filter(value > 0)

  if (nrow(plot_data) == 0) {
    message("No data with scores greater than 0 to plot.")
    return(invisible(NULL))
  }

  # --- 2. Calculate Percentages and Label Positions/Colors ---
  plot_data <- plot_data %>%
    dplyr::mutate(percentage = value / sum(value) * 100) %>%
    { if (sort_order == "desc") dplyr::arrange(., dplyr::desc(value)) else dplyr::arrange(., value) } %>%
    dplyr::mutate(id = factor(id, levels = unique(id))) %>%
    dplyr::mutate(
      pos = rev(cumsum(rev(percentage)) - 0.5 * rev(percentage)),
      name_label = id,
      value_label = dplyr::case_when(
        percentage > inner_label_threshold ~ paste0(round(percentage, 0), "%"),
        use_indicator ~ "*",
        TRUE ~ ""
      ),
      is_light = is_color_light(color),
      dark_color = sapply(color, darken_color),
      label_color = ifelse(is_light, dark_color, color),
      label_hjust = dplyr::case_when(
        pos > 95 | pos < 5  ~ 0.5,
        pos > 45 & pos < 55 ~ 0.5,
        pos >= 50 ~ 1,
        TRUE ~ 0
      )
    )

  # Determine if the caption is needed
  show_caption <- use_indicator && any(plot_data$percentage <= inner_label_threshold)
  caption_text <- if (show_caption) gsub("\\{threshold\\}", inner_label_threshold, indicator_text) else NULL


  # Create separate named vectors for fill and text colors
  fill_vec <- plot_data$color
  names(fill_vec) <- plot_data$id

  text_color_vec <- plot_data$label_color
  names(text_color_vec) <- plot_data$id


  # --- 3. Create the Doughnut Chart ---
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = 2, y = percentage, fill = id)) +
    ggplot2::geom_col(width = 1, color = "white") +
    ggplot2::geom_text(
      ggplot2::aes(y = pos, label = value_label),
      color = "white",
      size = 4 + name_size_mod,
      fontface = "bold"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(y = pos, label = name_label, hjust = label_hjust, color = id),
      x = 2.8,
      size = 3.5 + name_size_mod
    ) +
    ggplot2::scale_fill_manual(values = fill_vec) +
    ggplot2::scale_color_manual(values = text_color_vec) +
    ggplot2::guides(fill = "none", color = "none") +
    ggplot2::coord_polar(theta = "y", start = 0, clip = "off") +
    ggplot2::xlim(c(0.5, 3.5)) +
    ggplot2::theme_void() +
    ggplot2::labs(title = title, caption = caption_text) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        vjust = -2 + title_vjust_mod,
        size = 18 + title_size_mod,
        face = title_face,
        color = title_color
      ),
      plot.caption = ggplot2::element_text(
        hjust = 1,
        size = 8,
        face = "italic",
        margin = ggplot2::margin(t = 10)
      ),
      plot.margin = ggplot2::margin(20, 20, 20, 20)
    )

  # --- 4. Save and/or Show Plot ---
  if (save_plot) {
    ggplot2::ggsave(
      filename = output_path,
      plot = p,
      dpi = output_dpi,
      width = output_width,
      height = output_height,
      units = "in"
    )
    message("Plot saved to: ", output_path)
  }

  if (show_plot) {
    print(p)
  }

  return(invisible(p))
}

