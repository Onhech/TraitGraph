# --- Doughnut Chart Votes/Opinion Data ---
# Creates a doughnut chart to visualize proportions, building on the logic
# refined in the testing script.

# Required libraries: dplyr, ggplot2, rlang, stringr, ggtext

#' Create and Save a Doughnut Chart for Votes/Opinion Data
#'
#' @description
#' This function takes a dataset and creates a doughnut chart to represent the
#' proportional breakdown of a numeric column. It includes robust logic for
#' label placement, coloring, and dynamic title formatting to ensure readability.
#' It also automatically generates a legend in the caption for categories
#' that fall below a certain threshold.
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
#'   smaller than this value will be replaced by an asterisk. Defaults to 5.
#' @param outer_label_threshold A numeric value (0-100). Name labels for slices smaller
#'   than this value will be hidden. Defaults to the same value as `inner_label_threshold`.
#' @param footnote_text_width The acceptable character width of each line in the footnote. Defaults to 50.
#' @param footnote_size The font size for the footnote text. Defaults to 12.
#' @param footnote_hjust The horizontal justification of the footnote (0-1). Defaults to 0.5 (center).
#' @param footnote_vjust The vertical justification of the footnote. Defaults to 0.
#' @param footnote_lineheight The line height for multi-line footnotes. Defaults to 1.3.
#' @param footnote_margin_t Numeric. The top margin (space) above the footnote. Defaults to 15. Smaller values move the footnote up.
#' @param hole_size Numeric value between 0 and 1 to control the size of the doughnut's hole. Defaults to 0.5.
#' @param name_size_mod A multiplier to adjust the final label font size (e.g., 1 = default, 1.2 = 20% larger).
#' @param title_size_mod A numeric value to add or subtract from the dynamically calculated title font size.
#' @param title_vjust_mod A numeric value to add or subtract from the dynamically calculated title's vertical adjustment.
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
                              title_color = "grey30",
                              sort_order = "asc",
                              inner_label_threshold = 5,
                              outer_label_threshold = NULL,
                              footnote_text_width = 65,
                              footnote_size = 12,
                              footnote_hjust = 0,
                              footnote_vjust = 1,
                              footnote_lineheight = 1.3,
                              footnote_margin_t = -42,
                              hole_size = 0.5,
                              name_size_mod = 1,
                              title_size_mod = 1,
                              title_vjust_mod = 0,
                              output_path = "doughnut_plot.png",
                              output_width = 7,
                              output_height = 7,
                              output_dpi = 300,
                              save_plot = TRUE,
                              show_plot = TRUE) {

  # --- 0. Check for necessary packages ---
  if (!requireNamespace("ggtext", quietly = TRUE)) {
    stop("Package 'ggtext' is required for the formatted caption. Please install it using install.packages('ggtext').", call. = FALSE)
  }


  # --- 1. Input Validation & Data Prep ---
  if (is.null(outer_label_threshold)) {
    outer_label_threshold <- inner_label_threshold
  }

  if (!show_title) { title <- NULL }
  else if (is.null(title)) { title <- column_name }

  # Get dynamic title properties if a title exists
  if (!is.null(title)) {
    title_params <- get_dynamic_title_doughnut(title)
  } else {
    # Set reasonable defaults if no title is to be shown
    title_params <- list(text = NULL, size = 18, vjust = -2)
  }


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
    { if (sort_order == "desc") dplyr::arrange(., dplyr::desc(value), id) else dplyr::arrange(., value, id) } %>%
    dplyr::mutate(id = factor(id, levels = unique(id))) %>%
    dplyr::mutate(
      pos = rev(cumsum(rev(percentage)) - 0.5 * rev(percentage)),
      name_label = ifelse(percentage > outer_label_threshold, as.character(id), ""),
      value_label = ifelse(percentage > inner_label_threshold, paste0(round(percentage, 0), "%"), "*"),
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

  # --- 2a. Generate Caption/Legend for Small Slices ---
  small_slices <- plot_data %>%
    dplyr::filter(percentage <= outer_label_threshold) %>%
    # *** CORRECTED: Sort by percentage (desc), then by name (asc) to break ties ***
    dplyr::arrange(dplyr::desc(percentage), id)

  if (nrow(small_slices) > 0) {
    # Define the intro text and the indent string for hanging indents
    plain_intro <- paste0("* ", outer_label_threshold, "% or less: ")
    html_intro <- paste0("<b>*</b><i>", outer_label_threshold, "% or less: </i>")
    indent_str <- paste(rep("&nbsp;", nchar(plain_intro)), collapse = "")


    # Build the caption line-by-line to wrap text without breaking HTML tags
    caption_lines <- c()
    current_line <- html_intro
    current_len <- nchar(plain_intro)

    for (i in 1:nrow(small_slices)) {
      name <- as.character(small_slices$id[i])
      color <- small_slices$label_color[i]
      # Add a comma if it's not the first name on the line
      prefix <- if (current_len > nchar(plain_intro)) ", " else ""
      formatted_name <- paste0("<b style='color:", color, ";'>", name, "</b>")
      name_len <- nchar(name)

      if ((current_len + name_len + nchar(prefix)) > footnote_text_width) {
        caption_lines <- c(caption_lines, current_line)
        current_line <- paste0(indent_str, formatted_name)
        current_len <- nchar(plain_intro) + name_len # Reset length for new line
      } else {
        current_line <- paste0(current_line, prefix, formatted_name)
        current_len <- current_len + name_len + nchar(prefix)
      }
    }
    caption_lines <- c(caption_lines, current_line) # Add the last line
    caption_text <- paste(caption_lines, collapse = "<br>")

  } else {
    caption_text <- NULL
  }


  # Create separate named vectors for fill and text colors
  fill_vec <- plot_data$color
  names(fill_vec) <- plot_data$id

  text_color_vec <- plot_data$label_color
  names(text_color_vec) <- plot_data$id


  # --- 3. Create the Doughnut Chart ---
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = 2, y = percentage, fill = id)) +
    ggplot2::geom_col(width = 1, color = "white") +
    # Add inner labels for DARK backgrounds (white text)
    ggplot2::geom_text(
      data = . %>% dplyr::filter(!is_light),
      ggplot2::aes(y = pos, label = value_label),
      color = "white",
      size = 5.25 * name_size_mod,
      fontface = "bold"
    ) +
    # Add inner labels for LIGHT backgrounds (grey text)
    ggplot2::geom_text(
      data = . %>% dplyr::filter(is_light),
      ggplot2::aes(y = pos, label = value_label),
      color = "grey40",
      size = 5.25 * name_size_mod,
      fontface = "bold"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(y = pos, label = name_label, hjust = label_hjust, color = id),
      x = 2.8,
      size = 5.25 * name_size_mod
    ) +
    ggplot2::scale_fill_manual(values = fill_vec) +
    ggplot2::scale_color_manual(values = text_color_vec) +
    ggplot2::guides(fill = "none", color = "none") +
    ggplot2::coord_polar(theta = "y", start = 0, clip = "off") +
    ggplot2::xlim(c(0.5, 3.5)) +
    ggplot2::theme_void() +
    ggplot2::labs(title = title_params$text, caption = caption_text) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        vjust = title_params$vjust + title_vjust_mod,
        size = title_params$size * title_size_mod,
        face = title_face,
        color = title_color,
        lineheight = 0.9
      ),
      plot.caption = ggtext::element_markdown(
        hjust = footnote_hjust,
        vjust = footnote_vjust,
        size = footnote_size,
        lineheight = footnote_lineheight,
        color = "grey40",
        margin = ggplot2::margin(t = footnote_margin_t)
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

