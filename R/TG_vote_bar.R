# ----------------------------------------------------------------------------
# FILE: R/TG_vote_bar.R
# DESCRIPTION: Simple horizontal vote bar chart with tie handling and footnotes.
# AUTHOR: Lynden Jensen
# LAST EDIT: 2025-02-09
# DEPENDS: ggplot2, dplyr, scales
# STATUS: ACTIVE
# ----------------------------------------------------------------------------

#' Create a Simple Vote Bar Chart
#'
#' @description
#' Generates a horizontal bar chart for a single vote question, showing up to
#' five unique vote counts with tie-aware labels and optional footnotes.
#'
#' @param dataset A data frame containing the vote data.
#' @param vote_column Column name containing vote counts for the question.
#' @param name Column name containing respondent names. Defaults to "name".
#' @param palette A list with `high` and `mid` hex codes used for the bar gradient.
#'   `mid` represents 0 votes and `high` represents the maximum votes. Defaults to
#'   `list(high = "#1F7A1F", mid = "#E6E6E6")`.
#' @param max_bars Maximum number of unique vote counts to display. Defaults to 5.
#' @param show_title Logical; if TRUE, include the title in the plot.
#' @param title Optional title string.
#' @param bar_width Numeric; width of the bars (0-1). Smaller values increase spacing.
#'   Defaults to 0.6.
#' @param label_size_single Numeric; label size for single-name rows. Defaults to 5.
#' @param label_size_tie2 Numeric; label size for two-way ties. Defaults to 5.
#' @param label_size_tien Numeric; label size for 3+ way ties. Defaults to 5.
#' @param label_color Hex color for labels. Defaults to bar colors when NULL.
#' @param tie_markers Character vector of footnote markers to cycle through.
#'   Defaults to `c("†", "‡", "§", "¶", "*")`.
#' @param font_family Font family for labels and callouts. Defaults to "sans".
#' @param marker_font_family Font family for tie markers and footnote markers.
#'   Defaults to "Arial Unicode MS".
#' @param footnote_marker_gap Numeric; horizontal gap between marker and text (vote units).
#'   Defaults to `0.06 * max_votes`.
#' @param marker_x_nudge Numeric; horizontal nudge for tie markers relative to the
#'   label position (in vote units). Defaults to `0.015 * max_votes`.
#' @param marker_y_nudge Numeric; vertical nudge for tie markers relative to the
#'   label position (in row units). Defaults to 0.25.
#' @param marker_size Numeric; font size for tie markers. Defaults to 3.2.
#' @param footnote_x_nudge Numeric; horizontal nudge for footnotes (vote units).
#'   Defaults to `-0.26 * max_votes`.
#' @param footnote_y_nudge Numeric; vertical nudge for footnotes (row units).
#'   Defaults to 0.3.
#' @param callout_x_nudge Numeric; horizontal nudge for vote callouts (vote units).
#'   Defaults to 0.2.
#' @param callout_y_nudge Numeric; vertical nudge for vote callouts (row units).
#'   Defaults to 0.
#' @param callout_size Numeric; font size for vote callouts. Defaults to 5.
#' @param footnote_size Numeric; font size for footnotes. Defaults to 3.5.
#' @param footnote_color Hex color for footnotes. Defaults to "#3C3C3C".
#' @param output_path File path for saving the plot.
#' @param output_width Output width in inches.
#' @param output_height Output height in inches.
#' @param output_dpi Output resolution.
#' @param save_plot Logical. If TRUE, save the plot.
#' @param show_plot Logical. If TRUE, print the plot.
#'
#' @return Invisibly returns a list with the ggplot and footnote strings.
#' @export
#'
#' @examples
#' \dontrun{
#' TG_vote_bar(
#'   dataset = group_dataset,
#'   vote_column = "vote_q01",
#'   palette = list(high = "#2F7D32", mid = "#E6F4E6"),
#'   show_title = FALSE
#' )
#' }
TG_vote_bar <- function(
    dataset,
    vote_column,
    name = "name",
    palette = list(high = "#1F7A1F", mid = "#E6E6E6"),
    max_bars = 5,
    show_title = FALSE,
    title = NULL,
    bar_width = 0.6,
    label_size_single = 5,
    label_size_tie2 = 5,
    label_size_tien = 5,
    label_color = NULL,
    tie_markers = c("†", "‡", "§", "¶", "*"),
    font_family = "sans",
    marker_font_family = "Arial Unicode MS",
    footnote_marker_gap = NULL,
    marker_x_nudge = NULL,
    marker_y_nudge = 0.25,
    marker_size = 3.2,
    footnote_x_nudge = NULL,
    footnote_y_nudge = 0.3,
    callout_x_nudge = 0.2,
    callout_y_nudge = 0,
    callout_size = 5,
    footnote_size = 3.5,
    footnote_color = "#3C3C3C",
    output_path = "vote_bar.jpg",
    output_width = 7,
    output_height = 5,
    output_dpi = 300,
    save_plot = FALSE,
    show_plot = TRUE
) {

  if (!(vote_column %in% names(dataset))) {
    stop("vote_column not found in dataset: ", vote_column)
  }
  if (!(name %in% names(dataset))) {
    stop("name column not found in dataset: ", name)
  }

  high <- palette$high
  mid <- palette$mid
  if (is.null(high) || is.na(high) || !nzchar(high)) high <- "#1F7A1F"
  if (is.null(mid) || is.na(mid) || !nzchar(mid)) mid <- "#E6E6E6"

  data <- dataset %>%
    dplyr::select(
      person = !!rlang::sym(name),
      votes = !!rlang::sym(vote_column)
    ) %>%
    dplyr::mutate(
      person = as.character(person),
      votes = as.numeric(votes)
    ) %>%
    dplyr::filter(!is.na(votes) & votes > 0)

  if (nrow(data) == 0) {
    return(invisible(list(plot = NULL, footnotes = character())))
  }

  counts <- data %>%
    dplyr::group_by(person) %>%
    dplyr::summarise(votes = max(votes, na.rm = TRUE), .groups = "drop")

  unique_counts <- sort(unique(counts$votes), decreasing = TRUE)
  top_counts <- unique_counts[seq_len(min(length(unique_counts), max_bars))]
  top <- counts %>% dplyr::filter(votes %in% top_counts)

  # Build labels per count
  if (is.null(tie_markers) || length(tie_markers) == 0) {
    tie_markers <- c("†", "‡", "§", "¶", "*")
  }
  tie_symbol_index <- 1

  label_rows <- lapply(top_counts, function(cnt) {
    names_at <- sort(top$person[top$votes == cnt])
    tie_note <- NA_character_
    tie_marker <- NA_character_

    if (length(names_at) == 1) {
      label <- names_at
      label_type <- "single"
    } else if (length(names_at) == 2) {
      names_at <- names_at[order(-nchar(names_at), names_at)]
      label <- paste0(names_at[1], "
& ", names_at[2])
      label_type <- "tie2"
    } else {
      marker <- tie_markers[min(tie_symbol_index, length(tie_markers))]
      tie_marker <- marker
      tie_symbol_index <<- tie_symbol_index + 1
      label <- paste0(length(names_at), "-Way Tie")
      tie_note <- paste0(marker, paste(names_at, collapse = ", "))
      label_type <- "tien"
    }
    data.frame(
      label = label,
      label_type = label_type,
      tie_marker = tie_marker,
      votes = cnt,
      tie_note = tie_note,
      stringsAsFactors = FALSE
    )
  })
  label_df <- dplyr::bind_rows(label_rows)

  footnotes <- c()
  tie_notes <- label_df$tie_note[!is.na(label_df$tie_note)]
  if (length(tie_notes) > 0) {
    footnotes <- c(footnotes, tie_notes)
  }

  honorable <- counts %>%
    dplyr::filter(!(votes %in% top_counts) & votes > 0) %>%
    dplyr::arrange(dplyr::desc(votes), person)

  if (nrow(honorable) > 0) {
    footnotes <- c(footnotes, paste0("Honourable mentions: ", paste(honorable$person, collapse = ", ")))
  }

  label_df <- label_df %>%
    dplyr::arrange(dplyr::desc(votes)) %>%
    dplyr::mutate(
      order_id = factor(label, levels = label),
      label_size = dplyr::case_when(
        label_type == "single" ~ label_size_single,
        label_type == "tie2" ~ label_size_tie2,
        label_type == "tien" ~ label_size_tien,
        TRUE ~ label_size_single
      )
    )

  max_votes <- max(label_df$votes)
  label_df$label_x <- -0.06 * max_votes
  if (is.null(marker_x_nudge)) {
    marker_x_nudge <- 0.015 * max_votes
  }
  label_df$marker_x <- label_df$label_x + marker_x_nudge
  label_df <- label_df %>%
    dplyr::mutate(
      fill_color = grDevices::colorRamp(c(mid, high))(votes / max_votes),
      fill_color = grDevices::rgb(fill_color[, 1], fill_color[, 2], fill_color[, 3], maxColorValue = 255)
    )

  label_df <- label_df %>%
    dplyr::mutate(label_color = if (is.null(label_color)) fill_color else label_color)

  plot_title <- if (isTRUE(show_title)) title else NULL

  p <- ggplot2::ggplot(label_df, ggplot2::aes(y = order_id, x = votes)) +
    ggplot2::geom_col(fill = label_df$fill_color, width = bar_width) +
    ggplot2::geom_text(
      ggplot2::aes(label = votes),
      hjust = -0.4,
      nudge_x = callout_x_nudge,
      nudge_y = callout_y_nudge,
      size = callout_size,
      color = label_df$fill_color,
      family = font_family
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = label_x, label = label, size = label_size, color = label_color),
      hjust = 1,
      lineheight = 1.1,
      family = font_family
    ) +
    ggplot2::geom_text(
      data = label_df[!is.na(label_df$tie_marker), ],
      ggplot2::aes(x = marker_x, y = order_id, label = tie_marker, color = fill_color),
      hjust = 0,
      vjust = 0,
      nudge_y = marker_y_nudge,
      size = marker_size,
      family = marker_font_family,
      show.legend = FALSE
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 18, face = "bold", hjust = 0.5, family = font_family),
      plot.margin = ggplot2::unit(c(0.5, 0.5, 1.2, 3.0), "cm"),
      text = ggplot2::element_text(family = font_family)
    ) +
    ggplot2::scale_x_continuous(limits = c(-max_votes * 0.3, max_votes * 1.15)) +
    ggplot2::scale_y_discrete(limits = rev(levels(label_df$order_id)), expand = ggplot2::expansion(add = c(0.6, 1))) +
    ggplot2::scale_color_identity(guide = "none") +
    ggplot2::scale_size_identity() +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::ggtitle(plot_title)

  if (length(footnotes) > 0) {
    if (is.null(footnote_x_nudge)) {
      footnote_x_nudge <- -0.26 * max_votes
    }
    footnote_df <- data.frame(
      line = footnotes,
      idx = seq_along(footnotes),
      stringsAsFactors = FALSE
    )
    footnote_df$marker <- substr(footnote_df$line, 1, 1)
    footnote_df$text <- trimws(sub("^.", "", footnote_df$line))
    footnote_df$y <- footnote_y_nudge - (footnote_df$idx - 1) * 0.25
    footnote_df$marker_x <- footnote_x_nudge
    if (is.null(footnote_marker_gap)) {
      footnote_marker_gap <- 0.06 * max_votes
    }
    footnote_df$text_x <- footnote_x_nudge + footnote_marker_gap

    p <- p + ggplot2::geom_text(
      data = footnote_df,
      ggplot2::aes(x = marker_x, y = y, label = marker),
      hjust = 0,
      vjust = 0,
      size = footnote_size,
      color = footnote_color,
      family = marker_font_family
    ) +
      ggplot2::geom_text(
        data = footnote_df,
        ggplot2::aes(x = text_x, y = y, label = text),
        hjust = 0,
        vjust = 0,
        size = footnote_size,
        color = footnote_color,
        family = font_family
      )
  }

  if (save_plot) {
    ggplot2::ggsave(filename = output_path, plot = p, dpi = output_dpi, width = output_width, height = output_height, units = "in")
  }
  if (show_plot) {
    print(p)
  }

  return(invisible(list(plot = p, footnotes = footnotes)))
}
