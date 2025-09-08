# -----------------------------------------------------------------------------
# FILE: R/utils.R
# STATUS: FINAL
# -----------------------------------------------------------------------------
# This script contains internal helper functions used by the main plotting
# functions in the package. These functions are not exported for users.
# -----------------------------------------------------------------------------

#' Check if a color is light
#' @param hex A vector of hex color codes.
#' @return A logical vector, TRUE if the color is considered light.
is_color_light <- function(hex) {
  # Handle color names by converting them to hex first
  hex_codes <- sapply(hex, function(x) {
    tryCatch(grDevices::rgb(t(grDevices::col2rgb(x))), error = function(e) x)
  })
  rgb_matrix <- grDevices::col2rgb(hex_codes) / 255
  luminance <- 0.299 * rgb_matrix[1, ] + 0.587 * rgb_matrix[2, ] + 0.114 * rgb_matrix[3, ]
  luminance > 0.95 # Threshold for "too light"
}


  #' Darken colors
  #' @param hex A hex color code.
  #' @param factor A number between 0 and 1 for darkening.
  #' @return A darkened hex color code.
  darken_color <- function(hex, factor = 0.5) {
    rgb_matrix <- grDevices::col2rgb(hex) / 255
    dark_rgb <- rgb_matrix * factor
    dark_rgb <- pmax(0, pmin(dark_rgb, 1))
    grDevices::rgb(dark_rgb[1], dark_rgb[2], dark_rgb[3], maxColorValue = 1)
  }

  #' Dynamically adjust title properties based on text length
  #' @param title_text The raw string for the title.
  #' @return A list containing the wrapped text, calculated size, and vjust.
  get_dynamic_title <- function(title_text) {
    title_length <- nchar(title_text)

    # Determine font size, wrap width, and vertical justification based on character length
    if (title_length <= 40) {
      font_size <- 23
      wrap_width <- 40
      final_vjust <- -19
    } else if (title_length <= 85) {
      font_size <- 19
      wrap_width <- 42
      final_vjust <- -23
    } else if (title_length <= 120) {
      font_size <- 16
      wrap_width <- 60
      final_vjust <- -28
    } else {
      font_size <- 12
      wrap_width <- 60
      final_vjust <- -36
    }

    # Wrap the text
    wrapped_title <- stringr::str_wrap(title_text, width = wrap_width)

    return(list(text = wrapped_title, size = font_size, vjust = final_vjust))
  }

