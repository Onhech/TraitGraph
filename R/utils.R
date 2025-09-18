# -----------------------------------------------------------------------------
# FILE: R/utils.R
# STATUS: FINAL
# -----------------------------------------------------------------------------
# This script contains internal helper functions used by the main plotting
# functions in the package. These functions are not exported for users.
# -----------------------------------------------------------------------------


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Color Formatter Function ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#' Check if a color is light
#'
#' Calculates the perceived luminance of a color and compares it to a threshold.
#'
#' @param hex A character vector of hex color codes or R color names.
#' @param threshold A numeric value between 0 and 1. Colors with luminance above this value are considered light. Defaults to 0.85.
#' @return A logical vector, \code{TRUE} for each color that is considered light.
#' @export
#' @examples
#' is_color_light("#fefb41") # Bright yellow, should be TRUE
#' is_color_light("blue") # Dark color, should be FALSE
is_color_light <- function(hex, threshold = 0.85) {
  # Handle color names by converting them to hex first
  hex_codes <- sapply(hex, function(x) {
    tryCatch(grDevices::rgb(t(grDevices::col2rgb(x))), error = function(e) x)
  })
  rgb_matrix <- grDevices::col2rgb(hex_codes) / 255
  luminance <- 0.299 * rgb_matrix[1, ] + 0.587 * rgb_matrix[2, ] + 0.114 * rgb_matrix[3, ]
  luminance > threshold
}

#' Darken a color
#'
#' Reduces the brightness of a color by a given factor.
#'
#' @param hex A character vector of hex color codes or R color names.
#' @param factor A number between 0 and 1 for darkening (e.g., 0.5 makes the color 50% darker).
#' @return A character vector of the darkened hex color codes.
#' @export
#' @examples
#' darken_color("yellow", factor = 0.7)
darken_color <- function(hex, factor = 0.5) {
  rgb_matrix <- grDevices::col2rgb(hex)
  dark_rgb <- rgb_matrix * factor
  dark_rgb <- pmax(0, pmin(dark_rgb, 255))
  grDevices::rgb(dark_rgb[1], dark_rgb[2], dark_rgb[3], maxColorValue = 255)
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Dynamic Text Function ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#' Adjust title properties based on text length
#'
#' Dynamically calculates the appropriate font size, wrap width, and vertical
#' justification for a main title based on its character count.
#'
#' @param title_text The character string for the title.
#' @return A list containing the wrapped text (`text`), the calculated font size (`size`), and the vertical justification value (`vjust`).
#' @export
#' @examples
#' get_dynamic_title("A very long title that will need to be wrapped and resized.")

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


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Dynamic Text Function for Voting   ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#' Adjust voting title properties based on text length
#'
#' A specialized version of get_dynamic_title tailored to the larger font sizes
#' and different layout requirements of the voting graphs.
#'
#' @param title_text The character string for the title.
#' @return A list containing the wrapped text (`text`), the calculated font size (`size`), and the vertical justification value (`vjust`).
#' @export
#' @examples
#' get_dynamic_title_votes("Who is the most likely to lead a Martian colony?")

  get_dynamic_title_votes <- function(title_text) {
    title_length <- nchar(title_text)

    # Determine font size, wrap width, and vertical justification based on character length
  # Largest font, 1 line
    if (title_length <= 24) {
      font_size <- 35
      wrap_width <- 24
      final_vjust <- -12.5
  # Smaller font, 1 lines
    } else if (title_length <= 28) {
      font_size <- 28
      wrap_width <- 28
      final_vjust <- -16
  # Smaller font, 2 lines
    } else if (title_length <= 52) {
      font_size <- 28
      wrap_width <- 26
      final_vjust <- -15.5
  # Small font, 2 lines
    } else if (title_length <= 70) {
      font_size <- 24
      wrap_width <- 35
      final_vjust <- -18
  # Small font, 3 lines
    } else if (title_length <= 105) {
      font_size <- 24
      wrap_width <- 35
      final_vjust <- -17
  # Smallest font, 3 lines
    } else if (title_length <= 120) {
      font_size <- 20
      wrap_width <- 40
      final_vjust <- -21
  # Smallest font, 4 lines
    } else {
      font_size <- 20
      wrap_width <- 40
      final_vjust <- -20
    }

    # Wrap the text
    wrapped_title <- stringr::str_wrap(title_text, width = wrap_width)

    return(list(text = wrapped_title, size = font_size, vjust = final_vjust))
  }



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Dynamic Text Function for Voting   ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#' Adjust voting title properties based on text length
#'
#' A specialized version of get_dynamic_title tailored to the larger font sizes
#' and different layout requirements of the voting graphs.
#'
#' @param title_text The character string for the title.
#' @return A list containing the wrapped text (`text`), the calculated font size (`size`), and the vertical justification value (`vjust`).
#' @export
#' @examples
#' get_dynamic_title_votes("Who is the most likely to lead a Martian colony?")

get_dynamic_title_doughnut <- function(title_text) {
  title_length <- nchar(title_text)

  # Determine font size, wrap width, and vertical justification based on character length
  # Largest font, 1 line
  if (title_length <= 24) {
    font_size <- 35
    wrap_width <- 24
    final_vjust <- -3.5
    # Smaller font, 1 lines
  } else if (title_length <= 28) {
    font_size <- 28
    wrap_width <- 28
    final_vjust <- -8
    # Smaller font, 2 lines
  } else if (title_length <= 52) {
    font_size <- 28
    wrap_width <- 26
    final_vjust <- -7.5
    # Small font, 2 lines
  } else if (title_length <= 70) {
    font_size <- 24
    wrap_width <- 35
    final_vjust <- -10
    # Small font, 3 lines
  } else if (title_length <= 105) {
    font_size <- 24
    wrap_width <- 35
    final_vjust <- -9
    # Smallest font, 3 lines
  } else if (title_length <= 120) {
    font_size <- 20
    wrap_width <- 40
    final_vjust <- -13
    # Smallest font, 4 lines
  } else {
    font_size <- 20
    wrap_width <- 40
    final_vjust <- -12
  }

  # Wrap the text
  wrapped_title <- stringr::str_wrap(title_text, width = wrap_width)

  return(list(text = wrapped_title, size = font_size, vjust = final_vjust))
}


