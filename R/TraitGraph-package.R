#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom dplyr %>%
#' @importFrom geomtextpath geom_textpath
## usethis namespace: end
NULL

#' TraitGraph: Circular Visualizations for Group Psychology Data
#'
#' The TraitGraph package provides a suite of functions to create stylish,
#' publication-quality circular charts for visualizing psychological traits,
#' opinions, and dichotomous scores within a group.
#'
#' @section Main Functions:
#' The three main functions in this package are:
#' \itemize{
#'   \item \code{\link{TG_voting}}: For visualizing the results of ranked-choice or voting questions.
#'   \item \code{\link{TG_trait}}: For displaying percentile scores and comparing them against a group average.
#'   \item \code{\link{TG_jung}}: For creating charts for dichotomous traits (e.g., Introversion vs. Extroversion) with unique aesthetics.
#'   \item \code{\\link{TG_achievements}}: For calculating and allocating achievement badges from cohort data.
#' }
#'
#' @section Examples:
#' \preformatted{
#' # 1. Create Sample Data
#' sample_data <- data.frame(
#'   names = c("Heather", "Taylor", "Edison", "Nathaniel"),
#'   favourite_color = c("#FF0000", "#0000FF", "#008000", "white"),
#'   zombie_apocalypse_partner = c(9, 1, 7, 4),
#'   Extroversion = c(75, 52, 83, 23)
#' )
#'
#' # 2. Run the TG_voting() function
#' TG_voting(
#'   dataset = sample_data,
#'   column_name = "zombie_apocalypse_partner",
#'   title = "Vote: Zombie Apocalypse Partner"
#' )
#'
#' # 3. Run the TG_trait() function
#' # This function automatically adds the "Group Average"
#' TG_trait(
#'   dataset = sample_data,
#'   column_name = "Extroversion"
#' )
#'
#' # 4. Run the TG_jung() function
#' TG_jung(
#'   dataset = sample_data,
#'   column_name = "Extroversion",
#'   title = "Team Extroversion Profile",
#'   label_top = "Introversion",
#'   label_bottom = "Extroversion"
#' )
#' }
#'
#' @section Data Preparation Best Practices:
#' \itemize{
#'  \item Your input data frame should have one row per person.
#'  \item Ensure the column specified in the `name` argument contains unique names for each individual.
#'  \item The `color` column should contain valid R color names or hex codes (e.g., "blue", "#FFC0CB").
#'  \item For `TG_trait` and `TG_jung`, do NOT include a "Group Average" row in your input data; the function handles this automatically.
#' }
#'
#' @docType package
#' @name TraitGraph
