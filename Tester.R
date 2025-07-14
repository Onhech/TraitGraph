# -----------------------------------------------------------------------------
# TraitGraph Package - Development & Testing Script
# -----------------------------------------------------------------------------
# This script is for managing, rebuilding, and testing the TraitGraph package.
# It is not part of the package itself but a helper for the developer.
# -----------------------------------------------------------------------------

# --- 1. SETUP ---

# Load devtools for package management
# install.packages("devtools") # Run once if not installed
library(devtools)

# Set the working directory to the package's root folder.
# IMPORTANT: This needs to be the correct path on your machine.
# Using RStudio Projects (the .Rproj file) is the best way to manage this
# automatically and avoid hardcoding the path.
setwd('/Users/lyndenjensen/Library/CloudStorage/GoogleDrive-lynden.jensen@gmail.com/My Drive/Work/PsychologyReports/Product/Reports/TraitGraph')

# Create a directory for example plots if it doesn't already exist
if (!dir.exists("ExamplePlots")) {
  dir.create("ExamplePlots")
}


# --- 2. PACKAGE WORKFLOWS ---

# --- Workflow A: Fast, Interactive Development (RECOMMENDED) ---
# Use this workflow 99% of the time. It loads all your functions directly
# into memory, which is extremely fast for testing changes.
# Simply run this line after you save a change in any of your R/ files.
devtools::load_all()


# --- Workflow B: Full Rebuild and Install ---
# Use this workflow occasionally, especially before pushing to GitHub, to ensure
# the complete package builds correctly from start to finish.
# remove.packages("TraitGraph") # Optional: for a completely fresh install
# devtools::document()          # Update documentation and NAMESPACE
# devtools::install()           # Build and install the package locally
# library(TraitGraph)            # Load the newly installed package


# --- Workflow C: Test GitHub Installation ---
# Use this to simulate a new user installing from your public repository.
# It's best to run this in a completely fresh R session.
# remove.packages("TraitGraph") # Ensure no local version is present
# remotes::install_github("Onhech/TraitGraph")
# library(TraitGraph)


# --- 3. CREATE SAMPLE DATA ---

# Create one master sample data frame for testing.
sample_data <- data.frame(
  names = c("Heather", "Taylor", "Edison", "Nathaniel", "Muhammed", "Timothy", "Alan", "Alexander", "Bartholomew", "Christopher", "LongNameMcGee", "NameHere", "Lynden"),
  favourite_color = c("#FF0000", "#0000FF", "#008000", "white", "#800080", "#FFFF00", "#f9f1f1", "#00FFFF", "#008080", "#FF00FF", "#00FF00", "#A52A2A", "#FFA500"),
  rankedQ_1 = c(9, 0, 0, 1, 0, 0, 1, 0, 0, 7, 4, 0, 3),
  Extroversion = c(75.2, 52.1, 82.6, 22.5, 67.3, 48.9, 59.5, 72.8, 39.6, 85.4, 41.2, 64.7, 99.1)
)


# --- 4. FUNCTION TESTING ---
# After running `devtools::load_all()`, you can run these calls to test.

# --- Trait Example ---
# This function now handles the group average internally, so we use the raw data.
TG_trait(
  dataset = sample_data,
  column_name = "Extroversion",
  save_plot = F,
  show_plot = T
)

# --- Ranked Question Example ---
TG_voting(
  dataset = sample_data,
  column_name = "rankedQ_1",
  title = "Who would you choose as a partner for the impending Zombie Apocalypse?",
  output_path = "ExamplePlots/rank_plot_1.jpg"
)

# --- Jungian Dichotomy Example ---
TG_jung(
  dataset = sample_data,
  column_name = "Extroversion",
  title = "Team Extroversion Profile",
  label_top = "Introversion",
  label_bottom = "Extroversion",
  output_path = "ExamplePlots/jung_plot_1.jpg"
)

message("Testing script finished.")