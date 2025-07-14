# -----------------------------------------------------------------------------
# TraitGraph Package - Development & Testing Script
# -----------------------------------------------------------------------------
# This script is for managing, rebuilding, and testing the TraitGraph package.
# It is not part of the package itself but a helper for the developer.
# -----------------------------------------------------------------------------

# --- 1. SETUP ---

# Load devtools for package management
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


# --- 2. PACKAGE WORKFLOW ---

# --- Option A: Fast, Interactive Workflow (Recommended for most changes) ---
# This loads all package functions directly into memory without a full install.
# It's the fastest way to test changes as you make them.
# devtools::load_all()


# --- Option B: Full Rebuild Workflow (Use before pushing to GitHub) ---
# Uncomment the lines below to do a full documentation and installation cycle.
# This is slower but is the best way to test the package as a user would.
remove.packages(TraitGraph)
library(TraitGraph) 
?TraitGraph
devtools::document()          # Update documentation and NAMESPACE
devtools::install()           # Build and install the package locally
library(TraitGraph)            # Load the newly installed package

# --- 3. CREATE SAMPLE DATA ---

# Create one master sample data frame for testing.
# Note: All columns must have the same number of entries.
sample_data <- data.frame(
  names = c("Heather", "Taylor", "Edison", "Nathaniel", "Muhammed", "Timothy", "Alan", "Alexander", "Bartholomew", "Christopher", "LongNameMcGee", "NameHere", "Lynden"),
  favourite_color = c("#FF0000", "#0000FF", "#008000", "white", "#800080", "#FFFF00", "#f9f1f1", "#00FFFF", "#008080", "#FF00FF", "#00FF00", "#A52A2A", "#FFA500"),
  rankedQ_1 = c(9, 0, 0, 1, 0, 0, 1, 0, 0, 7, 4, 0, 3),
  Extroversion = c(75.2, 52.1, 82.6, 22.5, 67.3, 48.9, 59.5, 72.8, 39.6, 85.4, 41.2, 64.7, 99.1)
)

# --- 4. FUNCTION TESTING ---

# --- Trait Example ---
# This function now handles the group average internally, so we use the raw data.
TG_trait(
  dataset = sample_data,
  column_name = "Extroversion",y_outer_limit = 240
    
)


# --- Ranked Question Example ---
TG_votes(
  dataset = sample_data,
  column_name = "rankedQ_1",
  title = "Who would you choose as a partner for the impending Zombie Apocalypse?",
  output_path = "ExamplePlots/votes_plot_1.jpg"
)

# --- Jungian Dichotomy Example ---
TG_jung(
  dataset = sample_data,
  column_name = "Extroversion",
  label_top = "Introversion",
  label_bottom = "Extroversion",
  output_path = "ExamplePlots/jung_plot_1.jpg"
)