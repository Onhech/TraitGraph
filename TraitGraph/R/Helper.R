# -----------------------------------------------------------------------------
# TraitGraph Package - Demonstration Script
# -----------------------------------------------------------------------------
# This script showcases the main functions of the TraitGraph package using a
# randomly generated sample dataset.
# -----------------------------------------------------------------------------

# --- 1. INSTALLATION (Run this once) ---

# The 'remotes' package is needed to install packages from GitHub.
# If you don't have it, uncomment and run the line below.
# install.packages("remotes")

# Install the TraitGraph package directly from GitHub.
remove.packages(TraitGraph)
remotes::install_github("Onhech/GroupReport/TraitGraph")

https://github.com/Onhech/Group-Personality-Report
# --- 2. SETUP ---

# Load the TraitGraph package
library(TraitGraph)

# Create a directory for the output plots if it doesn't already exist
if (!dir.exists("ExamplePlots")) {
  dir.create("ExamplePlots")
}


# --- 3. GENERATE REALISTIC SAMPLE DATA ---

# Set a seed for reproducibility, so the "random" data is the same every time
set.seed(42)

# Define the participants
participant_names <- c(
  "Heather", "Taylor", "Edison", "Nathaniel", "Muhammed", "Timothy",
  "Alan", "Alexander", "Bartholomew", "Christopher", "LongNameMcGee",
  "NameHere", "Lynden", "Sarah", "Michael"
)
num_participants <- length(participant_names)

# Create the sample data frame
sample_data <- data.frame(
  names = participant_names,
  favourite_color = colorRampPalette(c("#FF6B6B", "#4ECDC4", "#45B7D1", "#F7B801", "#FAD390"))(num_participants)
)

# Add random percentile scores for personality traits (1-100)
trait_names <- c("Honesty", "Emotionality", "Extroversion", "Agreeableness", "Conscientiousness", "Openness")
for (trait in trait_names) {
  sample_data[[trait]] <- round(runif(num_participants, 1, 100), 1)
}

# Add random "votes" for ranked-choice questions
for (i in 1:10) {
  question_name <- paste0("rankedQ_", i)
  # Each person votes for someone else from the group
  sample_data[[question_name]] <- sample(participant_names, num_participants, replace = TRUE)
}


# --- 4. DEMONSTRATE PACKAGE FUNCTIONS ---

# --- Example 1: TG_trait() ---
# Visualize the "Conscientiousness" trait, including the auto-generated group average.
TG_trait(
  dataset = sample_data,
  column_name = "Conscientiousness",
  output_path = "ExamplePlots/trait_conscientiousness.jpg"
)

# --- Example 2: TG_voting() ---
# Visualize the results of a ranked-choice question.
TG_voting(
  dataset = sample_data,
  column_name = "rankedQ_1",
  title = "Who would you want on your trivia team?",
  output_path = "ExamplePlots/voting_trivia_partner.jpg",
  sort_order = "desc" # You can also try "asc"
)

# --- Example 3: TG_jung() ---
# Visualize the "Extroversion" trait as a dichotomy.
TG_jung(
  dataset = sample_data,
  column_name = "Extroversion",
  title = "Team Extroversion Profile",
  label_top = "Introversion",
  label_bottom = "Extroversion",
  output_path = "ExamplePlots/jung_extroversion.jpg"
)

# --- END OF DEMO ---
message("Demonstration complete! Check the 'ExamplePlots' directory for output.")