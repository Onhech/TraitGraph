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
  library(tidyverse)

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
  # --- Workflow A: Full Rebuild and Install ---
  # Use this workflow occasionally, especially before pushing to GitHub, to ensure
  # the complete package builds correctly from start to finish.
   remove.packages("TraitGraph")
  # in terminal you can delete cache using
  # % rm -rf /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/library/TraitGraph
   devtools::clean_dll()
   devtools::document()
   devtools::load_all()
   devtools::install()           # Build and install the package locally
   library(TraitGraph)            # Load the newly installed package


  # --- Workflow B: Fast, Interactive Development (RECOMMENDED) ---
  # Use this workflow 99% of the time. It loads all your functions directly
  # into memory, which is extremely fast for testing changes.
  # Simply run this line after you save a change in any of your R/ files.
   devtools::load_all()


  # --- Workflow C: Test GitHub Installation ---
  # Use this to simulate a new user installing from your public repository.
  # It's best to run this in a completely fresh R session.
  # remove.packages("TraitGraph") # Ensure no local version is present
  # remotes::install_github("Onhech/TraitGraph")
   library(TraitGraph)


  # --- 3. CREATE SAMPLE DATA ---

  # Create one master sample data frame for testing.
  set.seed(42) # for reproducibility
  sample_data <- tibble::tribble(
    ~name,    ~favourite_color, ~HonestyHumility, ~Emotionality, ~Extroversion, ~Agreeableness, ~Conscientiousness, ~Openness,
    "Alice Frank",   "#FF6B6B",         75,               40,            80,            85,             90,                 70,
    "Bob Steward",     "#4ECDC4",         80,               45,            75,            90,             85,                 65,
    "Frank Bobby",   "#45B7D1",         70,               50,            85,            80,             95,                 75,
    "Eve Twingle",     "#F7B801",         85,               70,            75,            80,             40,                 95,
    "Grace Hunter",   "#FAD390",         90,               65,            80,            75,             35,                 90,
    "Heidi Probosky",   "#FF8C61",         80,               75,            70,            85,             45,                 85,
    "Charlie Kane", "#3D5A80",         40,               80,            30,            50,             60,                 45,
    "David Turner Salvadora III",   "#98C1D9",         45,               75,            35,            55,             65,                 50,
    "Ivan Evans",    "#E0FBFC",         35,               85,            25,            45,             55,                 40,
    "Judy Perch",    "#293241",         50,               70,            40,            60,             70,                 55
  ) %>%
    dplyr::mutate(rankedQ_1 = sample(0:100, 10, replace = TRUE))


  # --- 4. FUNCTION TESTING ---
  # After running `devtools::load_all()`, you can run these calls to test.

  # --- Trait Example ---
  TG_trait(
    dataset = sample_data,
    column_name = "Extroversion",
    save_plot = T,
    output_path = 'ExamplePlots/trait_graph_example.jpg'
  )


  # --- Voting Question Example ---
  TG_votes(
    dataset = sample_data,
    column_name = "rankedQ_1",
    title = "Who apologizes even when someone else bumps into them?",
    save_plot = T,
    output_path = 'ExamplePlots/voting_graph_example.jpg',title_size_mod = 1.6
  )


  # --- Jungian Dichotomy Example ---
  TG_jung(
    dataset = sample_data,
    column_name = "Extroversion",
    title = "Team Extroversion Profile",
    label_top = "Introversion",
    label_bottom = "Extroversion",
    callout_text_color = "other",
    save_plot = T,
    output_path = 'ExamplePlots/jung_graph_example_0.jpg',
  )

  TG_jung(
    dataset = sample_data,
    column_name = "Extroversion",
    title = "Introversion vs Extroversion",
    label_top = "Introversion",
    label_bottom = "Extroversion",
    callout_text_color = "other",
    save_plot = T,
    color_bars = "sunset",
    color_bars_opacity = c(0.4, 0.2, 0.2, 0.5, 0.7),
    output_path = 'ExamplePlots/jung_graph_example_1a.jpg',
  )



# --- Similarity Network Example ---
similarity_columns <- c("HonestyHumility", "Emotionality", "Extroversion", "Agreeableness","Conscientiousness", "Openness")

# Define the columns to be used for the similarity calculation
TG_similarity(
  dataset = sample_data[1:8,],
  connection_threshold = .5,
  columns = similarity_columns,
  name = "name",
  use_initials = T, # Use full names for clarity
  save_plot = T,
  output_path = 'ExamplePlots/similarity_graph_example.png',
  zoom_out_factor = 1.2
)

# --- Similarity Table Example with Custom Threshold ---
# Create a tidy table of pairwise similarities, bolding and coloring
# correlations with an absolute value greater than 0.7.
similarity_table_custom_threshold <- TG_similarity_table(
  dataset = sample_data[1:8,],
  columns = similarity_columns,
  name = "name",
  format = "html"
)

# Render the table in the RStudio Viewer
htmltools::html_print(similarity_table_custom_threshold)

# --- Similarity Plot (Heatmap) Example ---
similarity_heatmap <- TG_similarity_plot(
  dataset = sample_data[1:8,],
  columns = similarity_columns,
  name = "name"
)

  # Print the plot to the Viewer
  print(similarity_heatmap)

  # Save the plot to a file
  ggplot2::ggsave(
    filename = "ExamplePlots/similarity_heatmap_example.png",
    plot = similarity_heatmap,
    width = 8,
    height = 7,
    dpi = 300
  )


# --- Similarity Plot (Heatmap) Example ---
similarity_heatmap <- TG_similarity_plot(
  dataset = sample_data[1:8,],
  columns = similarity_columns,
  name = "name",
  show_legend = F
)

# Print the plot to the Viewer
print(similarity_heatmap)

# Save the plot to a file
ggplot2::ggsave(
  filename = "ExamplePlots/similarity_heatmap_example.jpg",
  plot = similarity_heatmap,
  width = 10,
  height = 5,
  dpi = 300
)
