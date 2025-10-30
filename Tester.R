# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # TraitGraph Package - Development & Testing Script
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # This script is for managing, rebuilding, and testing the TraitGraph package.
  # It is not part of the package itself but a helper for the developer.
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

## --- 0. PACKAGES & PROJECT ROOT -------------------------------------------------
# install.packages(c("devtools","tidyverse","here","withr"), dep = TRUE)
suppressPackageStartupMessages({
  library(devtools)
  library(tidyverse)
  library(here)
  library(withr)
})

# Ensure we're in the project (use the .Rproj)
proj_root <- here::here()
message("Project root: ", proj_root)

# Output folder (centralized)
plots_dir <- file.path(proj_root, "ExamplePlots")
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)

## --- 1. WORKFLOW HELPERS --------------------------------------------------------
  # --- Workflow A: Full Rebuild and Install ---
  # Use this workflow occasionally, especially before pushing to GitHub, to ensure
  # the complete package builds correctly from start to finish.
remove.packages("TraitGraph")
  # in terminal you can delete cache using
  # % rm -rf /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/library/TraitGraph
devtools::clean_dll()
devtools::document()
devtools::load_all()
devtools::install(upgrade = 'always')           # Build and install the package locally
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


# --- 2. CREATE SAMPLE DATA ---
# Create one master sample data frame for testing.
set.seed(42) # for reproducibility

# Generate the main sample data
sample_data <- tibble::tibble(
  name = c(  "Alice Frank", "Bob Steward", "Frank Bobby", "Eve Twingle", "Grace Hunter", "Heidi Probosky", "Charlie Kane", "David Turner Salvadora III", "Ivan Evans", "Judy Perch", "Karl Moritz", "Lana Perez", "Mason Lee", "Nina Hollis", "Oscar Grant", "Paula White", "Quinn Baxter", "Riley Chen", "Sara Diaz", "Theo Nguyen", "Uma Patel", "Victor Stone", "Willow Adams", "Xavier Brooks", "Yara Cohen", "Zane Foster", "Abby Kim", "Ben Torres", "Clara Wells", "Derek Young"),
  favourite_color = c( "#FF6B6B", "#4ECDC4", "#45B7D1", "#F7B801", "#FAD390","#FF8C61", "#3D5A80", "#98C1D9", "#9B59B6", "#293241","#E67E22", "#1ABC9C", "#E74C3C", "#3498DB", "#9B59B6","#F1C40F", "#2ECC71", "#16A085", "#D35400", "#C0392B","#7F8C8D", "#BDC3C7", "#8E44AD", "#27AE60", "#F39C12","#2980B9", "#C0392B", "#D35400", "#1ABC9C", "#9B59B6"),
  HonestyHumility     = round(runif(30, 30, 95)),  # varied range per trait
  Emotionality        = round(runif(30, 30, 90)),
  Extroversion        = round(runif(30, 25, 95)),
  Agreeableness       = round(runif(30, 40, 95)),
  Conscientiousness   = round(runif(30, 35, 95)),
  Openness            = round(runif(30, 40, 95)),
  rankedQ_1 = sample(0:80, 30, replace = TRUE),
  rsummedQ_1 = as.vector(rmultinom(n = 1, size = 100, prob = rep(1, 30)))
  )

# Preview
sample_data<-dplyr::slice_sample(sample_data,n = 15) # Default (ideal) size
sample_data_5<-dplyr::slice_sample(sample_data,n = 5)
sample_data_15<-dplyr::slice_sample(sample_data,n = 15)
sample_data_20<-dplyr::slice_sample(sample_data,n = 20)
sample_data_25<-dplyr::slice_sample(sample_data,n = 25)
sample_data_30<-dplyr::slice_sample(sample_data,n = 30)



# --- 3. FUNCTION TESTING ---
# After running `devtools::load_all()`, you can run these calls to test.

# ~~~~~~~~~~~~~~~~~~~ #
# Jungian ####
# ~~~~~~~~~~~~~~~~~~~ #
TG_jung(
  dataset = sample_data,
  column_name = "Extroversion",
  title = "Team Extroversion Profile",
  label_top = "Introversion",
  label_bottom = "Extroversion",
  callout_text_color = "other",
  show_points = F,
  save_plot = TRUE,
  output_path = 'ExamplePlots/jung_graph_example_0.jpg'
)

#TG_jung(
#  dataset = sample_data,
#  column_name = "Extroversion",
#  title = "Introversion vs Extroversion",
#  label_top = "Introversion",
#  label_bottom = "Extroversion",
#  callout_text_color = "other",
#  save_plot = T,
#  color_bars = "sunset",
#  color_bars_opacity = c(0.4, 0.2, 0.2, 0.5, 0.7),
#  output_path = 'ExamplePlots/jung_graph_example_1a.jpg',
#)


  # ~~~~~~~~~~~~~~~~~~~ #
  # --- Trait Example   ####
  # ~~~~~~~~~~~~~~~~~~~ #
TG_trait(
    dataset = sample_data_20,
    title = "",
    column_name = "Extroversion",
    save_plot = T,show_plot = T,
    #color_opacity = .,
    output_path = 'ExamplePlots/trait_graph_example.jpg'
  )

  # ~~~~~~~~~~~~~~~~~~~ #
  # --- Doughnut Graph  ####
  # ~~~~~~~~~~~~~~~~~~~ #
  # Add another test case for descending order and no title
TG_doughnut_chart(
    dataset = sample_data,
    column_name = "rsummedQ_1",
    sort_order = "asc",
    show_title = T,title = "Who would win the Nobel Prize and then apologize for taking up everyone's time during their acceptance speech?",
    title_size_mod = 1,
    name_size_mod = 1,
    save_plot = TRUE,
    inner_label_threshold = 15,
    footnote_text_width = 65,
    footnote_hjust = 0,
    footnote_vjust = 1,
    show_plot = T,
    title_color = "grey30",
    #footnote_margin_t = -42,
    output_path = "ExamplePlots/doughnut_chart_desc_no_title.jpeg"
  )



  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Similarity       ####
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  similarity_columns <- c(
    "HonestyHumility", "Emotionality", "Extroversion",
    "Agreeableness", "Conscientiousness", "Openness"
  )

  # --- 1. Network Graph Example ---
  network_plot <- TG_similarity_network(
    dataset = sample_data[1:8, ],
    title = NULL, subtitle = NULL,
    use_initials = T,
    connection_threshold = .35,
    color_col = "favourite_color",
    columns = similarity_columns,
    #zoom_out_factor = 1.2,
    name = "name",
    save_plot = T,show_plot = T,
    output_path = "ExamplePlots/similarity_network.png"
  )


  # --- 2. Heatmap Example ---
  heatmap_plot <- TG_similarity_heatmap(
    dataset = sample_data[1:8, ],
    columns = similarity_columns,
    name = "name",
    save_plot = T,show_plot = T,
    show_legend = F,
    output_path = "ExamplePlots/similarity_network.png"
  )

  # --- 3. Heatmap Example ---
similarity_summary <- TG_similarity_table(
  dataset = sample_data[1:8, ],
  columns = similarity_columns,
  name_col = "name",
  format = "ranked_summary"
)

print(similarity_summary)



# ~~~~~~~~~~~~~~~~~~~ #
# Voting ####
# ~~~~~~~~~~~~~~~~~~~ #
TG_votes(
  dataset = sample_data,
  column_name = "rankedQ_1",
  title = "1234 6789.",
  save_plot = T,
  output_path = 'ExamplePlots/voting_graph_example_10.jpg',title_size_mod = 1.6
)

TG_votes(
  dataset = sample_data,
  column_name = "rankedQ_1",
  title = "1234 6789 1234.",
  save_plot = T,
  output_path = 'ExamplePlots/voting_graph_example_15.jpg',title_size_mod = 1.6
)


TG_votes(
  dataset = sample_data,
  column_name = "rankedQ_1",
  title = "1234 6789 1234 6789.",
  save_plot = T,
  output_path = 'ExamplePlots/voting_graph_example_20.jpg',title_size_mod = 1.6
)

TG_votes(
  dataset = sample_data,
  column_name = "rankedQ_1",
  title = "1234 6789 1234 6789 1234.",
  save_plot = T,
  output_path = 'ExamplePlots/voting_graph_example_25.jpg',title_size_mod = 1.6
)


TG_votes(
  dataset = sample_data,
  column_name = "rankedQ_1",
  title = "1234 6789 1234 6789 1234 6789.",
  save_plot = T,
  output_path = 'ExamplePlots/voting_graph_example_30.jpg',title_size_mod = 1.6
)

TG_votes(
  dataset = sample_data,
  column_name = "rankedQ_1",
  title = "1234 6789 1234 6789 1234 6789 1234.",
  save_plot = T,
  output_path = 'ExamplePlots/voting_graph_example_35.jpg',title_size_mod = 1.6
)

TG_votes(
  dataset = sample_data,
  column_name = "rankedQ_1",
  title = "1234 6789 1234 6789 1234 6789 1234 6789.",
  save_plot = T,
  output_path = 'ExamplePlots/voting_graph_example_40.jpg',title_size_mod = 1.6
)

TG_votes(
  dataset = sample_data,
  column_name = "rankedQ_1",
  title = "1234 6789 1234 6789 1234 6789 1234 6789 1234.",
  save_plot = T,
  output_path = 'ExamplePlots/voting_graph_example_45.jpg',title_size_mod = 1.6
)

TG_votes(
  dataset = sample_data,
  column_name = "rankedQ_1",
  title = "1234 6789 1234 6789 1234 6789 1234 6789 1234 6789.",
  save_plot = T,
  output_path = 'ExamplePlots/voting_graph_example_50.jpg',title_size_mod = 1.6
)

TG_votes(
  dataset = sample_data,
  column_name = "rankedQ_1",
  title = "1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234.",
  save_plot = T,
  output_path = 'ExamplePlots/voting_graph_example_55.jpg',title_size_mod = 1.6
)

TG_votes(
  dataset = sample_data,
  column_name = "rankedQ_1",
  title = "1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789.",
  save_plot = T,
  output_path = 'ExamplePlots/voting_graph_example_60.jpg',title_size_mod = 1.6
)

TG_votes(
  dataset = sample_data,
  column_name = "rankedQ_1",
  title = "1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234.",
  save_plot = T,
  output_path = 'ExamplePlots/voting_graph_example_65.jpg',title_size_mod = 1.6
)

TG_votes(
  dataset = sample_data,
  column_name = "rankedQ_1",
  title = "1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789  1234 6789",
  save_plot = T,
  output_path = 'ExamplePlots/voting_graph_example_70.jpg',title_size_mod = 1.6
)

TG_votes(
  dataset = sample_data,
  column_name = "rankedQ_1",
  title = "1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789  1234 6789 1234",
  save_plot = T,
  output_path = 'ExamplePlots/voting_graph_example_75.jpg',title_size_mod = 1.6
)


TG_votes(
  dataset = sample_data,
  column_name = "rankedQ_1",
  title = "1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789  1234 6789 1234 6789",
  save_plot = T,
  output_path = 'ExamplePlots/voting_graph_example_80.jpg',title_size_mod = 1.6
)

TG_votes(
  dataset = sample_data,
  column_name = "rankedQ_1",
  title = "1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789  1234 6789 1234 6789 1234",
  save_plot = T,
  output_path = 'ExamplePlots/voting_graph_example_85.jpg',title_size_mod = 1.6
)

TG_votes(
  dataset = sample_data,
  column_name = "rankedQ_1",
  title = "1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789  1234 6789 1234 6789",
  save_plot = T,
  output_path = 'ExamplePlots/voting_graph_example_90.jpg',title_size_mod = 1.6
)

TG_votes(
  dataset = sample_data,
  column_name = "rankedQ_1",
  title = "1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789  1234 6789 1234 6789 1234",
  save_plot = T,
  output_path = 'ExamplePlots/voting_graph_example_95.jpg',title_size_mod = 1.6
)


TG_votes(
  dataset = sample_data,
  column_name = "rankedQ_1",
  title = "1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789  1234 6789 1234 6789",
  save_plot = T,
  output_path = 'ExamplePlots/voting_graph_example_100.jpg',title_size_mod = 1.6
)

TG_votes(
  dataset = sample_data,
  column_name = "rankedQ_1",
  title = "1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789  1234 6789 1234 6789",
  save_plot = T,
  output_path = 'ExamplePlots/voting_graph_example_110.jpg',title_size_mod = 1.6
)

TG_votes(
  dataset = sample_data,
  column_name = "rankedQ_1",
  title = "1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789  1234 6789 1234 6789",
  save_plot = T,
  output_path = 'ExamplePlots/voting_graph_example_120.jpg',title_size_mod = 1.6
)

TG_votes(
  dataset = sample_data,
  column_name = "rankedQ_1",
  title = "1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789  1234 6789 1234 6789",
  save_plot = T,
  output_path = 'ExamplePlots/voting_graph_example_130.jpg',title_size_mod = 1.6
)

TG_votes(
  dataset = sample_data,
  column_name = "rankedQ_1",
  title = "1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789 1234 6789  1234 6789 1234 6789 1234",
  save_plot = T,
  output_path = 'ExamplePlots/voting_graph_example_135.jpg',title_size_mod = 1.6
)
