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

find_project_root <- function(start_dir = getwd()) {
  dir <- normalizePath(start_dir, winslash = "/", mustWork = FALSE)
  repeat {
    rproj <- file.path(dir, "TraitGraph.Rproj")
    desc <- file.path(dir, "DESCRIPTION")
    if (file.exists(rproj)) return(dir)
    if (file.exists(desc)) {
      d1 <- readLines(desc, n = 1, warn = FALSE)
      if (length(d1) == 1 && grepl("^Package: TraitGraph", d1)) return(dir)
    }
    parent <- dirname(dir)
    if (parent == dir) break
    dir <- parent
  }
  stop("Could not find TraitGraph project root from: ", start_dir)
}

# Ensure we're in the project (use the .Rproj or DESCRIPTION)
proj_root <- find_project_root()
setwd(proj_root)
message("Project root: ", proj_root)

# Output folder (centralized)
plots_dir <- file.path(proj_root, "ExamplePlots")
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)






## --- 1. WORKFLOW HELPERS --------------------------------------------------------
  # --- Workflow A: Full Rebuild and Install ---
  # Use this workflow occasionally, especially before pushing to GitHub, to ensure
  # the complete package builds correctly from start to finish.
if ("TraitGraph" %in% rownames(installed.packages())) {
  remove.packages("TraitGraph")
}
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
#set.seed(42) # for reproducibility

# Generate the main sample data
sample_data <- tibble::tibble(
  name = c(  "Alice Frank Longnamer", "Bob Steward", "Frank Bobby", "Eve Twingle", "Grace Hunter", "Heidi Probosky", "Charlie Kane", "David Turner Salvadora III", "Ivan Evans", "Judy Perch", "Karl Moritz", "Lana Perez", "Mason Lee", "Nina Hollis", "Oscar Grant", "Paula White", "Quinn Baxter", "Riley Chen", "Sara Diaz", "Theo Nguyen", "Uma Patel", "Victor Stone", "Willow Adams", "Xavier Brooks", "Yara Cohen", "Zane Foster", "Abby Kim", "Ben Torres", "Clara Wells", "Derek Young"),
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
sample_data_16<-dplyr::slice_sample(sample_data,n = 16)
sample_data_17<-dplyr::slice_sample(sample_data,n = 17)
sample_data_18<-dplyr::slice_sample(sample_data,n = 18)
sample_data_20<-dplyr::slice_sample(sample_data,n = 20)
sample_data_25<-dplyr::slice_sample(sample_data,n = 25)
sample_data_30<-dplyr::slice_sample(sample_data,n = 30)





demo_paths <- c(
  "ExamplePlots/demo_trait_1.jpg",
  "ExamplePlots/demo_trait_2.jpg",
  "ExamplePlots/demo_trait_3.jpg"
)

# --- Progress Demo (batch) ---
progress_demo <- TraitGraph:::tg_progress_bar(
  total = length(demo_paths),
  label = "Generating %d Demo Plots",
  interval_secs = 0.5
)

for (i in seq_along(demo_paths)) {
  TG_trait(
    dataset = sample_data_20,
    show_title = FALSE,
    column_name = "Extroversion",
    save_plot = TRUE,
    show_plot = FALSE,
    verbose = FALSE,
    output_path = demo_paths[i]
  )
  progress_demo <- TraitGraph:::tg_progress_update(progress_demo)
}
TraitGraph:::tg_progress_done(progress_demo)

  # --- 3. FUNCTION TESTING ---
# After running `devtools::load_all()`, you can run these calls to test.
# ~~~~~~~~~~~~~~~~~~~ #
# --- Trait Example   ####
# ~~~~~~~~~~~~~~~~~~~ #
TG_trait(
    dataset = sample_data_20,
    show_title = F,
    column_name = "Extroversion",
    random_seed = 12345,
    save_plot = T,show_plot = T,
    #color_opacity = .,
    output_path = 'ExamplePlots/trait_graph_example.jpg'
  )

# Experimental inside/outside value labels (no callout boxes)
TG_trait_test(
    dataset = sample_data_18,
    color_opacity = 100,
    show_title = F,
    column_name = "Extroversion",
    random_seed = 12345,
    callout_inside_threshold = 30,
    callout_outside_nudge = 15,
    callout_text_size = 3.4,
    callout_text_size_group = 3.4,
    callout_text_face = "bold",
    callout_style = "boxed",
    save_plot = TRUE,
    show_plot = T,
    output_path = 'ExamplePlots/trait_graph_example_label_test_boxed.jpg'
  )

TG_trait_test(
  dataset = sample_data_18,
  color_opacity = 100,
  show_title = F,
  column_name = "Extroversion",
  random_seed = 12345,
  callout_inside_threshold = 30,
  callout_outside_nudge = 13,
  callout_text_size = 3.8,
  callout_text_size_group = 3.8,
  callout_text_face = "bold",
  callout_style = "inside_outside",
  save_plot = TRUE,
  show_plot = T,
  output_path = 'ExamplePlots/trait_graph_example_label_test_unboxed.jpg'
)

# ~~~~~~~~~~~~~~~~~~~ #
# Similarity Data ####
# ~~~~~~~~~~~~~~~~~~~ #
sim_data <- TG_similarity_data(
  dataset = sample_data_15,
  columns = c("HonestyHumility", "Emotionality", "Extroversion", "Agreeableness", "Conscientiousness", "Openness"),
  name_col = "name"
)
print(names(sim_data))
print(dim(sim_data$matrix))

sim_json_path <- file.path(proj_root, "ExamplePlots", "similarity_data_example.json")
TG_similarity_data(
  dataset = sample_data_15,
  columns = c("HonestyHumility", "Emotionality", "Extroversion", "Agreeableness", "Conscientiousness", "Openness"),
  name_col = "name",
  format = "json",
  output_path = sim_json_path
)
message("Wrote similarity JSON: ", sim_json_path)



# ~~~~~~~~~~~~~~~~~~~ #
# Achievements ####
# ~~~~~~~~~~~~~~~~~~~ #
ach_inputs <- file.path(proj_root, "achievement_inputs")
ach_output <- file.path(proj_root, "ExamplePlots", "achievements_test")
if (!dir.exists(ach_output)) dir.create(ach_output, recursive = TRUE)

TG_achievements(
  trait_map_path = file.path(ach_inputs, "trait_map.csv"),
  vote_map_path = file.path(ach_inputs, "vote_map.csv"),
  data_path = file.path(ach_inputs, "group_dataset.csv"),
  achievements_path = file.path(ach_inputs, "achievements.csv"),
  voting_data_path = file.path(ach_inputs, "voting_data.csv"),
  output_dir = ach_output,
  max_awards = 10,
  write_qa = F
)

# --- Achievements soft-fail demo (missing columns) ---
ach_demo_dir <- file.path(proj_root, "ExamplePlots", "achievements_soft_fail_demo")
if (!dir.exists(ach_demo_dir)) dir.create(ach_demo_dir, recursive = TRUE)

demo_inputs <- file.path(ach_demo_dir, "inputs")
if (!dir.exists(demo_inputs)) dir.create(demo_inputs, recursive = TRUE)

file.copy(file.path(ach_inputs, "trait_map.csv"), file.path(demo_inputs, "trait_map.csv"), overwrite = TRUE)
file.copy(file.path(ach_inputs, "vote_map.csv"), file.path(demo_inputs, "vote_map.csv"), overwrite = TRUE)
file.copy(file.path(ach_inputs, "achievements.csv"), file.path(demo_inputs, "achievements.csv"), overwrite = TRUE)
file.copy(file.path(ach_inputs, "voting_data.csv"), file.path(demo_inputs, "voting_data.csv"), overwrite = TRUE)

# Create a broken group_dataset.csv by dropping required timing + a trait column.
demo_people <- read.csv(file.path(ach_inputs, "group_dataset.csv"), stringsAsFactors = FALSE)
demo_people$start_time <- NULL
demo_people$completion_time <- NULL
demo_people$total_duration_sec <- NULL
if ("HEX_E_S" %in% names(demo_people)) {
  demo_people$HEX_E_S <- NULL
}
write.csv(demo_people, file.path(demo_inputs, "group_dataset.csv"), row.names = FALSE)

# Soft-fail: should warn + write achievements_input_issues.csv in output_dir.
TG_achievements(
  trait_map_path = file.path(demo_inputs, "trait_map.csv"),
  vote_map_path = file.path(demo_inputs, "vote_map.csv"),
  data_path = file.path(demo_inputs, "group_dataset.csv"),
  achievements_path = file.path(demo_inputs, "achievements.csv"),
  voting_data_path = file.path(demo_inputs, "voting_data.csv"),
  output_dir = ach_demo_dir,
  max_awards = 10,
  write_qa = FALSE,
  soft_fail = TRUE
)

# ~~~~~~~~~~~~~~~~~~~ #
# Jungian ####
# ~~~~~~~~~~~~~~~~~~~ #
TG_jung(
  dataset = sample_data,
  column_name = "Extroversion",
  title = "Team Extroversion Profile",
  show_title = FALSE,
  label_top = "Introversion",
  label_bottom = "Extroversion",
  callout_text_color = "other",
  show_points = F,
  save_plot = TRUE,
  verbose = TRUE,
  output_path = 'ExamplePlots/jung_graph_example_0.jpg'
)

# ~~~~~~~~~~~~~~~~~~~ #
# Vote-bar ####
# ~~~~~~~~~~~~~~~~~~~ #
# Simple vote bar chart with tie handling and footnotes.

if (!exists("find_project_root", inherits = TRUE)) {
  find_project_root <- function(start_dir = getwd()) {
    dir <- normalizePath(start_dir, winslash = "/", mustWork = FALSE)
    repeat {
      rproj <- file.path(dir, "TraitGraph.Rproj")
      desc <- file.path(dir, "DESCRIPTION")
      if (file.exists(rproj)) return(dir)
      if (file.exists(desc)) {
        d1 <- readLines(desc, n = 1, warn = FALSE)
        if (length(d1) == 1 && grepl("^Package: TraitGraph", d1)) return(dir)
      }
      parent <- dirname(dir)
      if (parent == dir) break
      dir <- parent
    }
    stop("Could not find TraitGraph project root from: ", start_dir)
  }
}
if (!exists("proj_root", inherits = TRUE)) {
  proj_root <- find_project_root()
}
if (!exists("plots_dir", inherits = TRUE)) {
  plots_dir <- file.path(proj_root, "ExamplePlots")
  if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)
}
vote_data <- read.csv(file.path(proj_root, "achievement_inputs", "group_dataset.csv"), stringsAsFactors = FALSE)

TG_vote_bar(
  dataset = vote_data,
  vote_column = "vote_q14",
  palette = list(high = "#2F7D32", mid = "#E6F4E6", low = "#B3B3B3"),
  palette_mode = "mid_high",
  label_size_single = 7,
  marker_font_family = "sans",
  tie_markers = c("*", "†", "‡"),
  footnote_color = "#2F7D32",
  save_plot = TRUE,
  show_plot = TRUE,
  verbose = TRUE,
  output_path = file.path(plots_dir, "vote_bar_example.jpg")
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
    show_title = FALSE,
    use_initials = T,
    connection_threshold = .35,
    color_col = "favourite_color",
    columns = similarity_columns,
    #zoom_out_factor = 1.2,
    name = "name",
    save_plot = T,show_plot = T,
    verbose = TRUE,
    output_path = "ExamplePlots/similarity_network.png"
  )


  # --- 2. Heatmap Example ---
  heatmap_plot <- TG_similarity_heatmap(
    dataset = sample_data[1:8, ],
    columns = similarity_columns,
    name = "name",
    save_plot = T,show_plot = T,
    show_title = FALSE,
    show_legend = F,
    verbose = TRUE,
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
   show_title = FALSE,
  save_plot = T,
  output_path = 'ExamplePlots/voting_graph_example_10.jpg',title_size_mod = 1.6
)

# ~~~~~~~~~~~~~~~~~~~ #
# Profiles ####
# ~~~~~~~~~~~~~~~~~~~ #
  # Uses achievement_inputs/trait_map.csv to demo per-trait gradients.

profile_traits <- read.csv(file.path(proj_root, "achievement_inputs", "trait_map.csv"), stringsAsFactors = FALSE)
profile_traits <- profile_traits[profile_traits$Type == "trait", ]
profile_traits$group_score <- round(runif(nrow(profile_traits), 15, 95))

profile_plot <- TG_profile(
  dataset = profile_traits,
  value_column = "group_score",
  trait_column = "Name",
  low_color_column = "Low_Color",
  high_color_column = "High_Color",
  title = "Group Psychological Profile",
  show_title = FALSE,
  save_plot = TRUE,
  show_plot = T,
  output_path = file.path(plots_dir, "profile_example.jpg")
)
