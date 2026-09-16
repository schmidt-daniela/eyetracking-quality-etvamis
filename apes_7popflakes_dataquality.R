# General -----------------------------------------------------------------
rm(list = ls())

# Packages ----------------------------------------------------------------
library(here)
library(tidyverse)
library(gazer) # or: install.packages("remotes"), remotes::install_github("dmirman/gazer")

# Functions ---------------------------------------------------------------
source(here("R", "cleaning.R"))
source(here("R", "blink.R"))
source(here("R", "eyetracking_data_quality.R"))
source(here("R", "eyetracking_outcomes.R"))
source(here("R", "utils.R"))

# For creating dataframe with descriptives
calc_dq <- function(df, val_col, dq_label) {
  df |> 
    group_by(species) |> 
    summarise(
      mean  = mean({{ val_col }}, na.rm = TRUE),
      sd    = sd({{ val_col }}, na.rm = TRUE),
      lower = mean - (sd / sqrt(n())) * qt(0.975, df = n() - 1),
      upper = mean + (sd / sqrt(n())) * qt(0.975, df = n() - 1),
      dq    = dq_label,
      .groups = "drop"
    )
}

# Set Parameters ----------------------------------------------------------
buffer <- 120
species <- "orangs" # "bonobos" or "orangs" or "bonobos2" or "b_chimps"
plot_color <- switch(species,
                     "orangs"   = "#E69F00",
                     "bonobos"  = "#A01C99",
                     "bonobos2" = "#56B4E9",
                     "b_chimps"  = "#009E73",
                     "a_chimps"  = "#F0E442",
                     "#999999"  # "fallback-color, if species cannot be found
)
data_path <- here("data", "data_apes_5popflakes", species)
files <- list.files(path = data_path, pattern = "\\.tsv$", full.names = TRUE) # get file names
species_label <- str_to_title(species)  # "Bonobos" / "Orangs"

# Define AOIs -------------------------------------------------------------
top1_x_topleft <- 154 - buffer
top1_y_topleft <- 78 - buffer
top1_x_botright <- 354 + buffer
top1_y_botright <- 278 + buffer

top2_x_topleft <- 594 - buffer
top2_y_topleft <- 78 - buffer
top2_x_botright <- 794 + buffer
top2_y_botright <- 278 + buffer

top3_x_topleft <- 1124 - buffer
top3_y_topleft <- 78 - buffer
top3_x_botright <- 1324 + buffer
top3_y_botright <- 278 + buffer

top4_x_topleft <- 1564 - buffer
top4_y_topleft <- 78 - buffer
top4_x_botright <- 1764 + buffer
top4_y_botright <- 278 + buffer

actora_x_topleft <- 475 - buffer
actora_y_topleft <- 464 - buffer
actora_x_botright <- 675 + buffer
actora_y_botright <- 664 + buffer

actorb_x_topleft <- 1244 - buffer
actorb_y_topleft <- 464 - buffer
actorb_x_botright <- 1444 + buffer
actorb_y_botright <- 664 + buffer

to_x_topleft <- 859 - buffer
to_y_topleft <- 814 - buffer
to_x_botright <- 1059 + buffer
to_y_botright <- 1014 + buffer

# Read Data ---------------------------------------------------------------
raw <- read.table(here("data", "data_apes_7popflakes", species, "main_data.tsv"), header = TRUE, sep = "\t")
df <- raw

# Tidy Data ---------------------------------------------------------------
# Rename columns
df <- df |>  
  rename_with(~ .x |> 
                str_replace_all("[^A-Za-z0-9]+", "_") |>  # replace everything that's not a number or a letter with _
                str_remove("^_+") |>  # remove one or more _ at the beginning of a string
                str_remove("_+$") |>  # remove one or more _ at the end of a string
                str_to_lower()) # make all letters lowercase

# Add Gaze-Sample Duration
df$recording_timestamp <- as.numeric(df$recording_timestamp)
df$gaze_sample_duration <- c(diff(df$recording_timestamp), NA) / 1000

# Remove Columns That Are Not of Interest/ Not Informative
df <- df |> 
  select(-c(ungrouped, client_area_position_x_dacspx, client_area_position_y_dacspx, viewport_position_x, # variables with NA only
            viewport_position_y, viewport_width, viewport_height, full_page_width, full_page_height, # variables with NA only
            mouse_position_x,mouse_position_y)) |> # variable not of interest
  select(-starts_with("aoi_size")) |> 
  select(-starts_with("aoi_hit"))

# Prepare stimulus information
df <- df |>
  separate(presented_stimulus_name,
           into = c("pre_post", "trial", "stimulus", "position", "duration"), 
           remove = FALSE, sep = "_")

# Create session, name, and order column
df <- df |> 
  extract(
    recording_name,
    into = c("name", "order", "session"),
    regex = "([A-Za-z]+) (E\\d+)_(\\d+)",
    remove = FALSE
  ) |> 
  mutate(
    name = str_to_lower(name),
    session = as.numeric(session)
  )

# Create session_trial column
df <- df |> 
  unite(col = "session_trial", session, trial, sep = "_", remove = F)

# Add cumulative duration per trial
df <- df |> 
  group_by(name, session_trial, duration) |> 
  mutate(timeline_trial_units = cumsum(gaze_sample_duration)) |> 
  group_by(name, session_trial) |> 
  mutate(timeline_trial_tot = cumsum(gaze_sample_duration)) |> 
  ungroup()

# Define AOIs 
df <- df |>
  mutate(gaze_point_x = as.numeric(gaze_point_x),
         gaze_point_y = as.numeric(gaze_point_y)) |>
  mutate(fixation_point_x = as.numeric(fixation_point_x),
         fixation_point_y = as.numeric(fixation_point_y))

# Based on Fixations
df$aoi_fixation <- "not_in_aoi"

df <- df |>
  mark_aoi(name = "top_left", x_min = top1_x_topleft, x_max = top1_x_botright, y_min = top1_y_topleft, y_max = top1_y_botright,
           stimulus_name = "popflake", position_name = "top1", x_col = "fixation_point_x", y_col = "fixation_point_y",
           aoi_col = "aoi_fixation") |>
  mark_aoi(name = "bot_left", top2_x_topleft, top2_x_botright, top2_y_topleft, top2_y_botright,
           stimulus_name = "popflake", position_name = "top2", x_col = "fixation_point_x", y_col = "fixation_point_y",
           aoi_col = "aoi_fixation") |>
  mark_aoi(name = "top_right", top3_x_topleft, top3_x_botright, top3_y_topleft, top3_y_botright, 
           stimulus_name = "popflake", position_name = "top3", x_col = "fixation_point_x", y_col = "fixation_point_y",
           aoi_col = "aoi_fixation") |>
  mark_aoi(name = "bot_right",  top4_x_topleft, top4_x_botright, top4_y_topleft,  top4_y_botright, 
           stimulus_name = "popflake", position_name = "top4", x_col = "fixation_point_x", y_col = "fixation_point_y",
           aoi_col = "aoi_fixation") |>
  mark_aoi(name = "center_center", actora_x_topleft, actora_x_botright, actora_y_topleft,  actora_y_botright, 
           stimulus_name = "popflake", position_name = "actora", x_col = "fixation_point_x", y_col = "fixation_point_y",
           aoi_col = "aoi_fixation") |> 
mark_aoi(name = "center_center", actorb_x_topleft, actorb_x_botright, actorb_y_topleft,  actorb_y_botright, 
         stimulus_name = "popflake", position_name = "actorb", x_col = "fixation_point_x", y_col = "fixation_point_y",
         aoi_col = "aoi_fixation") |> 
mark_aoi(name = "center_center", to_x_topleft, to_x_botright, to_y_topleft,  to_y_botright, 
         stimulus_name = "popflake", position_name = "to", x_col = "fixation_point_x", y_col = "fixation_point_y",
         aoi_col = "aoi_fixation")
