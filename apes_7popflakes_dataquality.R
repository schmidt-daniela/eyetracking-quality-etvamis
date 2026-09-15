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
# Popflakes
# topleftflake_x_topleft <- 380 - buffer
# topleftflake_y_topleft <- 170 - buffer
# topleftflake_x_botright <- 580 + buffer
# topleftflake_y_botright <- 370 + buffer
# 
# botleftflake_x_topleft <- 380 - buffer
# botleftflake_y_topleft <- 710 - buffer
# botleftflake_x_botright <- 580 + buffer
# botleftflake_y_botright <- 910 + buffer
# 
# toprightflake_x_topleft <- 1340 - buffer
# toprightflake_y_topleft <- 170 - buffer
# toprightflake_x_botright <- 1540 + buffer
# toprightflake_y_botright <- 370 + buffer
# 
# botrightflake_x_topleft <- 1340 - buffer
# botrightflake_y_topleft <- 710 - buffer
# botrightflake_x_botright <- 1540 + buffer
# botrightflake_y_botright <- 910 + buffer
# 
# centralflake_x_topleft <- 860 - buffer
# centralflake_y_topleft <- 440 - buffer
# centralflake_x_botright <- 1060 + buffer
# centralflake_y_botright <- 640 + buffer

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
