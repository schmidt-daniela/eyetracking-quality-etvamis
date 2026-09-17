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
data_path <- here("data", "data_apes_7popflakes", species)
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

# Make position values to lower case
df$position <- tolower(as.character(df$position))

# Based on Fixations
df$aoi_fixation <- "not_in_aoi"

df <- df |>
  mark_aoi(name = "top1", x_min = top1_x_topleft, x_max = top1_x_botright, y_min = top1_y_topleft, y_max = top1_y_botright,
           stimulus_name = "popflake", position_name = "top1", x_col = "fixation_point_x", y_col = "fixation_point_y",
           aoi_col = "aoi_fixation") |>
  mark_aoi(name = "top2", top2_x_topleft, top2_x_botright, top2_y_topleft, top2_y_botright,
           stimulus_name = "popflake", position_name = "top2", x_col = "fixation_point_x", y_col = "fixation_point_y",
           aoi_col = "aoi_fixation") |>
  mark_aoi(name = "top3", top3_x_topleft, top3_x_botright, top3_y_topleft, top3_y_botright, 
           stimulus_name = "popflake", position_name = "top3", x_col = "fixation_point_x", y_col = "fixation_point_y",
           aoi_col = "aoi_fixation") |>
  mark_aoi(name = "top4",  top4_x_topleft, top4_x_botright, top4_y_topleft,  top4_y_botright, 
           stimulus_name = "popflake", position_name = "top4", x_col = "fixation_point_x", y_col = "fixation_point_y",
           aoi_col = "aoi_fixation") |>
  mark_aoi(name = "actora", actora_x_topleft, actora_x_botright, actora_y_topleft,  actora_y_botright, 
           stimulus_name = "popflake", position_name = "actora", x_col = "fixation_point_x", y_col = "fixation_point_y",
           aoi_col = "aoi_fixation") |> 
mark_aoi(name = "actorb", actorb_x_topleft, actorb_x_botright, actorb_y_topleft,  actorb_y_botright, 
         stimulus_name = "popflake", position_name = "actorb", x_col = "fixation_point_x", y_col = "fixation_point_y",
         aoi_col = "aoi_fixation") |> 
mark_aoi(name = "to", to_x_topleft, to_x_botright, to_y_topleft,  to_y_botright, 
         stimulus_name = "popflake", position_name = "to", x_col = "fixation_point_x", y_col = "fixation_point_y",
         aoi_col = "aoi_fixation")

# Define AOIs (Based on Gaze Samples)
df$aoi_samples <- "not_in_aoi"

df <- df |>
  mark_aoi(name = "top1", x_min = top1_x_topleft, x_max = top1_x_botright, y_min = top1_y_topleft, y_max = top1_y_botright,
           stimulus_name = "popflake", position_name = "top1", x_col = "gaze_point_x", y_col = "gaze_point_y",
           aoi_col = "aoi_samples") |>
  mark_aoi(name = "top2", top2_x_topleft, top2_x_botright, top2_y_topleft, top2_y_botright,
           stimulus_name = "popflake", position_name = "top2", x_col = "gaze_point_x", y_col = "gaze_point_y",
           aoi_col = "aoi_samples") |>
  mark_aoi(name = "top3", top3_x_topleft, top3_x_botright, top3_y_topleft, top3_y_botright, 
           stimulus_name = "popflake", position_name = "top3", x_col = "gaze_point_x", y_col = "gaze_point_y",
           aoi_col = "aoi_samples") |>
  mark_aoi(name = "top4",  top4_x_topleft, top4_x_botright, top4_y_topleft,  top4_y_botright, 
           stimulus_name = "popflake", position_name = "top4", x_col = "gaze_point_x", y_col = "gaze_point_y",
           aoi_col = "aoi_samples") |>
  mark_aoi(name = "actora", actora_x_topleft, actora_x_botright, actora_y_topleft,  actora_y_botright, 
           stimulus_name = "popflake", position_name = "actora", x_col = "gaze_point_x", y_col = "gaze_point_y",
           aoi_col = "aoi_samples") |> 
  mark_aoi(name = "actorb", actorb_x_topleft, actorb_x_botright, actorb_y_topleft,  actorb_y_botright, 
           stimulus_name = "popflake", position_name = "actorb", x_col = "gaze_point_x", y_col = "gaze_point_y",
           aoi_col = "aoi_samples") |> 
  mark_aoi(name = "to", to_x_topleft, to_x_botright, to_y_topleft,  to_y_botright, 
           stimulus_name = "popflake", position_name = "to", x_col = "gaze_point_x", y_col = "gaze_point_y",
           aoi_col = "aoi_samples")

# Identify whether at least one fixation within AOI
fixation_in_aoi <- df |>
  filter(eye_movement_type == "Fixation") |>
  select(name, session_trial, position, aoi_fixation) |>
  filter(aoi_fixation != "not_in_aoi") |>
  distinct() |>
  select(name, session_trial, position) |>
  mutate(excluded_fixation = "included")

df <- df |>
  left_join(fixation_in_aoi, by = c("name", "session_trial", "position")) |>
  mutate(excluded_fixation = replace_na(excluded_fixation, "excluded"))

# Time within session
df <- df |> 
  group_by(recording_name) |> 
  mutate(timeline_experiment = cumsum(gaze_sample_duration)) |> 
  ungroup()

# Blink Detection ----
df <- df |> 
  mutate(pupil_diameter_left = as.numeric(str_replace(pupil_diameter_left, ",", "."))) |> 
  mutate(pupil_diameter_right = as.numeric(str_replace(pupil_diameter_right, ",", ".")))

# Pre Smoothing
png(here("img", "blink_7popflakes", paste0("blink_1", ".png")), width = 2048, height = 1152, res = 300)
p1 <- ggplot(df |>
               mutate(time = cumsum(c(0, diff(recording_timestamp)))) |> 
               mutate(time = cumsum(time)/1000),
             aes(x = time, y = pupil_diameter_left)) + 
  geom_point() + 
  geom_line(colour="black") +
  ylim(1,7)
print(p1)
dev.off()

# Smooth Data
df <- df |> 
  mutate(pupil_diameter_left = moving_average_pupil(pupil_diameter_left, n = 10))

## Save Plot (Post Smoothing)
png(here("img", "blink_7popflakes", paste0("blink_2_postsmooth", ".png")), width = 2048, height = 1152, res = 300)
p2 <- ggplot(df |>
               mutate(time = cumsum(c(0, diff(recording_timestamp)))) |> 
               mutate(time = cumsum(time)/1000),
             aes(x = time, y = pupil_diameter_left)) + 
  geom_point() + 
  geom_line(colour="black") +
  ylim(1,7)
print(p2)
dev.off()

# Interpolate Outliers
df <- interpolate_outliers(df = df, pupil_left_col  = "pupil_diameter_left", pupil_right_col = "pupil_diameter_right", n_sd = 3)
df <- df |> rowwise() |>  mutate(pupil_diameter_average = mean(c(pupil_diameter_left, pupil_diameter_right), na.rm = T)) |> ungroup()

## Save Plot (Post Outlierinterpolation)
png(here("img", "blink_7popflakes", paste0("blink_3_postoutlier", ".png")), width = 2048, height = 1152, res = 300)
p3 <- ggplot(df |>
               mutate(time = cumsum(c(0, diff(recording_timestamp)))) |> 
               mutate(time = cumsum(time)/1000),
             aes(x = time, y = pupil_diameter_left)) + 
  geom_point() + 
  geom_line(colour="black") +
  ylim(1,7)
print(p3)
dev.off()

# Add Velocity
df <- add_pupil_velocity(df = df, timestamp_col = "recording_timestamp", timestamp_unit = "s", 
                         pupil_left_col = "pupil_diameter_left", pupil_right_col = "pupil_diameter_right")

# Add Onset Offset of NA Chains
df <- mark_na_chain_onset_offset(df = df, col = "pupil_diameter_left", onset_col = "pupil_na_onset.left", offset_col = "pupil_na_offset.left", min_run = 2)
df <- mark_na_chain_onset_offset(df = df, col = "pupil_diameter_right", onset_col = "pupil_na_onset.right", offset_col = "pupil_na_offset.right", min_run = 2)

# Add Velocity Threshold + Evaluate Whether It Was Crossed
df <- detect_velocity_thresholds(df = df, vel_left_col  = "Velocity.left", vel_right_col = "Velocity.right",
                                 center = "median", n_sd = 0.1, onset_col_left   = "threshold_onset.left", 
                                 onset_col_right  = "threshold_onset.right", offset_col_left  = "threshold_offset.left",
                                 offset_col_right = "threshold_offset.right")

# Detect Blinks Based on Onset of Threshold-Crossing
df <- detect_blinks_from_onset_offset(df = df, pupil_left_col = "pupil_diameter_left", pupil_right_col = "pupil_diameter_right",
                                      onset_col_left = "threshold_onset.left", onset_col_right = "threshold_onset.right",
                                      offset_col_left = "threshold_offset.left", offset_col_right = "threshold_offset.right",
                                      onset_value = "onset", offset_value = "offset",
                                      blink_col_left = "blink_detection.left", blink_col_right = "blink_detection.right",
                                      blink_value = "blink", lookback = 5, lookahead = 5)

# Exclude Too Short/ Long Blinks
df <- filter_blinks_by_duration(df = df, blink_col = "blink_detection.left", blink_value = "blink",
                                timestamp_col  = "recording_timestamp",  # assumed in ms
                                min_ms = 10, max_ms = 400)
df <- filter_blinks_by_duration(df = df, blink_col = "blink_detection.right", blink_value = "blink",
                                timestamp_col  = "recording_timestamp",  # assumed in ms
                                min_ms = 10, max_ms = 400)

# Visualize Detected Blinks
plots <- plot_blinks(
  df = df,
  eye = "both",
  flank_n = 10,
  min_run = 2,
  time_col  = "timeline_trial_tot",
  pupil_left_col = "pupil_diameter_left",
  pupil_right_col = "pupil_diameter_right",
  blink_col_left  = "blink_detection.left",
  blink_col_right = "blink_detection.right",
  title_cols = c("recording_name", "session_trial"),
  trial_col = "session_trial"
)
save_detected_blinks_pdf(plots, out_dir = here("img", "blink_7popflakes"), df)

# Cleaning II ----
# Correction of Fixation Durations
# Why? Because gaze_event_duration refers to the duration of a fixation, irrespective of
# whether the duration was within one trial or across two trials. The analyses, however,
# are conducted on a trial level. Therefore, we need the fixation duration in one trial
# and cut off the duration of the fixation in a previous or subsequent trial.
df <- df |>
  group_by(name, session_trial, eye_movement_type_index, eye_movement_type) |>
  mutate(gaze_event_duration_revised = sum(gaze_sample_duration)) |>
  ungroup()

# Accuracy ----
df_accuracy <- df

# Exclude Trials Without Fixation in Target AOI
df_accuracy <- df_accuracy |> filter(excluded_fixation == "included")

