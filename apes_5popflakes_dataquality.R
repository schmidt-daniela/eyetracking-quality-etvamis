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
species <- "b_chimps" # "bonobos" or "orangs" or "bonobos2" or "b_chimps"
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
topleftflake_x_topleft <- 380 - buffer
topleftflake_y_topleft <- 170 - buffer
topleftflake_x_botright <- 580 + buffer
topleftflake_y_botright <- 370 + buffer

botleftflake_x_topleft <- 380 - buffer
botleftflake_y_topleft <- 710 - buffer
botleftflake_x_botright <- 580 + buffer
botleftflake_y_botright <- 910 + buffer

toprightflake_x_topleft <- 1340 - buffer
toprightflake_y_topleft <- 170 - buffer
toprightflake_x_botright <- 1540 + buffer
toprightflake_y_botright <- 370 + buffer

botrightflake_x_topleft <- 1340 - buffer
botrightflake_y_topleft <- 710 - buffer
botrightflake_x_botright <- 1540 + buffer
botrightflake_y_botright <- 910 + buffer

centralflake_x_topleft <- 860 - buffer
centralflake_y_topleft <- 440 - buffer
centralflake_x_botright <- 1060 + buffer
centralflake_y_botright <- 640 + buffer

# Read Data ---------------------------------------------------------------
raw <- read.table(here("data", "data_apes_5popflakes", species, "main_data.tsv"), header = TRUE, sep = "\t")
df <- raw

# Correct Naming Mistake --------------------------------------------------
df <- df  |> 
  mutate(Presented.Stimulus.name = str_trim(Presented.Stimulus.name)) # removes Leerzeichen in beginning or end

df <- df |>
  mutate(
    Recording.name = if (species == "b_chimps") {
      case_match(
        Recording.name, # rename recording names to make it consistent with bonobos and orangs
        "Alex"      ~ "CalibrationCheck_Alex_M_Session1",
        "Daza"      ~ "CalibrationCheck_Daza_F_Session1",
        "Frederike" ~ "CalibrationCheck_Frederike_F_Session1",
        "Hope"      ~ "CalibrationCheck_Hope_F_Session1",
        "Jeudi"     ~ "CalibrationCheck_Jeudi_F_Session1",
        "Zira"      ~ "CalibrationCheck_Zira_F_Session1",
        .default    = Recording.name # if there are other names, they won't change
      )
    } else {
      Recording.name # don't do anything, if species is not b_chimps
    }
  )

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
            mouse_position_x,mouse_position_y)) # variable not of interest

# Filter checkflakes Only
df <- df |> 
  filter(str_detect(presented_stimulus_name, "checkflake"))

# Prepare stimulus information
df <- df |>
  separate(presented_stimulus_name,
           into = c("pre_post", "trial", "stimulus", "duration", "position_y", "position_x"), 
           remove = FALSE, sep = "_") 

# Create session column
df$session <- sub(".*_", "", df$recording_name)
df$session <- tolower(sub(".*_", "", df$recording_name))

# Create session_trial column
df <- df |> 
  unite(col = "session_trial", session, trial, sep = "_")

# Remove "invalid" trials
if(species == "bonobos2"){
  df <- df |> 
    filter(!(str_detect(recording_name, "Lexi") & session_trial %in% c("session3_6", "session4_2", "session4_3")))
}

# Create position column
df <- df |>
  unite(position, position_y, position_x, sep = "_", remove = FALSE)

# Add cumulative duration per trial
df <- df |> 
  group_by(recording_name, session_trial, duration) |> 
  mutate(timeline_trial_units = cumsum(gaze_sample_duration)) |> 
  group_by(recording_name, session_trial) |> 
  mutate(timeline_trial_tot = cumsum(gaze_sample_duration)) |> 
  ungroup()

# Add name of individual
df <- df |> 
  mutate(name = str_split_i(recording_name, "_", 2) |> tolower())

# Define AOIs 
df <- df |>
  mutate(gaze_point_x = as.numeric(gaze_point_x),
         gaze_point_y = as.numeric(gaze_point_y)) |>
  mutate(fixation_point_x = as.numeric(fixation_point_x),
         fixation_point_y = as.numeric(fixation_point_y))

# Based on Fixations
df$aoi_fixation <- "not_in_aoi"

df <- df |>
  mark_aoi(name = "top_left", x_min = topleftflake_x_topleft, x_max = topleftflake_x_botright, y_min = topleftflake_y_topleft, y_max = topleftflake_y_botright,
           stimulus_name = "checkflake", position_name = "top_left", x_col = "fixation_point_x", y_col = "fixation_point_y",
           aoi_col = "aoi_fixation") |>
  mark_aoi(name = "bot_left", botleftflake_x_topleft, botleftflake_x_botright, botleftflake_y_topleft, botleftflake_y_botright,
           stimulus_name = "checkflake", position_name = "bot_left", x_col = "fixation_point_x", y_col = "fixation_point_y",
           aoi_col = "aoi_fixation") |>
  mark_aoi(name = "top_right", toprightflake_x_topleft, toprightflake_x_botright, toprightflake_y_topleft, toprightflake_y_botright, 
           stimulus_name = "checkflake", position_name = "top_right", x_col = "fixation_point_x", y_col = "fixation_point_y",
           aoi_col = "aoi_fixation") |>
  mark_aoi(name = "bot_right",  botrightflake_x_topleft, botrightflake_x_botright, botrightflake_y_topleft,  botrightflake_y_botright, 
           stimulus_name = "checkflake", position_name = "bot_right", x_col = "fixation_point_x", y_col = "fixation_point_y",
           aoi_col = "aoi_fixation") |>
  mark_aoi(name = "center_center", centralflake_x_topleft, centralflake_x_botright, centralflake_y_topleft,  centralflake_y_botright, 
           stimulus_name = "checkflake", position_name = "center_center", x_col = "fixation_point_x", y_col = "fixation_point_y",
           aoi_col = "aoi_fixation")

# Define AOIs (Based on Gaze Samples)
df$aoi_samples <- "not_in_aoi"

df <- df |>
  mark_aoi(name = "top_left", topleftflake_x_topleft, topleftflake_x_botright, topleftflake_y_topleft, topleftflake_y_botright,
           stimulus_name = "checkflake", position_name = "top_left", x_col = "gaze_point_x", y_col = "gaze_point_y",
           aoi_col = "aoi_samples") |>
  mark_aoi(name = "bot_left", botleftflake_x_topleft, botleftflake_x_botright, botleftflake_y_topleft, botleftflake_y_botright,
           stimulus_name = "checkflake", position_name = "bot_left", x_col = "gaze_point_x", y_col = "gaze_point_y",
           aoi_col = "aoi_samples") |>
  mark_aoi(name = "top_right", toprightflake_x_topleft, toprightflake_x_botright, toprightflake_y_topleft, toprightflake_y_botright, 
           stimulus_name = "checkflake", position_name = "top_right", x_col = "gaze_point_x", y_col = "gaze_point_y",
           aoi_col = "aoi_samples") |>
  mark_aoi(name = "bot_right",  botrightflake_x_topleft, botrightflake_x_botright, botrightflake_y_topleft,  botrightflake_y_botright, 
           stimulus_name = "checkflake", position_name = "bot_right", x_col = "gaze_point_x", y_col = "gaze_point_y",
           aoi_col = "aoi_samples") |>
  mark_aoi(name = "center_center", centralflake_x_topleft, centralflake_x_botright, centralflake_y_topleft,  centralflake_y_botright, 
           stimulus_name = "checkflake", position_name = "center_center", x_col = "gaze_point_x", y_col = "gaze_point_y",
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
png(here("img", "blink", paste0("blink_1_", ".png")), width = 2048, height = 1152, res = 300)
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
png(here("img", "blink", paste0("blink_2_postsmooth", ".png")), width = 2048, height = 1152, res = 300)
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
png(here("img", "blink", paste0("blink_3_postoutlier", ".png")), width = 2048, height = 1152, res = 300)
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
  title_cols = c("participant_name", "recording_name", "session_trial"),
  trial_col = "session_trial"
)
save_detected_blinks_pdf(plots, out_dir = here("img", "blink"), df)

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

# Popflake Top Left
df_acc_poptopleft <- calculate_accuracy(
  df = df_accuracy |> filter(stimulus == "checkflake" & position %in% "top_left"),
  xmin = 260 + 120 - buffer, # first number contained aoi buffer of 120px
  xmax = 700 - 120 + buffer, # first number contained aoi buffer of 120px
  ymin = 50 + 120 - buffer, # first number contained aoi buffer of 120px
  ymax = 490 - 120 + buffer, # first number contained aoi buffer of 120px
  stimulus_vec = "checkflake",
  media_col = "stimulus",
  gaze_event_col = "eye_movement_type",
  id_col = "recording_name",
  gaze_event_index_col = "eye_movement_type_index",
  x_fix = "fixation_point_x",
  y_fix = "fixation_point_y",
  trial = "session_trial",
  stimulus_height = 200 + (2 * buffer), # contains aoi buffer of 120px
  stimulus_width = 200 + (2 * buffer), # contains aoi buffer of 120px
  aoi_buffer_px_x = 0, 
  aoi_buffer_px_y = 0 
) |> 
  mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
  mutate(stimulus = "checkflake") |>
  mutate(position = "top_left")

# Popflake Top Right
df_acc_poptopright <- calculate_accuracy(
  df_accuracy |> filter(stimulus == "checkflake" & position == "top_right"),
  xmin = 1220 + 120 - buffer, # first number contained aoi buffer of 120px
  xmax = 1660 - 120 + buffer, # first number contained aoi buffer of 120px
  ymin = 50 + 120 - buffer, # first number contained aoi buffer of 120px
  ymax = 490 - 120 + buffer, # first number contained aoi buffer of 120px
  stimulus_vec = "checkflake",
  media_col = "stimulus",
  gaze_event_col = "eye_movement_type",
  id_col = "recording_name",
  gaze_event_index_col = "eye_movement_type_index",
  x_fix = "fixation_point_x",
  y_fix = "fixation_point_y",
  trial = "session_trial",
  stimulus_height = 200 + (2 * buffer), # contains aoi buffer of 120px
  stimulus_width = 200 + (2 * buffer), # contains aoi buffer of 120px
  aoi_buffer_px_x = 0,
  aoi_buffer_px_y = 0
) |> 
  mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
  mutate(stimulus = "checkflake") |>
  mutate(position = "top_right")

# Popflake Bottom Left
df_acc_popbotleft <- calculate_accuracy(
  df_accuracy |> filter(stimulus == "checkflake" & position == "bot_left"),
  xmin = 260 + 120 - buffer, # first number contained aoi buffer of 120px
  xmax = 700 - 120 + buffer, # first number contained aoi buffer of 120px
  ymin = 590 + 120 - buffer, # first number contained aoi buffer of 120px
  ymax = 1030 - 120 + buffer, # first number contained aoi buffer of 120px
  stimulus_vec = "checkflake",
  media_col = "stimulus",
  gaze_event_col = "eye_movement_type",
  id_col = "recording_name",
  gaze_event_index_col = "eye_movement_type_index",
  x_fix = "fixation_point_x",
  y_fix = "fixation_point_y",
  trial = "session_trial",
  stimulus_height = 200 + (2 * buffer), # contains aoi buffer of 120px
  stimulus_width = 200 + (2 * buffer), # contains aoi buffer of 120px
  aoi_buffer_px_x = 0,
  aoi_buffer_px_y = 0
) |> 
  mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
  mutate(stimulus = "checkflake") |>
  mutate(position = "bot_left")

# Popflake Bottom Right
df_acc_popbotright <- calculate_accuracy(
  df_accuracy |> filter(stimulus == "checkflake" & position == "bot_right"),
  xmin = 1220 + 120 - buffer, # first number contained aoi buffer of 120px
  xmax = 1660 - 120 + buffer, # first number contained aoi buffer of 120px
  ymin = 590 + 120 - buffer, # first number contained aoi buffer of 120px
  ymax = 1030 - 120 + buffer, # first number contained aoi buffer of 120px
  stimulus_vec = "checkflake",
  media_col = "stimulus",
  gaze_event_col = "eye_movement_type",
  id_col = "recording_name",
  gaze_event_index_col = "eye_movement_type_index",
  x_fix = "fixation_point_x",
  y_fix = "fixation_point_y",
  trial = "session_trial",
  stimulus_height = 200 + (2 * buffer), # contains aoi buffer of 120px
  stimulus_width = 200 + (2 * buffer), # contains aoi buffer of 120px
  aoi_buffer_px_x = 0, 
  aoi_buffer_px_y = 0 
) |> 
  mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
  mutate(stimulus = "checkflake") |>
  mutate(position = "bot_right")

# Popflake Center
df_acc_popcenter <- calculate_accuracy(
  df_accuracy |> filter(stimulus == "checkflake" & position == "center_center"),
  xmin = 740 + 120 - buffer, # first number contained aoi buffer of 120px
  xmax = 1180 - 120 + buffer, # first number contained aoi buffer of 120px
  ymin = 320 + 120 - buffer, # first number contained aoi buffer of 120px
  ymax = 760 - 120 + buffer, # first number contained aoi buffer of 120px
  stimulus_vec = "checkflake",
  media_col = "stimulus",
  gaze_event_col = "eye_movement_type",
  id_col = "recording_name",
  gaze_event_index_col = "eye_movement_type_index",
  x_fix = "fixation_point_x",
  y_fix = "fixation_point_y",
  trial = "session_trial",
  stimulus_height = 200 + (2 * buffer), # contains aoi buffer of 120px
  stimulus_width = 200 + (2 * buffer), # contains aoi buffer of 120px
  aoi_buffer_px_x = 0, 
  aoi_buffer_px_y = 0 
) |> 
  mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
  mutate(stimulus = "checkflake") |>
  mutate(position = "center_center")

# Merge All Stimuli
df_acc_tot <- df_acc_poptopleft |>
  bind_rows(
    df_acc_poptopright,
    df_acc_popbotleft,
    df_acc_popbotright,
    df_acc_popcenter
  ) |>
  mutate(data_quality = "accuracy")

# Plot Accuracy ----
df_plot_acc <- df_acc_tot |>
  separate(recording_name, into = c("experiment_unit", "name", "sex", "session"), sep = "_") |>
  separate(session_trial, into = c("session", "trial"), sep = "_") |> 
  mutate(name = str_to_lower(name)) |> 
  group_by(name) |> 
  summarise(
    acc_visd = mean(acc_visd, na.rm = TRUE),
    .groups = "drop"
  )

if(species %in% c("a_chimps", "b_chimps")){
  ref_mean <- 4.11
  ref_sd   <- 0.83
  ref_n    <- 17
  ref_se   <- ref_sd / sqrt(ref_n)
  ref_ci   <- ref_se * qt(0.975, df = ref_n - 1)
  
  df_ref <- data.frame(
    species = paste0("2P_Chimps_FACET"),
    mean    = ref_mean,
    sd = ref_sd,
    sd = ref_sd,
    lower   = ref_mean - ref_ci,
    upper   = ref_mean + ref_ci
  )
}

if(species %in% c("orangs")){
  ref_mean <- 3.36
  ref_sd   <- 1.19
  ref_n    <- 6
  ref_se   <- ref_sd / sqrt(ref_n)
  ref_ci   <- ref_se * qt(0.975, df = ref_n - 1)
  
  df_ref <- data.frame(
    species = paste0("2P_Orangs_REJOINTComp"),
    mean    = ref_mean,
    sd = ref_sd,
    lower   = ref_mean - ref_ci,
    upper   = ref_mean + ref_ci
  )
}

if(species %in% c("bonobos", "bonobos2")){
  ref_mean <- 3.15
  ref_sd   <- 1.01
  ref_n    <- 9
  ref_se   <- ref_sd / sqrt(ref_n)
  ref_ci   <- ref_se * qt(0.975, df = ref_n - 1)
  
  df_ref <- data.frame(
    species = paste0("2P_Bonobos_REJOINTComp"),
    mean    = ref_mean,
    sd = ref_sd,
    lower   = ref_mean - ref_ci,
    upper   = ref_mean + ref_ci
  )
}

df_plot_acc <- df_plot_acc |>
  mutate(species = paste0("5P_", str_to_title(.env$species), "_ETVamis"))

ggplot_acc <- ggplot(df_plot_acc, aes(x = species, y = acc_visd)) +
  geom_jitter(
    width = 0.04,
    height = 0,
    size = 1.5,
    alpha = 0.9,
    color = plot_color,
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3.2,
    color = "black"
  ) +
  stat_summary(
    fun.data = mean_cl_normal,
    geom = "errorbar",
    width = 0.08,
    linewidth = 0.8,
    color = "black"
  ) +
  geom_point( # manual value from prior 2-point calibration studies
    data = df_ref,
    aes(x = species, y = mean),
    size = 3.2,
    color = "black",
    inherit.aes = FALSE
  ) +
  geom_errorbar(
    data = df_ref,
    aes(x = species, y = mean, ymin = lower, ymax = upper),
    width = 0.08,
    linewidth = 0.8,
    color = "black",
    inherit.aes = FALSE
  ) +
  scale_y_continuous(
    breaks = c(0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5, 6.0, 6.5, 7.0),
    limits = c(0, 7)
  ) +
  labs(
    x = NULL,
    y = "Accuracy\nin visual degrees"
  ) +
  theme_classic(base_size = 18) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 16),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16)
  )

png(here("img", paste0(species, "_acc_paperplot.png")), width = 2480/2, height = 3508/4, res = 100)
ggplot_acc
dev.off()

print(paste0("ACCURACY. Mean: ", df_plot_acc |> summarize(M = round(mean(acc_visd, na.rm = T),2)),
             "; SD: ", df_plot_acc |> summarize(M = round(sd(acc_visd, na.rm = T),2)), ". ",
             "(", species_label, ")"))

dq_2p <- df_ref |> 
  mutate(dq = "accuracy") |> 
  mutate(n = df_plot_acc |> nrow())

# Precision (RMS) ----
df_preproc_temp_precisionrms <- df

param_precrms <- data.frame(
  position = c(
    "top_left",
    "bot_left",
    "top_right",
    "bot_right",
    "center_center"
  ),
  xmin = c(380 - buffer, 380 - buffer, 1340 - buffer, 1340 - buffer, 860 -
             buffer),
  xmax = c(580 + buffer, 580 + buffer, 1540 + buffer, 1540 + buffer, 1060 +
             buffer),
  ymin = c(170 - buffer, 710 - buffer, 170 - buffer, 710 - buffer, 440 -
             buffer),
  ymax = c(370 + buffer, 910 + buffer, 370 + buffer, 910 + buffer, 640 +
             buffer),
  df_name = c(
    "df_precrms_poptopleft",
    "df_precrms_popbotleft",
    "df_precrms_poptopright",
    "df_precrms_popbotright",
    "df_precrms_popcenter"
  )
)

for (j in c(1:5)) {
  df_precrms_temp <- calculate_precision_rms(
    df_preproc_temp_precisionrms |> filter(stimulus == "checkflake" &
                                             position == param_precrms$position[j]),
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "recording_name",
    stimulus_vec = "checkflake",
    gaze_event_index_col = "eye_movement_type_index",
    gaze_event_dur_col = "gaze_event_duration_revised",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    x = "gaze_point_x",
    y = "gaze_point_y",
    trial = "session_trial",
    screen_height_min = 0 - buffer,
    screen_width_min = 0 - buffer,
    screen_height_max = 1080 + buffer,
    screen_width_max = 1920 + buffer,
    aoi_buffer_px_x = 0,
    aoi_buffer_px_y = 0,
    xmin = param_precrms$xmin[j],
    xmax = param_precrms$xmax[j],
    ymin = param_precrms$ymin[j],
    ymax = param_precrms$ymax[j],
    off_exclude = TRUE,
    longest_fix_only = FALSE,
    AOI_only = FALSE
  ) |>
    mutate(precrms_visd = precrms * onepx_in_visd(60, 92)) |>
    left_join(
      df_preproc_temp_precisionrms |> select(stimulus, position, session_trial) |> distinct(),
      by = "session_trial"
    ) |>
    drop_na(precrms_visd)
  
  assign(param_precrms$df_name[j], df_precrms_temp)
  rm(df_precrms_temp)
}

df_precrms_tot <- df_precrms_poptopleft |>
  bind_rows(
    df_precrms_popbotleft,
    df_precrms_poptopright,
    df_precrms_popbotright,
    df_precrms_popcenter
  ) |>
  mutate(data_quality = "precisionrms") |>
  drop_na(precrms_visd)

rm(df_precrms_popbotleft, df_precrms_popbotright, df_precrms_popcenter, df_precrms_poptopleft,
  df_precrms_poptopright, df_preproc_temp_precisionrms)

# Plot Precision (RMS) ----
df_plot_precrms <- df_precrms_tot |>
  separate(recording_name, into = c("experiment_unit", "name", "sex", "session"), sep = "_") |>
  separate(session_trial, into = c("session", "trial"), sep = "_") |> 
  mutate(name = str_to_lower(name)) |> 
  group_by(name, session, trial) |> 
  summarise(
    precrms_visd = mean(precrms_visd, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  group_by(name) |> 
  summarise(
    precrms_visd = mean(precrms_visd, na.rm = TRUE),
    .groups = "drop"
  )

if(species %in% c("a_chimps", "b_chimps")){
  ref_mean <- 0.52
  ref_sd   <- 0.33
  ref_n    <- 17
  ref_se   <- ref_sd / sqrt(ref_n)
  ref_ci   <- ref_se * qt(0.975, df = ref_n - 1)
  
  df_ref <- data.frame(
    species = paste0("2P_Chimps_FACET"),
    mean    = ref_mean,
    sd = ref_sd,
    lower   = ref_mean - ref_ci,
    upper   = ref_mean + ref_ci
  )
}

if(species %in% c("orangs")){
  ref_mean <- 0.22
  ref_sd   <- 0.09
  ref_n    <- 6
  ref_se   <- ref_sd / sqrt(ref_n)
  ref_ci   <- ref_se * qt(0.975, df = ref_n - 1)
  
  df_ref <- data.frame(
    species = paste0("2P_Orangs_REJOINTComp"),
    mean    = ref_mean,
    sd = ref_sd,
    lower   = ref_mean - ref_ci,
    upper   = ref_mean + ref_ci
  )
}

if(species %in% c("bonobos", "bonobos2")){
  ref_mean <- 0.37
  ref_sd   <- 0.19
  ref_n    <- 9
  ref_se   <- ref_sd / sqrt(ref_n)
  ref_ci   <- ref_se * qt(0.975, df = ref_n - 1)
  
  df_ref <- data.frame(
    species = paste0("2P_Bonobos_REJOINTComp"),
    mean    = ref_mean,
    sd = ref_sd,
    lower   = ref_mean - ref_ci,
    upper   = ref_mean + ref_ci
  )
}

df_plot_precrms <- df_plot_precrms |>
  mutate(species = paste0("5P_", str_to_title(.env$species), "_ETVamis"))

ggplot_precrms <- ggplot(df_plot_precrms, aes(x = species, y = precrms_visd)) +
  geom_jitter(
    width = 0.04,
    height = 0,
    size = 1.5,
    alpha = 0.9,
    color = plot_color,
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3.2,
    color = "black"
  ) +
  stat_summary(
    fun.data = mean_cl_normal,
    geom = "errorbar",
    width = 0.08,
    linewidth = 0.8,
    color = "black"
  ) +
  geom_point( # manual value from prior 2-point calibration studies
    data = df_ref,
    aes(x = species, y = mean),
    size = 3.2,
    color = "black",
    inherit.aes = FALSE
  ) +
  geom_errorbar(
    data = df_ref,
    aes(x = species, y = mean, ymin = lower, ymax = upper),
    width = 0.08,
    linewidth = 0.8,
    color = "black",
    inherit.aes = FALSE
  ) +
  scale_y_continuous(
    breaks = c(0, 0.5, 1.0, 1.5, 2.0),
    limits = c(0, 2)
  ) +
  labs(
    x = NULL,
    y = "Precision (RMS)\nin visual degrees"
  ) +
  theme_classic(base_size = 18) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 16),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16)
  )

png(here("img", paste0(species, "_precrms_paperplot.png")), width = 2480/2, height = 3508/4, res = 100)
ggplot_precrms
dev.off()

dq_2p <- dq_2p |> 
  bind_rows(df_ref |> mutate(dq = "precision_rms")) |> 
  mutate(n = df_plot_precrms |> nrow())

# Precision (SD) ----
df_preproc_temp_precisionsd <- df

param_precsd <- data.frame(
  position = c(
    "top_left",
    "bot_left",
    "top_right",
    "bot_right",
    "center_center"
  ),
  xmin = c(380 - buffer, 380 - buffer, 1340 - buffer, 1340 - buffer, 860 - buffer),
  xmax = c(580 + buffer, 580 + buffer, 1540 + buffer, 1540 + buffer, 1060 + buffer),
  ymin = c(170 - buffer, 710 - buffer, 170 - buffer, 710 - buffer, 440 - buffer),
  ymax = c(370 + buffer, 910 + buffer, 370 + buffer, 910 + buffer, 640 + buffer),
  df_name = c(
    "df_precsd_poptopleft",
    "df_precsd_popbotleft",
    "df_precsd_poptopright",
    "df_precsd_popbotright",
    "df_precsd_popcenter"
  )
)

for (j in c(1:5)) {
  df_precsd_temp <- calculate_precision_sd(
    df_preproc_temp_precisionsd |> filter(stimulus == "checkflake" &
                                            position == param_precsd$position[j]),
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "recording_name",
    stimulus_vec = "checkflake",
    gaze_event_index_col = "eye_movement_type_index",
    gaze_event_dur_col = "gaze_event_duration_revised",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    x = "gaze_point_x",
    y = "gaze_point_y",
    trial = "session_trial",
    screen_height_min = 0 - buffer,
    screen_width_min = 0 - buffer,
    screen_height_max = 1080 + buffer,
    screen_width_max = 1920 + buffer,
    aoi_buffer_px_x = 0,
    aoi_buffer_px_y = 0,
    xmin = param_precsd$xmin[j],
    xmax = param_precsd$xmax[j],
    ymin = param_precsd$ymin[j],
    ymax = param_precsd$ymax[j],
    off_exclude = TRUE,
    longest_fix_only = FALSE,
    AOI_only = FALSE
  ) |>
    mutate(precsd_visd = precsd * onepx_in_visd(60, 92)) |>
    left_join(
      df_preproc_temp_precisionsd |> select(stimulus, position, session_trial) |> distinct(),
      by = "session_trial"
    ) |>
    drop_na(precsd_visd)
  
  assign(param_precsd$df_name[j], df_precsd_temp)
  rm(df_precsd_temp)
}

df_precsd_tot <- df_precsd_poptopleft |>
  bind_rows(
    df_precsd_popbotleft,
    df_precsd_poptopright,
    df_precsd_popbotright,
    df_precsd_popcenter
  ) |>
  mutate(data_quality = "precisionsd") |>
  drop_na(precsd_visd)

rm(df_precsd_popbotleft, df_precsd_popbotright, df_precsd_popcenter, df_precsd_poptopleft,
   df_precsd_poptopright, df_preproc_temp_precisionsd)

# Plot Precision (SD) ----
df_plot_precsd <- df_precsd_tot |>
  separate(recording_name, into = c("experiment_unit", "name", "sex", "session"), sep = "_") |>
  separate(session_trial, into = c("session", "trial"), sep = "_") |> 
  mutate(name = str_to_lower(name)) |> 
  group_by(name, session, trial) |> 
  summarise(
    precsd_visd = mean(precsd_visd, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  group_by(name) |> 
  summarise(
    precsd_visd = mean(precsd_visd, na.rm = TRUE),
    .groups = "drop"
  )

if(species %in% c("a_chimps", "b_chimps")){
  ref_mean <- 0.36
  ref_sd   <- 0.27
  ref_n    <- 17
  ref_se   <- ref_sd / sqrt(ref_n)
  ref_ci   <- ref_se * qt(0.975, df = ref_n - 1)
  
  df_ref <- data.frame(
    species = paste0("2P_Chimps_FACET"),
    mean    = ref_mean,
    sd = ref_sd,
    lower   = ref_mean - ref_ci,
    upper   = ref_mean + ref_ci
  )
}

if(species %in% c("orangs")){
  ref_mean <- 0.14
  ref_sd   <- 0.09
  ref_n    <- 6
  ref_se   <- ref_sd / sqrt(ref_n)
  ref_ci   <- ref_se * qt(0.975, df = ref_n - 1)
  
  df_ref <- data.frame(
    species = paste0("2P_Orangs_REJOINTComp"),
    mean    = ref_mean,
    sd = ref_sd,
    lower   = ref_mean - ref_ci,
    upper   = ref_mean + ref_ci
  )
}

if(species %in% c("bonobos", "bonobos2")){
  ref_mean <- 0.25
  ref_sd   <- 0.15
  ref_n    <- 9
  ref_se   <- ref_sd / sqrt(ref_n)
  ref_ci   <- ref_se * qt(0.975, df = ref_n - 1)
  
  df_ref <- data.frame(
    species = paste0("2P_Bonobos_REJOINTComp"),
    mean    = ref_mean,
    sd = ref_sd,
    lower   = ref_mean - ref_ci,
    upper   = ref_mean + ref_ci
  )
}

df_plot_precsd <- df_plot_precsd |>
  mutate(species = paste0("5P_", str_to_title(.env$species), "_ETVamis"))

ggplot_precsd <- ggplot(df_plot_precsd, aes(x = species, y = precsd_visd)) +
  geom_jitter(
    width = 0.04,
    height = 0,
    size = 1.5,
    alpha = 0.9,
    color = plot_color,
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3.2,
    color = "black"
  ) +
  stat_summary(
    fun.data = mean_cl_normal,
    geom = "errorbar",
    width = 0.08,
    linewidth = 0.8,
    color = "black"
  ) +
  geom_point( # manual value from prior 2-point calibration studies
    data = df_ref,
    aes(x = species, y = mean),
    size = 3.2,
    color = "black",
    inherit.aes = FALSE
  ) +
  geom_errorbar(
    data = df_ref,
    aes(x = species, y = mean, ymin = lower, ymax = upper),
    width = 0.08,
    linewidth = 0.8,
    color = "black",
    inherit.aes = FALSE
  ) +
  scale_y_continuous(
    breaks = c(-0.5, 0, 0.5, 1.0, 1.5, 2.0),
    limits = c(-0.5, 2)
  ) +
  labs(
    x = NULL,
    y = "Precision (SD)\nin visual degrees"
  ) +
  theme_classic(base_size = 18) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 16),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16)
  )

png(here("img", paste0(species, "_precsd_paperplot.png")), width = 2480/2, height = 3508/4, res = 100)
ggplot_precsd
dev.off()

dq_2p <- dq_2p |> 
  bind_rows(df_ref |> mutate(dq = "precision_sd")) |> 
  mutate(n = df_plot_precsd |> nrow())

# Robustness ----
if(species %in% c("bonobos","bonobos2")){truncate <- 42739}
if(species == "orangs"){truncate <- 32129}
if(species == "b_chimps"){truncate <- 13698} # to make it comparable with FACET

df_robustness <- df |> 
  group_by(name) |> 
  group_modify(~ calculate_robustness_2(
    df                      = .x,
    trial_col               = "session_trial",
    gaze_x_col              = "gaze_point_x",
    gaze_y_col              = "gaze_point_y",
    sample_duration_col     = "gaze_sample_duration",
    blink_left_col          = "blink_detection.left",
    blink_right_col         = "blink_detection.right",
    blink_removal           = TRUE,
    blink_label             = "blink",
    blink_replacement_value = 99999,
    robustness_check_col    = "robustness_check",
    cum_duration_col        = "cum_duration",
    truncate_at_t_ms        = truncate,
    print_max_cum           = TRUE
  )) |> 
  ungroup() |> 
  mutate(robustness_prop_2 = robustness_ms_2 / truncate)

# Plot Robustness ----
df_plot_rob <- df_robustness |>
  mutate(
    species = paste0("5P_", str_to_title(.env$species), "_ETVamis"),
    rob_prop = robustness_prop_2
  )

if (species %in% c("a_chimps", "b_chimps")) {
  ref_mean <- 0.03
  ref_sd   <- 0.02
  ref_n    <- 17
  ref_se   <- ref_sd / sqrt(ref_n)
  ref_ci   <- ref_se * qt(0.975, df = ref_n - 1)
  
  df_ref <- data.frame(
    species = "2P_Chimps_FACET",
    mean    = ref_mean,
    sd = ref_sd,
    lower   = ref_mean - ref_ci,
    upper   = ref_mean + ref_ci
  )
}

if (species %in% c("orangs")) {
  ref_mean <- 0.02
  ref_sd   <- 0.0099
  ref_n    <- 6
  ref_se   <- ref_sd / sqrt(ref_n)
  ref_ci   <- ref_se * qt(0.975, df = ref_n - 1)
  
  df_ref <- data.frame(
    species = "2P_Orangs_REJOINTComp",
    mean    = ref_mean,
    sd = ref_sd,
    lower   = ref_mean - ref_ci,
    upper   = ref_mean + ref_ci
  )
}

if (species %in% c("bonobos", "bonobos2")) {
  ref_mean <- 0.02
  ref_sd   <- 0.02
  ref_n    <- 9
  ref_se   <- ref_sd / sqrt(ref_n)
  ref_ci   <- ref_se * qt(0.975, df = ref_n - 1)
  
  df_ref <- data.frame(
    species = "2P_Bonobos_REJOINTComp",
    mean    = ref_mean,
    sd = ref_sd,
    lower   = ref_mean - ref_ci,
    upper   = ref_mean + ref_ci
  )
}

ggplot_rob <- ggplot(df_plot_rob, aes(x = species, y = rob_prop)) +
  geom_jitter(
    width = 0.04,
    height = 0,
    size = 1.5,
    alpha = 0.9,
    color = plot_color
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3.2,
    color = "black"
  ) +
  stat_summary(
    fun.data = mean_cl_normal,
    geom = "errorbar",
    width = 0.08,
    linewidth = 0.8,
    color = "black"
  ) +
  geom_point(
    data = df_ref,
    aes(x = species, y = mean),
    size = 3.2,
    color = "black",
    inherit.aes = FALSE
  ) +
  geom_errorbar(
    data = df_ref,
    aes(x = species, y = mean, ymin = lower, ymax = upper),
    width = 0.08,
    linewidth = 0.8,
    color = "black",
    inherit.aes = FALSE
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.08, by = 0.02),
    limits = c(0, 0.08)
  ) +
  labs(
    x = NULL,
    y = "Robustness\n(proportion)"
  ) +
  theme_classic(base_size = 18) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 16),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16)
  )

png(here("img", paste0(species, "_robustness_paperplot.png")), width = 2480 / 2, height = 3508 / 4, res = 100)
ggplot_rob
dev.off()

# Summed Up Data ----
dq_2p <- dq_2p |> 
  bind_rows(df_ref |> mutate(dq = "robustness")) |> 
  mutate(points = "2") |> 
  mutate(n = df_plot_rob |> nrow())

dq_5p <- bind_rows(
  calc_dq(df_plot_acc,     acc_visd,      "accuracy"),
  calc_dq(df_plot_precrms, precrms_visd, "precision_rms"),
  calc_dq(df_plot_precsd,  precsd_visd,  "precision_sd"),
  calc_dq(df_plot_rob,     robustness_prop_2,     "robustness")
) |> 
  mutate(points = "5") |> 
  mutate(n = df$name |> unique() |> length())

dq_overall <- dq_2p |> 
  bind_rows(dq_5p)

# Save summed up data
write.table(dq_overall, here("data", "sumdata_apes_5popflakes", species, "dq_overall.txt"), sep = "\t",
  row.names = FALSE, quote = FALSE)

# Valid Trials ----
acc_trials <- df_acc_tot |>
  mutate(individual = str_extract(recording_name, "(?<=CalibrationCheck_)[A-Za-z]+")) |>
  count(individual, name = "n_trials")
write.table(acc_trials, here("data", "sumdata_apes_5popflakes", species, "acc_trials.txt"), sep = "\t",
            row.names = FALSE, quote = FALSE)

precrms_trials <- df_precrms_tot |>
  mutate(individual = str_extract(recording_name, "(?<=CalibrationCheck_)[A-Za-z]+")) |>
  select(individual, session_trial) |>
  distinct() |>
  count(individual, name = "n_trials")
write.table(precrms_trials, here("data", "sumdata_apes_5popflakes", species, "precrms_trials.txt"), sep = "\t",
            row.names = FALSE, quote = FALSE)

precsd_trials <- df_precsd_tot |>
  mutate(individual = str_extract(recording_name, "(?<=CalibrationCheck_)[A-Za-z]+")) |>
  select(individual, session_trial) |>
  distinct() |>
  count(individual, name = "n_trials")
write.table(precsd_trials, here("data", "sumdata_apes_5popflakes", species, "precsd_trials.txt"), sep = "\t",
            row.names = FALSE, quote = FALSE)

# Presented Trials ----
# df |>
#   select(recording_name, session_trial, stimulus, position) |>
#   mutate(recording_name = tolower(recording_name)) |>
#   distinct() |>
#   group_by(recording_name) |>
#   count() |>
#   ungroup()
