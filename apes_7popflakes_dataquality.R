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

# Function to save summed data as txt and nicely formatted excel file
save_sumdata <- function(df, name) {
  
  # save as txt-file
  write.table(df, file = file.path(output_dir, paste0(name, ".txt")), sep = "\t", row.names = FALSE, quote = FALSE)
  
  # save as Excel file
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheetName = name)
  openxlsx::writeDataTable(wb, sheet = name, x = df, tableStyle = "TableStyleMedium2", withFilter = TRUE)
  openxlsx::freezePane(wb, sheet = name, firstRow = TRUE)
  openxlsx::setColWidths(wb, sheet = name, cols = 1:ncol(df), widths = "auto")
  header_style <- openxlsx::createStyle(textDecoration = "bold", halign = "center", valign = "center")
  openxlsx::addStyle(wb, sheet = name, style = header_style, rows = 1, cols = 1:ncol(df), gridExpand = TRUE, stack = TRUE)
  openxlsx::saveWorkbook(wb, file = file.path(output_dir, paste0(name, ".xlsx")), overwrite = TRUE)
}

# Set Parameters ----------------------------------------------------------
buffer <- 120
species <- "orangs" # "bonobos" or "orangs" or "b_chimps" or "a_chimps" or "chimps"
data_path <- here("data", "data_apes_7popflakes_5p", species)
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
raw <- read.table(here("data", "data_apes_7popflakes_5p", species, "main_data.tsv"), header = TRUE, sep = "\t")
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
df <- df |>
  group_by(recording_name) |>
  mutate(
    recording_timestamp = as.numeric(recording_timestamp),
    gaze_sample_duration = c(0, diff(recording_timestamp) / 1000),
    gaze_sample_duration = if_else(row_number() == n(), NA_real_, gaze_sample_duration)
  ) |>
  ungroup()

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
           into = c("pre_post", "trial_within_session", "stimulus", "position", "duration"), 
           remove = FALSE, sep = "_")

# Add overall trial number across all presented popflakes (not only within one session)
df_2 <- df |>
  mutate(recording_name_2 = str_remove(recording_name, " E\\d+_\\d+$")) |>
  group_by(recording_name_2) |>
  mutate(pre_number = str_extract(presented_media_name, "(?<=^pre_)\\d+"),
    stimulus_id = str_extract(presented_media_name, "(?<=popflake_)[^_]+(?=_(?:2s|30s)\\.mp4$)"),
    trial_id = paste(pre_number, stimulus_id, sep = "_"),
    trial = cumsum(row_number() == 1 | trial_id != lag(trial_id))) |>
  ungroup() |>
  select(-c(recording_name_2, pre_number, stimulus_id, trial_id))

df <- df |> 
  left_join(df_2 |> select(trial, recording_name, presented_media_name) |> distinct(), 
            by = c("recording_name", "presented_media_name"))

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

# Remove "invalid" trials
if(species == "orangs"){
  df <- df |> 
    filter(!(str_detect(recording_name, "Raja") & session == 1 & trial_within_session %in% c("2", "3"))) |> 
    filter(!(str_detect(recording_name, "Raja") & session == 2 & trial_within_session %in% c("1", "2", "3"))) |> 
    filter(!(str_detect(recording_name, "Raja") & session == 6 & trial_within_session %in% c("1", "3", "4", "5", "6", "7"))) |> 
    filter(!(str_detect(recording_name, "Lursa") & session == 1 & trial_within_session %in% c("5", "6", "7"))) |> 
    filter(!(str_detect(recording_name, "Lursa") & session == 2 & trial_within_session %in% c("2", "3"))) |>
    filter(!(str_detect(recording_name, "Lursa") & session == 4 & trial_within_session %in% c("1", "2", "3"))) |> 
    filter(!(str_detect(recording_name, "Lursa") & session == 6 & trial_within_session %in% c("6", "7")))
}

if(species == "bonobos"){
  df <- df |> 
    filter(!(str_detect(recording_name, "Luiza") & session == 1 & trial_within_session =="6")) |> 
    filter(!(str_detect(recording_name, "Luiza") & session == 2 & trial_within_session == "6")) |> 
    filter(!(str_detect(recording_name, "Lexi") & session == 4 & trial_within_session %in% c("1", "2"))) |> 
    filter(!(str_detect(recording_name, "Lexi") & session == 6 & trial_within_session == "5")) |>
    filter(!(str_detect(recording_name, "Gerda") & session == 6 & trial_within_session == "5"))
}

if(species %in% c("chimps", "a_chimps")){
  df <- df |> 
    filter(!(str_detect(recording_name, "Evi") & session == 1 & trial_within_session %in% c("4", "6", "7"))) |> 
    filter(!(str_detect(recording_name, "Evi") & session == 3 & trial_within_session %in% c("1", "2", "3", "4", "5", "6", "7"))) |> 
    filter(!(str_detect(recording_name, "Evi") & session == 4 & trial_within_session %in% c("1", "2", "3", "4", "5", "6", "7"))) |> 
    filter(!(str_detect(recording_name, "Evi") & session == 6 & trial_within_session %in% c("3", "7"))) |> 
    filter(!(str_detect(recording_name, "Swela") & session == 1 & trial_within_session == "2")) |>
    filter(!(str_detect(recording_name, "Swela") & session == 2 & trial_within_session %in% c("1", "2", "3", "4"))) |> 
    filter(!(str_detect(recording_name, "Swela") & session == 3 & trial_within_session %in% c("1", "2", "3", "4", "5", "6"))) |> 
    filter(!(str_detect(recording_name, "Swela") & session == 4 & trial_within_session %in% c("1", "2", "3", "4", "5", "6", "7"))) |> 
    filter(!(str_detect(recording_name, "Swela") & session == 5 & trial_within_session %in% c("2", "3", "4", "6"))) |> 
    filter(!(str_detect(recording_name, "Swela") & session == 6 & trial_within_session %in% c("1", "2", "3", "4", "5", "6", "7")))
}

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
png(here("img", "blink_7popflakes", paste0("blink_1_", species, ".png")), width = 2048, height = 1152, res = 300)
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
png(here("img", "blink_7popflakes", paste0("blink_2_postsmooth_", species, ".png")), width = 2048, height = 1152, res = 300)
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
png(here("img", "blink_7popflakes", paste0("blink_3_postoutlier_", species, ".png")), width = 2048, height = 1152, res = 300)
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

# Top 1
df_acc_top1 <- calculate_accuracy(
  df = df_accuracy |> filter(stimulus == "popflake" & position == "top1"),
  xmin = top1_x_topleft, # includes aoi buffer (but whether buffer is included or not doesn't matter anyway...)
  xmax = top1_x_botright, # includes aoi buffer (... as it is only used to find the stimulus center)
  ymin = top1_y_topleft, # includes aoi buffer
  ymax = top1_y_botright, # includes aoi buffer
  stimulus_vec = "popflake",
  media_col = "stimulus",
  gaze_event_col = "eye_movement_type",
  id_col = "name",
  gaze_event_index_col = "eye_movement_type_index",
  x_fix = "fixation_point_x",
  y_fix = "fixation_point_y",
  trial = "session_trial",
  stimulus_height = 200,
  stimulus_width = 200,
  aoi_buffer_px_x = 120,
  aoi_buffer_px_y = 120
) |> 
  mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
  mutate(stimulus = "popflake") |>
  mutate(position = "top1")

# Top 2
df_acc_top2 <- calculate_accuracy(
  df = df_accuracy |> filter(stimulus == "popflake" & position == "top2"),
  xmin = top2_x_topleft, # includes aoi buffer (but whether buffer is included or not doesn't matter anyway...)
  xmax = top2_x_botright, # includes aoi buffer (... as it is only used to find the stimulus center)
  ymin = top2_y_topleft, # includes aoi buffer
  ymax = top2_y_botright, # includes aoi buffer
  stimulus_vec = "popflake",
  media_col = "stimulus",
  gaze_event_col = "eye_movement_type",
  id_col = "name",
  gaze_event_index_col = "eye_movement_type_index",
  x_fix = "fixation_point_x",
  y_fix = "fixation_point_y",
  trial = "session_trial",
  stimulus_height = 200,
  stimulus_width = 200,
  aoi_buffer_px_x = 120,
  aoi_buffer_px_y = 120
) |> 
  mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
  mutate(stimulus = "popflake") |>
  mutate(position = "top2")

# Top 3
df_acc_top3 <- calculate_accuracy(
  df = df_accuracy |> filter(stimulus == "popflake" & position == "top3"),
  xmin = top3_x_topleft, # includes aoi buffer (but whether buffer is included or not doesn't matter anyway...)
  xmax = top3_x_botright, # includes aoi buffer (... as it is only used to find the stimulus center)
  ymin = top3_y_topleft, # includes aoi buffer
  ymax = top3_y_botright, # includes aoi buffer
  stimulus_vec = "popflake",
  media_col = "stimulus",
  gaze_event_col = "eye_movement_type",
  id_col = "name",
  gaze_event_index_col = "eye_movement_type_index",
  x_fix = "fixation_point_x",
  y_fix = "fixation_point_y",
  trial = "session_trial",
  stimulus_height = 200, 
  stimulus_width = 200, 
  aoi_buffer_px_x = 120,
  aoi_buffer_px_y = 120
) |> 
  mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
  mutate(stimulus = "popflake") |>
  mutate(position = "top3")

# Top 4
df_acc_top4 <- calculate_accuracy(
  df = df_accuracy |> filter(stimulus == "popflake" & position == "top4"),
  xmin = top4_x_topleft, # includes aoi buffer (but whether buffer is included or not doesn't matter anyway...)
  xmax = top4_x_botright, # includes aoi buffer (... as it is only used to find the stimulus center)
  ymin = top4_y_topleft, # includes aoi buffer
  ymax = top4_y_botright, # includes aoi buffer
  stimulus_vec = "popflake",
  media_col = "stimulus",
  gaze_event_col = "eye_movement_type",
  id_col = "name",
  gaze_event_index_col = "eye_movement_type_index",
  x_fix = "fixation_point_x",
  y_fix = "fixation_point_y",
  trial = "session_trial",
  stimulus_height = 200, 
  stimulus_width = 200, 
  aoi_buffer_px_x = 120,
  aoi_buffer_px_y = 120
) |> 
  mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
  mutate(stimulus = "popflake") |>
  mutate(position = "top4")

# Actor A
df_acc_actora <- calculate_accuracy(
  df = df_accuracy |> filter(stimulus == "popflake" & position == "actora"),
  xmin = actora_x_topleft, # includes aoi buffer (but whether buffer is included or not doesn't matter anyway...)
  xmax = actora_x_botright, # includes aoi buffer (... as it is only used to find the stimulus center)
  ymin = actora_y_topleft, # includes aoi buffer
  ymax = actora_y_botright, # includes aoi buffer
  stimulus_vec = "popflake",
  media_col = "stimulus",
  gaze_event_col = "eye_movement_type",
  id_col = "name",
  gaze_event_index_col = "eye_movement_type_index",
  x_fix = "fixation_point_x",
  y_fix = "fixation_point_y",
  trial = "session_trial",
  stimulus_height = 200, 
  stimulus_width = 200, 
  aoi_buffer_px_x = 120,
  aoi_buffer_px_y = 120
) |> 
  mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
  mutate(stimulus = "popflake") |>
  mutate(position = "actora")

# Actor B
df_acc_actorb <- calculate_accuracy(
  df = df_accuracy |> filter(stimulus == "popflake" & position == "actorb"),
  xmin = actorb_x_topleft, # includes aoi buffer (but whether buffer is included or not doesn't matter anyway...)
  xmax = actorb_x_botright, # includes aoi buffer (... as it is only used to find the stimulus center)
  ymin = actorb_y_topleft, # includes aoi buffer
  ymax = actorb_y_botright, # includes aoi buffer
  stimulus_vec = "popflake",
  media_col = "stimulus",
  gaze_event_col = "eye_movement_type",
  id_col = "name",
  gaze_event_index_col = "eye_movement_type_index",
  x_fix = "fixation_point_x",
  y_fix = "fixation_point_y",
  trial = "session_trial",
  stimulus_height = 200, 
  stimulus_width = 200, 
  aoi_buffer_px_x = 120,
  aoi_buffer_px_y = 120
) |> 
  mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
  mutate(stimulus = "popflake") |>
  mutate(position = "actorb")

# TO
df_acc_to <- calculate_accuracy(
  df = df_accuracy |> filter(stimulus == "popflake" & position == "to"),
  xmin = to_x_topleft, # includes aoi buffer (but whether buffer is included or not doesn't matter anyway...)
  xmax = to_x_botright, # includes aoi buffer (... as it is only used to find the stimulus center)
  ymin = to_y_topleft, # includes aoi buffer
  ymax = to_y_botright, # includes aoi buffer
  stimulus_vec = "popflake",
  media_col = "stimulus",
  gaze_event_col = "eye_movement_type",
  id_col = "name",
  gaze_event_index_col = "eye_movement_type_index",
  x_fix = "fixation_point_x",
  y_fix = "fixation_point_y",
  trial = "session_trial",
  stimulus_height = 200,
  stimulus_width = 200,
  aoi_buffer_px_x = 120,
  aoi_buffer_px_y = 120
) |> 
  mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
  mutate(stimulus = "popflake") |>
  mutate(position = "to")

# Merge All Stimuli
df_acc_tot <- df_acc_top1 |>
  bind_rows(
    df_acc_top2,
    df_acc_top3,
    df_acc_top4,
    df_acc_actora,
    df_acc_actorb,
    df_acc_to
  ) |>
  mutate(data_quality = "accuracy")

# Precision (RMS) ----
df_preproc_temp_precisionrms <- df

param_precrms <- data.frame(
  position = c(
    "top1",
    "top2",
    "top3",
    "top4",
    "actora",
    "actorb",
    "to"
  ),
  xmin = c(top1_x_topleft, top2_x_topleft, top3_x_topleft, top4_x_topleft, actora_x_topleft, actorb_x_topleft, to_x_topleft),
  xmax = c(top1_x_botright, top2_x_botright, top3_x_botright, top4_x_botright, actora_x_botright, actorb_x_botright, to_x_botright),
  ymin = c(top1_y_topleft, top2_y_topleft, top3_y_topleft, top4_y_topleft, actora_y_topleft, actorb_y_topleft, to_y_topleft),
  ymax = c(top1_y_botright, top2_y_botright, top3_y_botright, top4_y_botright, actora_y_botright, actorb_y_botright, to_y_botright),
  df_name = c(
    "df_precrms_top1",
    "df_precrms_top2",
    "df_precrms_top3",
    "df_precrms_top4",
    "df_precrms_actora",
    "df_precrms_actorb",
    "df_precrms_to"
  )
)

for (j in c(1:7)) {
  df_precrms_temp <- calculate_precision_rms(
    df_preproc_temp_precisionrms |> filter(stimulus == "popflake" & position == param_precrms$position[j]),
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "recording_name",
    stimulus_vec = "popflake",
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
      df_preproc_temp_precisionrms |> select(stimulus, position, session_trial, recording_name) |> distinct(),
      by = c("session_trial", "recording_name")
    ) |>
    drop_na(precrms_visd)
  
  assign(param_precrms$df_name[j], df_precrms_temp)
  rm(df_precrms_temp)
}

df_precrms_tot <- df_precrms_top1 |>
  bind_rows(
    df_precrms_top2,
    df_precrms_top3,
    df_precrms_top4,
    df_precrms_actora,
    df_precrms_actorb,
    df_precrms_to
  ) |>
  mutate(data_quality = "precisionrms") |>
  drop_na(precrms_visd)

rm(df_precrms_top1, df_precrms_top2, df_precrms_top3, df_precrms_top4,
   df_precrms_actora, df_precrms_actorb, df_precrms_to, df_preproc_temp_precisionrms)

# Precision (SD) ----
df_preproc_temp_precisionsd <- df

param_precsd <- data.frame(
  position = c(
    "top1",
    "top2",
    "top3",
    "top4",
    "actora",
    "actorb",
    "to"
  ),
  xmin = c(top1_x_topleft, top2_x_topleft, top3_x_topleft, top4_x_topleft, actora_x_topleft, actorb_x_topleft, to_x_topleft),
  xmax = c(top1_x_botright, top2_x_botright, top3_x_botright, top4_x_botright, actora_x_botright, actorb_x_botright, to_x_botright),
  ymin = c(top1_y_topleft, top2_y_topleft, top3_y_topleft, top4_y_topleft, actora_y_topleft, actorb_y_topleft, to_y_topleft),
  ymax = c(top1_y_botright, top2_y_botright, top3_y_botright, top4_y_botright, actora_y_botright, actorb_y_botright, to_y_botright),
  df_name = c(
    "df_precsd_top1",
    "df_precsd_top2",
    "df_precsd_top3",
    "df_precsd_top4",
    "df_precsd_actora",
    "df_precsd_actorb",
    "df_precsd_to"
  )
)

for (j in c(1:7)) {
  df_precsd_temp <- calculate_precision_sd(
    df_preproc_temp_precisionsd |> filter(stimulus == "popflake" & position == param_precsd$position[j]),
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "recording_name",
    stimulus_vec = "popflake",
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
      df_preproc_temp_precisionsd |> select(stimulus, position, session_trial, recording_name) |> distinct(),
      by = c("session_trial", "recording_name")
    ) |>
    drop_na(precsd_visd)
  
  assign(param_precsd$df_name[j], df_precsd_temp)
  rm(df_precsd_temp)
}

df_precsd_tot <- df_precsd_top1 |>
  bind_rows(
    df_precsd_top2,
    df_precsd_top3,
    df_precsd_top4,
    df_precsd_actora,
    df_precsd_actorb,
    df_precsd_to
  ) |>
  mutate(data_quality = "precisionsd") |>
  drop_na(precsd_visd)

rm(df_precsd_top1, df_precsd_top2, df_precsd_top3, df_precsd_top4,
   df_precsd_actora, df_precsd_actorb, df_precsd_to, df_preproc_temp_precisionsd)

# Robustness ----
# The values saved in truncate are the minimum "Max cum_duration overall" that is
# shown when running the calculate_robustness_2-function.
if(species == "bonobos"){truncate <- 95955}
if(species == "orangs"){truncate <- 45605}
if(species == "b_chimps"){truncate <- 136313} 
if(species == "a_chimps"){truncate <- 19032} 
if(species == "chimps"){truncate <- 19032}

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

# Trials ----
presented_trials <- df |> 
  filter(trial == max(trial)) |> 
  pull(trial) |> 
  unique()

acc_trials_5p <- df_acc_tot |>
  count(name, name = "valid_trials") |> 
  mutate(presented_trials = presented_trials)

precrms_trials_5p <- df_precrms_tot |>
  mutate(name = recording_name |> 
           str_remove(" E\\d+_\\d+$") |>
           str_to_lower()) |> 
  select(name, session_trial) |> 
  distinct() |> 
  count(name, name = "valid_trials") |> 
  mutate(presented_trials = presented_trials)

precsd_trials_5p <- df_precsd_tot |>
  mutate(name = recording_name |> 
           str_remove(" E\\d+_\\d+$") |>
           str_to_lower()) |> 
  select(name, session_trial) |> 
  distinct() |> 
  count(name, name = "valid_trials") |> 
  mutate(presented_trials = presented_trials)

# Summed Up Data ----
# Accuracy
df_acc_pertrial <- df_acc_tot |> 
  rename(acc_pixel = accuracy,
         stimulus_position = position) |> 
  left_join(acc_trials_5p, by = "name") |> 
  mutate(calibration_points = "5",
         study = "et_vamis") |> 
  select(session_trial, name, acc_pixel, acc_visd, stimulus, stimulus_position,
         data_quality, calibration_points, valid_trials, presented_trials, study)

df_acc_perid <- df_acc_pertrial |>
  group_by(name, stimulus, data_quality, calibration_points, valid_trials, presented_trials, study) |> 
  summarize(acc_pixel = mean(acc_pixel, na.rm = T), 
            acc_visd = mean(acc_visd, na.rm = T))

# Precision (RMS)
df_precrms_pertrial <- df_precrms_tot |>
  rename(precrms_pixel = precrms,
         stimulus_position = position,
         name = recording_name) |> 
  mutate(name = name |> 
           str_remove(" E\\d+_\\d+$") |>
           str_to_lower()) |> 
  mutate(calibration_points = "5",
         study = "et_vamis") |> 
  left_join(acc_trials_5p, by = "name") |> 
  group_by(session_trial, name, stimulus, stimulus_position, data_quality, calibration_points, valid_trials, presented_trials, study) |> 
  summarize(precrms_pixel = mean(precrms_pixel, na.rm = T), 
            precrms_visd = mean(precrms_visd, na.rm = T)) |> 
  ungroup() |> 
  select(session_trial, name, precrms_pixel, precrms_visd, stimulus, stimulus_position, data_quality, calibration_points, valid_trials, presented_trials, study)

df_precrms_perid <- df_precrms_pertrial |>
  group_by(name, stimulus, data_quality, calibration_points, valid_trials, presented_trials, study) |> 
  summarize(precrms_pixel = mean(precrms_pixel, na.rm = T), 
            precrms_visd = mean(precrms_visd, na.rm = T))

# Precision (SD)
df_precsd_pertrial <- df_precsd_tot |>
  rename(precsd_pixel = precsd,
         stimulus_position = position,
         name = recording_name) |> 
  mutate(name = name |> 
           str_remove(" E\\d+_\\d+$") |>
           str_to_lower()) |> 
  mutate(calibration_points = "5",
         study = "et_vamis") |> 
  left_join(acc_trials_5p, by = "name") |> 
  group_by(session_trial, name, stimulus, stimulus_position, data_quality, calibration_points, valid_trials, presented_trials, study) |> 
  summarize(precsd_pixel = mean(precsd_pixel, na.rm = T), 
            precsd_visd = mean(precsd_visd, na.rm = T)) |> 
  ungroup() |> 
  select(session_trial, name, precsd_pixel, precsd_visd, stimulus, stimulus_position, data_quality, calibration_points, valid_trials, presented_trials, study)

df_precsd_perid <- df_precsd_pertrial |>
  group_by(name, stimulus, data_quality, calibration_points, valid_trials, presented_trials, study) |> 
  summarize(precsd_pixel = mean(precsd_pixel, na.rm = T), 
            precsd_visd = mean(precsd_visd, na.rm = T))

# Robustness
df_rob_perid <- df_robustness |> 
  mutate(data_quality = "robustness",
         calibration_points = "5",
         study = "et_vamis")

# Save all four datasets
output_dir <- here("data", "sumdata_apes_7popflakes_5p", species)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

save_sumdata(df_acc_pertrial,      "sumdata_accuracy_pertrial")
save_sumdata(df_acc_perid,      "sumdata_accuracy_perid")
save_sumdata(df_precrms_pertrial,  "sumdata_precisionrms_pertrial")
save_sumdata(df_precrms_perid,  "sumdata_precisionrms_perid")
save_sumdata(df_precsd_pertrial,   "sumdata_precisionsd_pertrial")
save_sumdata(df_precsd_perid,   "sumdata_precisionsd_perid")
save_sumdata(df_rob_perid,      "sumdata_robustness")

# Print Descriptives ----
cat("SUMMARY\n", 
    "[1] Accuracy. Mean:", round(mean(df_acc_pertrial$acc_visd), 2),"- Standard Deviation:", round(sd(df_acc_pertrial$acc_visd), 2), "\n",
    "[2] Precision (RMS). Mean:", round(mean(df_precrms_pertrial$precrms_visd), 2),"- Standard Deviation:", round(sd(df_precrms_pertrial$precrms_visd), 2), "\n",
    "[3] Precision (SD). Mean:", round(mean(df_precsd_pertrial$precsd_visd), 2), "- Standard Deviation:", round(sd(df_precsd_pertrial$precsd_visd), 2), "\n",
    "[4] Robustness. Mean:", round(mean(df_rob_perid$robustness_prop_2), 2), "- Standard Deviation:", round(sd(df_rob_perid$robustness_prop_2), 2), "\n")


