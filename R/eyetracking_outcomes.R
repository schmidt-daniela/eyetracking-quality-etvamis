# Fixation Duration ----
calculate_fixdur <- function(df, gaze_event_col = "Eye.movement.type", media_col = "Presented.Media.name",
                             stimulus_vec = c("ATTENTION_Familiarization.mp4", "ATTENTION_Preflooking.mp4"),
                             id_col = "Recording.name", gaze_event_index_col = "Eye.movement.type.index", trial = "trial",
                             gaze_event_dur_col = "Gaze.event.duration", x_fix = "Fixation.point.X", y_fix = "Fixation.point.Y",
                             screen_height_min = 0, screen_width_min = 0,
                             screen_height_max = 1080, screen_width_max = 1920, aoi_buffer_px_x = 40, aoi_buffer_px_y = 40,
                             off_exclude = F, longest_fix_only = F){
  
  if(off_exclude == T){
    exclude_rows <- which(df[[x_fix]] < screen_width_min - aoi_buffer_px_x | df[[x_fix]] > screen_width_max + aoi_buffer_px_x |
                            df[[y_fix]] < screen_height_min - aoi_buffer_px_x | df[[y_fix]] > screen_height_max + aoi_buffer_px_y)
    if(exclude_rows |> length() > 0){df <- df[-exclude_rows,]} # checked, works
  }
  
  if(longest_fix_only == T){
    df <- df |> 
      filter(.data[[gaze_event_col]] == "Fixation") |> 
      group_by(.data[[id_col]], .data[[trial]]) |>
      filter(.data[[gaze_event_dur_col]] == max(.data[[gaze_event_dur_col]], na.rm = T)) |> 
      ungroup() # manually checked for one data example
  }
  
  df <- df |> 
    filter(.data[[gaze_event_col]] == "Fixation") |> 
    filter(.data[[media_col]] %in% stimulus_vec) |> 
    select({{media_col}}, {{id_col}}, {{gaze_event_index_col}}, {{gaze_event_dur_col}}, {{trial}}) |> 
    distinct() |>     
    group_by(.data[[id_col]], .data[[trial]]) |> 
    summarize(mean_fixation_duration = mean(.data[[gaze_event_dur_col]], na.rm = T)) |> 
    as.data.frame() # necessary to avoid rounding # manually checked for two data examples
}

# Fixation Number ----
calculate_fixnum <- function(df, gaze_event_col = "Eye.movement.type", media_col = "Presented.Media.name",
                             stimulus_vec = c("ATTENTION_Familiarization.mp4", "ATTENTION_Preflooking.mp4"),
                             id_col = "Recording.name", gaze_event_index_col = "Eye.movement.type.index", trial = "trial",
                             gaze_event_dur_col = "Gaze.event.duration", x_fix = "Fixation.point.X", y_fix = "Fixation.point.Y",
                             screen_height_min = 0, screen_width_min = 0,
                             screen_height_max = 1080, screen_width_max = 1920, aoi_buffer_px_x = 40, aoi_buffer_px_y = 40,
                             off_exclude = F){
  
  if(off_exclude == T){
    exclude_rows <- which(df[[x_fix]] < screen_width_min - aoi_buffer_px_x | df[[x_fix]] > screen_width_max + aoi_buffer_px_x |
                            df[[y_fix]] < screen_height_min - aoi_buffer_px_x | df[[y_fix]] > screen_height_max + aoi_buffer_px_y)
    if(exclude_rows |> length() > 0){df <- df[-exclude_rows,]}
  }
  
  df <- df |> 
    filter(.data[[gaze_event_col]] == "Fixation") |> 
    filter(.data[[media_col]] %in% stimulus_vec) |> 
    select({{media_col}}, {{id_col}}, {{gaze_event_index_col}}, {{trial}}) |> 
    distinct() |> 
    group_by(.data[[id_col]])
  
  df <- df |> 
    group_by(.data[[id_col]], .data[[trial]]) |> 
    count() # manually checked for the first five n's/ data examples
}

# Looking Time in AOI ----
calculate_ltaoi <- function(df, media_col = "Presented.Media.name", stimulus_vec = c("ATTENTION_Familiarization.mp4", "ATTENTION_Preflooking.mp4"),
                            rectime = "Recording.timestamp", id_col = "Recording.name", trial = "trial",
                            x_fix = "Fixation.point.X", y_fix = "Fixation.point.Y", x = "Gaze.point.X", y = "Gaze.point.Y", 
                            aoi_left_upper = c(849-40, 417-40), aoi_right_lower = c(1072+40, 672+40), is_00_upleftcorner = T){
  
  if(is_00_upleftcorner == F){
    stop("This function only works for eye-tracking data where the (0,0) coordinate is in the upper left corner.
          If this is the case, please change the argument to is_00_upleftcorner = T.")
  }

  # Fixation points in AOI? Gaze samples in AOI?
  df$fix_in_aoi <- ifelse(df |> pull({{x_fix}}) >= aoi_left_upper[1] & df |> pull({{x_fix}}) <= aoi_right_lower[1] & 
                            df |> pull({{y_fix}}) >= aoi_left_upper[2] & df |> pull({{y_fix}}) <= aoi_right_lower[2], "in", "out")
  df$gaze_in_aoi <- ifelse(df |> pull({{x}}) >= aoi_left_upper[1] & df |> pull({{x}}) <= aoi_right_lower[1] & 
                            df |> pull({{y}}) >= aoi_left_upper[2] & df |> pull({{y}}) <= aoi_right_lower[2], "in", "out")
  
  df <- df |> 
    filter(.data[[media_col]] %in% stimulus_vec)
  
  df <- df |> 
    group_by(.data[[id_col]], .data[[trial]]) |> 
    mutate(GazeSampleDuration = c(diff(.data[[rectime]]), NA) / 1000) |> 
    slice(-n()) |> 
    ungroup()
  
  # Calculate LT in AOI (based on gaze samples)
  df_gaze <- df |> 
    group_by(.data[[id_col]], .data[[trial]]) |> 
    summarize(abs_gaze_in_aoi_duration = sum(GazeSampleDuration[gaze_in_aoi == "in"], na.rm = T),
              abs_gaze_out_aoi_duration = sum(GazeSampleDuration[gaze_in_aoi == "out"], na.rm = T)) |> 
    mutate(abs_gaze_recorded_duration = abs_gaze_in_aoi_duration + abs_gaze_out_aoi_duration) |> 
    mutate(rel_gaze_in_aoi = abs_gaze_in_aoi_duration / abs_gaze_recorded_duration) # manually checked Recording 1, trial 1
  colnames(df_gaze)[1] <- "id" 
  
  # Calculate LT in AOI (based on fixations)
  df_fix <- df |> 
    group_by(.data[[id_col]], .data[[trial]]) |> 
    summarize(abs_fix_in_aoi_duration = sum(GazeSampleDuration[fix_in_aoi == "in"], na.rm = T),
              abs_fix_out_aoi_duration = sum(GazeSampleDuration[fix_in_aoi == "out"], na.rm = T)) |> 
    mutate(abs_fix_recorded_duration = abs_fix_in_aoi_duration + abs_fix_out_aoi_duration) |> 
    mutate(rel_fix_in_aoi = abs_fix_in_aoi_duration / abs_fix_recorded_duration)
  colnames(df_fix)[1] <- "id" 
  
  list_all <- list(df_gaze, df_fix)
  
  return(list_all)
}
