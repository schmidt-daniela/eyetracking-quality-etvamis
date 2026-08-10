## Remove Outliers ----
interpolate_outliers <- function(df,
                                 pupil_left_col  = "Pupil.diameter.left",
                                 pupil_right_col = "Pupil.diameter.right",
                                 n_sd = 3) { # reference: Pedrotti et al. (2011) (10.3758/s13428-010-0055-7) & Michel et al. (2021)
  stopifnot(is.numeric(n_sd), n_sd > 0)
  
  # function to mask outliers for a single numeric vector
  interpolate_outliers_vec <- function(x, n_sd) {
    x <- as.numeric(x)
    mu <- mean(x, na.rm = TRUE)
    s  <- sd(x, na.rm = TRUE)
    
    # If sd is NA or zero (constant/too few data), do nothing
    if (is.na(s) || s == 0) return(x)
    
    lim <- n_sd * s
    out_idx <- !is.na(x) & ( (x < mu - lim) | (x > mu + lim) )
    
    # Interpolate outliers
    y <- x
    y[out_idx] <- NA_real_
    
    idx <- seq_along(y)
    keep <- which(!is.na(y))  # positions of non-outliers
    if (length(keep) >= 2) {
      y_interp <- approx(x = keep, y = y[keep], xout = idx, method = "linear", rule = 1)$y # interpolation
      y[out_idx] <- y_interp[out_idx]
    }
    return(y)
  }
  
  # apply to each pupil column independently
  df[[pupil_left_col]]  <- interpolate_outliers_vec(df[[pupil_left_col]],  n_sd)
  df[[pupil_right_col]] <- interpolate_outliers_vec(df[[pupil_right_col]], n_sd)
  
  return(df)
}

## Calculate Pupil Velocity ----
add_pupil_velocity <- function(df,
                               timestamp_col = "Recording.timestamp",
                               timestamp_unit = c("s", "ms")[2], # is time stamp in seconds or milliseconds (Tobii default: ms)
                               pupil_left_col  = "Pupil.diameter.left",
                               pupil_right_col = "Pupil.diameter.right") {
  
  # Convert time differences to seconds based on declared unit
  div <- ifelse(timestamp_unit == "ms", 1e3, 1e6)
  dt  <- c(NA, diff(as.numeric(df[[timestamp_col]]))) / div
  
  # Calculate velocity
  Ls <- df[[pupil_left_col]]
  Rs <- df[[pupil_right_col]]
  vL <- c(NA, diff(Ls)) / dt
  vR <- c(NA, diff(Rs)) / dt
  
  # Add velocity (and speed) column to df
  df |> 
    mutate(
      Velocity.left  = vL, # + = dilation, - = constriction
      Velocity.right = vR,
      Speed.left     = abs(vL),
      Speed.right    = abs(vR)
    )
}

## Identify Potential Blink Onsets and Offsets ----
mark_na_chain_onset_offset <- function(df,
                                       col     = "Pupil.diameter.left",
                                       onset_col = "Pupil.NA.onset.left",
                                       offset_col = "Pupil.NA.offset.left",
                                       min_run = 2) {
  x <- df[[col]]
  n <- length(x)
  
  na_flags <- is.na(x)
  r <- rle(na_flags)
  ends   <- cumsum(r$lengths)
  starts <- c(1, head(ends, -1) + 1)
  
  lab_on  <- rep(NA_character_, n)
  lab_off <- rep(NA_character_, n)
  
  for (i in seq_along(r$lengths)) {
    if (r$values[i] && r$lengths[i] >= min_run) {   # this run is NA and long enough
      s <- starts[i]          # first NA index in the run
      e <- ends[i]            # last  NA index in the run
      p <- s - 1              # index of number before the run  (onset)
      q <- e + 1              # index of number after  the run  (offset)
      
      # only mark if both neighbors exist and are numbers
      if (p >= 1 && q <= n && !is.na(x[p]) && !is.na(x[q])) {
        lab_on[p]  <- "onset"
        lab_off[q] <- "offset"
      }
    }
  }
  
  df[[onset_col]]  <- lab_on
  df[[offset_col]] <- lab_off
  return(df)
}

## Identify Blinks Based on Velocity ----
detect_velocity_thresholds <- function(
    df,
    vel_left_col  = "Velocity.left",
    vel_right_col = "Velocity.right",
    center = c("mean","median"),
    n_sd = 3,
    onset_col_left   = "threshold_onset.left",
    onset_col_right  = "threshold_onset.right",
    offset_col_left  = "threshold_offset.left",
    offset_col_right = "threshold_offset.right"
){
  center <- match.arg(center)
  center_fun <- if (center == "mean") base::mean else stats::median
  
  vL <- as.numeric(df[[vel_left_col]])
  vL[is.nan(vL)] <- NA_real_
  vL[is.infinite(vL)] <- NA_real_
  vR <- as.numeric(df[[vel_right_col]])
  vR[is.nan(vR)] <- NA_real_
  vR[is.infinite(vR)] <- NA_real_
  
  # functions to determine threshold from negative values (onset) / positive values (offset)
  neg_thr <- function(v){
    vv <- v[!is.na(v) & v < 0]
    if (length(vv) < 2) return(NA_real_)
    t <- center_fun(vv, na.rm = TRUE) - n_sd * sd(vv, na.rm = TRUE)
    if (is.na(t)) NA_real_ else min(t, 0) 
  }
  pos_thr <- function(v){
    vv <- v[!is.na(v) & v > 0]
    if (length(vv) < 2) return(NA_real_)
    t <- center_fun(vv, na.rm = TRUE) + n_sd * sd(vv, na.rm = TRUE)
    if (is.na(t)) NA_real_ else max(t, 0)
  }
  
  thrL_neg <- neg_thr(vL)
  thrR_neg <- neg_thr(vR)
  thrL_pos <- pos_thr(vL)
  thrR_pos <- pos_thr(vR)
  
  # flag if either eye crosses its respective threshold
  onset_L  <- (!is.na(vL) & !is.na(thrL_neg) & vL < thrL_neg)
  onset_R  <- (!is.na(vR) & !is.na(thrR_neg) & vR < thrR_neg)
  offset_L <- (!is.na(vL) & !is.na(thrL_pos) & vL > thrL_pos)
  offset_R <- (!is.na(vR) & !is.na(thrR_pos) & vR > thrR_pos)
  
  df[[onset_col_left]]   <- ifelse(onset_L,  "onset",  NA_character_)
  df[[onset_col_right]]  <- ifelse(onset_R,  "onset",  NA_character_)
  df[[offset_col_left]]  <- ifelse(offset_L, "offset", NA_character_)
  df[[offset_col_right]] <- ifelse(offset_R, "offset", NA_character_)
  
  return(df)
}

# Detect Blinks Based on Onset of Threshold-Crossing
detect_blinks_from_onset_offset <- function(
    df,
    pupil_left_col  = "Pupil.diameter.left",
    pupil_right_col = "Pupil.diameter.right",
    onset_col_left  = "threshold_onset.left",
    onset_col_right = "threshold_onset.right",
    offset_col_left = "threshold_offset.left",
    offset_col_right= "threshold_offset.right",
    onset_value = "onset",
    offset_value = "offset",
    blink_col_left  = "blink_detection.left",
    blink_col_right = "blink_detection.right",
    blink_value = "blink",
    min_na_run = 2,
    lookback = 10,
    lookahead = 10
){
  mark_one_eye <- function(pupil_col, onset_col, offset_col, blink_col) {
    n <- nrow(df)
    out <- rep(NA_character_, n)
    
    x <- df[[pupil_col]]
    na_flags <- is.na(x)
    
    r <- rle(na_flags)
    ends <- cumsum(r$lengths)
    starts <- c(1L, head(ends, -1L) + 1L)
    
    onset_flag  <- !is.na(df[[onset_col]])  & as.character(df[[onset_col]])  == onset_value
    offset_flag <- !is.na(df[[offset_col]]) & as.character(df[[offset_col]]) == offset_value
    
    for (i in which(r$values & r$lengths >= min_na_run)) {
      s <- starts[i]; e <- ends[i]
      
      pre_start  <- max(1L, s - lookback)
      pre_end    <- max(1L, s - 1L)
      post_start <- min(n, e + 1L)
      post_end   <- min(n, e + lookahead)
      
      pre  <- if (pre_start <= pre_end) pre_start:pre_end else integer(0)
      post <- if (post_start <= post_end) post_start:post_end else integer(0)
      
      has_onset  <- length(pre)  > 0 && any(onset_flag[pre],  na.rm = TRUE)
      has_offset <- length(post) > 0 && any(offset_flag[post], na.rm = TRUE)
      
      if (has_onset && has_offset) out[s:e] <- blink_value
    }
    
    df[[blink_col]] <<- out
    invisible(NULL)
  }
  
  mark_one_eye(pupil_left_col,  onset_col_left,  offset_col_left,  blink_col_left)
  mark_one_eye(pupil_right_col, onset_col_right, offset_col_right, blink_col_right)
  
  df
}


# detect_blinks_from_onset <- function(
#     df,
#     pupil_left_col   = "Pupil.diameter.left",
#     pupil_right_col  = "Pupil.diameter.right",
#     onset_col_left   = "threshold_onset.left",
#     onset_col_right  = "threshold_onset.right",
#     onset_value      = "onset",
#     blink_col_left   = "blink_detection.left",
#     blink_col_right  = "blink_detection.right",
#     blink_value      = "blink"
# ){
#   n <- nrow(df)
#   if (n == 0L) {
#     df[[blink_col_left]]  <- character(0)
#     df[[blink_col_right]] <- character(0)
#     return(df)
#   }
#   
#   # per-eye onset flags
#   onset_flag_left  <- !is.na(df[[onset_col_left]])  & as.character(df[[onset_col_left]])  == onset_value
#   onset_flag_right <- !is.na(df[[onset_col_right]]) & as.character(df[[onset_col_right]]) == onset_value
#   
#   # run the NA-chain marking logic for a given eye, using that eye's onset flag
#   mark_eye <- function(pupil_col, onset_flag) {
#     na_eye <- is.na(df[[pupil_col]])
#     mask <- rep(FALSE, n)
#     i <- 1L
#     while (i <= n) {
#       if (isTRUE(onset_flag[i])) {
#         j <- i + 1L
#         while (j <= n && isTRUE(onset_flag[j]) && !na_eye[j]) {
#           j <- j + 1L
#         }
#         if (j <= n && na_eye[j]) {
#           k <- j
#           while (k <= n && na_eye[k]) {
#             mask[k] <- TRUE
#             k <- k + 1L
#           }
#           i <- k
#           next
#         }
#       }
#       i <- i + 1L
#     }
#     mask
#   }
#   
#   mask_left  <- mark_eye(pupil_left_col,  onset_flag_left)
#   mask_right <- mark_eye(pupil_right_col, onset_flag_right)
#   
#   out_left  <- rep(NA_character_, n); out_left[mask_left]   <- blink_value
#   out_right <- rep(NA_character_, n); out_right[mask_right] <- blink_value
#   
#   df[[blink_col_left]]  <- out_left
#   df[[blink_col_right]] <- out_right
#   df
# }

## Remove Too Short / Long Blinks ----
# Generic: filter blinks by duration in a single blink column
filter_blinks_by_duration <- function(
    df,
    blink_col      = "blink_detection.left",
    blink_value    = "blink",
    timestamp_col  = "Recording.timestamp",  # assumed in ms
    min_ms         = 30,
    max_ms         = 400
){
  n <- nrow(df)
  if (n == 0L) {return(df)}
  
  # Identify blink rows
  is_blink <- !is.na(df[[blink_col]]) & df[[blink_col]] == blink_value
  if (!any(is_blink)){
    return(df)}
  
  r <- rle(is_blink)
  ends   <- cumsum(r$lengths)
  starts <- c(1L, head(ends, -1L) + 1L)
  
  ts <- as.numeric(df[[timestamp_col]])
  
  # Iterate over each TRUE run (blink chunk)
  for (j in which(r$values)) {
    a <- starts[j]
    b <- ends[j]
    
    # robust to NA timestamps: use first/last non-NA within the run
    rng <- ts[a:b]
    i1 <- which(!is.na(rng))[1]
    i2 <- tail(which(!is.na(rng)), 1)
    
    if (length(i1) == 0 || length(i2) == 0) next  # skip if no valid time
    duration_ms <- rng[i2] - rng[i1]
    duration_ms <- duration_ms / 1000
    
    # If duration outside [min_ms, max_ms], remove the blink labels in this chunk
    if (!is.finite(duration_ms) || duration_ms < min_ms || duration_ms > max_ms) {
      df[[blink_col]][a:b] <- NA_character_
    }
  }
  df
}


# According to Stern et al. (1984), most blinks last between 150
# and 400 ms, where the eyes are completely closed for about 50 ms.
# Recent examples of simple data loss criteria to
# detect blinks are given by Hoogerbrugge et al. (2022), who
# used periods where “no pupil data was measured”, which
# were longer than 30 ms and shorter than 3 s and Demiral
# et al. (2022) considered intervals between 100 and 400 ms
# where the pupil diameter signal was missing or smaller than
# 25 units as blinks. Cornelis et al. (2025) consider 10ms as the
# lower threshold of blinks in human adults.

# library(dplyr)
# library(purrr)
# library(stringr)
# library(ggplot2)
# library(rlang)

#' Plot blink windows in pupil traces (one blink run = one plot/page)
#'
#' Returns one ggplot per contiguous blink run (>= `min_run` rows labeled `"blink"`).
#' Intended for multi-page PDF export (one plot per page).
#'
#' @param df A data frame containing time, pupil size, and blink labels.
#' @param eye Which eye(s) to plot. One of `"left"`, `"right"`, `"both"`.
#' @param flank_n Integer. Rows before/after each blink run to include in the plotted window.
#' @param min_run Integer. Minimum contiguous `"blink"` run length (in rows) to be plotted.
#' @param time_col Name of the time column used for the x-axis (character).
#' @param pupil_left_col Name of the left pupil column (character).
#' @param pupil_right_col Name of the right pupil column (character).
#' @param blink_col_left Name of the left blink label column (character).
#' @param blink_col_right Name of the right blink label column (character).
#' @param title_cols Columns used to build the main title (joined by underscores).
#'
#' @details
#' The x-axis uses `time_col` directly. Any duration values reported are in the same
#' units as `time_col` (e.g., ms if `time_col` is in ms).
#'
#' @return For `"left"`/`"right"`: named list of ggplots. For `"both"`: list with `left` and `right`.
#' @export

plot_blinks <- function(
    df,
    eye = c("left", "right", "both"),
    flank_n = 10,
    min_run = 2,
    time_col  = "timeline_trial_tot",
    pupil_left_col = "Pupil.diameter.left",
    pupil_right_col = "Pupil.diameter.right",
    blink_col_left  = "blink_detection.left",
    blink_col_right = "blink_detection.right",
    title_cols = c("participant_name", "recording_name", "trial"),
    drop_na_trial = TRUE,
    trial_col = "trial"
) {
  eye_selected <- match.arg(eye)
  
  # Optionally drop moments where trial is NA
  if (drop_na_trial) {
    if (!trial_col %in% names(df)) {
      stop("trial_col = '", trial_col, "' not found in df.")
    }
    df <- df |> dplyr::filter(!is.na(.data[[trial_col]]))
  }
  
  title_from <- function(d) {
    vals <- purrr::map_chr(title_cols, function(cc) {
      if (!cc %in% names(d)) stop("title column '", cc, "' not found in df.")
      u <- unique(stats::na.omit(d[[cc]]))
      if (length(u) == 0) "NA" else as.character(u[[1]])
    })
    stringr::str_c(vals, collapse = "_")
  }
  
  get_segments <- function(is_blink, min_run) {
    r <- rle(is_blink)
    ends <- cumsum(r$lengths)
    starts <- c(1L, head(ends, -1L) + 1L)
    keep <- r$values & r$lengths >= min_run
    
    tibble::tibble(
      seg_id    = seq_len(sum(keep)),
      seg_start = starts[keep],
      seg_end   = ends[keep]
    )
  }
  
  # use provided time column for x-axis
  df <- df |>
    dplyr::mutate(
      .time = as.numeric(.data[[time_col]]),
      .time = dplyr::if_else(is.finite(.time), .time, NA_real_)
    )
  
  make_plots_one_eye <- function(blink_col, pupil_col, eye_label) {
    
    # Split by title_cols so segments never cross trials (or recordings/participants)
    groups <- df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(title_cols))) |>
      dplyr::group_split(.keep = TRUE)
    
    all_plots <- list()
    
    for (g in groups) {
      is_blink <- !is.na(g[[blink_col]]) & g[[blink_col]] == "blink"
      if (!any(is_blink)) next
      
      segs <- get_segments(is_blink, min_run = min_run)
      if (nrow(segs) == 0) next
      
      n <- nrow(g)
      
      plots_g <- purrr::pmap(
        segs,
        function(seg_id, seg_start, seg_end) {
          a <- max(1L, seg_start - flank_n)
          b <- min(n,  seg_end + flank_n)
          
          sub <- g[a:b, , drop = FALSE] |>
            dplyr::mutate(.blink_flag = is_blink[a:b])
          
          core <- g[seg_start:seg_end, , drop = FALSE]
          main_title <- title_from(core)
          
          dur_units <- g$.time[seg_end] - g$.time[seg_start]
          blink_id <- sprintf("%s_%02d (Δ=%.0f %s-units)", eye_label, seg_id, dur_units, time_col)
          
          ggplot2::ggplot(sub, ggplot2::aes(x = .time, y = .data[[pupil_col]])) +
            ggplot2::geom_line() +
            ggplot2::geom_point(data = sub |> dplyr::filter(.blink_flag), size = 1) +
            ggplot2::labs(
              title = main_title,
              subtitle = sprintf("%s | %s | run >= %d rows | flank = %d rows",
                                 eye_label, blink_id, min_run, flank_n),
              x = sprintf("Time (%s)", time_col),
              y = sprintf("Pupil diameter (%s eye)", eye_label)
            )
        }
      )
      
      # Name plots including title info so they’re traceable
      nm_prefix <- paste0(eye_label, "_", title_from(g))
      names(plots_g) <- paste0(nm_prefix, "_", sprintf("%02d", seq_along(plots_g)))
      
      all_plots <- c(all_plots, plots_g)
    }
    
    all_plots
  }
  
  switch(
    eye_selected,
    left  = make_plots_one_eye(blink_col_left,  pupil_left_col,  "left"),
    right = make_plots_one_eye(blink_col_right, pupil_right_col, "right"),
    both  = list(
      left  = make_plots_one_eye(blink_col_left,  pupil_left_col,  "left"),
      right = make_plots_one_eye(blink_col_right, pupil_right_col, "right")
    )
  )
}


# plot_blinks <- function(
#     df,
#     eye = c("left", "right", "both"),
#     flank_n = 10,
#     min_run = 2,
#     time_col  = "timeline_trial_tot",
#     pupil_left_col = "Pupil.diameter.left",
#     pupil_right_col = "Pupil.diameter.right",
#     blink_col_left  = "blink_detection.left",
#     blink_col_right = "blink_detection.right",
#     title_cols = c("participant_name", "recording_name", "trial")
# ) {
#   eye_selected <- match.arg(eye)
# 
#   title_from <- function(d) {
#     vals <- map_chr(title_cols, function(cc) {
#       u <- unique(na.omit(d[[cc]]))
#       if (length(u) == 0) "NA" else as.character(u[[1]])
#     })
#     str_c(vals, collapse = "_")
#   }
# 
#   get_segments <- function(is_blink, min_run) {
#     r <- rle(is_blink)
#     ends <- cumsum(r$lengths)
#     starts <- c(1L, head(ends, -1L) + 1L)
#     keep <- r$values & r$lengths >= min_run
# 
#     tibble(
#       seg_id    = seq_len(sum(keep)),
#       seg_start = starts[keep],
#       seg_end   = ends[keep]
#     )
#   }
# 
#   # use provided time column for x-axis
#   df <- df |>
#     mutate(
#       .time = as.numeric(.data[[time_col]]),
#       .time = if_else(is.finite(.time), .time, NA_real_)
#     )
# 
#   make_plots_one_eye <- function(blink_col, pupil_col, eye_label) {
#     is_blink <- !is.na(df[[blink_col]]) & df[[blink_col]] == "blink"
#     if (!any(is_blink)) return(list())
# 
#     segs <- get_segments(is_blink, min_run = min_run)
#     if (nrow(segs) == 0) return(list())
# 
#     n <- nrow(df)
# 
#     plots <- pmap(
#       segs,
#       function(seg_id, seg_start, seg_end) {
#         a <- max(1L, seg_start - flank_n)
#         b <- min(n,  seg_end + flank_n)
# 
#         sub <- df[a:b, , drop = FALSE] |>
#           mutate(.blink_flag = is_blink[a:b])
# 
#         core <- df[seg_start:seg_end, , drop = FALSE]
#         main_title <- title_from(core)
# 
#         dur_units <- df$.time[seg_end] - df$.time[seg_start]
#         blink_id <- sprintf("%s_%02d (Δ=%.0f %s-units)", eye_label, seg_id, dur_units, time_col)
# 
#         ggplot(sub, aes(x = .time, y = .data[[pupil_col]])) +
#           geom_line() +
#           geom_point(data = sub |> filter(.blink_flag), size = 1) +
#           labs(
#             title = main_title,
#             subtitle = sprintf("%s | %s | run >= %d rows | flank = %d rows",
#                                eye_label, blink_id, min_run, flank_n),
#             x = sprintf("Time (%s)", time_col),
#             y = sprintf("Pupil diameter (%s eye)", eye_label)
#           )
#       }
#     )
# 
#     names(plots) <- sprintf("%s_%02d", eye_label, seq_along(plots))
#     plots
#   }
# 
#   switch(
#     eye_selected,
#     left  = make_plots_one_eye(blink_col_left,  pupil_left_col,  "left"),
#     right = make_plots_one_eye(blink_col_right, pupil_right_col, "right"),
#     both  = list(
#       left  = make_plots_one_eye(blink_col_left,  pupil_left_col,  "left"),
#       right = make_plots_one_eye(blink_col_right, pupil_right_col, "right")
#     )
#   )
# }


#' Save detected blink plots as one PDF
#'
#' @param plots Output of [plot_blinks()].
#' @param df The data used to create `plots` (needed for naming).
#' @param out_dir Output directory (character). Default: here("exp1","doc","chimps").
#' @param prefix Filename prefix. Default: "detected_blinks_".
#' @param width,height PDF size in inches.
#'
#' @return Invisibly returns the file path.
#' @export
save_detected_blinks_pdf <- function(
    plots,
    df,
    out_dir = here::here("exp1", "doc", "chimps"),
    prefix = "detected_blinks_",
    width = 10,
    height = 7
) {
  if (!("participant_name" %in% names(df))) {
    stop("df must contain a column named 'participant_name'.")
  }
  
  p <- unique(na.omit(df$participant_name))
  if (length(p) != 1) {
    stop("Expected exactly 1 unique participant_name in df, found: ",
         paste(p, collapse = ", "))
  }
  participant <- tolower(as.character(p))
  
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  file <- file.path(out_dir, paste0(prefix, participant, ".pdf"))
  
  grDevices::pdf(file = file, width = width, height = height, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  
  if (is.list(plots) && all(c("left", "right") %in% names(plots))) {
    purrr::walk(c(plots$left, plots$right), print)
  } else {
    purrr::walk(plots, print)
  }
  
  invisible(file)
}
