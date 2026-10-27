# Remove everything from environment
rm(list = ls())

# Load packages
library(tidyverse)
library(here)

# Specify file path and filename
folder_path <- here("data", "data_apes_7popflakes_5p", "orangs") # define the path where you put the file that you specify below
file_name <- "newexportedtobiidata.tsv" # add file name of your original file that you put in the path above and where you want to remove the calibration-rows

# Read data (tab-separated)
data <- read.table(file.path(folder_path, file_name), header = TRUE, sep = "\t")

# # Remove eye tracker calibration trials
main_data <- data |>
  filter(!Presented.Stimulus.name %in% c("Eyetracker Calibration", ""))

# # Save filtered data in the same folder
write.table(main_data, file = file.path(folder_path, "main_data.tsv"), sep = "\t", quote = FALSE, row.names = FALSE)
