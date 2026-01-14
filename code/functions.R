# code/functions.R
# Code shared across the project


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(ncdf4)
library(geosphere) # For determining distance between points
library(patchwork)
library(doParallel); registerDoParallel(cores = detectCores() - 2)


# Setup -------------------------------------------------------------------

# Define the wavelength (nm) band colour palette
labels_nm <- c("350-400", "400-450", "450-500", "500-550", "550-600", "600-650", "650-700", "700-750", "750-800")
colour_nm <- c('darkviolet', 'violet', 'blue', 'darkgreen', 'yellow', 'orange', 'red', "firebrick", 'sienna')
names(colour_nm) <- labels_nm

# Disable scientific notation
# NB: Necessary for correct time stamp conversion
options(scipen = 9999)


# Utilities ---------------------------------------------------------------

# Function that assembles file directory based on desired variable and sensors
file_path_build <- function(var_name, sensor_X, sensor_Y){
  file_path <- paste0("~/pCloudDrive/Documents/OMTAB/HYPERNETS/tara_matchups_results/",
                      toupper(var_name),"_",toupper(sensor_X),"_vs_", toupper(sensor_Y))
}

# Load a single matchup file and create mean values from all replicates
load_matchup_mean <- function(file_name){
  
  # Load the csv file
  suppressMessages(
    df_match <- read_delim(file_name, delim = ";", col_types = "ciccnnic")
  )
  colnames(df_match)[1] <- "sensor"
  
  # Get means per file
  df_mean <- df_match |> 
    dplyr::select(-radiometer_id, -data_id, -type) |> 
    mutate(sensor = gsub(" 1$| 2$| 3$| 4$| 5$| 6$| 7$| 8$| 9$", "", sensor)) |> 
    # Remove HyperNets pre-processed data
    filter(sensor != "Hyp_nosc") |> 
    # mutate(dateTime = as.POSIXct(paste(day, time), format = "%Y%m%d %H%M%S")) |> # make this later
    # NB: Choosing to allow lon/lat be averaged to prevent different pixels from counting as different records
    group_by(sensor, day, time) |> #, longitude, latitude) |>
    summarise_all(mean, na.rm = TRUE) |> 
    ungroup()
  return(df_mean)
}

# Load a single matchup file directly into long format
# file_name <- file.path(folder_path, file_uniq_list$file_name)[1]
load_matchup_long <- function(file_name){
  
  df_mean <- load_matchup_mean(file_name)
  
  # Pivot longer
  df_long <- df_mean |> 
    pivot_longer(cols = matches("1|2|3|4|5|6|7|8|9"), names_to = "wavelength", values_to = "value") |> 
    dplyr::select(-day, -time, -longitude, -latitude) |>
    pivot_wider(names_from = sensor, values_from = value) |>
    na.omit() |> 
    mutate(wavelength = as.numeric(wavelength),
           file_name = basename(file_name), .before = "wavelength") |> 
    mutate(wavelength_group = cut(wavelength,
                                  breaks = c(350, 400, 450, 500, 550, 600, 650, 700, 750, 800),
                                  labels = labels_nm,
                                  include.lowest = FALSE, right = FALSE), .after = "wavelength")
}

# Load all files in a given folder
load_matchups_folder <- function(var_name, sensor_X, sensor_Y, long = FALSE){
  
  # Create file path
  folder_path <- file_path_build(var_name, sensor_X, sensor_Y)
  
  # List all files in directory
  file_list <- list.files(folder_path, pattern = "*.csv", full.names = TRUE)
  
  # Remove stats output files
  file_list_clean <- file_list[!grepl("all|global", file_list)]
  
  # Load data
  if(long){
    match_base <- plyr::ldply(file_list_clean, load_matchup_long, .parallel = TRUE)
    print(unique(match_base$wavelength))
  } else {
    match_base <- plyr::ldply(file_list_clean, load_matchup_mean, .parallel = TRUE)
  }
  
  # Exit
  return(match_base)
}
  

# Statistics --------------------------------------------------------------

# Basic statistic calculations
# Expects data as two vectors of equal length sampled at the same time/space
base_stats <- function(x_vec, y_vec){
  
  if(!is.numeric(x_vec)) stop("x_vec is not numeric")
  if(!is.numeric(y_vec)) stop("y_vec is not numeric")
  
  # Calculate RMSE (Root Mean Square Error)
  rmse <- sqrt(mean((y_vec - x_vec)^2, na.rm = TRUE))
  
  # Calculate MAPE (Mean Absolute Percentage Error)
  mape <- mean(abs((y_vec - x_vec) / x_vec), na.rm = TRUE) * 100
  
  # Calculate MSA (Mean Squared Adjustment)
  msa <- mean(abs(y_vec - x_vec), na.rm = TRUE)
  
  # Calculate linear slope
  lin_fit <- lm(y_vec ~ x_vec)
  slope <- coef(lin_fit)[2]
  
  # Calculate Bias and Error (Pahlevan's method)
  log_ratio <- log10(y_vec / x_vec)
  bias_pahlevan <- median(log_ratio, na.rm = TRUE)
  bias_pahlevan_final <- sign(bias_pahlevan) * (10^abs(bias_pahlevan) - 1)
  bias_pahlevan_final_perc <- bias_pahlevan_final * 100
  
  error_pahlevan <- median(abs(log_ratio), na.rm = TRUE)
  error_pahlevan_final <- 10^error_pahlevan - 1
  error_pahlevan_final_in_perc <- error_pahlevan_final * 100
  
  # Combine int data.frame and exit
  df_stats <- data.frame(row.names = NULL,
                         Slope = round(slope, 2),
                         RMSE = round(rmse, 4),
                         MSA = round(msa, 4),
                         MAPE = round(mape, 1),
                         Bias = round(bias_pahlevan_final_perc, 1),
                         Error = round(error_pahlevan_final_in_perc, 1))
  return(df_stats)
}

