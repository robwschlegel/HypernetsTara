# code/functions.R
# Code shared across the project


# Libraries ---------------------------------------------------------------

library(tidyverse)
# library(readxl)
library(ncdf4)
library(FNN) # Needed for fastest nearest neighbor searching
library(geosphere) # For determining distance between points
library(ggtext) # For rich text labels
library(patchwork) # For complex paneling of figures
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
  file_path <- paste0("~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/tara_matchups_results_20260203/",
                      toupper(var_name),"_",toupper(sensor_X),"_vs_", toupper(sensor_Y),"/")
}

# Load a single matchup file and create mean values from all replicates
# file_name <- "~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/tara_matchups_results_mwm_595/RHOW_HYPERNETS_vs_HYPERPRO/HYPERNETS_vs_HYPERPRO_vs_20240809T073700_RHOW.csv"
# file_name <- "~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/tara_matchups_results_mwm_595/ED_HYPERNETS_vs_TRIOS/HYPERNETS_vs_TRIOS_vs_20240808T065700_ED.csv"
# file_name <- "~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/tara_matchups_results_mwm_595/RHOW_HYPERNETS_vs_AQUA/HYPERNETS_vs_AQUA_vs_20240810T110400_RHOW.csv"
# file_name <- "~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/tara_matchups_results_20260203/RHOW_TRIOS_vs_VIIRS_N/TRIOS_vs_VIIRS_N_vs_20240812T121332_RHOW.csv"
load_matchup_mean <- function(file_name){
  
  # message(paste0("Started loading : ", file_name))
  
  # Load the csv file
  suppressMessages(
    df_match <- read_delim(file_name, delim = ";", col_types = "ccccnnic")
  )
  colnames(df_match)[1] <- "sensor"
  
  # Get means per file
  # NB: Satellite matchups have a different structure than in situ matchups
  if("weighted" %in% df_match$data_type){
    df_mean <- df_match |>
      mutate(sensor = gsub(" 1$| 2$| 3$| 4$| 5$| 6$| 7$| 8$| 9$", "", sensor)) |>
      filter(sensor != "Hyp_nosc") |> 
      filter(data_type %in% c("mean", "weighted")) |>
      # NB: Simple mean for in situ data is taken instead of the weighted mean
      # This is an important point for discussion
      mutate(data_check = case_when(sensor %in% c("Hyp", "TRIOS", "HYPERPRO") & data_type == "mean" ~ "keep",
                                    !(sensor %in% c("Hyp", "TRIOS", "HYPERPRO")) & data_type == "weighted" ~ "keep"), 
             .after = "data_type") |>
      filter(data_check == "keep") |>
      dplyr::select(-radiometer_id, -data_type, -data_check, -type)
  } else {
    df_mean <- df_match |> 
      filter(grepl(" 1", sensor)) |> 
      mutate(sensor = gsub(" 1$| 2$| 3$| 4$| 5$| 6$| 7$| 8$| 9$", "", sensor)) |>
      filter(sensor != "Hyp_nosc") |> 
      dplyr::select(-radiometer_id, -data_type, -type)
  }

  # Double check that only two rows of data have been selected
  if(nrow(df_mean) != 2) warning(paste0("More than two rows in : ", file_path))
  
  # Exit
  # message(paste0("Finished loading : ", file_name))
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
    filter(wavelength < 700) |> # Prevent one data point for 700-750 from appearing on figures
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
    # print(unique(match_base$wavelength))
  } else {
    match_base <- plyr::ldply(file_list_clean, load_matchup_mean, .parallel = TRUE)
  }
  
  # Exit
  return(match_base)
}

# Convenience function to get lon/lat coords from SeaBird files
# file_name <- "~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/Trios_processed_data/TARA_HyperBOOST_Ed_20240323_20240821_Version_20250911.sb"
# file_name <- "/home/calanus/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/Hypernets_processed_data/07/SEQ20240807T123357/HYPERNETS_W_MAFR_L1B_RAD_20240807T1233_20240912T1039_v2.0.nc"
# file_name <- "~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/Hypernets_processed_data/10/SEQ20240810T173010/HYPERNETS_W_MAFR_L1B_RAD_20240810T1730_20240912T1013_v2.0.nc"
# file_name <- "~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/Hypernets_processed_data/18/SEQ20240818T093051/HYPERNETS_W_MAFR_L0A_BLA_20240818T0930_20240912T1031_v2.0.nc"
load_HYPERNETS_coords <- function(file_name){
  ncdump::NetCDF(file_name)$attribute$global[c("site_longitude", "site_latitude")]
}

# Check the amount of variance in satellite files and return a message if there is an issue
# file_name <- "~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/tara_matchups_results_mwm_595/RHOW_HYPERNETS_vs_AQUA/HYPERNETS_vs_AQUA_vs_20240810T110400_RHOW.csv"
# file_name <- "~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/tara_matchups_results_20260203/RHOW_TRIOS_vs_VIIRS_N/TRIOS_vs_VIIRS_N_vs_20240812T121332_RHOW.csv"
# file_name <- "/home/calanus/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/tara_matchups_results_20260203/RHOW_HYPERPRO_vs_S3A/HYPERPRO_vs_S3A_vs_20240809T084500_RHOW.csv"
# file_name <- "/home/calanus/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/tara_matchups_results_20260203/RHOW_HYPERNETS_vs_PACE_V2/HYPERNETS_vs_PACE_V2_vs_20240809T090000_RHOW.csv"
sat_var_check <- function(file_name, var_limit = 0.1){
  
  # Load the csv file
  suppressMessages(
    df_match <- read_delim(file_name, delim = ";", col_types = "ccccnnic")
  )
  colnames(df_match)[1] <- "sensor"
  
  # PACE files don't have weighted mean values
  if(!("weighted" %in% df_match$data_type)){
    df_match <- df_match |> 
      mutate(data_type = case_when(data_type == "rhow" ~ "weighted", TRUE ~ data_type))
  }
  
  # Check for variance in W_nm columns
  df_check <- df_match |> 
    mutate(sensor = gsub(" 1$| 2$| 3$| 4$| 5$| 6$| 7$| 8$| 9$", "", sensor)) |>
    filter(!(sensor %in% c("Hyp_nosc", "Hyp", "TRIOS", "HYPERPRO"))) |> 
    dplyr::select(-day, -time, -latitude, -longitude, -radiometer_id, -type) |> 
    pivot_longer(cols = matches("1|2|3|4|5|6|7|8|9"), names_to = "wavelength", values_to = "value") |> 
    pivot_wider(names_from = data_type, values_from = value) |>
    na.omit() |> 
    mutate(max_sd_diff = weighted / std_max,
           min_sd_diff = weighted / std_min,
           wavelength = as.numeric(wavelength)) |> 
    filter(wavelength <= 500)
  
  # If variables are too different, issue a warning and omit file from being loaded
  # TODO: Change this to a flat 25% difference from min to max
  if(any(df_check$max_sd_diff >= 1+var_limit) | any(df_check$min_sd_diff <= 1-var_limit)){
    # warning(paste0("Weighted mean has too much variance in file : ", file_name))
    # return(basename(file_name))
    file_check <- basename(file_name)
  } else {
    file_check <- NULL
  }
  return(data.frame(file_name = file_check))
}

# Create a grid of sensor to ply over
sensor_grid <- function(var_name, sensor_Z){
  
  # Check that satellites aren't being called with Ed etc.
  if(var_name != "RHOW" & sensor_Z != "HYPERPRO") stop("Only RHOW data for satellites.")
  
  # Get sorrect sensor names
  if(var_name %in% c("LU", "LD")){
    sensor_X <- "HYPERNETS"
    sensor_Y <- "TRIOS"
    sensor_Z <- "Hyp_vs_Trios"
  } else if(sensor_Z == "HYPERPRO"){
    sensor_X <- c("HYPERNETS", "HYPERNETS", "TRIOS")
    sensor_Y <- c("TRIOS", "HYPERPRO", "HYPERPRO")
    sensor_Z <- "in_situ"
  } else {
    sensor_X <- c("HYPERNETS", "TRIOS", "HYPERPRO")
    sensor_Y <- NULL
  }
  
  # Continue with satellite versions if necessary
  if(is.null(sensor_Y)){
    if(sensor_Z == "MODIS"){
      sensor_Y <- c("AQUA")
    } else if(sensor_Z == "OCI"){
      sensor_Y <- c("PACE_V2", "PACE_V30", "PACE_V31")
    } else if(sensor_Z == "VIIRS"){
      sensor_Y <- c("VIIRS_N", "VIIRS_J1", "VIIRS_J2")
    } else if(sensor_Z == "OLCI"){
      sensor_Y <- c("S3A", "S3B")
    } else {
      stop("Incorrect name given for sensor_Z")
    }
  }
  
  # Print sensors for ease of use
  message("sensor_X : ", paste0(sensor_X, collapse = ", ")); message("sensor_Y : ",paste0(sensor_Y, collapse = ", "))
  
  # Create grid for mdply()
  ply_grid <- expand_grid(var_name = var_name, sensor_X = sensor_X, sensor_Y = sensor_Y) |> distinct()
  ply_grid <- ply_grid[ply_grid$sensor_X != ply_grid$sensor_Y,]
}

# Output desired wavelengths based on sensor_Y and var_name
W_nm_out <- function(sensor_Y, var_name){
  if(sensor_Y %in% c("HYPERNETS", "TRIOS", "HYPERPRO")){
    # NB: This allows for comparisons of higher wavelengths to be made for ED etc.
    if(var_name != "RHOW"){
      W_nm <- c(400, 412, 443, 490, 510, 560, 620, 673)
    } else {
      W_nm <- c(400, 412, 443, 490, 510, 560)
    }
  } else if(sensor_Y %in% c("PACE_V2", "PACE_V30", "PACE_V31")){
    W_nm <- c(412, 443, 490, 510, 560)#, 673)
  } else if(sensor_Y == "AQUA"){
    W_nm <- c(412, 443, 488, 531, 555)#, 667)
  } else if(sensor_Y == "VIIRS_N"){
    W_nm = c(410, 443, 486, 551)#, 671)
  } else if(sensor_Y %in% c("VIIRS_J1", "VIIRS_J2")){
    W_nm <- c(411, 445, 489, 556)#, 667)
  } else if(sensor_Y %in% c("S3A", "S3B", "S3", "S3_all")){
    W_nm <- c(413, 443, 490, 560)#, 665, 681)
  } else {
    stop(paste0("Incorrect value for 'sensor_Y' : ",sensor_Y))
  }
}

# Get possible MODIS files
# NB: At the moment this is optimized to work with just one day of data
MODIS_dl <- function(prod_id, dl_date, bbox, usrname, psswrd, dl_files = TRUE, dl_dir = "data/MODIS"){
  
  # If download is FALSE, just print possible files
  if(!dl_files){
    message("Data files : ")
    luna::getNASA(prod_id, dl_date, dl_date, aoi = bbox, download = FALSE)
    message("Mask files : ")
    luna::getNASA("MOD44W", dl_date, dl_date, aoi = bbox, download = FALSE)
  } else {
    message("Data files : ")
    luna::getNASA(prod_id, dl_start, dl_start, aoi = bbox, download = TRUE, overwrite = FALSE,
                  path = dl_dir, username = earth_up$usrname, password = earth_up$psswrd)
    message("Mask files : ")
    luna::getNASA("MOD44W", dl_start, dl_start, aoi = bbox, download = TRUE, overwrite = FALSE,
                  path = dl_dir, username = usrname, password = psswrd)
  }
}

# Process MODIS data in a batch
# file_names <- list.files(path = "data/MODIS", pattern = "MOD", full.names = TRUE)
# file_names <- list.files(path = "data/MODIS", pattern = "MYD", full.names = TRUE)
MODIS_proc <- function(file_names, bbox, water_mask = FALSE){
  
  # Load files with desired layers etc.
  # NB: If run in parallel, merge() causes a crash to desktop
  if(water_mask){
    data_layers <- lapply(file_names, rast, subds = 2)
    data_merge <- do.call(merge, data_layers)
    # plot(data_merge)
    data_base <- terra::ifel(data_merge %in% c(1, 2, 3, 4, 5), NA, data_merge)
    # plot(data_base)
  } else {
    data_layers <- lapply(file_names, rast, subds = 3) # Blue green band width; 459-479 nm
    data_base <- do.call(merge, data_layers)
    # plot(data_base)
  }
  
  # Project to EPSG:4326
  data_base_proj <- project(data_base, y = "EPSG:4326")
  # plot(data_base_proj)
  
  # Crop to bbox and exit
  data_crop <- crop(data_base_proj, bbox)
  # plot(data_crop)
  return(data_crop)
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


# Matchup processing ------------------------------------------------------

# get n nearest pixels
get_nearest_pixels <- function(df_data, target_lat, target_lon, n_pixels){
  
  # Extract latitude and longitude into a matrix
  df_coords <- S3A_band_1[, c("latitude", "longitude")]
  
  # Target coordinate as a data.frame
  target_coord <- data.frame(latitude = target_lat, 
                             longitude = target_lon)
  
  # Find the indices of the 5 nearest neighbors
  knn_indices <- get.knnx(df_coords, target_coord, k = n_pixels)
  
  # Extract the 5 nearest rows
  df_res <- df_data[as.vector(knn_indices$nn.index), ]
  return(df_res)
}

# Function that interrogates each matchup file to produce the needed output for all following comparisons
# file_path <- "~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/tara_matchups_results_v2/RHOW_HYPERNETS_vs_HYPERPRO/HYPERNETS_vs_HYPERPRO_vs_20240809T073700_RHOW.csv"
# file_path <- "~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/tara_matchups_results_v2/ED_HYPERNETS_vs_TRIOS/HYPERNETS_vs_TRIOS_vs_20240808T065700_ED.csv"
# file_path <- file_list[1]
process_matchup_file <- function(file_path){
  
  # Load the mean data
  df_mean <- load_matchup_mean(file_path)
  
  # Sensors to be compared
  sensors <- unique(df_mean$sensor)
  
  # Prep empty df for 2+ sensor comparisons
  df_results <- data.frame()
  
  # The double loop
  for(i in 1:length(sensors)){
    for(j in 1:length(sensors)){
      if(sensors[j] != sensors[i]){
        
        # Get data.frame for matchup based on the two sensors being compared
        df_sensor_sub <- df_mean |> 
          filter(sensor %in% c(sensors[i], sensors[j])) |> 
          mutate(dateTime = as.POSIXct(paste(day, time), format = "%Y%m%d %H%M%S"), .after = "latitude", .keep = "unused")
        
        # get distances
        hav_dist <- round(distHaversine(df_sensor_sub[c("longitude", "latitude")])/1000, 2) # distance in km
        
        # Time differences
        time_diff <- round(as.numeric(abs(difftime(df_sensor_sub$dateTime[[1]],
                                                   df_sensor_sub$dateTime[[2]], units = "mins"))))
        
        # Melt it for additional stats
        df_sensor_long <- df_sensor_sub |> 
          pivot_longer(cols = matches("1|2|3|4|5|6|7|8|9"), names_to = "Wavelength", values_to = "Value") |> 
          dplyr::select(-dateTime, -longitude, -latitude) |> 
          pivot_wider(names_from = sensor, values_from = Value) |> 
          na.omit()
        
        # get vectors
        x_vec <- df_sensor_long[[sensors[i]]]
        y_vec <- df_sensor_long[[sensors[j]]]
        
        # Base stats
        df_stats <- base_stats(x_vec, y_vec)
        
        # Create data.frame of results and add them to df_results
        df_res <- data.frame(row.names = NULL,
                             sensor_X = sensors[i],
                             sensor_Y = sensors[j],
                             lon_X = df_sensor_sub$longitude[[i]],
                             lat_X = df_sensor_sub$latitude[[i]],
                             lon_Y = df_sensor_sub$longitude[[j]],
                             lat_Y = df_sensor_sub$latitude[[j]],
                             dist = hav_dist,
                             dateTime_X = df_sensor_sub$dateTime[[i]],
                             dateTime_Y = df_sensor_sub$dateTime[[j]],
                             diff_time = time_diff,
                             n = nrow(df_sensor_long),
                             Slope = df_stats$Slope,
                             # RMSE = df_stats$RMSE,
                             # MSA = df_stats$MSA,
                             MAPE = df_stats$MAPE,
                             Bias = df_stats$Bias,
                             Error = df_stats$Error)
        df_results <- rbind(df_results, df_res)
      }
    }
  }
  
  # For loop that cycles through the requested wavelengths and calculates stats
  df_results <- df_results |> mutate(file_name = basename(file_path), .before = sensor_X)
  return(df_results)
}

# Function that runs this over all matchup files in a directory
# var_name = "RHOW"; sensor_X = "HYPERPRO"; sensor_Y = "PACE_v31"
# var_name = "RHOW"; sensor_X = "HYPERNETS"; sensor_Y = "AQUA"
# var_name = "RHOW"; sensor_X = "HYPERNETS"; sensor_Y = "AQUA"
process_matchup_folder <- function(var_name, sensor_X, sensor_Y){
  
  # Create file path
  folder_path <- file_path_build(var_name, sensor_X, sensor_Y)
  
  # List all files in directory
  file_list <- list.files(folder_path, pattern = "*.csv", full.names = TRUE)
  
  # Remove files with 'all' in the name
  file_list <- file_list[!grepl("all|global", file_list)]
  
  # Initialise results data.frame
  df_results <- plyr::ldply(file_list, process_matchup_file, .parallel = TRUE)
  
  # Exit
  return(df_results)
}

# Process multiple folders based on request
# var_name = "LU"; sensor_Z = "HYPERPRO"
# var_name = "ED"; sensor_Z = "HYPERPRO"
# var_name = "RHOW"; sensor_Z = "VIIRS"
# var_name = "RHOW"; sensor_Z = "OLCI"; stat_choice = "global"
process_sensor <- function(var_name, sensor_Z, stat_choice = "matchup"){
  
  # Create ply grid
  ply_grid <- sensor_grid(var_name, sensor_Z)
  
  # Add S3_all if needed
  if(sensor_Z == "OLCI" & stat_choice == "global"){
    ply_grid_bonus <- data.frame(var_name = var_name,
                                 sensor_X = unique(ply_grid$sensor_X),
                                 sensor_Y = "S3_all")
    ply_grid <- rbind(ply_grid, ply_grid_bonus)
    message("Added S3_all to sensor_Y list")
  }
  
  # Correct sensor_Z for file names upon saving
  if(var_name %in% c("LU", "LD")){
    sensor_Z <- "Hyp_vs_Trios"
  } else if(sensor_Z == "HYPERPRO"){
    sensor_Z <- "in_situ"
  } else {
    sensor_Z <- sensor_Z
  }
  
  # Process matchups and save output
  if(stat_choice == "matchup"){
    proc_res <- plyr::mdply(ply_grid, process_matchup_folder, .parallel = TRUE)
    write_csv(proc_res, paste0("output/matchup_stats_",var_name,"_",sensor_Z,".csv"))
  } else {
    proc_res <- plyr::mdply(ply_grid, global_stats, .parallel = TRUE)
    write_csv(proc_res, paste0("output/global_stats_",var_name,"_",sensor_Z,".csv"))
  }
}


# Global matchup stats ----------------------------------------------------

# Global stats per matchup wavelength
# testers..
# var_name = "ED"; sensor_X = "HYPERNETS"; sensor_Y = "TRIOS"
# var_name = "LU"; sensor_X = "HYPERNETS"; sensor_Y = "TRIOS"
# var_name = "RHOW"; sensor_X = "HYPERNETS"; sensor_Y = "TRIOS"
# var_name = "RHOW"; sensor_X = "TRIOS"; sensor_Y = "AQUA"
# var_name = "RHOW"; sensor_X = "HYPERNETS"; sensor_Y = "PACE_V31"
# var_name = "RHOW"; sensor_X = "HYPERNETS"; sensor_Y = "S3_all"
# W_nm = c(412, 443, 488, 531, 555, 667)
global_stats <- function(var_name, sensor_X, sensor_Y){
  
  # Create multiple folder paths if requested
  if(sensor_Y == "S3_all"){
    folder_path <- c(file_path_build(var_name, sensor_X, "S3A"),
                     file_path_build(var_name, sensor_X, "S3B"))
    # NB: Different wavebands for VIIRS_N than VIIRS_J1 and VIIRS_J2
  # } else if(sensor_Y == "VIIRS_all"){
  #   folder_path <- c(file_path_build(var_name, sensor_X, "VIIRS_N"),
  #                    file_path_build(var_name, sensor_X, "VIIRS_J1"),
  #                    file_path_build(var_name, sensor_X, "VIIRS_J2"))
  } else {
    folder_path <- file_path_build(var_name, sensor_X, sensor_Y)
  }
  
  # List all files in directory
  file_list <- list.files(folder_path, pattern = "*.csv", full.names = TRUE)
  
  # Get outlier lists
  suppressMessages(
    outliers_sat <- read_csv("meta/satellite_outliers.csv")
  )
  suppressMessages(
    outliers_insitu <- read_csv("meta/in_situ_outliers.csv")
  )
  outliers_all <- bind_rows(outliers_sat, outliers_insitu)
  
  # Filter accordingly
  file_list_clean <- file_list[!basename(file_list) %in% outliers_all$file_name]
  
  # Load data
  match_base <- map_dfr(file_list_clean, load_matchup_long)
  
  # Melt if S3_all
  if(sensor_Y == "S3_all"){
    match_base <- match_base |> 
      pivot_longer(S3A:S3B, names_to = "name", values_to = "S3_all") |> 
      dplyr::select(-name) |> 
      filter(!is.na(S3_all))
  }
  
  # Continue with satellite versions if necessary
  if(sensor_Y  == "AQUA"){
    sensor_Z <- "MODIS"
  } else if(sensor_Y %in% c("PACE_V2", "PACE_V30", "PACE_V31")){
    sensor_Z <- "OCI"
  } else if(sensor_Y %in% c("VIIRS_N", "VIIRS_J1", "VIIRS_J2", "VIIRS_all")){
    sensor_Z <- "VIIRS"
  } else if(sensor_Y %in% c("S3A", "S3B", "S3_all")){
    sensor_Z <- "OLCI"
  } else {
  }
  
  # Get filestub based on sensor_Y and var_name
  if(var_name %in% c("ED", "LW")){
    filestub <- "_in_situ.csv"
  } else if(var_name %in% c("LU", "LD")){
    filestub <- "_Hyp_vs_Trios.csv"
  } else if(sensor_Y %in% c("TRIOS", "HYPERPRO")){
    filestub <- "_in_situ.csv"
  } else {
    filestub <- paste0("_",sensor_Z,".csv")
  }
  
  # Correct sensor_X for filtering
  if(sensor_X == "HYPERNETS"){
    sensor_X_filt <- "Hyp"
  } else {
    sensor_X_filt <- sensor_X
  }
  
  # Load individual matchup results to access difftime values
  suppressMessages(
    match_base_details <- read_csv(paste0("output/matchup_stats_",var_name,filestub))
  )
  match_base_details <- dplyr::select(match_base_details, var_name, file_name, dist, diff_time) |> distinct()
  if(nrow(match_base_details) == 0) stop("Individual matchup file not loaded correctly.")
  
  # Join for further use
  match_base_plus <- left_join(match_base, match_base_details, by = join_by(file_name))
  
  # Filter out matches outside of allowed time window
  if(!(sensor_Y %in% c("HYPERNETS", "TRIOS", "HYPERPRO"))){
    time_window <- 240
  } else {
    time_window <- 20
  }
  match_base_filt <- filter(match_base_plus, diff_time <= time_window)
  
  # Get the count of matchups after filtering by time window
  match_count_difftime <- length(unique(match_base_filt$file_name))
  
  # Get pre-determined wavelengths
  W_nm <- W_nm_out(sensor_Y, var_name)
  
  # For loop that cycles through the requested wavelengths and calculates stats
  df_results <- data.frame()
  for(i in 1:length(W_nm)){
    
    # Get data.frame for matchup based on the wavelength of choice
    matchup_filt <- filter(match_base_filt, wavelength == W_nm[i])
    n_match <- nrow(matchup_filt)
    
    # Calculate stats
    if(n_match > 0){
      
      # Correct sensor labels as necessary
      if(sensor_X == "HYPERNETS"){
        sensor_X_col <- "Hyp"
      } else {
        sensor_X_col <- sensor_X
      }
      if(sensor_Y == "HYPERNETS"){
        sensor_Y_col <- "Hyp"
      } else {
        sensor_Y_col <- sensor_Y
      }
      
      # Create vectors from filtered columns
      x_vec <- matchup_filt[[sensor_X_col]]
      y_vec <- matchup_filt[[sensor_Y_col]]
      
      # Calculate statistics
      df_stats_XY <- base_stats(x_vec, y_vec)
      df_stats_YX <- base_stats(y_vec, x_vec)
      
      # Create data.frame of results and add them to df_results
      df_XY <- data.frame(row.names = NULL,
                            n_w_nm = n_match,
                            sensor_X = sensor_X,
                            sensor_Y = sensor_Y,
                            Wavelength_nm = W_nm[i],
                            Slope = df_stats_XY$Slope,
                            Bias = df_stats_XY$Bias,
                            Error = df_stats_XY$Error)
      df_YX <- data.frame(row.names = NULL,
                          n_w_nm = n_match,
                          sensor_X = sensor_Y,
                          sensor_Y = sensor_X,
                          Wavelength_nm = W_nm[i],
                          Slope = df_stats_YX$Slope,
                          Bias = df_stats_YX$Bias,
                          Error = df_stats_YX$Error)
      df_temp <- rbind(df_XY, df_YX)
      df_results <- rbind(df_results, df_temp)
    } else {
      print(paste0("No data for wavelength ", W_nm[i]))
    }
  }
  
  # Add matchup count and exit
  df_results <- df_results |> 
    mutate(n_base = length(file_list),
           n_clean = length(file_list_clean),
           diff_time_window = time_window, 
           n_diff_time = match_count_difftime,
           .before = "n_w_nm")
  return(df_results)
}


# Plotting functions ------------------------------------------------------

# Plot data based on wavelength group
plot_matchup_nm <- function(df, var_name, x_sensor, y_sensor){
  df |> 
    filter(!is.na(!!sym(x_sensor)), !is.na(!!sym(y_sensor))) |>
    ggplot(aes_string(x = x_sensor, y = y_sensor)) +
    geom_point(aes(colour = wavelength_group)) +
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
    labs(title = paste(var_name,"-", x_sensor, "vs", y_sensor),
         x = paste(var_name, x_sensor),
         y = paste(var_name, y_sensor),
         colour = "Wavelength (nm)") +
    scale_colour_manual(values = colour_nm)  +
    theme_minimal() +
    theme(panel.border = element_rect(fill = NA, color = "black"),
          legend.position = "bottom")
}

# Plot based on date of collection
plot_matchup_date <- function(df, var_name, x_sensor, y_sensor){
  df |> 
    filter(!is.na(!!sym(x_sensor)), !is.na(!!sym(y_sensor))) |>
    mutate(date = as.factor(as.Date(dateTime_X))) |> 
    ggplot(aes_string(x = x_sensor, y = y_sensor)) +
    geom_point(aes(colour = date)) +
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
    labs(title = paste(var_name,"-", x_sensor, "vs", y_sensor),
         x = paste(var_name, x_sensor),
         y = paste(var_name, y_sensor),
         colour = "date") +
    # scale_colour_brewer(palette = "Dark2")  +
    theme_minimal() +
    theme(panel.border = element_rect(fill = NA, color = "black"),
          legend.position = "bottom")
}

# Plot based on dateTime of collection
plot_matchup_dateTime <- function(df, var_name, x_sensor, y_sensor, date_filter){
  df |> 
    filter(!is.na(!!sym(x_sensor)), !is.na(!!sym(y_sensor))) |> 
    mutate(date = as.Date(dateTime_X)) |> 
    filter(date == as.Date(date_filter)) |>
    ggplot(aes_string(x = x_sensor, y = y_sensor)) +
    geom_point(aes(colour = dateTime_X)) +
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
    labs(title = paste(var_name,"-", x_sensor, "vs", y_sensor,"-", date_filter),
         x = paste(var_name, x_sensor),
         y = paste(var_name, y_sensor),
         colour = "time (UTC)") +
    theme_minimal() +
    theme(panel.border = element_rect(fill = NA, color = "black"),
          legend.position = "bottom")
}

# Plot scatterplot based on the MAPE values of each comparison
plot_matchup_MAPE_Bias <- function(df, var_name, x_sensor, y_sensor){
  pl_MAPE <- df |> 
    filter(!is.na(!!sym(x_sensor)), !is.na(!!sym(y_sensor))) |> 
    ggplot(aes_string(x = x_sensor, y = y_sensor)) +
    geom_point(aes(colour = MAPE)) +
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
    scale_colour_viridis_c(option = "D") +
    labs(title = paste(var_name,"-", x_sensor, "vs", y_sensor,"- MAPE"),
         x = paste(var_name, x_sensor),
         y = paste(var_name, y_sensor),
         colour = "MAPE [%]") +
    theme_minimal() +
    theme(panel.border = element_rect(fill = NA, color = "black"),
          legend.position = "bottom")
  pl_Bias <- df |>
    filter(!is.na(!!sym(x_sensor)), !is.na(!!sym(y_sensor))) |> 
    ggplot(aes_string(x = x_sensor, y = y_sensor)) +
    geom_point(aes(colour = Bias)) +
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
    scale_colour_viridis_c(option = "A") +
    labs(title = paste(var_name,"-", x_sensor, "vs", y_sensor,"- Bias"),
         x = paste(var_name, x_sensor),
         y = paste(var_name, y_sensor),
         colour = "Bias [%]") +
    theme_minimal() +
    theme(panel.border = element_rect(fill = NA, color = "black"),
          legend.position = "bottom")
  ggpubr::ggarrange(pl_MAPE, pl_Bias, nrow = 2, ncol = 1)
}

# Plot the relationship between difftime and distance for MAPE and bias for all variables
# df = matchup_ED_in_situ; var_name = "ED"
plot_matchup_scatter <- function(df, var_name, pl_height = 6, pl_width = 9){
  
  # Relationship between MAPE, distance and difftime
  pl_MAPE <- df |> 
    mutate(comp_sensors = paste0(sensor_X," vs ",sensor_Y)) |> 
    filter(comp_sensors %in% c("Hyp vs TRIOS", "Hyp vs HYPERPRO", "TRIOS vs HYPERPRO")) |> 
    ggplot(aes(x = diff_time, y = MAPE)) +
    geom_point(aes(colour = dist), size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE) +
    scale_colour_viridis_c() +
    labs(x = "Time difference [minutes]", y = "MAPE [%]", colour = "Distance\n[km]",
         title = paste0(var_name," - MAPE (%) : Effect of sampling time difference"),
         subtitle = "Colour shows distance (km) between samples") +
    facet_wrap(~comp_sensors) +
    theme(panel.border = element_rect(colour = "black"))#, 
  # legend.position = "bottom")
  # pl_MAPE
  
  # The same but for bias
  pl_Bias <- df |> 
    mutate(comp_sensors = paste0(sensor_X," vs ",sensor_Y)) |> 
    filter(comp_sensors %in% c("Hyp vs TRIOS", "Hyp vs HYPERPRO", "TRIOS vs HYPERPRO")) |> 
    ggplot(aes(x = diff_time, y = Bias)) +
    geom_point(aes(colour = dist), size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE) +
    scale_colour_viridis_c() +
    labs(x = "Time difference [minutes]", y = "Bias [%]", colour = "Distance\n[km]",
         title = paste0(var_name," - Bias (%) : Effect of sampling time difference"),
         subtitle = "Colour shows distance (km) between samples") +
    facet_wrap(~comp_sensors) +
    theme(panel.border = element_rect(colour = "black"))#, 
  # legend.position = "bottom")
  # pl_Bias
  
  pl_combi <- ggpubr::ggarrange(pl_MAPE, pl_Bias, nrow = 2, ncol = 1)
  ggsave(paste0("figures/outliers_",var_name,"_in_situ.png"), pl_combi, height = pl_height, width = pl_width)
  # pl_combi
}

pretty_label_func <- function(char_string){
  
  # Set default values
  # NB: Many names are already correct and don't need to be tuned below
  sensor_col <- char_string
  sensor_lab <- char_string
  units_lab <- char_string
  
  # Correct satellite names
  if(char_string == "HYPERNETS"){
    sensor_col <- "Hyp"
  } else if(char_string == "HYPERPRO"){
    sensor_lab <- "HyperPRO"
  } else if(char_string == "TRIOS"){
    sensor_lab <- "So-Rad"
  } else if(char_string == "AQUA"){
    sensor_lab <- "MODIS-A" 
  } else if(char_string == "S3"){ # NB: This is a special case
    sensor_lab <- "S3A; S3B" 
  } else if(char_string == "PACE_V2"){
    sensor_lab <- "PACE v2.0"
  } else if(char_string == "PACE_V30"){
    sensor_lab <- "PACE v3.0"
  } else if(char_string == "PACE_V31"){
    sensor_lab <- "PACE v3.1"
  } else if(char_string == "VIIRS_N"){
    sensor_lab <- "SNPP"
  } else if(char_string == "VIIRS_J1"){
    sensor_lab <- "JPSS1"
  } else if(char_string == "VIIRS_J2"){
    sensor_lab <- "JPSS2"
  }
  
  # Correct variable units
  if(char_string == "ED"){
    units_lab <- "<i>E<sub>d</sub></i> (W m<sup>-2</sup> nm<sup>-1</sup>)"
  } else if(char_string == "LD"){
    units_lab <- "<i>L<sub>d</sub></i> (W m<sup>-2</sup> sr<sup>-1</sup>)"
  } else if(char_string == "LU"){
    units_lab <- "<i>L<sub>u</sub></i> (W m<sup>-2</sup> sr<sup>-1</sup>)"
  } else if(char_string == "LW"){
    units_lab <- "<i>L<sub>w</sub></i> (W m<sup>-2</sup> sr<sup>-1</sup>)"
  } else if(char_string == "RHOW"){
    units_lab <- "<i>ρ<sub>w</sub></i> (sr<sup>-1</sup>)"
  }
  
  # Combine into data.frame
  if(char_string %in% c("ED", "LD", "LU", "LW", "RHOW")){
    pretty_labels <- data.frame(var_name = char_string,
                                units_lab = units_lab)
    
  } else {
    pretty_labels <- data.frame(sesnor_name = char_string,
                                sensor_col = sensor_col,
                                sensor_lab = sensor_lab)
  }
  return(pretty_labels)
}

# Plot data based on wavelength group
plot_global_nm <- function(df, var_name, sensor_X, sensor_Y){
  
  # Create sensor and unit labels
  sensor_X_labs <- pretty_label_func(sensor_X)
  sensor_Y_labs <- pretty_label_func(sensor_Y)
  var_labs <- pretty_label_func(var_name)
  
  # Detect Sentinel-3 data and react accordingly
  if(sensor_Y == "S3"){
    df_prep <- df |> 
      pivot_longer(cols = c(S3A, S3B), names_to = "Platform", values_to = "S3") |> 
      na.omit()
  } else {
    df_prep <- df
  }
  
  # Get max values
  max_X <- max(df_prep[sensor_X_labs$sensor_col], na.rm = TRUE)
  max_Y <- max(df_prep[sensor_Y_labs$sensor_col], na.rm = TRUE)
  max_axis <- max(max_X, max_Y)
  
  # Get global stats
  x_vec <- df_prep[[sensor_X_labs$sensor_col]]
  y_vec <- df_prep[[sensor_Y_labs$sensor_col]]
  
  # Calculate statistics
  df_stats <- base_stats(x_vec, y_vec)
  
  if(sensor_Y %in% sensor_Y %in% c("PACE_V2", "PACE_V30", "PACE_V31")){
    point_alpha <- 0.3
  } else if(var_name != "RHOW"){
    point_alpha <- 0.3
  } else {
    point_alpha <- 0.9
  }
  
  # Get pre-determined wavelengths
  W_nm <- W_nm_out(sensor_Y, var_name)
  
  # Filter dataframe to only plot linear models for chosen wavebands
  df_sub <- filter(df_prep, wavelength %in% W_nm)
  
  # Plot
  if(sensor_Y == "S3"){
    pl_base <- ggplot(data = df_prep, 
                      aes_string(x = sensor_X_labs$sensor_col, y = sensor_Y_labs$sensor_col)) +
      geom_point(aes(colour = wavelength_group, shape = Platform), size = 2, alpha = point_alpha)
  } else {
    pl_base <- ggplot(data = df_prep, 
                      aes_string(x = sensor_X_labs$sensor_col, y = sensor_Y_labs$sensor_col)) +
      geom_point(aes(colour = wavelength_group), alpha = point_alpha)
  }
  pl_clean <- pl_base +
    # Add 1:1 line
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid") +
    # Add global stats linear model
    geom_smooth(method = "lm", formula = y ~ x, colour = "white", linewidth = 1.5, linetype = "dashed", se = FALSE) +
    geom_smooth(method = "lm", formula = y ~ x, colour = "black", linewidth = 1, linetype = "dashed", se = FALSE) +
    # Add dashed linear model lines for each waveband
    # geom_smooth(data = df_sub, method = "lm", formula = y ~ x, linewidth = 1, linetype = "dashed", se = FALSE,
    #             aes(group = as.factor(wavelength)), colour = "black", show.legend = FALSE) +
    geom_smooth(data = df_sub, method = "lm", formula = y ~ x, linewidth = 1.5, linetype = "solid", se = FALSE,
                aes(group = as.factor(wavelength)), colour = "white", alpha = 0.7, show.legend = FALSE) +
    geom_smooth(data = df_sub, method = "lm", formula = y ~ x, linewidth = 1, linetype = "dashed", se = FALSE,
                aes(colour = wavelength_group, group = as.factor(wavelength)), alpha = 0.7, show.legend = FALSE) +
    # Add global stats text
    annotate(geom = "text", x = 0, y = max_axis, hjust = 0, vjust = 1, size = 4,
             label = paste0("Slope: ", df_stats$Slope, "\nβ: ", df_stats$Bias,"% \nϵ: ",df_stats$Error,"%")) +
    # Make it pretty
    labs(x = paste0(sensor_X_labs$sensor_lab,"; ", var_labs$units_lab),
         y = paste0(sensor_Y_labs$sensor_lab,"; ", var_labs$units_lab),
         colour = "Wavelength (nm)") +
    scale_colour_manual(values = colour_nm) +
    guides(colour = guide_legend(nrow = 1, override.aes = list(alpha = 1.0, size = 3))) +
    coord_fixed(xlim = c(0, max_axis), ylim = c(0, max_axis)) +
    theme_minimal() +
    theme(panel.border = element_rect(fill = NA, color = "black"),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.position = "bottom",
          axis.title.x = element_markdown(size = 12),
          axis.title.y = element_markdown(size = 12),
          axis.text = element_text(size = 10))
  # pl_clean
  return(pl_clean)
}

# For the PACE supplementary figure
map_PACE <- function(df){
  df |> 
    filter(!is.na(Rrs)) |> 
    ggplot() +
    borders(fill = "grey80") +
    geom_tile(aes(x = longitude, y = latitude, fill = Rrs)) +
    scale_colour_viridis_c(aesthetics = c("colour", "fill")) +
    labs(x = NULL, y = NULL) +
    coord_quickmap(xlim = c(min(df$longitude), max(df$longitude)),
                   ylim = c(min(df$latitude), max(df$latitude)))
}

