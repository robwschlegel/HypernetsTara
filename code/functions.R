# code/functions.R
# Code shared across the project


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(ncdf4)
library(FNN) # Needed for fastest nearest neighbor searching
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
  file_path <- paste0("~/pCloudDrive/Documents/OMTAB/HYPERNETS/tara_matchups_results_20260126/",
                      toupper(var_name),"_",toupper(sensor_X),"_vs_", toupper(sensor_Y))
}

# Load a single matchup file and create mean values from all replicates
# file_name <- "~/pCloudDrive/Documents/OMTAB/HYPERNETS/tara_matchups_results_v2/RHOW_HYPERNETS_vs_HYPERPRO/HYPERNETS_vs_HYPERPRO_vs_20240809T073700_RHOW.csv"
# file_name <- "~/pCloudDrive/Documents/OMTAB/HYPERNETS/tara_matchups_results_v2/ED_HYPERNETS_vs_TRIOS/HYPERNETS_vs_TRIOS_vs_20240808T065700_ED.csv"
load_matchup_mean <- function(file_name){
  
  # message(paste0("Started loading : ", file_name))
  
  # Load the csv file
  suppressMessages(
    df_match <- read_delim(file_name, delim = ";", col_types = "ciccnnic")
  )
  colnames(df_match)[1] <- "sensor"
  
  # Get means per file
  # Satellite matchups have a different structure than in situ matchups
  # TODO: Test this with a satellite matchup file
  # if("mean" %in% df_match$data_id){
    
  # } else {
    df_mean <- df_match |> 
      dplyr::select(-radiometer_id, -data_id, -type) |>
      filter(grepl(" 1", sensor)) |> 
      mutate(sensor = gsub(" 1$| 2$| 3$| 4$| 5$| 6$| 7$| 8$| 9$", "", sensor)) |>
      filter(sensor != "Hyp_nosc")
  # }

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

# Create a grid of sensor to ply over
sensor_grid <- function(var_name, sensor_Z){
  
  # Check that satellites aren't being called with Ed etc.
  if(var_name != "RHOW" & sensor_Z != "HYPERPRO") stop("Only RHOW data for satellites.")
  
  # Get sorrect sensor names
  if(var_name == "LD"){
    sensor_X <- c("HYPERNETS")
    sensor_Y <- c("TRIOS")
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

# Output desired wavelengths based on sensor_Y
W_nm_out <- function(sensor_Y){
  if(sensor_Y %in% c("HYPERNETS", "TRIOS", "HYPERPRO")){
    W_nm <- c(400, 412, 443, 490, 510, 560, 620, 673)
  } else if(sensor_Y %in% c("PACE_V2", "PACE_V30", "PACE_V31")){
    W_nm <- c(412, 443, 490, 510, 560, 673)
  } else if(sensor_Y == "AQUA"){
    W_nm <- c(412, 443, 488, 531, 555, 667)
  } else if(sensor_Y == "VIIRS_N"){
    W_nm = c(410, 443, 486, 551, 671)
  } else if(sensor_Y %in% c("VIIRS_J1", "VIIRS_J2")){
    W_nm <- c(411, 445, 489, 556, 667)
  } else if(sensor_Y %in% c("S3A", "S3B")){
    W_nm <- c(413, 443, 490, 560, 665, 681)
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
# file_path <- "~/pCloudDrive/Documents/OMTAB/HYPERNETS/tara_matchups_results_v2/RHOW_HYPERNETS_vs_HYPERPRO/HYPERNETS_vs_HYPERPRO_vs_20240809T073700_RHOW.csv"
# file_path <- "~/pCloudDrive/Documents/OMTAB/HYPERNETS/tara_matchups_results_v2/ED_HYPERNETS_vs_TRIOS/HYPERNETS_vs_TRIOS_vs_20240808T065700_ED.csv"
# file_path <- file_list[1]
process_matchup_file <- function(file_path, filter_table = NULL){
  
  # Load the mean data
  df_mean <- load_matchup_mean(file_path)
  
  # Filter based on filter_table if provided
  # NB: Not currently used
  if(!is.null(filter_table)){
    df_mean <- semi_join(df_mean, filter_table, by = c("sensor", "day", "time", "longitude", "latitude"))
  }
  
  if(nrow(df_mean) < 2){
    print(paste("Not enough data in", basename(file_path), "after filtering, skipping..."))
    return(NULL)
  }
  
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
                             RMSE = df_stats$RMSE,
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
# var_name = "RHOW"; sensor_X = "HYPERPRO"; sensor_Y = "PACE_v31"; filter_table = NULL
# var_name = "RHOW"; sensor_X = "HYPERNETS"; sensor_Y = "AQUA"; filter_table = NULL
process_matchup_folder <- function(var_name, sensor_X, sensor_Y, filter_table = NULL){
  
  # Create file path
  folder_path <- file_path_build(var_name, sensor_X, sensor_Y)
  
  # List all files in directory
  file_list <- list.files(folder_path, pattern = "*.csv", full.names = TRUE)
  
  # Remove files with 'all' in the name
  file_list <- file_list[!grepl("all|global", file_list)]
  
  # Initialise results data.frame
  df_results <- plyr::ldply(file_list, process_matchup_file, .parallel = TRUE, 
                            filter_table = filter_table)
  
  # Exit
  return(df_results)
}

# Process multiple folders based on request
# var_name = "LD"; sensor_Z = "HYPERPRO"
# var_name = "ED"; sensor_Z = "HYPERPRO"
# var_name = "RHOW"; sensor_Z = "VIIRS"
process_sensor <- function(var_name, sensor_Z, stat_choice = "matchup"){
  
  # Create ply grid
  ply_grid <- sensor_grid(var_name, sensor_Z)
  
  # Correct sensor_Z for file names upon saving
  if(var_name == "LD"){
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
# var_name = "RHOW"; sensor_X = "TRIOS"; sensor_Y = "AQUA"; W_nm = c(412, 443, 488, 531, 555, 667)
# var_name = "RHOW"; sensor_X = "HYPERNETS"; sensor_Y = "PACE_V31"; W_nm = c(412, 443, 490, 510, 560, 673)
# MAPE_limit = NULL; filter_table = NULL;
# W_nm = c(412, 443, 488, 531, 555, 667)
global_stats <- function(var_name, sensor_X, sensor_Y){#, 
                         # filter_table = NULL, MAPE_limit = NULL, 
                         # W_nm = c(400, 412, 443, 490, 510, 560, 620, 673)){
  
  # NB: Commented text here is used to filter by unique matchups
  # This was decided against as it limits the number of matchups dramatically
  
  # Get unique matchups
  # uniq_base <- n_matchup_uniq(var_name, sensor_X, sensor_Y, basic = FALSE)
  # uniq_base <- uniq_base[uniq_base$sensor_X == sensor_Y,] # NB: This X to Y cross is intentional
  
  # Create file path
  folder_path <- file_path_build(var_name, sensor_X, sensor_Y)
  
  # List all files in directory
  file_list <- list.files(folder_path, pattern = "*.csv", full.names = TRUE)
  
  # Remove stats output files
  # NB: No longer necessary
  file_list_clean <- file_list[!grepl("all|global", file_list)]
  
  # Load processed results for further filtering
  # suppressMessages(
  # file_all <- read_csv(file_list[grepl("all_", file_list)])
  # )
  
  # List files that respect unique matchups
  # NB: This allows for multiple in situ matchups against satellites
  # Ultimately this was considered desirable as otherwise the samples would be very small
  # file_uniq_list <- right_join(file_all, uniq_base, by = c("sensor_X", "sensor_Y", "dateTime_X"))
  
  # Filter based on filter_table if provided
  # Correct this to work with an ED file
  # if(!is.null(filter_table)){
  #   file_uniq_list <- semi_join(file_uniq_list, filter_table, by = c("sensor", "day", "time", "longitude", "latitude"))
  # }
  
  # Load data
  # match_base <- map_dfr(file.path(folder_path, file_uniq_list$file_name), load_matchup_long)
  # NB: Decided to run global results on all possible matchups
  match_base <- map_dfr(file_list_clean, load_matchup_long)
  # print(unique(match_base$wavelength))
  
  # The VIIRS versions have different wavelengths...
  # This hard coded fix is one method of dealing with this.
  # It may need to change in the future
  W_nm <- W_nm_out(sensor_Y)
  
  # For loop that cycles through the requested wavelengths and calculates stats
  df_results <- data.frame()
  for(i in 1:length(W_nm)){
    
    # Get data.frame for matchup based on the wavelength of choice
    matchup_filt <- filter(match_base, wavelength == W_nm[i])
    
    # if(!is.null(MAPE_limit)){
    #   matchup_filt <- filter(matchup_filt, MAPE <= MAPE_limit)
    # }
    n_match <- nrow(matchup_filt)
    
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
      df_stats <- base_stats(x_vec, y_vec)
      
      # Create data.frame of results and add them to df_results
      df_temp <- data.frame(row.names = NULL,
                            sensor_X = sensor_X,
                            sensor_Y = sensor_Y,
                            Wavelength_nm = W_nm[i],
                            n = n_match,
                            Slope = df_stats$Slope,
                            RMSE = df_stats$RMSE,
                            # MSA = df_stats$MSA,
                            MAPE = df_stats$MAPE,
                            Bias = df_stats$Bias,
                            Error = df_stats$Error)
      df_results <- rbind(df_results, df_temp)
    } else {
      print(paste0("No data for wavelength ", W_nm[i]))
    }
  }
  
  # Get file name and save
  # write_csv(df_results, paste0(folder_path, "/", var_name, "_", sensor_X, "_vs_", sensor_Y, "_global_stats.csv"))
  return(df_results)
}

