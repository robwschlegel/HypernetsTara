# code/matchups.R
# Get the stats for all matchups and visualise results


# Setup -------------------------------------------------------------------

source("code/functions.R")


# Individual matchup stats ------------------------------------------------

# Function that interrogates each matchup file to produce the needed output for all following comparisons
# file_path <- "~/pCloudDrive/Documents/OMTAB/HYPERNETS/tara_matchups_results/RHOW_HYPERNETS_vs_HYPERPRO/HYPERNETS_vs_HYPERPRO_vs_20240809T073700_RHOW.csv"
# file_path <- file_list[1]
process_matchup_file <- function(file_path, filter_table = NULL){
  
  # Load the data
  suppressMessages(
    df_base <- read_delim(file_path, delim = ";")
  )
  colnames(df_base)[1] <- "sensor"
  
  # Removes 1,2,3 etc. from sensor column values
  df_mean <- df_base |> 
    dplyr::select(-radiometer_id, -data_id) |>
    mutate(sensor = gsub(" 1$| 2$| 3$| 4$| 5$| 6$| 7$| 8$| 9$", "", sensor),
           day = as.character(day),
           time = as.character(time)) |> 
    # Remove HyperNets pre-processed data
    filter(sensor != "Hyp_nosc") |> 
    # mutate(dateTime = as.POSIXct(paste(day, time), format = "%Y%m%d %H%M%S")) |> # make this later
    # NB: It is a choice to allow the lon/lat values to get averaged
    # This is done for the satellite data as each pixel is given as a row and need to be meaned
    group_by(sensor, type, day, time) |> #, longitude, latitude) |>
    summarise_all(mean, na.rm = TRUE) |> 
    ungroup()
  # print(df_mean[,1:10])
  
  # Filter based on filter_table if provided
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
          mutate(dateTime = as.POSIXct(paste(day, time), format = "%Y%m%d %H%M%S"), .after = "type", .keep = "unused")
        
        # get distances
        hav_dist <- round(distHaversine(df_sensor_sub[c("longitude", "latitude")])/1000, 2) # distance in km
        
        # Time differences
        time_diff <- round(as.numeric(abs(difftime(df_sensor_sub$dateTime[[1]],
                                                   df_sensor_sub$dateTime[[2]], units = "mins"))))
        
        # Melt it for additional stats
        df_sensor_long <- df_sensor_sub |> 
          pivot_longer(cols = matches("1|2|3|4|5|6|7|8|9"), names_to = "Wavelength", values_to = "Value") |> 
          dplyr::select(-dateTime, -longitude, -latitude, -type) |> 
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
  
  # Save results and exit
  if(is.null(filter_table)){
    file_name <- paste0("all_",basename(folder_path),"_matchup_stats.csv")
  }  else {
    file_name <- paste0("all_",basename(folder_path),"_matchup_stats_filtered.csv")
  }
  write_csv(df_results, file.path(folder_path, file_name))
}

# Run for all folders
## In situ matchups
### ED
process_matchup_folder("ED", "HYPERNETS", "HYPERPRO")
process_matchup_folder("ED", "HYPERNETS", "TRIOS")
process_matchup_folder("ED", "TRIOS", "HYPERPRO")
### LD
process_matchup_folder("LD", "HYPERNETS", "TRIOS")
### LU
process_matchup_folder("LU", "HYPERNETS", "HYPERPRO")
process_matchup_folder("LU", "HYPERNETS", "TRIOS")
process_matchup_folder("LU", "TRIOS", "HYPERPRO")
### LW
process_matchup_folder("LW", "HYPERNETS", "HYPERPRO")
process_matchup_folder("LW", "HYPERNETS", "TRIOS")
process_matchup_folder("LW", "TRIOS", "HYPERPRO")
## Satellite
### OCI
#### PACE v2
##### Missing
#### PACE v3
##### Missing
#### PACE v3.1
process_matchup_folder("RHOW", "HYPERNETS", "PACE_V31")
process_matchup_folder("RHOW", "TRIOS", "PACE_V31")
process_matchup_folder("RHOW", "HYPERPRO", "PACE_V31")
### MODIS
#### AQUA
process_matchup_folder("RHOW", "HYPERNETS", "AQUA")
process_matchup_folder("RHOW", "TRIOS", "AQUA")
process_matchup_folder("RHOW", "HYPERPRO", "AQUA")
### VIIRS
#### SNPP
process_matchup_folder("RHOW", "HYPERNETS", "VIIRS_N")
process_matchup_folder("RHOW", "TRIOS", "VIIRS_N")
process_matchup_folder("RHOW", "HYPERPRO", "VIIRS_N")
#### JPSS1
process_matchup_folder("RHOW", "HYPERNETS", "VIIRS_J1")
process_matchup_folder("RHOW", "TRIOS", "VIIRS_J1")
process_matchup_folder("RHOW", "HYPERPRO", "VIIRS_J1")
#### JPSS2
process_matchup_folder("RHOW", "HYPERNETS", "VIIRS_J2")
process_matchup_folder("RHOW", "TRIOS", "VIIRS_J2")
process_matchup_folder("RHOW", "HYPERPRO", "VIIRS_J2")
### OLCI
#### S3A
process_matchup_folder("RHOW", "HYPERNETS", "S3A")
process_matchup_folder("RHOW", "TRIOS", "S3A")
process_matchup_folder("RHOW", "HYPERPRO", "S3A")
#### S3B
process_matchup_folder("RHOW", "HYPERNETS", "S3B")
process_matchup_folder("RHOW", "TRIOS", "S3B")
process_matchup_folder("RHOW", "HYPERPRO", "S3B")


# Duplicated matchups -----------------------------------------------------

# Convenience wrapper to count number of duplicated matchups
n_matchup_dup <- function(file_path){
  
  # Load data
  suppressMessages(
    match_base <- read_csv(file_path)
  )
  
  # Group by lon/lat and sensor and count
  df_counts <- match_base |> 
    summarise(n_matchups = n(), .by = c(dateTime_X, lon_X, lat_X, sensor_X)) |> 
    arrange(desc(n_matchups))
  
  return(df_counts)
}

# Count the number of times the same station is compared against others
# in_situ_ED_dup <- n_matchup_dup("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_ED/all_MATCHUPS_in-situ_dm_10_ED_matchup_stats.csv")
# in_situ_LD_dup <- n_matchup_dup("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_LD/all_MATCHUPS_in-situ_dm_10_LD_matchup_stats.csv")
# in_situ_LW_dup <- n_matchup_dup("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_LW/all_MATCHUPS_in-situ_dm_10_LW_matchup_stats.csv")
in_situ_RW_dup <- n_matchup_dup("~/pCloudDrive/Documents/OMTAB/HYPERNETS/tara_matchups_results/RHOW_HYPERNETS_vs_HYPERPRO/all_RHOW_HYPERNETS_vs_HYPERPRO_matchup_stats.csv")

# Plot Ed duplicates (all duplicates are same except Ld)
in_situ_RW_dup_map <- in_situ_RW_dup |> 
  filter(sensor_X == "HYPERPRO") |> 
  ggplot(aes(x = lon_X, y = lat_X)) +
  annotation_borders("world", colour = "gray80", fill = "gray80") +
  geom_point(aes(size = n_matchups), color = "darkblue", alpha = 0.7) +
  coord_quickmap(xlim = c(min(in_situ_RW_dup$lon_X)-2, max(in_situ_RW_dup$lon_X)+2),
                 ylim = c(min(in_situ_RW_dup$lat_X)-2, max(in_situ_RW_dup$lat_X))+1) +
  labs(title = "In situ HYPERPRO Matchup Duplicates",
       x = "Longitude (°E)",
       y = "Latitude (°N)",
       size = "Number of Matchups") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.border = element_rect(fill = NA, color = "black"))
ggsave("figures/test_HYPERPRO_matchup_duplicates.png", in_situ_RW_dup_map, width = 8, height = 6, dpi = 600)


# Unique matchups ---------------------------------------------------------

# Convenience wrapper to count number of unique matchups
# var_name = "rhow"; sensor_X = "HYPERPRO"; sensor_Y = "PACE_v31"
n_matchup_uniq <- function(var_name, sensor_X, sensor_Y, basic = TRUE){
  
  # Create file path
  folder_path <- file_path_build(var_name, sensor_X, sensor_Y)
  file_path <- dir(folder_path, pattern = "all_", full.names = TRUE)
  
  # Load data
  suppressMessages(
    match_base <- read_csv(file_path)
  )
  
  # Get sensors
  sensors_X <- unique(match_base$sensor_X)
  sensors_Y <- unique(match_base$sensor_Y)
  
  # Cycle through sensors
  df_uniq_res <- data.frame()
  for(i in 1:length(sensors_X)){
    for(j in 1:length(sensors_Y)){
      if(sensors_Y[j] != sensors_X[i]){
        
        # Get data.frame for matchup based on the two sensors being compared
        # and get only uniqe time stamps for sensor X
        df_sensor_uniq <- match_base |> 
          filter(sensor_X == sensors_X[i], 
                 sensor_Y == sensors_Y[j]) |> 
          dplyr::select(sensor_X, sensor_Y, dateTime_X, dateTime_Y) 
        
        # Filter out near-duplicate HyperPRO sample
        if(sensors_X[i] == "HYPERPRO"){
          df_sensor_uniq <- df_sensor_uniq |> 
            filter(as.character(dateTime_X) != "2024-08-16 07:58:00")
            # filter(dateTime_X != "2024-08-16 08:05:00")
        }
        if(sensors_Y[i] == "HYPERPRO"){
          df_sensor_uniq <- df_sensor_uniq |> 
            filter(as.character(dateTime_Y) != "2024-08-16 07:58:00")
          # filter(dateTime_X != "2024-08-16 08:05:00")
        }
        if(basic){
          df_sensor_uniq <- df_sensor_uniq |> 
            dplyr::select(-dateTime_Y) |> 
            distinct() |> 
            summarise(uniq = n(), .by = c("sensor_X", "sensor_Y"))
          
          # Add to dataframe of results
          df_uniq_res <- rbind(df_uniq_res, df_sensor_uniq)
        } else { 
          df_sensor_uniq <- df_sensor_uniq |> 
            dplyr::select(-dateTime_Y) |> 
            distinct()
          
          # Add to dataframe of results
          df_uniq_res <- rbind(df_uniq_res, df_sensor_uniq)
        }
      }
    }
  }
  return(df_uniq_res)
}

# In situ
## Ed
## NB: These counts are very different from Rhow
### ED
n_matchup_uniq("ED", "HYPERNETS", "HYPERPRO")
n_matchup_uniq("ED", "HYPERNETS", "TRIOS")
n_matchup_uniq("ED", "TRIOS", "HYPERPRO")
### LD
n_matchup_uniq("LD", "HYPERNETS", "TRIOS")
### LU
n_matchup_uniq("LU", "HYPERNETS", "HYPERPRO")
n_matchup_uniq("LU", "HYPERNETS", "TRIOS")
n_matchup_uniq("LU", "TRIOS", "HYPERPRO")
### LW
n_matchup_uniq("LW", "HYPERNETS", "HYPERPRO")
n_matchup_uniq("LW", "HYPERNETS", "TRIOS")
n_matchup_uniq("LW", "TRIOS", "HYPERPRO")
### Rhow
n_matchup_uniq("RHOW", "HYPERNETS", "HYPERPRO")
n_matchup_uniq("RHOW", "HYPERNETS", "TRIOS")
n_matchup_uniq("RHOW", "TRIOS", "HYPERPRO")

# Remote
## OCI
### PACE v2
#### Missing
### PACE v3
#### Missing
### PACE v3.1
n_matchup_uniq("RHOW", "HYPERNETS", "PACE_v31")
n_matchup_uniq("RHOW", "TRIOS", "PACE_v31")
n_matchup_uniq("RHOW", "HYPERPRO", "PACE_v31")
## MODIS
### AQUA
n_matchup_uniq("RHOW", "HYPERNETS", "AQUA")
n_matchup_uniq("RHOW", "TRIOS", "AQUA")
n_matchup_uniq("RHOW", "HYPERPRO", "AQUA")
## VIIRS
### SNPP
n_matchup_uniq("RHOW", "HYPERNETS", "VIIRS_N")
n_matchup_uniq("RHOW", "TRIOS", "VIIRS_N")
n_matchup_uniq("RHOW", "HYPERPRO", "VIIRS_N")
### JPSS1
n_matchup_uniq("RHOW", "HYPERNETS", "VIIRS_J1")
n_matchup_uniq("RHOW", "TRIOS", "VIIRS_J1")
n_matchup_uniq("RHOW", "HYPERPRO", "VIIRS_J1")
### JPSS2
n_matchup_uniq("RHOW", "HYPERNETS", "VIIRS_J2")
n_matchup_uniq("RHOW", "TRIOS", "VIIRS_J2")
n_matchup_uniq("RHOW", "HYPERPRO", "VIIRS_J2")
## OLCI
### S3A
n_matchup_uniq("RHOW", "HYPERNETS", "S3A")
n_matchup_uniq("RHOW", "TRIOS", "S3A")
n_matchup_uniq("RHOW", "HYPERPRO", "S3A")
### S3B
n_matchup_uniq("RHOW", "HYPERNETS", "S3B")
n_matchup_uniq("RHOW", "TRIOS", "S3B")
n_matchup_uniq("RHOW", "HYPERPRO", "S3B")


# List of Rw matchups -----------------------------------------------------

# NB: At the moment I am not using this as the Ed etc. matchups are meant to have been filtered beforehand

# Find all of the Rhow matchups and remove Ed and Lw that didn't pass QC to Rhow
# RW_files <- list.files(c("~/pCloudDrive/Documents/OMTAB/HYPERNETS/tara_matchups_results/RHOW_HYPERNETS_vs_TRIOS",
#                          "~/pCloudDrive/Documents/OMTAB/HYPERNETS/tara_matchups_results/RHOW_HYPERNETS_vs_HYPERPRO",
#                          "~/pCloudDrive/Documents/OMTAB/HYPERNETS/tara_matchups_results/RHOW_TRIOS_vs_HYPERPRO"), 
#                        pattern = "*.csv", full.names = TRUE, recursive = TRUE)
# RW_files <- RW_files[!grepl("all", RW_files)]

# There is an issue with some day or time values being non-numeric
# RW_load <- function(file_name){
#   suppressMessages(
#     df <- read_delim(file_name, delim = ";") |> 
#       mutate(day = as.character(day),
#              time = as.character(time))
#   )
#   colnames(df)[1] <- "sensor"
#   df <- dplyr::select(df, sensor, day, time, longitude, latitude) |> 
#     mutate(sensor = gsub(" 1$| 2$| 3$| 4$| 5$| 6$| 7$| 8$| 9$", "", sensor)) |> 
#     distinct()
#   return(df)
# }
# RW_all <- plyr::ldply(RW_files, RW_load, .parallel = TRUE) |>
#   distinct() |> arrange(sensor, day, time)
# write_csv(RW_all, "meta/all_in-situ_RHOW_stations.csv")
RW_all <- read_csv("meta/all_in-situ_RHOW_stations.csv", col_types = "cccdd") |> 
  filter(sensor != "Hyp_nosc") |> 
  filter(!(sensor == "HYPERPRO" & time == "095800")) # Remove near duplicate HyperPRO station
  # mutate(dateTime = as.POSIXct(paste(day, time), format = "%Y%m%d %H%M%S"), .after = "sensor", .keep = "unused")


# Global statistics --------------------------------------------------------

# Global stats per matchup wavelength
# testers..
# var_name = "RHOW"; sensor_X = "TRIOS"; sensor_Y = "AQUA"; W_nm = c(412, 443, 488, 531, 555, 667)
# var_name = "RHOW"; sensor_X = "HYPERNETS"; sensor_Y = "PACE_V31"; W_nm = c(412, 443, 490, 510, 560, 673)
# MAPE_limit = NULL; filter_table = NULL;
# W_nm = c(412, 443, 488, 531, 555, 667)
global_stats <- function(var_name, sensor_X, sensor_Y, 
                         filter_table = NULL, MAPE_limit = NULL, 
                         W_nm = c(400, 412, 443, 490, 510, 560, 620, 673)){
  
  # NB: Commented text here is used to filter by unique matchups
  # THis was decided against as it limits the number of matchups dramatically
  
  # Get unique matchups
  # uniq_base <- n_matchup_uniq(var_name, sensor_X, sensor_Y, basic = FALSE)
  # uniq_base <- uniq_base[uniq_base$sensor_X == sensor_Y,] # NB: This X to Y cross is intentional
  
  # Create file path
  folder_path <- file_path_build(var_name, sensor_X, sensor_Y)
  
  # List all files in directory
  file_list <- list.files(folder_path, pattern = "*.csv", full.names = TRUE)
  
  # Remove stats output files
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
  # TODO: Correct this to work with an ED file
  # if(!is.null(filter_table)){
  #   file_uniq_list <- semi_join(file_uniq_list, filter_table, by = c("sensor", "day", "time", "longitude", "latitude"))
  # }
  
  # Load data
  # match_base <- map_dfr(file.path(folder_path, file_uniq_list$file_name), load_matchup_long)
  # NB: Decided to run global results on all possible matchups
  match_base <- map_dfr(file_list_clean, load_matchup_long)
  print(unique(match_base$wavelength))
  
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
      
      # Get column names for subsetting
      # x_col <- paste0("X_value(", W_nm[k], ")")
      # y_col <- paste0("Y_value(", W_nm[k], ")")
      
      # if(length(matchup_filt[[y_col]][!is.na(matchup_filt[[y_col]])]) == 0){
      #   print(paste("No data for wavelength", W_nm[k], "nm, skipping..."))
      #   next
      # }
      
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
  write_csv(df_results, paste0(folder_path, "/", var_name, "_", sensor_X, "_vs_", sensor_Y, "_global_stats.csv"))
  return(df_results)
}

# Check stats
## In situ matchups
### ED
global_stats("ED", "HYPERNETS", "HYPERPRO")
global_stats("ED", "HYPERNETS", "TRIOS")
global_stats("ED", "TRIOS", "HYPERPRO")
### LD
global_stats("LD", "HYPERNETS", "TRIOS")
### LU
global_stats("LU", "HYPERNETS", "HYPERPRO")
global_stats("LU", "HYPERNETS", "TRIOS")
global_stats("LU", "TRIOS", "HYPERPRO")
### LW
global_stats("LW", "HYPERNETS", "HYPERPRO")
global_stats("LW", "HYPERNETS", "TRIOS")
global_stats("LW", "TRIOS", "HYPERPRO")
### Rhow
global_stats("RHOW", "HYPERNETS", "HYPERPRO")
global_stats("RHOW", "HYPERNETS", "TRIOS")
global_stats("RHOW", "TRIOS", "HYPERPRO")
## Satellite
### OCI
#### PACE v2
##### Missing
#### PACE v3
##### Missing
#### PACE v3.1
global_stats("RHOW", "HYPERNETS", "PACE_V31", W_nm = c(412, 443, 490, 510, 560, 673))
global_stats("RHOW", "TRIOS", "PACE_V31", W_nm = c(412, 443, 490, 510, 560, 673))
global_stats("RHOW", "HYPERPRO", "PACE_V31", W_nm = c(412, 443, 490, 510, 560, 673))
### MODIS
#### AQUA
global_stats("RHOW", "HYPERNETS", "AQUA", W_nm = c(412, 443, 488, 531, 555, 667))
global_stats("RHOW", "TRIOS", "AQUA", W_nm = c(412, 443, 488, 531, 555, 667))
global_stats("RHOW", "HYPERPRO", "AQUA", W_nm = c(412, 443, 488, 531, 555, 667))
### VIIRS
#### SNPP
global_stats("RHOW", "HYPERNETS", "VIIRS_N", W_nm = c(410, 443, 486, 551, 671))
global_stats("RHOW", "TRIOS", "VIIRS_N", W_nm = c(410, 443, 486, 551, 671))
global_stats("RHOW", "HYPERPRO", "VIIRS_N", W_nm = c(410, 443, 486, 551, 671))
#### JPSS1
global_stats("RHOW", "HYPERNETS", "VIIRS_J1", W_nm = c(411, 445, 489, 556, 667))
global_stats("RHOW", "TRIOS", "VIIRS_J1", W_nm = c(411, 445, 489, 556, 667))
global_stats("RHOW", "HYPERPRO", "VIIRS_J1", W_nm = c(411, 445, 489, 556, 667))
#### JPSS2
global_stats("RHOW", "HYPERNETS", "VIIRS_J2", W_nm = c(411, 445, 489, 556, 667))
global_stats("RHOW", "TRIOS", "VIIRS_J2", W_nm = c(411, 445, 489, 556, 667))
global_stats("RHOW", "HYPERPRO", "VIIRS_J2", W_nm = c(411, 445, 489, 556, 667))
### OLCI
#### S3A
global_stats("RHOW", "HYPERNETS", "S3A", W_nm = c(413, 443, 490, 560, 665, 681))
global_stats("RHOW", "TRIOS", "S3A", W_nm = c(413, 443, 490, 560, 665, 681))
global_stats("RHOW", "HYPERPRO", "S3A", W_nm = c(413, 443, 490, 560, 665, 681))
#### S3B
global_stats("RHOW", "HYPERNETS", "S3B", W_nm = c(413, 443, 490, 560, 665, 681))
global_stats("RHOW", "TRIOS", "S3B", W_nm = c(413, 443, 490, 560, 665, 681))
global_stats("RHOW", "HYPERPRO", "S3B", W_nm = c(413, 443, 490, 560, 665, 681))


# Filter global stats based on MAPE ---------------------------------------

# Load Ed
# Plot difference in Rw filtered vs unfiltered matchups
in_situ_ED_matchups <- read_csv("~/pCloudDrive/Documents/OMTAB/HYPERNETS/tara_matchups_results/ED_HYPERNETS_vs_TRIOS/all_ED_HYPERNETS_vs_TRIOS_matchup_stats.csv") |> 
  mutate(sensor_pair = paste(sensor_X, "vs", sensor_Y), .before = sensor_X,
         date = as.Date(dateTime_X)) |> 
  filter(sensor_pair %in% c("HYPERPRO vs Hyp", "HYPERPRO vs TRIOS", "Hyp vs TRIOS"))
# in_situ_ED_matchups_filt <- read_csv("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_ED/all_MATCHUPS_in-situ_dm_10_ED_matchup_stats_filtered.csv") |> 
#   mutate(sensor_pair = paste(sensor_X, "vs", sensor_Y), .before = sensor_X,
#          date = as.Date(dateTime_X)) |> 
#   filter(sensor_pair %in% c("HYPERPRO vs Hyp", "HYPERPRO vs TRIOS", "Hyp vs TRIOS"))

# Plot how the number of matchups is reduced with increasing MAPE threshold
plot_MAPE_hist <- function(df, var_name, extra_title = ""){
  ggplot(df, aes(x = MAPE)) +
    geom_histogram(binwidth = 1, fill = "darkblue", color = "black", alpha = 0.7) +
    geom_vline(xintercept = 5, color = "red", linetype = "dashed", linewidth = 1) +
    labs(title = paste0(var_name, " matchup MAPE Distribution",extra_title),
         x = "MAPE (%)",
         y = "Count") +
    theme_minimal()
}
plot_MAPE_hist(in_situ_ED_matchups, "Ed")
# plot_MAPE_hist(in_situ_ED_matchups_filt, "Ed", "; Rw filter")

# Save
plot_ed_MAPE_hist <- ggpubr::ggarrange(plot_MAPE_hist(in_situ_ED_matchups, "ED"),
                                       plot_MAPE_hist(in_situ_ED_matchups_filt, "ED", "; Rw filter"),
                                       ncol = 2, nrow = 1, labels = c("a)", "b)"))
ggsave("figures/test_Ed_MAPE_hist.png", plot_ed_MAPE_hist, width = 9, height = 4.5, dpi = 600)

# Also how this reduces the matchups on the map
plot_insitu_map_MAPE <- function(df, var_name, MAPE_filt, extra_title = ""){
  
  # Number of samples for label
  n_samples <- nrow(filter(df, MAPE <= MAPE_filt))
  
  # Plot
  df |> 
    filter(MAPE <= MAPE_filt) |> 
    ggplot(aes(x = lon_X, y = lat_X)) +
    annotation_borders("world", colour = "gray80", fill = "gray80") +
    geom_point(aes(colour = MAPE), size = 3) +
    annotate("label", x = min(df$lon_X)-1, y = max(df$lat_X)+1,
             label = paste0("n = ", n_samples),
             fill = "white", color = "black", size = 5, fontface = "bold") +
    scale_colour_viridis_c(option = "plasma") +
    # geom_point(aes(size = MAPE, colour = sensor_pair), alpha = 0.7) +
    # scale_colour_manual(values = c("HYPERPRO vs Hyp" = "darkblue",
    #                                "HYPERPRO vs TRIOS" = "darkgreen",
    #                                "Hyp vs TRIOS" = "darkred")) +
    coord_quickmap(xlim = c(min(df$lon_X)-2, max(df$lon_X)+2),
                   ylim = c(min(df$lat_X)-2, max(df$lat_X))+1) +
    labs(title = paste0("In situ ", var_name," Matchups (MAPE ≤ ", MAPE_filt, "%",extra_title,")"),
         x = "Longitude (°E)",
         y = "Latitude (°N)",
         # colour = "Sensor Pair",
         colour = "MAPE (%)") +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.border = element_rect(fill = NA, color = "black"))
}
plot_insitu_map_MAPE(in_situ_ED_matchups, "ED", 5)
# plot_insitu_map_MAPE(in_situ_ED_matchups_filt, "ED", 5, "; Rw filter")
plot_insitu_map_MAPE(in_situ_ED_matchups, "ED", 50)
# plot_insitu_map_MAPE(in_situ_ED_matchups_filt, "ED", 50, "; Rw filter")

# Save
plot_ed_MAPE_filt <- ggpubr::ggarrange(plot_insitu_map_MAPE(in_situ_ED_matchups, "ED", 5),
                                       plot_insitu_map_MAPE(in_situ_ED_matchups, "ED", 50),
                                       plot_MAPE_hist(in_situ_ED_matchups, "ED"),
                                       ncol = 3, nrow = 1, labels = c("a)", "b)", "c)"))
ggsave("figures/test_Ed_MAPE_filt.png", plot_ed_MAPE_filt, width = 18, height = 4.5, dpi = 600)
# plot_ed_MAPE_RW_filt <- ggpubr::ggarrange(plot_insitu_map_MAPE(in_situ_ED_matchups, "ED", 5),
#                                        plot_insitu_map_MAPE(in_situ_ED_matchups_filt, "ED", 5, "; Rw filter"),
#                                        plot_insitu_map_MAPE(in_situ_ED_matchups, "ED", 50),
#                                        plot_insitu_map_MAPE(in_situ_ED_matchups_filt, "ED", 50, "; Rw filter"),
#                                        ncol = 2, nrow = 2, labels = c("a)", "b)", "c)", "d)"), align = "hv")
# ggsave("figures/test_Ed_MAPE_RW_filt.png", plot_ed_MAPE_RW_filt, width = 12, height = 8.7, dpi = 600)

# Comparison of HypStar versus PACE_V31
rhow_hyp_pace31 <- read_csv("~/pCloudDrive/Documents/OMTAB/HYPERNETS/tara_matchups_results/RHOW_HYPERNETS_vs_PACE_V31/all_RHOW_HYPERNETS_vs_PACE_V31_matchup_stats.csv")

# Plot
## MAPE as function of difftime vs distance
rhow_hyp_pace31 |> 
  filter(sensor_X == "Hyp") |> 
  ggplot(aes( x = diff_time, y = dist)) +
  geom_point(aes(size = MAPE)) +
  geom_smooth(method = "lm")

## MAPE as function of difftime
rhow_hyp_pace31 |> 
  filter(sensor_X == "Hyp") |> 
  ggplot(aes( x = diff_time, y = MAPE)) +
  geom_point() +
  geom_smooth(method = "lm")

## MAPE as function of distance
rhow_hyp_pace31 |> 
  filter(sensor_X == "Hyp") |> 
  ggplot(aes( x = dist, y = MAPE)) +
  geom_point() +
  geom_smooth(method = "lm")


# List of bad matchups ----------------------------------------------------

# This is based on all of the high MAPE values detected above
# NB: This is just an example of what an output could look like
# One could then act on the bias value to decide whether to block the use of sensor X or Y
in_situ_ED_bad <- read_csv("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_ED/all_MATCHUPS_in-situ_dm_10_ED_matchup_stats.csv") |> 
  mutate(sensor_pair = paste(sensor_X, "vs", sensor_Y), .before = sensor_X,
         date = as.Date(dateTime_X)) |> 
  filter(sensor_pair %in% c("HYPERPRO vs Hyp", "HYPERPRO vs TRIOS", "Hyp vs TRIOS")) |> 
  filter(MAPE > 5)


# Plot matchups -----------------------------------------------------------

# Ed
in_situ_ED_all_stats <- read_csv("~/pCloudDrive/Documents/OMTAB/HYPERNETS/tara_matchups_results/ED_HYPERNETS_vs_HYPERPRO/all_ED_HYPERNETS_vs_HYPERPRO_matchup_stats.csv") |> 
  filter(!(sensor_X == "Hyp" & sensor_Y == "Hyp_nosc")) |> 
  filter(!(sensor_X == "Hyp_nosc" & sensor_Y == "Hyp")) |> 
  mutate(sensor_pair = paste(sensor_X, "vs", sensor_Y), .before = sensor_X,
         date = as.Date(dateTime_X))

# Reduce data density for plotting
in_situ_ED_all_stats_sub <- filter(in_situ_ED_all_stats, sensor_pair %in% c("HYPERPRO vs Hyp", "HYPERPRO vs TRIOS", "Hyp vs TRIOS"))

# diffTime as a function of distance
ggplot(in_situ_ED_all_stats_sub, aes(x = dist, y = diff_time)) +
  geom_point(aes(colour = sensor_pair)) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_grid(sensor_pair ~ date, scales = "free")

# MAPE as a function of distance
ggplot(in_situ_ED_all_stats_sub, aes(x = dist, y = MAPE)) +
  geom_point(aes(colour = sensor_pair)) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_grid(sensor_pair ~ date, scales = "free")

# MAPE as a function of diffTime
ggplot(in_situ_ED_all_stats_sub, aes(x = diff_time, y = MAPE)) +
  geom_point(aes(colour = sensor_pair)) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_grid(sensor_pair ~ date, scales = "free")

# MAPE as a function of distance and diffTime
ggplot(in_situ_ED_all_stats_sub, aes(x = dist, y = diff_time)) +
  geom_point(aes(size = MAPE, colour = sensor_pair)) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_grid(sensor_pair ~ date, scales = "free")

# Map the values based on Sensor x lon/lat
ggplot(in_situ_ED_all_stats_sub, aes(x = lon_X, y = lat_X)) +
  annotation_borders("world", colour = "gray80", fill = "gray80") +
  geom_point(aes(size = MAPE, colour = sensor_pair), alpha = 0.7) +
  scale_colour_manual(values = c("HYPERPRO vs Hyp" = "darkblue",
                                 "HYPERPRO vs TRIOS" = "darkgreen",
                                 "Hyp vs TRIOS" = "darkred")) +
  coord_quickmap(xlim = c(min(in_situ_ED_all_stats_sub$lon_X)-2, max(in_situ_ED_all_stats_sub$lon_X)+2),
                 ylim = c(min(in_situ_ED_all_stats_sub$lat_X)-2, max(in_situ_ED_all_stats_sub$lat_X))+1) +
  labs(title = "In situ ED Matchup",
       x = "Longitude (°E)",
       y = "Latitude (°N)",
       colour = "Sensor Pair",
       size = "MAPE (%)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.border = element_rect(fill = NA, color = "black"))

