# code/matchups.R
# Get the stats for all matchups and visualise results

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(geosphere) # For determining distance between points
library(doParallel); registerDoParallel(cores = detectCores() - 2)

# Define the wavelength (nm) band colour palette
labels_nm <- c("350-400", "400-450", "450-500", "500-550", "550-600", "600-650", "650-700", "700-750", "750-800")
colour_nm <- c('darkviolet', 'violet', 'blue', 'darkgreen', 'yellow', 'orange', 'red', "firebrick", 'sienna')
names(colour_nm) <- labels_nm


# Matchups count ----------------------------------------------------------

# Convenience wrapper to get counts
matchups_count <- function(file_path, remote = TRUE){
  
  # Load data
  match_base <- read_excel(file_path)
  
  # In situ sensors
  sensors_X <- unique(match_base$X_data)
  
  # Remote sensor(s)
  if(remote){
    sensors_Y <- unique(match_base$Y_data)[!unique(match_base$Y_data) %in% unique(match_base$X_data)]
  } else {
    sensors_Y <- unique(match_base$Y_data)
  }
  
  
  # Print sensors to make sure everything is good
  print(paste("In situ sensors:", paste(sensors_X, collapse = ", ")))
  print(paste("Remote sensors:", paste(sensors_Y, collapse = ", ")))
  
  # Print out matchups via nested for loop
  for(i in 1:length(sensors_Y)){
    for(j in 1:length(sensors_X)){
      if(sensors_X[j] != sensors_Y[i]){
        n_match <- filter(match_base, X_data == sensors_X[j], Y_data == sensors_Y[i], !is.na(dateTime)) |> nrow()
        print(paste(sensors_X[j], "vs", sensors_Y[i], "=", n_match))
      }
    }
  }
}

# Check matchups
## Satellite
matchups_count("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/MODIS/all_metrics_MODIS_AQUA_L2A_RHOW.xlsx")
matchups_count("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/OLCI/all_metrics_min_350_max_800_OLCI_S3B_L2A_RHOW.xlsx")
# matchups_count("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/OLCI_S3A/all_metrics_min_350_max_800_OLCI_S3A_L2A_RHOW.xlsx")
# matchups_count("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/OLCI_S3B/all_metrics_min_350_max_800_OLCI_S3B_L2A_RHOW.xlsx")
matchups_count("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/PACEV3/all_metrics_min_350_max_800_PACE_OCI_L2A_RHOW.xlsx")
matchups_count("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/VIIRS_J1/all_metrics_min_350_max_800_VIIRS_JPSS1_L2A_RHOW.xlsx")
matchups_count("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/VIIRS_J2/all_metrics_min_350_max_800_VIIRS_JPSS2_L2A_RHOW.xlsx")
matchups_count("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/VIIRS_N/all_metrics_min_350_max_800_VIIRS_SNPP_L2A_RHOW.xlsx")
## In situ
matchups_count("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_RHOW/all_metrics_min_350_max_800_L2A_RHOW.xlsx", remote = FALSE)
matchups_count("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_ED/all_metrics_min_350_max_800_L1C_ED.xlsx", remote = FALSE)
matchups_count("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_LD/all_metrics_min_350_max_800_L1C_LD.xlsx", remote = FALSE)
matchups_count("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_LW/all_metrics_min_350_max_800_L1C_LW.xlsx", remote = FALSE)


# Individual matchup stats ------------------------------------------------

# Function that interrogates each matchup file to produce the needed output for all following comparisons
# file_path <- "~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_ED/HYPERNETS_W_TAFR_L1C_ED_20240808T0657_270_vs_TRIOS_350_800.csv"
# file_path <- file_list[1]
process_matchup_file <- function(file_path){
  
  # Load the data
  suppressMessages(
    df_base <- read_delim(file_path, delim = ";")
  )
  colnames(df_base)[1] <- "sensor"
  
  # Removes 1,2,3 etc. from sensor column values
  df_mean <- df_base |> 
    mutate(sensor = gsub(" 1$| 2$| 3$| 4$| 5$| 6$| 7$| 8$| 9$", "", sensor)) |> 
    # mutate(dateTime = as.POSIXct(paste(day, time), format = "%Y%m%d %H%M%S")) |> # make this later
    group_by(sensor, day, time, longitude, latitude) |>
    summarise_all(mean, na.rm = TRUE) |> 
    ungroup()
  # print(df_mean[,1:10])
  
  # Sensors to be compared
  sensors <- unique(df_mean$sensor)
  
  # For loop that cycles through the requested wavelengths and calculates stats
  # TODO: The chunk of code that calculates statistics should be modularised into its own function
  # This would then be shared with the matchups_stats() function above
  df_results <- data.frame()
  for(i in 1:length(sensors)){
    for(j in 1:length(sensors)){
      if(sensors[j] != sensors[i]){
        
        # Get data.frame for matchup based on the two sensors being compared
        df_sensor_sub <- df_mean |> 
          filter(sensor %in% c(sensors[i], sensors[j]))
        
        # get distances
        hav_dist <- round(distHaversine(df_sensor_sub[c("longitude", "latitude")])/1000, 2) # distance in km
        
        # Time differences
        # TODO: Optimise this...
        df_sensor_sub_dateTime <- df_sensor_sub |> 
          mutate(dateTime = as.POSIXct(paste(day, time), format = "%Y%m%d %H%M%S")) |> 
          dplyr::select(sensor, dateTime)
        time_diff <- round(as.numeric(abs(difftime(df_sensor_sub_dateTime$dateTime[[1]],
                                                   df_sensor_sub_dateTime$dateTime[[2]],
                                                   units = "mins"))))
        
        # Melt it for additional stats
        df_sensor_long <- df_sensor_sub |> 
          pivot_longer(cols = matches("1|2|3|4|5|6|7|8|9"), names_to = "Wavelength", values_to = "Value") |> 
          dplyr::select(-day, -time, -longitude, -latitude) |> 
          pivot_wider(names_from = sensor, values_from = Value) |> 
          # TODO: Check this against a satellite matchup to ensure behaviour is correct
          na.omit()
        
        # Calculate RMSE (Root Mean Square Error)
        rmse <- sqrt(mean((df_sensor_long[[sensors[j]]] - df_sensor_long[[sensors[i]]])^2, na.rm = TRUE))
        
        # Calculate MAPE (Mean Absolute Percentage Error)
        mape <- mean(abs((df_sensor_long[[sensors[j]]] - df_sensor_long[[sensors[i]]]) / df_sensor_long[[sensors[i]]]), na.rm = TRUE) * 100
        
        # Calculate MSA (Mean Squared Adjustment)
        msa <- mean(abs(df_sensor_long[[sensors[j]]] - df_sensor_long[[sensors[i]]]), na.rm = TRUE)
        
        # Calculate linear slope
        lin_fit <- lm(df_sensor_long[[sensors[j]]] ~ df_sensor_long[[sensors[i]]])
        slope <- coef(lin_fit)[2]
        
        # Calculate Bias and Error (Pahlevan's method)
        log_ratio <- log10(df_sensor_long[[sensors[j]]] / df_sensor_long[[sensors[i]]])
        bias_pahlevan <- median(log_ratio, na.rm = TRUE)
        bias_pahlevan_final <- sign(bias_pahlevan) * (10^abs(bias_pahlevan) - 1)
        bias_pahlevan_final_perc <- bias_pahlevan_final * 100
        
        error_pahlevan <- median(abs(log_ratio), na.rm = TRUE)
        error_pahlevan_final <- 10^error_pahlevan - 1
        error_pahlevan_final_in_perc <- error_pahlevan_final * 100
        
        # Create data.frame of results and add them to df_results
        df_res <- data.frame(row.names = NULL,
                             sensor_X = sensors[i],
                             sensor_Y = sensors[j],
                             lon_X = df_sensor_sub$longitude[[1]],
                             lat_X = df_sensor_sub$latitude[[1]],
                             lon_Y = df_sensor_sub$longitude[[2]],
                             lat_Y = df_sensor_sub$latitude[[2]],
                             dist = hav_dist,
                             dateTime_X = df_sensor_sub_dateTime$dateTime[[1]],
                             dateTime_Y = df_sensor_sub_dateTime$dateTime[[2]],
                             diff_time = time_diff,
                             n = nrow(df_sensor_long),
                             Slope = round(slope, 2),
                             RMSE = round(rmse, 4),
                             MSA = round(msa, 4),
                             MAPE = round(mape, 1),
                             Bias = round(bias_pahlevan_final_perc, 1),
                             Error = round(error_pahlevan_final_in_perc, 1)
        )
        df_results <- rbind(df_results, df_res)
      }
    }
  }
  df_results <- mutate(df_results, file_name = basename(file_path), .before = sensor_X)
  return(df_results)
}

# Function that runs this over all matchup files in a directory
# dir_path <- "~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_ED"
process_matchup_folder <- function(dir_path){
  
  # List all files in directory
  file_list <- list.files(dir_path, pattern = "*.csv", full.names = TRUE)
  
  # Remove files with 'all' in the name
  file_list <- file_list[!grepl("all", file_list)]
  
  # Initialise results data.frame
  df_results <- plyr::ldply(file_list, process_matchup_file, .parallel = TRUE)
  
  # Save results and exit
  file_name <- paste0("all_",basename(dir_path),"_matchup_stats.csv")
  write_csv(df_results, file.path(dir_path, file_name))
}

# Run for all folders
## In situ matchups
process_matchup_folder("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_ED")
process_matchup_folder("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_LD")
process_matchup_folder("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_LW")
process_matchup_folder("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_RHOW")
## Satellite matchups
process_matchup_folder("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/MODIS")
process_matchup_folder("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/OLCI") # NB: This should process both S3A and S3B
process_matchup_folder("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/PACEV3")
process_matchup_folder("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/VIIRS_J1")
process_matchup_folder("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/VIIRS_J2")
process_matchup_folder("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/VIIRS_N/")


# Duplicated matchups -----------------------------------------------------

# Convenience wrapper to count number of matchups
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
in_situ_ED_dup <- n_matchup_dup("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_ED/all_MATCHUPS_in-situ_dm_10_ED_matchup_stats.csv")
in_situ_LD_dup <- n_matchup_dup("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_LD/all_MATCHUPS_in-situ_dm_10_LD_matchup_stats.csv")
in_situ_LW_dup <- n_matchup_dup("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_LW/all_MATCHUPS_in-situ_dm_10_LW_matchup_stats.csv")
in_situ_RW_dup <- n_matchup_dup("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_RHOW/all_MATCHUPS_in-situ_dm_10_RHOW_matchup_stats.csv")

# Plot Ed duplicates (all duplicates are same except Ld)
in_situ_ED_dup |> 
  filter(sensor_X == "HYPERPRO") |> 
  ggplot(aes(x = lon_X, y = lat_X)) +
  annotation_borders("world", colour = "gray80", fill = "gray80") +
  geom_point(aes(size = n_matchups), color = "darkblue", alpha = 0.7) +
  coord_quickmap(xlim = c(min(in_situ_ED_dup$lon_X)-2, max(in_situ_ED_dup$lon_X)+2),
                 ylim = c(min(in_situ_ED_dup$lat_X)-2, max(in_situ_ED_dup$lat_X))+1) +
  labs(title = "In situ HYPERPRO Matchup Duplicates",
       x = "Longitude (°E)",
       y = "Latitude (°N)",
       size = "Number of Matchups") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.border = element_rect(fill = NA, color = "black"))


# Global statistics --------------------------------------------------------

# Global stats per matchup wavelength
# testers..
# file_path = "~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_ED/all_metrics_min_350_max_800_L1C_ED.xlsx"
# remote = FALSE; W_nm = c(400, 412, 443, 490, 510, 560, 620, 673)
# file_path = "~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/MODIS/all_metrics_MODIS_AQUA_L2A_RHOW.xlsx"
# remote = TRUE; W_nm = c(412, 443, 488, 531, 555, 667)
global_stats <- function(file_path, MAPE_limit = NULL, remote = TRUE, W_nm = c(400, 412, 443, 490, 510, 560, 620, 673)){
  
  # Load data
  match_base <- read_excel(file_path)
  
  # Sensors X
  sensors_X <- unique(match_base$X_data)
  
  # Sensors Y
  if(remote){
    sensors_Y <- unique(match_base$Y_data)[!unique(match_base$Y_data) %in% unique(match_base$X_data)]
  } else {
    sensors_Y <- unique(match_base$Y_data)
  }
  
  # Print sensors to make sure everything is good
  print(paste("In situ sensors:", paste(sensors_X, collapse = ", ")))
  print(paste("Remote sensors:", paste(sensors_Y, collapse = ", ")))
  print(colnames(match_base))
  
  # For loop that cycles through the requested wavelengths and calculates stats
  df_results <- data.frame()
  for(i in 1:length(sensors_Y)){
    for(j in 1:length(sensors_X)){
      for(k in 1:length(W_nm)){
        
        # Get data.frame for matchup based on the two sensors being compared
        matchup_filt <- filter(match_base, X_data == sensors_X[j], Y_data == sensors_Y[i], !is.na(dateTime))
        # 
        # if(!is.null(MAPE_limit)){
        #   matchup_filt <- filter(matchup_filt, MAPE <= MAPE_limit)
        # }
        n_match <- nrow(matchup_filt)
        
        if(n_match > 0 & sensors_X[j] != sensors_Y[i]){
          
          # Get column names for subsetting
          x_col <- paste0("X_value(", W_nm[k], ")")
          y_col <- paste0("Y_value(", W_nm[k], ")")
          
          if(length(matchup_filt[[y_col]][!is.na(matchup_filt[[y_col]])]) == 0){
            print(paste("No data for wavelength", W_nm[k], "nm, skipping..."))
            next
          }
          
          # Calculate RMSE (Root Mean Square Error)
          rmse <- sqrt(mean((matchup_filt[[y_col]] - matchup_filt[[x_col]])^2, na.rm = TRUE))
          
          # Calculate MAPE (Mean Absolute Percentage Error)
          mape <- mean(abs((matchup_filt[[y_col]] - matchup_filt[[x_col]]) / matchup_filt[[x_col]]), na.rm = TRUE) * 100
          
          # Calculate MSA (Mean Squared Adjustment)
          msa <- mean(abs(matchup_filt[[y_col]] - matchup_filt[[x_col]]), na.rm = TRUE)
          
          # Calculate linear slope
          lin_fit <- lm(matchup_filt[[y_col]] ~ matchup_filt[[x_col]])
          slope <- coef(lin_fit)[2]
          
          # Calculate Bias and Error (Pahlevan's method)
          log_ratio <- log10(matchup_filt[[y_col]] / matchup_filt[[x_col]])
          bias_pahlevan <- median(log_ratio, na.rm = TRUE)
          bias_pahlevan_final <- sign(bias_pahlevan) * (10^abs(bias_pahlevan) - 1)
          bias_pahlevan_final_perc <- bias_pahlevan_final * 100
          
          error_pahlevan <- median(abs(log_ratio), na.rm = TRUE)
          error_pahlevan_final <- 10^error_pahlevan - 1
          error_pahlevan_final_in_perc <- error_pahlevan_final * 100
          
          # Create data.frame of results and add them to df_results
          df_temp <- data.frame(row.names = NULL,
                                sensor_X = sensors_X[j],
                                sensor_Y = sensors_Y[i],
                                Wavelength_nm = W_nm[k],
                                n = n_match,
                                Slope = round(slope, 2),
                                RMSE = round(rmse, 4),
                                MSA = round(msa, 4),
                                MAPE = round(mape, 1),
                                Bias = round(bias_pahlevan_final_perc, 1),
                                Error = round(error_pahlevan_final_in_perc, 1)
          )
          df_results <- rbind(df_results, df_temp)
        }
      }
    }
  }
  
  # Get file name and save
  file_name <- strsplit(basename(file_path), split = "\\.")[[1]][1]
  write_csv(df_results, paste0(dirname(file_path), "/", file_name, "_global_stats.csv"))
  return(df_results)
}

# Check stats
in_situ_Ed_stats <- global_stats("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_ED/all_metrics_min_350_max_800_L1C_ED.xlsx", remote = FALSE)
in_situ_Ld_stats <- global_stats("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_LD/all_metrics_min_350_max_800_L1C_LD.xlsx", remote = FALSE)
in_situ_Lw_stats <- global_stats("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_LW/all_metrics_min_350_max_800_L1C_LW.xlsx", remote = FALSE)
in_situ_Rhow_stats <- global_stats("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_RHOW/all_metrics_min_350_max_800_L2A_RHOW.xlsx", remote = FALSE)
MODIS_stats <- global_stats("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/MODIS/all_metrics_MODIS_AQUA_L2A_RHOW.xlsx",
                              remote = TRUE, W_nm = c(412, 443, 488, 531, 555, 667))
OLCI_stats <- global_stats("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/OLCI/all_metrics_min_350_max_800_OLCI_S3B_L2A_RHOW.xlsx",
                             remote = TRUE, W_nm = c(413, 443, 490, 560, 665, 709))
OLCI_S3A_stats <- global_stats("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/OLCI_S3A/all_metrics_min_350_max_800_OLCI_S3A_L2A_RHOW.xlsx",
                                 remote = TRUE, W_nm = c(413, 443, 490, 560, 665, 709))
OLCI_S3B_stats <- global_stats("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/OLCI_S3B/all_metrics_min_350_max_800_OLCI_S3B_L2A_RHOW.xlsx",
                                 remote = TRUE, W_nm = c(413, 443, 490, 560, 665, 709))
PACEV3_stats <- global_stats("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/PACEV3/all_metrics_min_350_max_800_PACE_OCI_L2A_RHOW.xlsx",
                               remote = TRUE, W_nm = c(412, 443, 490, 510, 560, 673))
VIIRS_N_stats <- global_stats("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/VIIRS_N/all_metrics_min_350_max_800_VIIRS_SNPP_L2A_RHOW.xlsx",
                                remote = TRUE, W_nm = c(410, 443, 486, 551, 671))
VIIRS_J1_stats <- global_stats("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/VIIRS_J1/all_metrics_min_350_max_800_VIIRS_JPSS1_L2A_RHOW.xlsx",
                                 remote = TRUE, W_nm = c(411, 445, 489, 556, 667))
VIIRS_J2_stats <- global_stats("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/VIIRS_J2/all_metrics_min_350_max_800_VIIRS_JPSS2_L2A_RHOW.xlsx",
                                 remote = TRUE, W_nm = c(411, 445, 489, 556, 667))


# Filter global stats based on MAPE ---------------------------------------

# Plot how the number of matchups is reduced with increasing MAPE threshold

# Also how this reduces the matchups on the map

# And how this affects the other statistical results


# List of bad matchups ----------------------------------------------------

# This is based on all of the high MAPE values detected above
in_situ_ED_bad <- read_csv("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_ED/all_MATCHUPS_in-situ_dm_10_ED_matchup_stats.csv") |> 
  mutate(sensor_pair = paste(sensor_X, "vs", sensor_Y), .before = sensor_X,
         date = as.Date(dateTime_X)) |> 
  filter(sensor_pair %in% c("HYPERPRO vs Hyp", "HYPERPRO vs TRIOS", "Hyp vs TRIOS")) |> 
  filter(MAPE > 5)


# Plot matchups -----------------------------------------------------------

# Ed
in_situ_ED_all_stats <- read_csv("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_ED/all_MATCHUPS_in-situ_dm_10_ED_matchup_stats.csv") |> 
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

