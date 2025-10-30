# code/outliers.R


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(geosphere)
library(readxl)
# library(janitor)

# Define the wavelength (nm) band colour palette
colour_all_nm <- c('darkviolet',
                   'violet',
                   'blue',
                   'darkgreen',
                   'yellow',
                   'orange',
                   'red',
                   'brown')
colour_short_nm <- c('violet',
                     'blue',
                     'darkgreen',
                     'yellow',
                     'orange',
                     'red')



# Map of Tara mission -----------------------------------------------------




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
      n_match <- filter(match_base, X_data == sensors_X[j], Y_data == sensors_Y[i]) |> nrow()
      print(paste(sensors_Y[i], "vs", sensors_X[j], "=", n_match))
    }
  }
}

# Check matchups
matchups_count("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/MODIS/all_metrics_MODIS_AQUA_L2A_RHOW.xlsx")
matchups_count("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/OLCI/all_metrics_min_350_max_800_OLCI_S3B_L2A_RHOW.xlsx")
matchups_count("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/OLCI_S3A/all_metrics_min_350_max_800_OLCI_S3A_L2A_RHOW.xlsx")
matchups_count("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/OLCI_S3B/all_metrics_min_350_max_800_OLCI_S3B_L2A_RHOW.xlsx")
matchups_count("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/PACEV3/all_metrics_min_350_max_800_PACE_OCI_L2A_RHOW.xlsx")
matchups_count("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/VIIRS_J1/all_metrics_min_350_max_800_VIIRS_JPSS1_L2A_RHOW.xlsx")
matchups_count("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/VIIRS_J2/all_metrics_min_350_max_800_VIIRS_JPSS2_L2A_RHOW.xlsx")
matchups_count("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/VIIRS_N/all_metrics_min_350_max_800_VIIRS_SNPP_L2A_RHOW.xlsx")
matchups_count("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_RHOW/all_metrics_min_350_max_800_L2A_RHOW.xlsx", remote = FALSE)
matchups_count("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_ED/all_metrics_min_350_max_800_L1C_ED.xlsx", remote = FALSE)
matchups_count("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_LD/all_metrics_min_350_max_800_L1C_LD.xlsx", remote = FALSE)
matchups_count("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_LW/all_metrics_min_350_max_800_L1C_LW.xlsx", remote = FALSE)


# Mean statistics ---------------------------------------------------------

# Convenience wrapper to get stats per matchup
# testers..
# file_path = "~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_ED/all_metrics_min_350_max_800_L1C_ED.xlsx"
# remote = FALSE; W_nm = c(400, 412, 443, 490, 510, 560, 620, 673)
matchups_stats <- function(file_path, remote = TRUE, W_nm = c(400, 412, 443, 490, 510, 560, 620, 673)){
  
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
        matchup_filt <- filter(match_base, X_data == sensors_X[j], Y_data == sensors_Y[i])
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
          mape <- mean(abs((matchup_filt[[x_col]] - matchup_filt[[y_col]]) / matchup_filt[[x_col]]), na.rm = TRUE) * 100
          
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
  write_csv(df_results, paste0(dirname(file_path), "/", file_name, "_matchup_stats.csv"))
  return(df_results)
}

# Check stats
in_situ_Ed_stats <- matchups_stats("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_ED/all_metrics_min_350_max_800_L1C_ED.xlsx", remote = FALSE)
in_situ_Ld_stats <- matchups_stats("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_LD/all_metrics_min_350_max_800_L1C_LD.xlsx", remote = FALSE)
in_situ_Lw_stats <- matchups_stats("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_LW/all_metrics_min_350_max_800_L1C_LW.xlsx", remote = FALSE)
in_situ_Rhow_stats <- matchups_stats("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_RHOW/all_metrics_min_350_max_800_L2A_RHOW.xlsx", remote = FALSE)
MODIS_stats <- matchups_stats("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/MODIS/all_metrics_MODIS_AQUA_L2A_RHOW.xlsx",
                              remote = TRUE, W_nm = c(412, 443, 488, 531, 555, 667))
OLCI_stats <- matchups_stats("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/OLCI/all_metrics_min_350_max_800_OLCI_S3B_L2A_RHOW.xlsx",
                             remote = TRUE, W_nm = c(413, 443, 490, 560, 665, 709))
OLCI_S3A_stats <- matchups_stats("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/OLCI_S3A/all_metrics_min_350_max_800_OLCI_S3A_L2A_RHOW.xlsx",
                                 remote = TRUE, W_nm = c(413, 443, 490, 560, 665, 709))
OLCI_S3B_stats <- matchups_stats("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/OLCI_S3B/all_metrics_min_350_max_800_OLCI_S3B_L2A_RHOW.xlsx",
                                 remote = TRUE, W_nm = c(413, 443, 490, 560, 665, 709))
PACEV3_stats <- matchups_stats("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/PACEV3/all_metrics_min_350_max_800_PACE_OCI_L2A_RHOW.xlsx",
                               remote = TRUE, W_nm = c(412, 443, 490, 510, 560, 673))
VIIRS_N_stats <- matchups_stats("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/VIIRS_N/all_metrics_min_350_max_800_VIIRS_SNPP_L2A_RHOW.xlsx",
                                 remote = TRUE, W_nm = c(410, 443, 486, 551, 671))
VIIRS_J1_stats <- matchups_stats("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/VIIRS_J1/all_metrics_min_350_max_800_VIIRS_JPSS1_L2A_RHOW.xlsx",
                                 remote = TRUE, W_nm = c(411, 445, 489, 556, 667))
VIIRS_J2_stats <- matchups_stats("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/VIIRS_J2/all_metrics_min_350_max_800_VIIRS_JPSS2_L2A_RHOW.xlsx",
                                 remote = TRUE, W_nm = c(411, 445, 489, 556, 667))


# Summary statistics ------------------------------------------------------

# Create summary stats that highlight overall success rates etc. for sorts of different matchups
# One idea is to rank each matchup based on certain metrics (RMSE, MAPE, Bias, Error) and then average the ranks to get an overall score
# Another idea is to set thresholds for each metric and count how many matchups meet those thresholds
# This can be done for each product (Ed, Ld, Lw, Rho_w) separately
# This can also be done for each sensor pair separately (e.g., HypStar vs Trios, HyperPRO vs Trios, etc.)
# This will help identify which sensor pairs and products are performing best overall


# Filter out samples with MAPE above an increasingly lax threshold
# Mark how the results and count change
# And mark the spaceTime change of these increasingly poor matchups
# This will require visualisation via both a time series plot and a map


# Outlier hunting ---------------------------------------------------------

# Load and melt a matchup file into a format that's easier for plotting
melt_matchup <- function(file_path){
  
  # Load the data
  base_df <- read_excel(file_path)
  
  # Column names to exclude
  exclude_cols <- c("filename", "RMSE", "MAPE", "MSA", "Slope", "Bias", "N", "Error")
  
  # Melt and exit
  long_df <- base_df |>
    filter(!is.na(dateTime)) |> 
    dplyr::select(!all_of(exclude_cols)) |> 
    pivot_longer(cols = contains("value"), names_to = "Wavelength", values_to = "Value") |>
    separate(Wavelength, into = c("Sensor", "Wavelength_nm"), sep = "_value\\(") |>
    mutate(Wavelength_nm = as.numeric(gsub("\\)", "", Wavelength_nm)),
           dateTime = as.POSIXct(dateTime, format = "%Y%m%dT%H%M%S"),
           date = as.Date(dateTime)) |> 
    mutate(Sensor = case_when(
      Sensor == "X" ~ X_data,
      Sensor == "Y" ~ Y_data)) |> 
    dplyr::select(-X_data, -Y_data) |> 
    distinct() |> 
    pivot_wider(names_from = Sensor, values_from = Value, id_cols = c(date, dateTime, Wavelength_nm), values_fn = mean) |> 
    mutate(Wavelength_group = cut(Wavelength_nm,
                                  breaks = c(350, 400, 450, 500, 550, 600, 650, 700, 750, 800),
                                  labels = c("350-400", "400-450", "450-500", "500-550", "550-600", "600-650", "650-700", "700-750", "750-800"),
                                  include.lowest = FALSE, right = FALSE))
  return(long_df)
}

# Plot data based on wavelength group
plot_matchup_nm <- function(df, var_name, x_sensor, y_sensor){
  df |> 
    filter(!is.na(!!sym(x_sensor)), !is.na(!!sym(y_sensor))) |>
    ggplot(aes_string(x = x_sensor, y = y_sensor)) +
    geom_point(aes(colour = Wavelength_group)) +
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
    labs(title = paste(var_name,"-", x_sensor, "vs", y_sensor),
         x = paste(var_name, x_sensor),
         y = paste(var_name, y_sensor),
         colour = "Wavelength (nm)") +
    scale_colour_discrete(palette = colour_short_nm)  +
    theme_minimal() +
    theme(panel.background = element_rect(fill = NA, color = "black"),
          legend.position = "bottom")
}

# Plot based on date of collection
plot_matchup_date <- function(df, var_name, x_sensor, y_sensor){
  df |> 
    filter(!is.na(!!sym(x_sensor)), !is.na(!!sym(y_sensor))) |>
    mutate(date = as.factor(date)) |> 
    ggplot(aes_string(x = x_sensor, y = y_sensor)) +
    geom_point(aes(colour = date)) +
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
    labs(title = paste(var_name,"-", x_sensor, "vs", y_sensor),
         x = paste(var_name, x_sensor),
         y = paste(var_name, y_sensor),
         colour = "date") +
    # scale_colour_brewer(palette = "Dark2")  +
    theme_minimal() +
    theme(panel.background = element_rect(fill = NA, color = "black"),
          legend.position = "bottom")
}

# Plot based on dateTime of collection
plot_matchup_dateTime <- function(df, var_name, x_sensor, y_sensor, date_filter){
  df |> 
    filter(!is.na(!!sym(x_sensor)), !is.na(!!sym(y_sensor))) |>
    filter(date == as.Date(date_filter)) |>
    ggplot(aes_string(x = x_sensor, y = y_sensor)) +
    geom_point(aes(colour = dateTime)) +
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
    labs(title = paste(var_name,"-", x_sensor, "vs", y_sensor,"-", date_filter),
         x = paste(var_name, x_sensor),
         y = paste(var_name, y_sensor),
         colour = "time (UTC)") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = NA, color = "black"),
          legend.position = "bottom")
}

# Load a single matchup and extract time and space differences
# file_path <- "~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_ED/HYPERNETS_W_TAFR_L1C_ED_20240814T0717_270_vs_TRIOS_350_800.csv"
spacetime_diff <- function(file_path, sensor_1, sensor_2){
  
  # Load the data
  suppressMessages(
    base_df <- read_delim(file_path, delim = ";")
  )
  colnames(base_df)[1] <- "sensor"
  
  # Removes 1,2,3 from sensor column values
  base_df <- base_df |> 
    mutate(sensor = gsub(" 1$| 2$| 3$", "", sensor)) |> 
    select(sensor:longitude) |> 
    filter(sensor != "Hyp_nosc") |> 
    distinct() |> 
    # mutate(dateTime = paste(date, time))
    mutate(dateTime = as.POSIXct(paste(day, time), format = "%Y%m%d %H%M%S"))
  print(base_df)
  
  # Filer down to the two sensors to be compared
  base_df_sub <- base_df |> 
    filter(sensor %in% c(sensor_1, sensor_2))
  sensors <- unique(base_df_sub$sensor)
  
  # get distances
  hav_dist <- round(distHaversine(base_df_sub[c("longitude", "latitude")])/1000, 2) # distance in km
  
  # Time differences
  time_diff <- round(as.numeric(abs(difftime(base_df$dateTime[base_df$sensor == sensors[1]],
                                             base_df$dateTime[base_df$sensor == sensors[2]],
                                             units = "mins"))))
  
  # Create data.frame of differences
  diff_df <- data.frame(
    Sensor_1 = sensors[1],
    Sensor_2 = sensors[2],
    Distance_km = hav_dist,
    Time_diff_mins = time_diff)
  return(diff_df)
}


## Ed ----------------------------------------------------------------------

# Load Ed matchups
in_situ_Ed_long <- melt_matchup("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_ED/all_metrics_min_350_max_800_L1C_ED.xlsx")


### HypStar vs Trios --------------------------------------------------------

# Plot all wavelength matchups
plot_matchup_nm(in_situ_Ed_long, "Ed", "Hypernets", "Trios")

# Plot matchups by date
plot_matchup_date(in_situ_Ed_long, "Ed", "Hypernets", "Trios")
# Dates 2024-08-14 and 2024-08-16 look funny

# 2024-08-14
plot_matchup_dateTime(in_situ_Ed_long, "Ed", "Hypernets", "Trios", "2024-08-14") # ~08:00 samples are funny; beginning of measurements
spacetime_diff("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_ED/HYPERNETS_W_TAFR_L1C_ED_20240814T0717_270_vs_TRIOS_350_800.csv",
               sensor_1 = "Hyp", sensor_2 = "TRIOS")

# 2024-08-14
plot_matchup_dateTime(in_situ_Ed_long, "Ed", "Hypernets", "Trios", "2024-08-16") # ~11:00 samples are funny; end of measurements
spacetime_diff("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_ED/HYPERNETS_W_TAFR_L1C_ED_20240816T1111_090_vs_TRIOS_350_800.csv",
               sensor_1 = "Hyp", sensor_2 = "TRIOS")

# Combine and save
plot_ed_hypstar_trios <- ggpubr::ggarrange(plot_matchup_nm(in_situ_Ed_long, "Ed", "Hypernets", "Trios"),
                                           plot_matchup_dateTime(in_situ_Ed_long, "Ed", "Hypernets", "Trios", "2024-08-14"),
                                           plot_matchup_dateTime(in_situ_Ed_long, "Ed", "Hypernets", "Trios", "2024-08-16"),
                                           ncol = 3, nrow = 1, labels = c("a)", "b)", "c)"))
ggsave("~/pCloudDrive/Documents/OMTAB/HYPERNETS/figures/test_Ed_HypStar_vs_Trios.png", plot_ed_hypstar_trios,
       width = 18, height = 6, dpi = 600)

# Randomly check a file that shouldn't have issues
spacetime_diff("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_ED/HYPERNETS_W_TAFR_L1C_ED_20240811T1431_090_vs_TRIOS_350_800.csv",
               sensor_1 = "Hyp", sensor_2 = "TRIOS")
spacetime_diff("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_ED/HYPERNETS_W_TAFR_L1C_ED_20240815T0910_090_vs_TRIOS_350_800.csv",
               sensor_1 = "Hyp", sensor_2 = "TRIOS")


### HyperPRO vs Trios -------------------------------------------------------

# Plot all wavelength matchups
plot_matchup_nm(in_situ_Ed_long, "Ed", "Hyperpro", "Trios")

# Plot matchups by date
plot_matchup_date(in_situ_Ed_long, "Ed", "Hyperpro", "Trios")
# 2024-08-13 look funny

# 2024-08-13
plot_matchup_dateTime(in_situ_Ed_long, "Ed", "Hyperpro", "Trios", "2024-08-13") # ~09:49; full sample
spacetime_diff("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_ED/HYPERNETS_W_TAFR_L1C_ED_20240813T0940_270_vs_TRIOS_vs_HYPERPRO_350_800.csv",
               sensor_1 = "HYPERPRO", sensor_2 = "TRIOS")
spacetime_diff("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_ED/HYPERNETS_W_TAFR_L1C_ED_20240813T1001_270_vs_TRIOS_vs_HYPERPRO_350_800.csv",
               sensor_1 = "HYPERPRO", sensor_2 = "TRIOS")

# Combine and save
plot_ed_hyperpro_trios <- ggpubr::ggarrange(plot_matchup_nm(in_situ_Ed_long, "Ed", "Hyperpro", "Trios"),
                                            plot_matchup_dateTime(in_situ_Ed_long, "Ed", "Hyperpro", "Trios", "2024-08-13"),
                                            ncol = 2, nrow = 1, labels = c("a)", "b)"))
ggsave("~/pCloudDrive/Documents/OMTAB/HYPERNETS/figures/test_Ed_HyperPRO_vs_Trios.png", plot_ed_hyperpro_trios,
       width = 12, height = 6, dpi = 600)


### HyperPRO vs HypStar ------------------------------------------------------

# Plot values by wavelength group
plot_matchup_nm(in_situ_Ed_long, "Ed", "Hyperpro", "Hypernets")

# Plot matchups by date
plot_matchup_date(in_situ_Ed_long, "Ed", "Hyperpro", "Hypernets")
# 2024-08-09 and 2024-08-13 look funny

# 2024-08-09
plot_matchup_dateTime(in_situ_Ed_long, "Ed", "Hyperpro", "Hypernets", "2024-08-09") # ~09:49; full sample
spacetime_diff("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_ED/HYPERNETS_W_TAFR_L1C_ED_20240809T0830_090_vs_HYPERPRO_350_800.csv",
               sensor_1 = "HYPERPRO", sensor_2 = "Hyp")

# 2024-08-13 
plot_matchup_dateTime(in_situ_Ed_long, "Ed", "Hyperpro", "Hypernets", "2024-08-13") # ~09:49; full sample
spacetime_diff("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_ED/HYPERNETS_W_TAFR_L1C_ED_20240813T0940_270_vs_TRIOS_vs_HYPERPRO_350_800.csv",
               sensor_1 = "HYPERPRO", sensor_2 = "Hyp")
spacetime_diff("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_ED/HYPERNETS_W_TAFR_L1C_ED_20240813T1001_270_vs_TRIOS_vs_HYPERPRO_350_800.csv",
               sensor_1 = "HYPERPRO", sensor_2 = "Hyp")

# Combine and save
plot_ed_hyperpro_hypstar <- ggpubr::ggarrange(plot_matchup_nm(in_situ_Ed_long, "Ed", "Hyperpro", "Hypernets"),
                                              plot_matchup_dateTime(in_situ_Ed_long, "Ed", "Hyperpro", "Hypernets", "2024-08-09"),
                                              plot_matchup_dateTime(in_situ_Ed_long, "Ed", "Hyperpro", "Hypernets", "2024-08-13"),
                                              ncol = 3, nrow = 1, labels = c("a)", "b)", "c)"))
ggsave("~/pCloudDrive/Documents/OMTAB/HYPERNETS/figures/test_Ed_HyperPRO_vs_HypStar.png", plot_ed_hyperpro_hypstar,
       width = 18, height = 6, dpi = 600)



## Lw ----------------------------------------------------------------------

# Load Ed matchups
in_situ_Lw_long <- melt_matchup("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_LW/all_metrics_min_350_max_800_L1C_LW.xlsx")


### HypStar vs Trios --------------------------------------------------------

# Plot all wavelength matchups
plot_matchup_nm(in_situ_Lw_long, "Lw", "Hypernets", "Trios")

# Plot matchups by date
plot_matchup_date(in_situ_Lw_long, "Lw", "Hypernets", "Trios")
# Bad values from 2024-08-11 to 2024-08-15

# 2024-08-14
plot_matchup_dateTime(in_situ_Lw_long, "Lw", "Hypernets", "Trios", "2024-08-14") # Values become progressively worse over the day
spacetime_diff("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_LW/HYPERNETS_W_TAFR_L1C_LW_20240814T1211_090_vs_TRIOS_350_800.csv",
               sensor_1 = "Hyp", sensor_2 = "TRIOS")

# 2024-08-14
plot_matchup_dateTime(in_situ_Ed_long, "Ed", "Hypernets", "Trios", "2024-08-16") # ~11:00 samples are funny; end of measurements
spacetime_diff("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_ED/HYPERNETS_W_TAFR_L1C_ED_20240816T1111_090_vs_TRIOS_350_800.csv",
               sensor_1 = "Hyp", sensor_2 = "TRIOS")

# Combine and save
plot_ed_hypstar_trios <- ggpubr::ggarrange(plot_matchup_nm(in_situ_Ed_long, "Ed", "Hypernets", "Trios"),
                                           plot_matchup_dateTime(in_situ_Ed_long, "Ed", "Hypernets", "Trios", "2024-08-14"),
                                           plot_matchup_dateTime(in_situ_Ed_long, "Ed", "Hypernets", "Trios", "2024-08-16"),
                                           ncol = 3, nrow = 1, labels = c("a)", "b)", "c)"))
ggsave("~/pCloudDrive/Documents/OMTAB/HYPERNETS/figures/test_Ed_HypStar_vs_Trios.png", plot_ed_hypstar_trios,
       width = 18, height = 6, dpi = 600)

# Randomly check a file that shouldn't have issues
spacetime_diff("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_ED/HYPERNETS_W_TAFR_L1C_ED_20240811T1431_090_vs_TRIOS_350_800.csv",
               sensor_1 = "Hyp", sensor_2 = "TRIOS")
spacetime_diff("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_in-situ_dm_10_ED/HYPERNETS_W_TAFR_L1C_ED_20240815T0910_090_vs_TRIOS_350_800.csv",
               sensor_1 = "Hyp", sensor_2 = "TRIOS")

## Rho_w -------------------------------------------------------------------

### VIIRS_J1 ---------------------------------------------------------------

viirs_J1_Rhow <- read_excel("~/pCloudDrive/Documents/OMTAB/HYPERNETS/MATCHUPS_dm_120_min_350_max_800/VIIRS_J1/all_metrics_min_350_max_800_VIIRS_JPSS1_L2A_RHOW.xlsx")
viirs_J1_Rhow_long <- viirs_J1_Rhow |>
  filter(!is.na(dateTime)) |> 
  dplyr::select(dateTime:Y_data, `X_value(411)`:`Y_value(671)`) |> 
  pivot_longer(cols = contains("value"), names_to = "Wavelength", values_to = "Value") |>
  separate(Wavelength, into = c("Sensor", "Wavelength_nm"), sep = "_value\\(") |>
  mutate(Wavelength_nm = as.numeric(gsub("\\)", "", Wavelength_nm)),
         dateTime = as.POSIXct(dateTime, format = "%Y%m%dT%H%M%S"),
         date = as.Date(dateTime)) |> 
  mutate(Sensor = case_when(
    Sensor == "X" ~ X_data,
    Sensor == "Y" ~ Y_data)) |> 
  dplyr::select(-X_data, -Y_data) |> 
  distinct() |> 
  pivot_wider(names_from = Sensor, values_from = Value, id_cols = c(date, dateTime, Wavelength_nm), values_fn = mean) |> 
  mutate(Wavelength_group = cut(Wavelength_nm,
                                breaks = c(350, 400, 450, 500, 550, 600, 650, 700, 750),
                                labels = c("350-400", "400-450", "450-500", "500-550", "550-600", "600-650", "650-700", "700-750"),
                                include.lowest = FALSE, right = FALSE))

### HyperPRO vs VIIRS_J1 ------------------------------------------------------

# Plot values by wavelength group
plot_rhow_hyperpro_viirsj1_wavelength <- viirs_J1_Rhow_long |> 
  filter(!is.na(Hyperpro)) |>
  ggplot(aes(x = Hyperpro, y = JPSS1)) +
  geom_point(aes(colour = Wavelength_group)) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  labs(title = "Rho_w - HyperPRO vs VIIRS_J1",
       x = "Rho_w HyperPRO",
       y = "Rho_w VIIRS_J1",
       colour = "Wavelength (nm)") +
  # scale_colour_discrete(palette = "dark2") +
  scale_colour_discrete(palette = colour_nm)  +
  theme_minimal() +
  theme(panel.background = element_rect(fill = NA, color = "black"),
        legend.position = "bottom")
plot_rhow_hyperpro_viirsj1_wavelength


# Plot lineqr model for each wavelength
plot_rhow_hyperpro_viirsj1_wavelength_lm <- viirs_J1_Rhow_long |> 
  filter(!is.na(Hyperpro)) |>
  ggplot(aes(x = Hyperpro, y = JPSS1)) +
  geom_point(aes(colour = as.factor(Wavelength_nm))) +
  geom_smooth(aes(group = as.factor(Wavelength_nm), colour = as.factor(Wavelength_nm)), method = "lm", se = FALSE) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  labs(title = "Rho_w - HyperPRO vs VIIRS_J1",
       x = "Rho_w HyperPRO",
       y = "Rho_w VIIRS_J1",
       colour = "Wavelength (nm)") +
  # scale_colour_discrete(palette = "dark2") +
  # scale_colour_discrete(palette = colour_nm)  +
  theme_minimal() +
  theme(panel.background = element_rect(fill = NA, color = "black"),
        legend.position = "bottom")
plot_rhow_hyperpro_viirsj1_wavelength_lm

# Plot lineqr model for each wavelength
plot_rhow_hyperpro_viirsj1_wavelength_lm_zoom <- viirs_J1_Rhow_long |> 
  filter(!is.na(Hyperpro)) |>
  filter(Wavelength_nm > 500) |>
  ggplot(aes(x = Hyperpro, y = JPSS1)) +
  geom_point(aes(colour = as.factor(Wavelength_nm))) +
  geom_smooth(aes(group = as.factor(Wavelength_nm), colour = as.factor(Wavelength_nm)), method = "lm", se = FALSE) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  labs(title = "Rho_w - HyperPRO vs VIIRS_J1",
       x = "Rho_w HyperPRO",
       y = "Rho_w VIIRS_J1",
       colour = "Wavelength (nm)") +
  # scale_colour_discrete(palette = "dark2") +
  # scale_colour_discrete(palette = colour_nm)  +
  theme_minimal() +
  theme(panel.background = element_rect(fill = NA, color = "black"),
        legend.position = "bottom")
plot_rhow_hyperpro_viirsj1_wavelength_lm_zoom

# Combine and save
plot_rhow_hyperpro_viirsj1 <- ggpubr::ggarrange(plot_rhow_hyperpro_viirsj1_wavelength,
                                                plot_rhow_hyperpro_viirsj1_wavelength_lm,
                                                plot_rhow_hyperpro_viirsj1_wavelength_lm_zoom,
                                                ncol = 3, nrow = 1, labels = c("a)", "b)", "c)"))
ggsave("~/pCloudDrive/Documents/OMTAB/HYPERNETS/figures/test_rhow_hyperpro_viirsj1.png", plot_rhow_hyperpro_viirsj1,
       width = 18, height = 6, dpi = 600)
