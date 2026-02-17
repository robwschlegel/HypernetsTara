# code/matchups.R
# Get the stats for all matchups and visualise results


# Setup -------------------------------------------------------------------

source("code/functions.R")


# Individual matchup stats ------------------------------------------------

# Run for all variables and satellites
## In situ
process_sensor("ED", "HYPERPRO")
process_sensor("LU", "HYPERPRO") # The function corrects the name
process_sensor("LD", "HYPERPRO") # The function corrects the name
process_sensor("LW", "HYPERPRO")
process_sensor("RHOW", "HYPERPRO")
## Satellite
process_sensor("RHOW", "MODIS")
process_sensor("RHOW", "OCI")
process_sensor("RHOW", "VIIRS")
process_sensor("RHOW", "OLCI")

# Re-load all single matchups
matchup_single_all <- map_dfr(dir("output", pattern = "matchup_stats_", full.names = TRUE), read_csv)

# Date and time range of samples per sensor
matchup_date_time_range <- matchup_single_all |> 
  dplyr::select(sensor_X, dateTime_X) |> 
  distinct() |> 
  mutate(date = as.Date(dateTime_X),
         time = format(dateTime_X, format = "%H:%M:%S")) |> 
  summarise(date_min = min(date), date_max = max(date),
            time_min = min(time), time_max = max(time), .by = "sensor_X")

# Unique number of satellite passes available for each platform+sensor/version
matchup_sat_uniq <- matchup_single_all |> 
  dplyr::select(sensor_X, dateTime_X) |> 
  filter(!(sensor_X %in% c("Hyp", "TRIOS", "HYPERPRO"))) |> 
  distinct() |> 
  summarise(sat_count = n(), .by = "sensor_X")


# Global statistics --------------------------------------------------------

# NB: Run code/outliers.R before the global stats in order to filter outliers

# Global stats
## In situ matchups
process_sensor("ED", "HYPERPRO", "global")
process_sensor("LU", "HYPERPRO", "global")
process_sensor("LD", "HYPERPRO", "global")
process_sensor("LW", "HYPERPRO", "global")
process_sensor("RHOW", "HYPERPRO", "global")
## Satellite
process_sensor("RHOW", "MODIS", "global")
process_sensor("RHOW", "VIIRS", "global")
process_sensor("RHOW", "OLCI", "global")
process_sensor("RHOW", "OCI", "global")

# Load all global stats
global_stats_all <- map_dfr(dir("output", pattern = "global", full.names = TRUE), read_csv)

# Visualise difference between linear-space and log-space slopes
global_stats_all |> 
  filter(sensor_X %in% c("HYPERNETS", "TRIOS", "HYPERPRO")) |> 
  ggplot() +
  geom_histogram(aes(x = Slope), colour = "green", alpha = 0.3, binwidth = 1) +
  geom_histogram(aes(x = Slope_log), colour = "red", alpha = 0.3, binwidth = 1) +
  facet_grid(sensor_X ~ sensor_Y)

# Get matchups counts, outliers, etc.
global_count_var_name <- global_stats_all |> 
  filter(sensor_X %in% c("HYPERNETS", "TRIOS", "HYPERPRO")) |> 
  dplyr::select(var_name:sensor_Y) |> 
  group_by(var_name, sensor_X, sensor_Y) |> 
  filter(n_w_nm == max(n_w_nm)) |> 
  ungroup() |> 
  distinct()

# Quantify which sensors matched most closely to which satellites
global_match_mean <- global_stats_all |> 
  filter(sensor_X %in% c("HYPERNETS", "TRIOS", "HYPERPRO")) |> 
  summarise(Slope = mean(Slope),
            Bias = mean(Bias),
            Error = mean(Error), .by = c("sensor_X", "sensor_Y"))


# Check individual matchups -----------------------------------------------

# Files of interest
# HYPERNETS_vs_TRIOS_vs_20240816T081000_LW.csv # Negative values cause massive MAPE (%)

# Load file
match_1 <- load_matchup_long(paste0(file_path_build("LW", "HYPERNETS", "TRIOS"), "HYPERNETS_vs_TRIOS_vs_20240816T081000_LW.csv"))

# Create vectors from filtered columns
(x_vec <- match_1[["Hyp"]])
(y_vec <- match_1[["TRIOS"]])

# Calculate stats one-by-one
message("Slope : ", round(coef(lm(y_vec ~ x_vec))[2], 4))
message("RMSE : ", round(sqrt(mean((y_vec - x_vec)^2, na.rm = TRUE)), 4))
message("MSA : ", round(mean(abs(y_vec - x_vec), na.rm = TRUE), 4))
message("MAPE (%) : ", round(mean(abs((y_vec - x_vec) / x_vec), na.rm = TRUE) * 100, 2))

# Calculate Bias and Error (Pahlevan's method)
log_ratio <- log10(y_vec / x_vec)
bias_pahlevan <- median(log_ratio, na.rm = TRUE)
bias_pahlevan_final <- sign(bias_pahlevan) * (10^abs(bias_pahlevan) - 1)
message("Bias (%) : ", round(bias_pahlevan_final * 100, 2))

error_pahlevan <- median(abs(log_ratio), na.rm = TRUE)
error_pahlevan_final <- 10^error_pahlevan - 1
message("Error (%) : ", round(error_pahlevan_final * 100, 2))

# Plot data
match_base |> 
  ggplot(aes(x = Hyp, y = TRIOS)) +
  geom_point(aes(colour = wavelength))


# Check individual global values ------------------------------------------

# Rhow - HyperPRO vs OLCI S3A - w_nm 665
folder_path <- file_path_build("RHOW", "HYPERPRO", "S3B")

# List all files in directory
file_list <- list.files(folder_path, pattern = "*.csv", full.names = TRUE)

# Load data
match_base <- map_dfr(file_list, load_matchup_long)
# print(unique(match_base$wavelength))

# Auto-generate chosen wavelengths
(W_nm <- W_nm_out("S3B"))

# Get data.frame for matchup based on the wavelength of choice
(matchup_filt <- filter(match_base, wavelength == W_nm[5]))

# Create vectors from filtered columns
(x_vec <- matchup_filt[["HYPERPRO"]])
(y_vec <- matchup_filt[["S3B"]])

# Calculate stats one-by-one
message("Slope : ", round(coef(lm(y_vec ~ x_vec))[2], 4))
message("RMSE : ", round(sqrt(mean((y_vec - x_vec)^2, na.rm = TRUE)), 4))
message("MSA : ", round(mean(abs(y_vec - x_vec), na.rm = TRUE), 4))
message("MAPE (%) : ", round(mean(abs((y_vec - x_vec) / x_vec), na.rm = TRUE) * 100, 2))

# Calculate Bias and Error (Pahlevan's method)
log_ratio <- log10(y_vec / x_vec)
bias_pahlevan <- median(log_ratio, na.rm = TRUE)
bias_pahlevan_final <- sign(bias_pahlevan) * (10^abs(bias_pahlevan) - 1)
message("Bias (%) : ", round(bias_pahlevan_final * 100, 2))

error_pahlevan <- median(abs(log_ratio), na.rm = TRUE)
error_pahlevan_final <- 10^error_pahlevan - 1
message("Error (%) : ", round(error_pahlevan_final * 100, 2))

# Plot data
match_base |> filter(wavelength == W_nm[5]) |> 
  ggplot(aes(x = HYPERPRO, y = S3A)) +
  geom_point(aes(colour = wavelength))

