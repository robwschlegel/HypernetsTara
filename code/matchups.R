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
process_sensor("RHOW", "VIIRS")
process_sensor("RHOW", "OLCI")
process_sensor("RHOW", "OCI")

# Re-load all single matchups
matchup_single_all <- map_dfr(dir("output", pattern = "matchup_stats_", full.names = TRUE), read_csv)

# Extract only Sentinel-3
matchup_single_S3 <- matchup_single_all |> 
  filter(sensor_Y %in% c("S3A", "S3B"))
write.csv(matchup_single_S3, "meta/matchup_single_S3.csv")

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


# Uncertainties ----------------------------------------------------------

## Rrs for CTD stations ---------------------------------------------------

# Load mission metadata for reference
Hyperboost_meta <- read_csv("data/HyperBoost_dataset_AOPs_2023_2024_10042026_Metadata.csv", 
    show_col_types = FALSE) |> 
  filter(Cruise == "TARA RETURN LEG") |> 
  mutate(CTD_time = as.POSIXct(CTD_time, format = "%d/%m/%Y %H:%M", tz = "UTC")) |> 
  dplyr::select(Cruise:StationName, Latitude_oN_, Longitude_oE_, CTD_time)

# Load the cleaned files
# NB: After further reflection, it is probably incorrect at this stage to screen the data
match_base_files <- read_csv("~/HypernetsTara/output/matchup_stats_RHOW_in_situ.csv", 
    show_col_types = FALSE) |> 
  filter(sensor_X == "Hyp") |> 
  separate(file_name, into = c("1", "2", "3", "4", "stub", "5"), sep = "_") |> 
  mutate(stub = str_sub(stub, 1, -3))
stubs <- paste(match_base_files$stub, collapse = "|")

# Get HYPERNETS files
L1C_HYPERNETS_files <- dir("~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/Hypernets_processed_data",
  full.names = TRUE, recursive = TRUE, pattern = "_L1C_")
L1C_HYPERNETS_files <- L1C_HYPERNETS_files[grepl("v2.0.nc", L1C_HYPERNETS_files)]
# L1C_HYPERNETS_files <- L1C_HYPERNETS_files[grepl(stubs, L1C_HYPERNETS_files)] # Clean files only
# NB: Rather get base values from L1C and compute SD etc.
# Also, this doesn't work as we are comparing Rrs, and we can't get the SD for that from the L2A files
# L2A_HYPERNETS_files <- dir("~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/Hypernets_processed_data",
#   full.names = TRUE, recursive = TRUE, pattern = "_L2A_")
# L2A_HYPERNETS_files <- L2A_HYPERNETS_files[grepl("v2.0.nc", L2A_HYPERNETS_files)]

# Load HYPERNETS NetCDF files directly
# NB: Seems to struggle on multiple cores
var_HYPERNETS_L1C <- plyr::ldply(L1C_HYPERNETS_files, proc_HYPERNETS_L1C, .parallel = FALSE)
var_HYPERNETS_L1C_raw <- plyr::ldply(L1C_HYPERNETS_files, proc_HYPERNETS_L1C, .parallel = FALSE, stat_calc = FALSE)
# var_HYPERNETS_L2A <- plyr::ldply(L1C_HYPERNETS_files, proc_HYPERNETS_L2A, .parallel = FALSE)

# Filter out the HYPERNETS samples to within +-30 minutes of the CTD stations during the mission
sd_Rrs_HYPERNETS <- var_HYPERNETS_L1C |> 
  filter(name == "Rrs",
         cv > 0,
         cv < 3) |>
  cross_join(Hyperboost_meta) |> 
  mutate(diff_time = abs(difftime(date, CTD_time, units = "mins"))) |> 
  filter(diff_time <= 30) |> 
  distinct() |> 
  dplyr::rename(N = n) |> 
  dplyr::select(system, StationNumber, N, wavelength, mean, sd, cv) |> 
  mutate(cv_perc = cv * 100)

# Load the sd values directly from the base data report
sd_Rrs_SoRad <- read_csv("data/HyperBoost_dataset_AOPs_2023_2024_10042026_Rrs_SoRad.csv",
                      show_col_types = FALSE) |> 
  filter(N > 0) |> 
  pivot_longer(cols = `Rrs_SoRad_355.2`:`Rrs_SoRad_800.7_std`) |> 
  mutate(var_name = case_when(grepl("std", name) ~ "sd", TRUE ~ "mean"),
         wavelength = gsub("Rrs_SoRad_", "", name),
         wavelength = round(as.numeric(gsub("_std", "", wavelength)))) |> 
  dplyr::select(StationNumber, N, wavelength, var_name, value) |> 
  pivot_wider(values_from = value, names_from = var_name) |> 
  mutate(cv = sd / mean,
         cv_perc = cv*100,
         system = "So-Rad") |> 
  filter(StationNumber %in% 189:202)
sd_Rrs_HyperPRO <- read_csv("data/HyperBoost_dataset_AOPs_2023_2024_10042026_Rrs_HTSRB.csv",
                      show_col_types = FALSE) |> 
  filter(N > 0) |> 
  pivot_longer(cols = `Rrs_HTSRB_349.58`:`Rrs_HTSRB_802.42_std`) |> 
  mutate(var_name = case_when(grepl("std", name) ~ "sd", TRUE ~ "mean"),
         wavelength = gsub("Rrs_HTSRB_", "", name),
         wavelength = round(as.numeric(gsub("_std", "", wavelength)))) |> 
  dplyr::select(StationNumber, N, wavelength, var_name, value) |> 
  pivot_wider(values_from = value, names_from = var_name) |> 
  mutate(cv = sd / mean,
         cv_perc = cv*100,
         system = "HyperPRO") |> 
  filter(StationNumber %in% 189:202)

# Combine for comparisons
sd_Rrs_all <- bind_rows(sd_Rrs_SoRad, sd_Rrs_HyperPRO, sd_Rrs_HYPERNETS) |> 
  filter(wavelength >= 400, wavelength <= 600) |> 
  filter(cv_perc > 0) #|> # These are from single samples with 0 for SD
  # filter(wavelength %in% sd_Rrs_SoRad$wavelength,
        #  wavelength %in% sd_Rrs_HyperPRO$wavelength) # There aren't data per nm for these two
write_csv(sd_Rrs_all, file = "output/sd_Rrs_all.csv")

# Mean values
sd_Rrs_all_labels <- sd_Rrs_all |> 
  summarise(sd_median = median(sd, na.rm = TRUE),
            sd_mean = mean(sd, na.rm = TRUE),
            cv_median = median(cv_perc, na.rm = TRUE),
            cv_mean = mean(cv_perc, na.rm = TRUE),
            n = n(), .by = "system") |> 
  mutate(cv_perc_label = paste0("median CV = ",round(cv_median, 1)),
         n_label = paste0("n = ",n))

# Bar plot of variance by wavelength
var_bar <- sd_Rrs_all |> 
  summarise(cv_perc = median(cv_perc, na.rm = TRUE), .by = c("system", "wavelength")) |> 
  # mutate(wavelength = round(wavelength/10)*10) |>
  ggplot(aes(x = wavelength, y = cv_perc, fill = system)) +
  geom_col(position = "dodge") +
  # facet_wrap(~var_name, scales = "free") +
  coord_cartesian(ylim = c(0, 25), expand = FALSE) +
  labs(x = "Wavelength [nm]", y = "Coefficient of variation [CV ; sd / mean ; %]", fill = "System",
       title = "Comparison of Rrs variance between the three systems at CTD cast locations",
       subtitle = "Bars show the median CV value per wavelength from all stations; note that only HYPERNETS has values for each wavelength") +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        legend.position = "bottom")
# var_bar

# Boxplot of variances
var_box <- ggplot(data = sd_Rrs_all, aes(x = system, y = cv_perc, fill = system)) +
  geom_boxplot(show.legend = FALSE, outliers = FALSE) +
  # geom_violin(show.legend = FALSE) +
  geom_text(data = sd_Rrs_all_labels, y = 17, aes(x = system, label = n_label)) +
  geom_text(data = sd_Rrs_all_labels, y = 15, aes(x = system, label = cv_perc_label)) +
   coord_cartesian(ylim = c(-1, 30), expand = FALSE) +
  labs(x = "System", y = "Coefficient of variation [CV ; sd / mean ; %]",
      #  title = "Comparison of Rrs variance between the three systems at CTD cast locations",
       subtitle = "Boxplots show the spread of values across wavelengths 400-600 nm from all stations; note larger sample size for HYPERNETS") +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA, colour = "black"))
# var_box

# Combine and save
var_combi <- ggpubr::ggarrange(var_bar, var_box, ncol = 1, nrow = 2)
ggsave("figures/test_sensors_var_Rrs.png", var_combi, width = 12, height = 10)


## Uncertainty for So-Rad and HyperPRO ------------------------------------

# Load HyperPRO seaBird files directly
base_HyperPRO_files <- dir("~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/hyperpro_processed_data/V2_2025", 
  pattern = "_R2.sb", full.names = TRUE)
unc_HyperPRO <- plyr::ldply(base_HyperPRO_files, proc_seaBass_HyperPRO, .parallel = TRUE)

# Load So-Rad seaBird files directly
base_SoRad_files <- dir("~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/Trios_processed_data", full.names = TRUE)
unc_SoRad <- plyr::ldply(base_SoRad_files, proc_seaBass_SoRad, .parallel = TRUE)

# Combine for plotting
unc_deux <- rbind(unc_HyperPRO, unc_SoRad)

# Plot uncertainty ranges for each variable
ggplot(data = unc_deux, aes(x = system, y = unc, fill = system)) +
  # geom_violin(show.legend = FALSE) +
  geom_boxplot(show.legend = FALSE, outliers = FALSE) +
  facet_wrap(~name, scales = "free") +
  labs(title = "Comparison of uncertainty between systems and variables",
       subtitle = "Values within boxplots show the spread across all wavelengths and samples",
       x = "System", y = "Uncertainty [units for variable]")
ggsave("figures/test_sensors_unc.png", width = 10, height = 9)


## Variance from Hypernets_matchups output --------------------------------

# Calculate variance stats from Hypernets_matchups .csv output files
sensor_uncertainty("ED", "HYPERPRO")
sensor_uncertainty("ED", "TRIOS")
sensor_uncertainty("ED", "HYPERNETS")
sensor_uncertainty("LD", "TRIOS")
sensor_uncertainty("LD", "HYPERNETS")
sensor_uncertainty("LU", "TRIOS")
sensor_uncertainty("LU", "HYPERNETS")
sensor_uncertainty("LW", "HYPERPRO")
sensor_uncertainty("LW", "TRIOS")
sensor_uncertainty("LW", "HYPERNETS")
sensor_uncertainty("RHOW", "HYPERPRO")
sensor_uncertainty("RHOW", "TRIOS")
# sensor_uncertainty("RHOW", "HYPERNETS") # No SD values for Hypernets
sensor_uncertainty("RHOW", "PACE_V2")
sensor_uncertainty("RHOW", "PACE_V30")
sensor_uncertainty("RHOW", "PACE_V31")

# Combine all
var_sensors <- map_dfr(dir("output", pattern = "var_stats_", full.names = TRUE), read_csv) |> 
  mutate(cv_median_perc = cv_median * 100,
         sensor = case_when(sensor == "Hyp" ~ "HYPERNETS",
                            sensor == "HYPERPRO" ~ "HyperPRO",
                            sensor == "TRIOS" ~ "So-Rad",
                            TRUE ~ sensor))

# Plot
ggplot(data = var_sensors, aes(x = sensor, y = cv_median_perc, fill = sensor)) +
  # geom_violin(show.legend = FALSE) +
  geom_boxplot(show.legend = FALSE, outliers = FALSE) +
  facet_wrap(~var_name, scales = "free") +
  labs(title = "Comparison of uncertainty between sensors and variables",
       subtitle = "Values within boxplots show the spread across all wavelengths and samples",
       x = "System / Version", y = "Coefficient of variation (%)")
ggsave("figures/test_sensors_var.png", width = 10, height = 6)


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

# Load outlier reports
outliers_sat <- read_csv("meta/satellite_outliers.csv") |> distinct()
outliers_insitu <- read_csv("meta/in_situ_outliers.csv") |> distinct()

# Load PACE separately to filter specific wavebands
global_stats_PACE <- read_csv("output/global_stats_RHOW_OCI.csv") |> 
  filter(Wavelength_nm %in% c(412, 443, 490, 510, 560))
# write_csv(global_stats_PACE, file = "output/global_stats_PACE.csv")

# Load all global stats
global_stats_all <- map_dfr(dir("output", pattern = "global", full.names = TRUE), read_csv) |> 
  filter(!sensor_X %in% c("PACE_V2", "PACE_V30", "PACE_V31"),
         !sensor_Y %in% c("PACE_V2", "PACE_V30", "PACE_V31")) |> 
  bind_rows(global_stats_PACE)

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
  filter(var_name == "RHOW") |>
  dplyr::select(var_name:sensor_Y) |> 
  group_by(var_name, sensor_X, sensor_Y) |> 
  filter(n_w_nm == max(n_w_nm)) |> 
  ungroup() |> 
  distinct()

# Quantify which sensors matched most closely to which satellites
global_match_mean <- global_stats_all |> 
  filter(sensor_X %in% c("HYPERNETS", "TRIOS", "HYPERPRO")) |> 
  filter(var_name == "RHOW") |>
  summarise(n = n(),
            Slope = mean(Slope, na.rm = TRUE),
            Bias_mean = mean(Bias),
            Bias_abs = mean(abs(Bias)),
            Error = mean(Error), .by = c("var_name", "sensor_X", "sensor_Y"))

# Mean again
global_match_mean_mean <- global_match_mean |> 
  filter(!(sensor_Y %in% c("HYPERNETS", "TRIOS", "HYPERPRO"))) |> 
  summarise(Slope = mean(Slope),
            Bias_mean = mean(Bias_mean),
            Bias_abs = mean(Bias_abs),
            Error = mean(Error), .by = c("var_name", "sensor_X"))

# Global mean matchups from the perspective of the satellites
global_match_sat_mean <- global_stats_all |> 
  filter(!(sensor_Y %in% c("HYPERNETS", "TRIOS", "HYPERPRO"))) |> 
  summarise(Slope = mean(Slope),
            Bias_mean = mean(Bias),
            Bias_abs = mean(abs(Bias)),
            Error = mean(Error), .by = c("sensor_Y"))

# Count of number of wavebands with negative or positive biases
global_match_bias_sign <- global_stats_all |> 
  filter(sensor_X %in% c("HYPERNETS", "TRIOS", "HYPERPRO")) |> 
  # filter(var_name == "RHOW") |> 
  mutate(Bias_positive = case_when(Bias > 0 ~ 1, TRUE ~ 0),
         Bias_negative = case_when(Bias < 0 ~ 1, TRUE ~ 0)) |> 
  summarise(Bias_positive = sum(Bias_positive),
            Bias_negative = sum(Bias_negative), .by = c("var_name", "sensor_X", "sensor_Y"))

# Summarise per in situ platform against satellites
global_match_bias_sign_remote <- global_match_bias_sign |> 
  filter(!(sensor_Y %in% c("HYPERNETS", "TRIOS", "HYPERPRO"))) |> 
  filter(var_name == "RHOW") |> 
  summarise(Bias_positive = sum(Bias_positive),
            Bias_negative = sum(Bias_negative), .by = c("sensor_X"))

# Average bias and error values per waveband
global_waveband_mean <- global_stats_all |> 
  filter(sensor_X %in% c("HYPERNETS", "TRIOS", "HYPERPRO")) |> 
  filter(!(sensor_Y %in% c("HYPERNETS", "TRIOS", "HYPERPRO"))) |> 
  filter(var_name == "RHOW") |>
  summarise(Slope = mean(Slope),
            Bias_mean = mean(Bias),
            Bias_abs = mean(abs(Bias)),
            Error = mean(Error), .by = c("Wavelength_nm"))

# Plot the global mean matchups per in situ sensor
global_match_mean |> 
  filter(!(sensor_Y %in% c("HYPERNETS", "TRIOS", "HYPERPRO"))) |> 
  ggplot(aes(x = sensor_X, y = Error)) +
  geom_boxplot(aes(fill = sensor_X))


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

