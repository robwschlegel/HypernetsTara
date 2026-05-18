# code/matchups.R
# Get the stats for all matchups and visualise results


# Setup -------------------------------------------------------------------

source("code/functions.R")


# BRDF for HYPERNETS ------------------------------------------------------

# In order to perform the BRDF correction for HYPERNETS we need to retrieve them from the So-Rad SeaBass files

# Load So-Rad Rrs file
wind_SoRad <- proc_seaBass_SoRad("~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/Trios_processed_data/TARA_HyperBOOST_Rrs_20240323_20240821_Version_20250911.sb") |> 
  dplyr::select(system:wind) |> 
  distinct() |> 
  dplyr::rename(date = dateTime) |> 
  filter(date >= as.POSIXct("2024-08-08 00:00:00", tz = "UTC"),
         date <= as.POSIXct("2024-08-18 23:59:59", tz = "UTC"))

# Load all HYPERNETS L1C files to get the wind and azimuth values
L1C_HYPERNETS_files <- dir("~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/Hypernets_processed_data",
  full.names = TRUE, recursive = TRUE, pattern = "_L1C_")
L1C_HYPERNETS_files <- L1C_HYPERNETS_files[grepl("v2.0.nc", L1C_HYPERNETS_files)]
hyp_L1C <- purrr::map_dfr(L1C_HYPERNETS_files, proc_HYPERNETS_L1C)
hyp_meta <- hyp_L1C |> 
  dplyr::select(date, name, mean) |>
  dplyr::filter(name %in% c("rhof_wind", "rhof_sza", "rhof_raa", "rhof_vza")) |>
  distinct() |>
  pivot_wider(names_from = name, values_from = mean) |> 
  # Convert all seconds value to 00 to match Hypernets_matchups files
  mutate(date = as.POSIXct(paste0(str_sub(as.character(date), 1, 17),"00"), format = "%Y-%m-%d %H:%M:%S", tz = "UTC+2"))

# Get all HYPERNETS matcups files
hyp_files <- dir("~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/tara_matchups_results_20260504", 
                 full.names = TRUE, recursive = TRUE, pattern = ".csv")
hyp_files <- hyp_files[grepl("RHOW_HYPERNETS_vs", hyp_files)]
hyp_files <- hyp_files[!grepl("RHOW_HYPERNETS_vs_TRIOS_vs_HYPERPRO", hyp_files)]

# Load all Rhow matchups with HYPERNETS data for BRDF correction
# All data must be loaded as these will then be used to replace the same files in the single and global matchups below
hyp_RHOW <- map_dfr(hyp_files, load_matchup_mean) |> 
  mutate(sensor_Y = lead(sensor, 1), .after = "sensor")

# Process dates and add wind from HYPERNETS and So-Rad
hyp_wind <- hyp_RHOW |> 
  filter(sensor == "Hyp") |> 
  mutate(var = "RHOW", date = as.POSIXct(paste0(day, time), format = "%Y%m%d%H%M%S", tz = "UTC+2")) |> 
  left_join(hyp_meta, by = "date") |> 
  difference_left_join(wind_SoRad[,c("date", "wind")], by = "date", max_dist = 12000, distance_col = "timeDiff") |> 
  dplyr::select(sensor, sensor_Y, var, date = date.x, latitude, longitude, rhof_wind:rhof_vza, wind, timeDiff, `380`:`700`) |> 
  # filter(!is.na(wind)) |>
  group_by(sensor, sensor_Y, date) |>
  filter(timeDiff == min(timeDiff, na.rm = TRUE)) |>
  ungroup() |> 
  distinct()
write_csv(hyp_wind, "data/BRDF_corr/HYPERNETS_all_wind.csv")
hyp_wind <- read_csv("data/BRDF_corr/HYPERNETS_all_wind.csv")

# Convert to Rrs and save
hyp_wind_rrs <- hyp_wind |> 
  mutate(across(`380`:`700`, ~ .x / pi), var = "Rrs")

# For loop that saves one file per sensor in sensor_Y
for (sensor_filt in unique(hyp_wind_rrs$sensor_Y)) {
  hyp_wind_rrs |> 
    filter(sensor_Y == sensor_filt) |> 
    pivot_longer(cols = `380`:`700`, names_to = "wavelength", values_to = "Rrs") |>
    filter(!is.na(Rrs)) |> 
    pivot_wider(names_from = wavelength, values_from = Rrs) |> 
    write_csv(file = paste0("data/BRDF_corr/HYPERNETS_",sensor_filt,"_rrs_wind.csv"))
}

# For loop that loads and merges the BRDF-corrected values
hyp_brdf_corr <- data.frame()
for (sensor_filt in unique(hyp_wind_rrs$sensor_Y)) {
  hyp_brdf_corr_sensor_base <- hyp_wind_rrs |> 
    dplyr::select(sensor:longitude) |> 
    filter(sensor_Y == sensor_filt) |> 
    mutate(var = "RHOW")
  hyp_brdf_corr_sensor_i <- tidync(paste0("data/BRDF_corr/HYPERNETS_",sensor_filt,"_rrs_wind_BRDF_corrected.nc")) |> 
    hyper_tibble() |> 
    mutate(Rrs_BRDF = Rrs_BRDF * pi) |> 
    pivot_wider(names_from = bands, values_from = Rrs_BRDF) |> 
    dplyr::select(-n)
  hyp_brdf_corr <- bind_rows(hyp_brdf_corr, bind_cols(hyp_brdf_corr_sensor_base, hyp_brdf_corr_sensor_i))
}; rm(hyp_brdf_corr_sensor_base, hyp_brdf_corr_sensor_i)
write_csv(hyp_brdf_corr, file = "data/BRDF_corr/HYPERNETS_all_rhow_BRDF_corr.csv")

# Compare the BRDF-corrected values to the original matchup values
hyp_brdf_corr_long <- hyp_brdf_corr |> 
  pivot_longer(cols = `400`:`700`, names_to = "wavelength", values_to = "Rhow_BRDF_corr") |> 
  mutate(wavelength = as.numeric(wavelength)) |> 
  filter(!is.na(Rhow_BRDF_corr)) |> 
  distinct()
hyp_wind_long <- hyp_wind |> 
  pivot_longer(cols = `380`:`700`, names_to = "wavelength", values_to = "Rhow_original") |> 
  mutate(wavelength = as.numeric(wavelength)) |> 
  filter(!is.na(Rhow_original)) |> 
  dplyr::select(sensor, sensor_Y, date, latitude, longitude, wavelength, Rhow_original)
hyp_compare <- hyp_wind_long |> 
  left_join(hyp_brdf_corr_long, by = c("sensor", "sensor_Y", "date", "latitude", "longitude", "wavelength")) |> 
  mutate(diff = Rhow_original - Rhow_BRDF_corr,
         diff_perc = diff / Rhow_original * 100)

# Plot the differences per wavelength as boxplots
brdf_comp_band <- ggplot(filter(hyp_compare, sensor_Y == "TRIOS"), aes(x = as.factor(wavelength), y = diff_perc)) +
  geom_boxplot() +
  labs(x = "Wavelength [nm]", y = "Difference between original and BRDF-corrected Rhow (%)",
       title = "Comparison of original and BRDF-corrected Rhow values from HYPERNETS matchups",
       subtitle = "Values within boxplots show the spread across all samples for each wavelength") +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA, colour = "black"))

# Boxplots per sensor_Y column
brdf_comp_sat <- ggplot(hyp_compare, aes(x = sensor_Y, y = diff_perc, fill = sensor_Y)) +
  geom_boxplot(show.legend = FALSE) +
  labs(x = "Satellite", y = "Difference between original and BRDF-corrected Rhow (%)",
       title = "Comparison of original and BRDF-corrected Rhow values from HYPERNETS matchups",
       subtitle = "Values within boxplots show the spread across all samples for each satellite") +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA, colour = "black"))

# Combine and save
brdf_comp <- ggpubr::ggarrange(brdf_comp_band, brdf_comp_sat, ncol = 1, nrow = 2)
ggsave("figures/test_BRDF_comp.png", brdf_comp, width = 12, height = 10)


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

# NB: Rather load the results directly below instead of running this code as it takes a while

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
  filter(name == "Rrs", cv > 0, cv < 3) |>
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

### Load results here for ease ###
read_csv("output/sd_Rrs_all.csv")

# Quick stats output
sd_Rrs_all |>
  summarise(cv_25 = quantile(cv_perc, 0.25, na.rm = TRUE),
            cv_median = median(cv_perc, na.rm = TRUE),
            cv_75 = quantile(cv_perc, 0.75, na.rm = TRUE),
            # cv_mean = mean(cv_perc, na.rm = TRUE),
            n = n(), .by = "system") 

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

# Decide which wavelengths to compare with wavebands
wave_length_bands <- c(380, 400, 412, 443, 490, 510, 560, 620, 673, 700)

# Load in situ and PACE data separately to filter specific wavebands
# NB: Careful with the exact indexing of files here
global_stats_wavelengths <- map_dfr(dir("output", pattern = "global", full.names = TRUE)[c(2:6, 8)], read_csv) |> 
  filter(Wavelength_nm %in% wave_length_bands)

# Load all global stats
# NB: Careful with the exact indexing of files here
global_stats_all <- map_dfr(dir("output", pattern = "global", full.names = TRUE)[c(7, 9, 10)], read_csv) |> 
  bind_rows(global_stats_wavelengths)
write_csv(global_stats_all, file = "output/global_stats_all.csv")

# Visualise difference between linear-space and log-space slopes
# global_stats_all |> 
#   filter(sensor_X %in% c("HYPERNETS", "TRIOS", "HYPERPRO")) |> 
#   ggplot() +
#   geom_histogram(aes(x = Slope), colour = "green", alpha = 0.3, binwidth = 1) +
#   geom_histogram(aes(x = Slope_log), colour = "red", alpha = 0.3, binwidth = 1) +
#   facet_grid(sensor_X ~ sensor_Y)

# Get matchups counts, outliers, etc.
global_count_var_name <- global_stats_all |> 
  filter(sensor_X %in% c("HYPERNETS", "TRIOS", "HYPERPRO")) |> 
  filter(var_name == "RHOW") |>
  dplyr::select(sensor_X, sensor_Y, var_name, n_clean, n_no_out) |> 
  group_by(var_name, sensor_X, sensor_Y) |> 
  filter(n_clean == max(n_clean, na.rm = TRUE)) |> 
  ungroup() |> 
  distinct()

# Quantify which sensors matched most closely to which satellites
global_match_mean <- global_stats_all |> 
  filter(sensor_X %in% c("HYPERNETS", "TRIOS", "HYPERPRO")) |> 
  filter(var_name == "RHOW") |>
  filter(Wavelength_nm >= 380, Wavelength_nm <= 600) |>
  summarise(Slope = mean(Slope, na.rm = TRUE),
            Bias_mean = mean(Bias, na.rm = TRUE),
            Bias_abs = mean(abs(Bias), na.rm = TRUE),
            Error = mean(Error, na.rm = TRUE), .by = c("var_name", "sensor_X", "sensor_Y"))

# Mean again
global_match_mean_mean <- global_stats_all |> 
  filter(sensor_X %in% c("HYPERNETS", "TRIOS", "HYPERPRO")) |> 
  filter(var_name == "RHOW") |>
  filter(Wavelength_nm >= 380, Wavelength_nm <= 600) |>
  filter(!(sensor_Y %in% c("HYPERNETS", "TRIOS", "HYPERPRO"))) |> 
  summarise(Slope = mean(Slope, na.rm = TRUE),
            Bias_mean = mean(Bias, na.rm = TRUE),
            Bias_abs = mean(abs(Bias), na.rm = TRUE),
            Error = mean(Error, na.rm = TRUE), .by = c("var_name", "sensor_X"))

# Global mean matchups from the perspective of the satellites
global_match_sat_mean <- global_stats_all |> 
  filter(sensor_X %in% c("HYPERNETS", "TRIOS", "HYPERPRO")) |> 
  filter(var_name == "RHOW") |>
  filter(Wavelength_nm >= 380, Wavelength_nm <= 600) |>
  filter(!(sensor_Y %in% c("HYPERNETS", "TRIOS", "HYPERPRO"))) |> 
  summarise(Slope = mean(Slope, na.rm = TRUE),
            Bias_mean = mean(Bias, na.rm = TRUE),
            Bias_abs = mean(abs(Bias), na.rm = TRUE),
            Error = mean(Error, na.rm = TRUE), .by = c("sensor_Y"))

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
  summarise(Slope = mean(Slope, na.rm = TRUE),
            Bias_mean = mean(Bias, na.rm = TRUE),
            Bias_abs = mean(abs(Bias), na.rm = TRUE),
            Error = mean(Error, na.rm = TRUE), .by = c("Wavelength_nm"))

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

# Choose acordingly
folder_path <- file_path_build("RHOW", "HYPERNETS", "HYPERPRO")

# List all files in directory
file_list <- list.files(folder_path, pattern = "*.csv", full.names = TRUE)
match_base_details <- read_csv("~/HypernetsTara/output/matchup_stats_RHOW_in_situ.csv") |>
  dplyr::select(file_name) |> distinct()
file_list_clean <- file_list[basename(file_list) %in% match_base_details$file_name]

# Load data
match_base <- map_dfr(file_list_clean, load_matchup_long)
# print(unique(match_base$wavelength))

# Auto-generate chosen wavelengths
(W_nm <- W_nm_out("HYPERNETS"))
# W_nm <- c(380, 400, 412, 443, 490, 510, 560, 620, 673, 700)

# Get data.frame for matchup based on the wavelength of choice
(matchup_filt <- filter(match_base, wavelength == W_nm[10]))

# Create vectors from filtered columns
(x_vec <- matchup_filt[["HYPERPRO"]])
(y_vec <- matchup_filt[["Hyp"]])

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
match_base |> filter(wavelength == W_nm[8]) |> 
  ggplot(aes(x = HYPERPRO, y = Hyp)) +
  geom_point(aes(colour = wavelength))

