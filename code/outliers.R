# code/outliers.R
# Convenience functions to rapidly visualise outliers


# Setup -------------------------------------------------------------------

source("code/functions.R")


# In situ Outliers ---------------------------------------------------------

## ED ----------------------------------------------------------------------

# Load processed in situ matchups
matchup_ED_in_situ <- read_csv("output/matchup_stats_ED_in_situ.csv") |> 
  mutate(comp_sensors = paste0(sensor_X," vs ",sensor_Y)) |> 
  filter(comp_sensors %in% c("Hyp vs TRIOS", "HYPERPRO vs Hyp", "HYPERPRO vs TRIOS"))

# Load base W_nm matchup values
base_ED_in_situ <- plyr::ldply(list.files(c(file_path_build("ED", "Hypernets", "Trios"), 
                                            file_path_build("ED", "Hypernets", "Hyperpro"),
                                            file_path_build("ED", "Trios", "Hyperpro")),
                                          pattern = "*.csv", full.names = TRUE), 
                               load_matchup_long, .parallel = TRUE)

# Join for full range of stats and to remove matchups outside of time diff limit
join_ED_in_situ <- right_join(base_ED_in_situ, matchup_ED_in_situ, by = join_by(file_name))

# Plot matchup by Error + Bias
plot_matchup_Error_Bias(join_ED_in_situ, "Ed", "Hyp", "TRIOS") # Bias < -50
plot_matchup_Error_Bias(join_ED_in_situ, "Ed", "HYPERPRO", "Hyp") # OK
plot_matchup_Error_Bias(join_ED_in_situ, "Ed", "HYPERPRO", "TRIOS") # OK

# Manual image checks
manual_ED_check <- base_ED_in_situ |> 
  filter(file_name == "HYPERNETS_vs_TRIOS_vs_20240810T151500_ED.csv") |> 
  distinct(file_name) |> 
  left_join(matchup_ED_in_situ, by = "file_name") |> 
  mutate(val_filter = "Manual image check")

# Filter all by Bias to get an initial idea of the issues
filter_ED_in_situ <- filter(matchup_ED_in_situ, Bias <= -50) |> mutate(val_filter = "Bias <= -50%") |> 
  bind_rows(manual_ED_check)
filter_join_ED_in_situ <- right_join(join_ED_in_situ, filter_ED_in_situ)
clean_join_ED_in_situ <- anti_join(join_ED_in_situ, filter_ED_in_situ)

# Plot all wavelength matchups
plot_matchup_nm(join_ED_in_situ, "Ed", "Hyp", "TRIOS")
plot_matchup_nm(filter_join_ED_in_situ, "Ed", "Hyp", "TRIOS")
plot_matchup_nm(clean_join_ED_in_situ, "Ed", "Hyp", "TRIOS")

# Plot matchups by date
plot_matchup_date(join_ED_in_situ, "Ed", "Hyp", "TRIOS")
plot_matchup_date(filter_join_ED_in_situ, "Ed", "Hyp", "TRIOS")
plot_matchup_date(filter_join_ED_in_situ, "Ed", "Hyp", "HYPERPRO") # OK
plot_matchup_date(filter_join_ED_in_situ, "Ed", "HYPERPRO", "TRIOS") # OK


## LU ----------------------------------------------------------------------

# Load processed in situ matchups
matchup_LU_in_situ <- read_csv("output/matchup_stats_LU_Hyp_vs_Trios.csv") |> 
  mutate(comp_sensors = paste0(sensor_X," vs ",sensor_Y)) |> 
  filter(comp_sensors %in% c("Hyp vs TRIOS", "HYPERPRO vs Hyp", "HYPERPRO vs TRIOS"))

# Load base W_nm matchup values
base_LU_in_situ <- plyr::ldply(list.files(c(file_path_build("LU", "Hypernets", "Trios"), 
                                            file_path_build("LU", "Hypernets", "Hyperpro"),
                                            file_path_build("LU", "Trios", "Hyperpro")),
                                          pattern = "*.csv", full.names = TRUE), 
                               load_matchup_long, .parallel = TRUE)

# Join for full range of stats
join_LU_in_situ <- right_join(base_LU_in_situ, matchup_LU_in_situ, by = join_by(file_name))

# Plot matchup by Error + Bias
# NB: Only HypStar vs Trios
plot_matchup_Error_Bias(join_LU_in_situ, "Lu", "Hyp", "TRIOS") # Bias < -50

# Filter all by Error to get an initial idea of the issues
filter_LU_in_situ <- filter(matchup_LU_in_situ, Bias <= -50) |> mutate(val_filter = "Bias <= -50%")
filter_join_LU_in_situ <- right_join(join_LU_in_situ, filter_LU_in_situ)
clean_join_LU_in_situ <- anti_join(join_LU_in_situ, filter_LU_in_situ)

# Plot all wavelength matchups
plot_matchup_nm(join_LU_in_situ, "LU", "Hyp", "TRIOS")
plot_matchup_nm(filter_join_LU_in_situ, "LU", "Hyp", "TRIOS")
plot_matchup_nm(clean_join_LU_in_situ, "LU", "Hyp", "TRIOS")

# Plot matchups by date
plot_matchup_date(filter_join_LU_in_situ, "LU", "Hyp", "TRIOS")


## LD ----------------------------------------------------------------------

# Load processed in situ matchups
matchup_LD_in_situ <- read_csv("output/matchup_stats_LD_Hyp_vs_Trios.csv") |> 
  mutate(comp_sensors = paste0(sensor_X," vs ",sensor_Y)) |> 
  filter(comp_sensors %in% c("Hyp vs TRIOS", "HYPERPRO vs Hyp", "HYPERPRO vs TRIOS"))

# Load base W_nm matchup values
base_LD_in_situ <- plyr::ldply(list.files(c(file_path_build("LD", "Hypernets", "Trios"), 
                                            file_path_build("LD", "Hypernets", "Hyperpro"),
                                            file_path_build("LD", "Trios", "Hyperpro")),
                                          pattern = "*.csv", full.names = TRUE), 
                               load_matchup_long, .parallel = TRUE)

# Join for full range of stats
join_LD_in_situ <- right_join(base_LD_in_situ, matchup_LD_in_situ, by = join_by(file_name))

# Plot matchup by Error + Bias
# NB: Only HypStar vs Trios
plot_matchup_Error_Bias(join_LD_in_situ, "Ld", "Hyp", "TRIOS") # Bias < -50

# Filter all by Error to get an initial idea of the issues
filter_LD_in_situ <- filter(matchup_LD_in_situ, Bias <= -50) |> mutate(val_filter = "Bias <= -50%")
filter_join_LD_in_situ <- right_join(join_LD_in_situ, filter_LD_in_situ)
clean_join_LD_in_situ <- anti_join(join_LD_in_situ, filter_LD_in_situ)

# Plot all wavelength matchups
plot_matchup_nm(join_LD_in_situ, "LD", "Hyp", "TRIOS")
plot_matchup_nm(filter_join_LD_in_situ, "LD", "Hyp", "TRIOS")
plot_matchup_nm(clean_join_LD_in_situ, "LD", "Hyp", "TRIOS")

# Plot matchups by date
plot_matchup_date(filter_join_LD_in_situ, "LD", "Hyp", "TRIOS")


## LW ----------------------------------------------------------------------

# Load processed in situ matchups
matchup_LW_in_situ <- read_csv("output/matchup_stats_LW_in_situ.csv") |> 
  mutate(comp_sensors = paste0(sensor_X," vs ",sensor_Y)) |> 
  filter(comp_sensors %in% c("Hyp vs TRIOS", "HYPERPRO vs Hyp", "HYPERPRO vs TRIOS"))

# Load base W_nm matchup values
base_LW_in_situ <- plyr::ldply(list.files(c(file_path_build("LW", "Hypernets", "Trios"), 
                                            file_path_build("LW", "Hypernets", "Hyperpro"),
                                            file_path_build("LW", "Trios", "Hyperpro")),
                                          pattern = "*.csv", full.names = TRUE), 
                               load_matchup_long, .parallel = TRUE)

# Join for full range of stats
join_LW_in_situ <- right_join(base_LW_in_situ, matchup_LW_in_situ, by = join_by(file_name))

# Plot matchup by Error + Bias
plot_matchup_Error_Bias(join_LW_in_situ, "Lw", "Hyp", "TRIOS") # Bias < -50
plot_matchup_Error_Bias(join_LW_in_situ, "Lw", "HYPERPRO", "Hyp") # OK
plot_matchup_Error_Bias(join_LW_in_situ, "Lw", "HYPERPRO", "TRIOS") # OK

# List of files with negative HypStar values
neg_LW_hypstar <- base_LW_in_situ |> 
  filter(Hyp < 0) |> 
  distinct(file_name) |> 
  left_join(matchup_LW_in_situ) |> 
  mutate(val_filter = "Negative HypStar values")

# Manual removals after image check
manual_LW_check <- base_LW_in_situ |> 
  filter(file_name == "HYPERNETS_vs_HYPERPRO_vs_20240812T104500_LW.csv") |>
  distinct(file_name) |> 
  left_join(matchup_LW_in_situ) |> 
  mutate(val_filter = "Manual image check")

# Filter all by Error to get an initial idea of the issues
filter_LW_in_situ <- filter(matchup_LW_in_situ, Bias <= -50) |> mutate(val_filter = "Bias <= -50%") |> 
  bind_rows(neg_LW_hypstar, manual_LW_check)
filter_join_LW_in_situ <- right_join(join_LW_in_situ, filter_LW_in_situ)
clean_join_LW_in_situ <- anti_join(join_LW_in_situ, filter_LW_in_situ)

# Plot all wavelength matchups
plot_matchup_nm(join_LW_in_situ, "LW", "Hyp", "TRIOS")
plot_matchup_nm(filter_join_LW_in_situ, "LW", "Hyp", "TRIOS")
plot_matchup_nm(clean_join_LW_in_situ, "LW", "Hyp", "TRIOS")
plot_matchup_nm(join_LW_in_situ, "LW", "HYPERPRO", "Hyp")
plot_matchup_nm(filter_join_LW_in_situ, "LW", "HYPERPRO", "Hyp")
plot_matchup_nm(clean_join_LW_in_situ, "LW", "HYPERPRO", "Hyp")
plot_matchup_nm(join_LW_in_situ, "LW", "HYPERPRO", "TRIOS")
plot_matchup_nm(filter_join_LW_in_situ, "LW", "HYPERPRO", "TRIOS") # OK
plot_matchup_nm(clean_join_LW_in_situ, "LW", "HYPERPRO", "TRIOS")

# Plot matchups by date
plot_matchup_date(filter_join_LW_in_situ, "LW", "TRIOS", "Hyp")
plot_matchup_date(filter_join_LW_in_situ, "LW", "HYPERPRO", "Hyp")


## Rhow --------------------------------------------------------------------

# Load processed in situ matchups
matchup_RHOW_in_situ <- read_csv("output/matchup_stats_RHOW_in_situ.csv") |> 
  mutate(comp_sensors = paste0(sensor_X," vs ",sensor_Y)) |> 
  filter(comp_sensors %in% c("Hyp vs TRIOS", "HYPERPRO vs Hyp", "HYPERPRO vs TRIOS"))

# Load base W_nm matchup values
base_RHOW_in_situ <- plyr::ldply(list.files(c(file_path_build("RHOW", "Hypernets", "Trios"), 
                                              file_path_build("RHOW", "Hypernets", "Hyperpro"),
                                              file_path_build("RHOW", "Trios", "Hyperpro")),
                                            pattern = "*.csv", full.names = TRUE), 
                                 load_matchup_long, .parallel = TRUE)

# Join for full range of stats
join_RHOW_in_situ <- right_join(base_RHOW_in_situ, matchup_RHOW_in_situ, by = join_by(file_name))

# Plot matchup by Error + Bias
plot_matchup_Error_Bias(join_RHOW_in_situ, "RHOW", "Hyp", "TRIOS") # OK
plot_matchup_Error_Bias(join_RHOW_in_situ, "RHOW", "HYPERPRO", "Hyp") # OK
plot_matchup_Error_Bias(join_RHOW_in_situ, "RHOW", "HYPERPRO", "TRIOS") # OK

# List of files with negative HypStar values
neg_RHOW_hypstar <- base_RHOW_in_situ |> 
  filter(Hyp < 0) |> 
  distinct(file_name) |> 
  left_join(matchup_RHOW_in_situ) |> 
  filter(!is.na(var_name)) |> 
  mutate(val_filter = "Negative HypStar values")

# Filter all by Error to get an initial idea of the issues
filter_RHOW_in_situ <- filter(matchup_RHOW_in_situ, Bias >= 50) |> mutate(val_filter = "Bias >= 50%") |> 
  bind_rows(neg_RHOW_hypstar)
filter_join_RHOW_in_situ <- right_join(join_RHOW_in_situ, filter_RHOW_in_situ)
clean_join_RHOW_in_situ <- anti_join(join_RHOW_in_situ, filter_RHOW_in_situ)

# Plot all wavelength matchups
plot_matchup_nm(join_RHOW_in_situ, "RHOW", "Hyp", "TRIOS")
plot_matchup_nm(filter_join_RHOW_in_situ, "RHOW", "Hyp", "TRIOS")
plot_matchup_nm(clean_join_RHOW_in_situ, "RHOW", "Hyp", "TRIOS")
plot_matchup_nm(join_RHOW_in_situ, "RHOW", "HYPERPRO", "Hyp")
plot_matchup_nm(filter_join_RHOW_in_situ, "RHOW", "HYPERPRO", "Hyp")
plot_matchup_nm(clean_join_RHOW_in_situ, "RHOW", "HYPERPRO", "Hyp")
plot_matchup_nm(join_RHOW_in_situ, "RHOW", "HYPERPRO", "TRIOS")
plot_matchup_nm(filter_join_RHOW_in_situ, "RHOW", "HYPERPRO", "TRIOS")
plot_matchup_nm(clean_join_RHOW_in_situ, "RHOW", "HYPERPRO", "TRIOS")

# Plot matchups by date
plot_matchup_date(filter_join_RHOW_in_situ, "RHOW", "Hyp", "TRIOS")
plot_matchup_date(filter_join_RHOW_in_situ, "RHOW", "HYPERPRO", "Hyp")
plot_matchup_date(filter_join_RHOW_in_situ, "RHOW", "HYPERPRO", "TRIOS")


## Combine in situ outliers ------------------------------------------------

# Stack all filtered data.frames with file names that appear to be outliers
in_situ_outliers <- rbind(filter_ED_in_situ, filter_LD_in_situ, filter_LU_in_situ, 
                          filter_LW_in_situ, neg_LW_hypstar, 
                          filter_RHOW_in_situ, neg_RHOW_hypstar) |> 
  dplyr::select(file_name, sensor_X, sensor_Y, var_name, comp_sensors, 
                dateTime_X, dateTime_Y, Slope, Error, Bias, val_filter) |> 
  distinct()
write_csv(in_situ_outliers, "meta/in_situ_outliers.csv")

# Scatterplots of difftime and distance for Error and Bias
plot_matchup_scatter(matchup_ED_in_situ, "ED")
plot_matchup_scatter(matchup_LD_in_situ, "LD")
plot_matchup_scatter(matchup_LU_in_situ, "LU")
plot_matchup_scatter(matchup_LW_in_situ, "LW")
plot_matchup_scatter(matchup_RHOW_in_situ, "RHOW")


# Satellite Outliers -------------------------------------------------------

## MODIS -------------------------------------------------------------------

# Load processed in situ matchups
matchup_MODIS <- read_csv("output/matchup_stats_RHOW_MODIS.csv") |> 
  filter(sensor_X %in% c("Hyp", "HYPERPRO", "TRIOS")) |> 
  mutate(comp_sensors = paste0(sensor_X," vs ",sensor_Y))

# File list
file_list_MODIS <- list.files(dir("~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/tara_matchups_results_20260203", 
                                  pattern = "AQUA", full.names = TRUE), pattern = "*.csv", full.names = TRUE)

# Load base W_nm matchup values
base_MODIS <- plyr::ldply(file_list_MODIS, load_matchup_long, .parallel = TRUE)

# Join for full range of stats
join_MODIS <- right_join(base_MODIS, matchup_MODIS, by = join_by(file_name))

# Plot matchup by Error + Bias
plot_matchup_Error_Bias(join_MODIS, "Rhow", "Hyp", "AQUA") # Error > 50
plot_matchup_Error_Bias(join_MODIS, "Rhow", "TRIOS", "AQUA") # OK
plot_matchup_Error_Bias(join_MODIS, "Rhow", "HYPERPRO", "AQUA") # OK

# Check satellite variance in files
sat_var_MODIS <- plyr::ldply(file_list_MODIS, sat_var_check, .parallel = TRUE) # OK

# Filter all by Error or Bias to get an initial idea of the issues
# No need to filter by satellite variance, there is one bad AQUA matchup across all sensors and dateTimes
# 2024-08-15 10:45:01
filter_MODIS <- filter(matchup_MODIS, Error >= 50) |> mutate(val_filter = "Error >= 50%")
filter_join_MODIS <- right_join(join_MODIS, filter_MODIS)
clean_join_MODIS <- anti_join(join_MODIS, filter_MODIS)

# Plot matchups by date
plot_matchup_date(filter_join_MODIS, "Rhow", "Hyp", "AQUA")
plot_matchup_date(filter_join_MODIS, "Rhow", "TRIOS", "AQUA")
plot_matchup_date(filter_join_MODIS, "Rhow", "HYPERPRO", "AQUA") # OK

# Plot all wavelength matchups
plot_matchup_nm(join_MODIS, "Rhow", "Hyp", "AQUA")
plot_matchup_nm(filter_join_MODIS, "Rhow", "Hyp", "AQUA")
plot_matchup_nm(clean_join_MODIS, "Rhow", "Hyp", "AQUA")
plot_matchup_nm(join_MODIS, "Rhow", "TRIOS", "AQUA")
plot_matchup_nm(filter_join_MODIS, "Rhow", "TRIOS", "AQUA")
plot_matchup_nm(clean_join_MODIS, "Rhow", "TRIOS", "AQUA")
plot_matchup_nm(join_MODIS, "Rhow", "HYPERPRO", "AQUA")
plot_matchup_nm(filter_join_MODIS, "Rhow", "HYPERPRO", "AQUA") # OK
plot_matchup_nm(clean_join_MODIS, "Rhow", "HYPERPRO", "AQUA")


## VIIRS -------------------------------------------------------------------

# Load processed in situ matchups
matchup_VIIRS <- read_csv("output/matchup_stats_RHOW_VIIRS.csv") |> 
  filter(sensor_X %in% c("Hyp", "HYPERPRO", "TRIOS")) |> 
  mutate(comp_sensors = paste0(sensor_X," vs ",sensor_Y))

# File list
file_list_VIIRS <- list.files(dir("~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/tara_matchups_results_20260203", 
                                  pattern = "VIIRS", full.names = TRUE), pattern = "*.csv", full.names = TRUE)

# Load base W_nm matchup values
base_VIIRS <- plyr::ldply(file_list_VIIRS, load_matchup_long, .parallel = TRUE)

# Join for full range of stats
join_VIIRS <- right_join(base_VIIRS, matchup_VIIRS, by = join_by(file_name))
                           
# Plot matchup by Error + Bias
# NB: VIIRS_N is visually the least similar, so using this for base reference
plot_matchup_Error_Bias(join_VIIRS, "Rhow", "Hyp", "VIIRS_N") # Error > 50
plot_matchup_Error_Bias(join_VIIRS, "Rhow", "TRIOS", "VIIRS_N") # Error > 50
plot_matchup_Error_Bias(join_VIIRS, "Rhow", "HYPERPRO", "VIIRS_N") # OK

# Check satellite variance in files
sat_var_VIIRS <- plyr::ldply(file_list_VIIRS, sat_var_check, .parallel = TRUE)
filter_var_VIIRS <- filter(matchup_VIIRS, file_name %in% sat_var_VIIRS$file_name) |> mutate(val_filter = "CV >= 20%")

# Filter all by Error or Bias to get an initial idea of the issues
filter_VIIRS <- matchup_VIIRS |> 
  filter(!file_name %in% filter_var_VIIRS$file_name) |> 
  filter(Error >= 50) |> mutate(val_filter = "Error >= 50%") |> 
  bind_rows(filter_var_VIIRS) |> 
  filter(!file_name %in% c("TRIOS_vs_VIIRS_N_vs_20240813T100254_RHOW.csv",
                           "TRIOS_vs_VIIRS_N_vs_20240813T101805_RHOW.csv",
                           "TRIOS_vs_VIIRS_N_vs_20240813T102853_RHOW.csv")) # Manually checked, not an outlier
filter_join_VIIRS <- right_join(join_VIIRS, filter_VIIRS)
clean_join_VIIRS <- anti_join(join_VIIRS, filter_VIIRS)

# Plot matchups by date
plot_matchup_date(filter_join_VIIRS, "Rhow", "Hyp", "VIIRS_N")
plot_matchup_date(filter_join_VIIRS, "Rhow", "TRIOS", "VIIRS_N")
plot_matchup_date(filter_join_VIIRS, "Rhow", "HYPERPRO", "VIIRS_N")

# Plot all wavelength matchups
plot_matchup_nm(join_VIIRS, "Rhow", "Hyp", "VIIRS_N")
plot_matchup_nm(filter_join_VIIRS, "Rhow", "Hyp", "VIIRS_N")
plot_matchup_nm(clean_join_VIIRS, "Rhow", "Hyp", "VIIRS_N")
plot_matchup_nm(join_VIIRS, "Rhow", "TRIOS", "VIIRS_N")
plot_matchup_nm(filter_join_VIIRS, "Rhow", "TRIOS", "VIIRS_N")
plot_matchup_nm(clean_join_VIIRS, "Rhow", "TRIOS", "VIIRS_N")
plot_matchup_nm(join_VIIRS, "Rhow", "HYPERPRO", "VIIRS_N")
plot_matchup_nm(filter_join_VIIRS, "Rhow", "HYPERPRO", "VIIRS_N")
plot_matchup_nm(clean_join_VIIRS, "Rhow", "HYPERPRO", "VIIRS_N")


## OLCI --------------------------------------------------------------------

# Load processed in situ matchups
matchup_OLCI <- read_csv("output/matchup_stats_RHOW_OLCI.csv") |> 
  filter(sensor_X %in% c("Hyp", "HYPERPRO", "TRIOS")) |> 
  mutate(comp_sensors = paste0(sensor_X," vs ",sensor_Y))

# OLCI files
file_list_OLCI <- list.files(dir("~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/tara_matchups_results_20260203", 
                                 pattern = "S3", full.names = TRUE), pattern = "*.csv", full.names = TRUE)

# Load base W_nm matchup values
base_OLCI <- plyr::ldply(file_list_OLCI, load_matchup_long, .parallel = TRUE)

# Join for full range of stats
join_OLCI <- right_join(base_OLCI, matchup_OLCI, by = join_by(file_name))

# Check satellite variance in files
sat_var_OLCI <- plyr::ldply(file_list_OLCI, sat_var_check, .parallel = TRUE)

# Plot matchup by Error + Bias
# NB: There are more S3A matchups, so using that sensor for analysis
plot_matchup_Error_Bias(join_OLCI, "Rhow", "Hyp", "S3A") # OK
plot_matchup_Error_Bias(join_OLCI, "Rhow", "TRIOS", "S3A") # Error > 50
plot_matchup_Error_Bias(join_OLCI, "Rhow", "HYPERPRO", "S3A") # OK

# Filter all by Error or Bias to get an initial idea of the issues
filter_OLCI <- filter(matchup_OLCI, Error >= 50) |> mutate(val_filter = "Error >= 50%") |> 
  filter(!file_name %in% c("TRIOS_vs_S3B_vs_20240813T101805_RHOW.csv",
                           "TRIOS_vs_S3A_vs_20240818T092817_RHOW.csv")) # Manually checked, not an outlier, just a poor matchup
filter_join_OLCI <- right_join(join_OLCI, filter_OLCI)
clean_join_OLCI <- anti_join(join_OLCI, filter_OLCI)

# Plot matchups by date
plot_matchup_date(filter_join_OLCI, "Rhow", "Hyp", "S3A") # OK
plot_matchup_date(filter_join_OLCI, "Rhow", "TRIOS", "S3A")
plot_matchup_date(filter_join_OLCI, "Rhow", "HYPERPRO", "S3A") # OK

# Plot all wavelength matchups
plot_matchup_nm(join_OLCI, "Rhow", "Hyp", "S3A")
plot_matchup_nm(filter_join_OLCI, "Rhow", "Hyp", "S3A") # OK
plot_matchup_nm(clean_join_OLCI, "Rhow", "Hyp", "S3A")
plot_matchup_nm(join_OLCI, "Rhow", "TRIOS", "S3A")
plot_matchup_nm(filter_join_OLCI, "Rhow", "TRIOS", "S3A")
plot_matchup_nm(clean_join_OLCI, "Rhow", "TRIOS", "S3A")
plot_matchup_nm(join_OLCI, "Rhow", "HYPERPRO", "S3A")
plot_matchup_nm(filter_join_OLCI, "Rhow", "HYPERPRO", "S3A") # OK
plot_matchup_nm(clean_join_OLCI, "Rhow", "HYPERPRO", "S3A")


## OCI ---------------------------------------------------------------------

# Load processed in situ matchups
matchup_OCI <- read_csv("output/matchup_stats_RHOW_OCI.csv") |> 
  filter(sensor_X %in% c("Hyp", "HYPERPRO", "TRIOS")) |> 
  mutate(comp_sensors = paste0(sensor_X," vs ",sensor_Y))

# File list
file_list_OCI <- list.files(dir("~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/tara_matchups_results_20260203", 
                                  pattern = "PACE", full.names = TRUE), pattern = "*.csv", full.names = TRUE)
file_list_OCI <- file_list_OCI[!grepl("RHOW_PACE_V2_vs_PACE_V30_vs_PACE_V31", file_list_OCI)] # Remove self-comparisons

# Load base W_nm matchup values
base_OCI <- plyr::ldply(file_list_OCI, load_matchup_long, .parallel = TRUE)

# Join for full range of stats
join_OCI <- right_join(base_OCI, matchup_OCI, by = join_by(file_name))

# Plot matchup by Error + Bias
# NB: PACE_v30 is visually the least similar, so using this for base reference
# NB: There are many PACE files with negative values
plot_matchup_Error_Bias(join_OCI, "Rhow", "Hyp", "PACE_V30") # Bias < -50
plot_matchup_Error_Bias(join_OCI, "Rhow", "TRIOS", "PACE_V30") # Bias < -50
plot_matchup_Error_Bias(join_OCI, "Rhow", "HYPERPRO", "PACE_V30") # Bias < -50

# Check satellite variance in files
sat_var_OCI <- plyr::ldply(file_list_OCI, sat_var_check, .parallel = TRUE)
filter_var_OCI <- filter(matchup_OCI, file_name %in% sat_var_OCI$file_name) |> mutate(val_filter = "CV >= 20%")

# Filter all by Error or Bias to get an initial idea of the issues
filter_OCI <- matchup_OCI |> 
  filter(!file_name %in% filter_var_OCI$file_name) |> 
  filter(Error >= 50) |> mutate(val_filter = "Error >= 50%") |> 
  bind_rows(filter_var_OCI) |> 
  filter(!file_name %in% c("HYPERNETS_vs_PACE_V31_vs_20240814T123100_RHOW.csv")) # Manually checked, not an outlier
filter_join_OCI <- right_join(base_OCI, filter_OCI, by = join_by(file_name))
clean_join_OCI <- anti_join(base_OCI, filter_OCI, by = join_by(file_name))

# Plot matchups by date
plot_matchup_date(filter_join_OCI, "Rhow", "Hyp", "PACE_V30")
plot_matchup_date(filter_join_OCI, "Rhow", "TRIOS", "PACE_V30")
plot_matchup_date(filter_join_OCI, "Rhow", "HYPERPRO", "PACE_V30")

# Plot all wavelength matchups
plot_matchup_nm(join_OCI, "Rhow", "Hyp", "PACE_V30")
plot_matchup_nm(filter_join_OCI, "Rhow", "Hyp", "PACE_V30")
plot_matchup_nm(clean_join_OCI, "Rhow", "Hyp", "PACE_V30")
plot_matchup_nm(join_OCI, "Rhow", "TRIOS", "PACE_V30")
plot_matchup_nm(filter_join_OCI, "Rhow", "TRIOS", "PACE_V30")
plot_matchup_nm(clean_join_OCI, "Rhow", "TRIOS", "PACE_V30")
plot_matchup_nm(join_OCI, "Rhow", "HYPERPRO", "PACE_V30")
plot_matchup_nm(filter_join_OCI, "Rhow", "HYPERPRO", "PACE_V30") # OK
plot_matchup_nm(clean_join_OCI, "Rhow", "HYPERPRO", "PACE_V30")


## Combine satellite outliers ----------------------------------------------

# Stack all filtered data.frames with file names that appear to be outliers
satellite_outliers <- rbind(filter_OLCI, filter_VIIRS, filter_MODIS, filter_OCI) |> 
  dplyr::select(file_name, sensor_X, sensor_Y, var_name, comp_sensors, 
                dateTime_X, dateTime_Y, Slope, Error, Bias, val_filter) |> 
  distinct()
write_csv(satellite_outliers, "meta/satellite_outliers.csv")


# OLCI v3.0 vs v4.0 ------------------------------------------------------

# Load outlier list
satellite_outliers <- read_csv("meta/satellite_outliers.csv")

# Get list of OLCI Hypernets_matchups
match_hyp_S3A <- dir("~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/tara_matchups_results_20260203/RHOW_HYPERNETS_vs_S3A", 
    full.names = TRUE, pattern = ".csv")
match_hyp_S3B <- dir("~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/tara_matchups_results_20260203/RHOW_HYPERNETS_vs_S3B", 
    full.names = TRUE, pattern = ".csv")
match_pro_S3A <- dir("~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/tara_matchups_results_20260203/RHOW_HYPERPRO_vs_S3A", 
    full.names = TRUE, pattern = ".csv")
match_pro_S3B <- dir("~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/tara_matchups_results_20260203/RHOW_HYPERPRO_vs_S3B", 
    full.names = TRUE, pattern = ".csv")
match_tri_S3A <- dir("~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/tara_matchups_results_20260203/RHOW_TRIOS_vs_S3A", 
    full.names = TRUE, pattern = ".csv")
match_tri_S3B <- dir("~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/tara_matchups_results_20260203/RHOW_TRIOS_vs_S3B", 
    full.names = TRUE, pattern = ".csv")

# S3A
hyp_S3A <- plyr::ldply(match_hyp_S3A, process_OLCI_matchups, .parallel = TRUE)
write_csv(hyp_S3A, "output/test_hyp_S3A.csv")
pro_S3A <- plyr::ldply(match_pro_S3A, process_OLCI_matchups, .parallel = TRUE)
write_csv(pro_S3A, "output/test_pro_S3A.csv")
tri_S3A <- plyr::ldply(match_tri_S3A, process_OLCI_matchups, .parallel = TRUE)
write_csv(tri_S3A, "output/test_tri_S3A.csv")

# S3B
hyp_S3B <- plyr::ldply(match_hyp_S3B, process_OLCI_matchups, .parallel = TRUE)
write_csv(hyp_S3B, "output/test_hyp_S3B.csv")
pro_S3B <- plyr::ldply(match_pro_S3B, process_OLCI_matchups, .parallel = TRUE)
write_csv(pro_S3B, "output/test_pro_S3B.csv")
tri_S3B <- plyr::ldply(match_tri_S3B, process_OLCI_matchups, .parallel = TRUE)
write_csv(tri_S3B, "output/test_tri_S3B.csv")

# Load calculated results
hyp_S3A <- read_csv("output/test_hyp_S3A.csv")
hyp_S3B <- read_csv("output/test_hyp_S3B.csv")
pro_S3A <- read_csv("output/test_pro_S3A.csv")
pro_S3B <- read_csv("output/test_pro_S3B.csv")
tri_S3A <- read_csv("output/test_tri_S3A.csv")
tri_S3B <- read_csv("output/test_tri_S3B.csv")

# Run stats on all matchups
hyp_S3A_stats <- process_OLCI_stats(hyp_S3A)
hyp_S3B_stats <- process_OLCI_stats(hyp_S3B)
pro_S3A_stats <- process_OLCI_stats(pro_S3A)
pro_S3B_stats <- process_OLCI_stats(pro_S3B)
tri_S3A_stats <- process_OLCI_stats(tri_S3A)
tri_S3B_stats <- process_OLCI_stats(tri_S3B)

# Combine stats and save
all_S3_stats <- bind_rows(hyp_S3A_stats, hyp_S3B_stats,
                          pro_S3A_stats, pro_S3B_stats,
                          tri_S3A_stats, tri_S3B_stats) |> 
  dplyr::select(system, sat, wavelength, everything()) |> 
  mutate(system = case_when(system == "Hyp" ~ "HYPERNETS",
                            system == "HYPERPRO" ~ "HyperPRO",
                            system == "TRIOS" ~ "So-Rad", TRUE ~ system))
write_csv(all_S3_stats, "output/stats_S3_all.csv")

# Plot Bias and Error as barplots
S3_error_col <- all_S3_stats |> 
  filter(wavelength < 600) |> 
  mutate(wavelength = as.character(wavelength)) |> 
  ggplot(aes(x = wavelength, y = Error)) +
  geom_col(aes(fill = sat_version), position = "dodge") +
  facet_grid(sat~system) +
  theme_minimal() +
  labs(x = "Wave band [nm]", y = "Error [%]", fill = "Platform / Version",
       title = "Comparisons of matchups per wavelength",
       subtitle = "Bars show the final Error (%) for all matchups") +
  theme(panel.border = element_rect(fill = NA, colour = "black"))
ggsave("figures/test_OLCI_error_col.png", S3_error_col, width = 12, height = 8)
S3_bias_col <- all_S3_stats |> 
  filter(wavelength < 600) |> 
  mutate(wavelength = as.character(wavelength)) |> 
  ggplot(aes(x = wavelength, y = Bias)) +
  geom_col(aes(fill = sat_version), position = "dodge") +
  facet_grid(sat~system) +
  theme_minimal() +
  labs(x = "Wave band [nm]", y = "Bias [%]", fill = "Platform / Version",
       title = "Comparisons of matchups per wavelength",
       subtitle = "Bars show the final Bias (%) for all matchups") +
  theme(panel.border = element_rect(fill = NA, colour = "black"))
ggsave("figures/test_OLCI_bias_col.png", S3_bias_col, width = 12, height = 8)

# Create long versions and stack for boxplots
hyp_S3A_long <- pivot_longer(hyp_S3A, cols = `S3A v3.0`:Hyp) |> 
  mutate(sat = "S3A", is = "HYPERNETS")
hyp_S3B_long <- pivot_longer(hyp_S3B, cols = `S3B v3.0`:Hyp) |> 
  mutate(sat = "S3B", is = "HYPERNETS")
pro_S3A_long <- pivot_longer(pro_S3A, cols = `S3A v3.0`:HYPERPRO) |> 
  mutate(sat = "S3A", is = "HyperPRO")
pro_S3B_long <- pivot_longer(pro_S3B, cols = `S3B v3.0`:HYPERPRO) |> 
  mutate(sat = "S3B", is = "HyperPRO")
tri_S3A_long <- pivot_longer(tri_S3A, cols = `S3A v3.0`:TRIOS) |> 
  mutate(sat = "S3A", is = "So-Rad")
tri_S3B_long <- pivot_longer(tri_S3B, cols = `S3B v3.0`:TRIOS) |> 
  mutate(sat = "S3B", is = "So-Rad")
all_S3_long <- bind_rows(hyp_S3A_long, hyp_S3B_long,
                         pro_S3A_long, pro_S3B_long,
                         tri_S3A_long, tri_S3B_long) |> 
  mutate(name = case_when(name == "Hyp" ~ "HYPERNETS",
                          name == "HYPERPRO" ~ "HyperPRO",
                          name == "TRIOS" ~ "So-Rad", TRUE ~ name)) |> 
  mutate(name = factor(name, 
                       levels = c("HYPERNETS", "HyperPRO", "So-Rad", 
                                  "S3A hm", "S3A v3.0", "S3A v4.0",
                                  "S3B hm", "S3B v3.0", "S3B v4.0")))

# Plot boxplots
S3_box_plot <- all_S3_long |> 
  filter(wavelength < 600) |> 
  mutate(wavelength = as.character(wavelength)) |> 
  ggplot(aes(x = wavelength, y = value)) +
  geom_boxplot(aes(fill = name)) +
  scale_fill_manual(values = c("springgreen1", "springgreen2", "springgreen3",
                               "steelblue1", "steelblue2", "steelblue3",
                               "royalblue1", "royalblue2", "royalblue3")) +
  facet_grid(sat~is) +
  labs(x = "Wave band [nm]", y = "Rhow", fill = "System / Version",
      title = "Comparisons of matchups per wavelength",
      subtitle = "Values within boxplots show spread across all matchups") +
    theme_minimal() +
    theme(panel.border = element_rect(fill = NA, colour = "black"))
ggsave("figures/test_OLCI_box.png", S3_box_plot, width = 12, height = 8)

# Create semi-long versions and stack for scatterplot
hyp_S3A_semi <- pivot_longer(hyp_S3A, cols = `S3A v3.0`:`S3A hm`) |> 
  mutate(sat = "S3A", is = "HYPERNETS") |> dplyr::rename(is_val = Hyp)
hyp_S3B_semi <- pivot_longer(hyp_S3B, cols = `S3B v3.0`:`S3B hm`) |> 
  mutate(sat = "S3B", is = "HYPERNETS") |> dplyr::rename(is_val = Hyp)
pro_S3A_semi <- pivot_longer(pro_S3A, cols = `S3A v3.0`:`S3A hm`) |> 
  mutate(sat = "S3A", is = "HyperPRO") |> dplyr::rename(is_val = HYPERPRO)
pro_S3B_semi <- pivot_longer(pro_S3B, cols = `S3B v3.0`:`S3B hm`) |> 
  mutate(sat = "S3B", is = "HyperPRO") |> dplyr::rename(is_val = HYPERPRO)
tri_S3A_semi <- pivot_longer(tri_S3A, cols = `S3A v3.0`:`S3A hm`) |> 
  mutate(sat = "S3A", is = "So-Rad") |> dplyr::rename(is_val = TRIOS)
tri_S3B_semi <- pivot_longer(tri_S3B, cols = `S3B v3.0`:`S3B hm`) |> 
  mutate(sat = "S3B", is = "So-Rad") |> dplyr::rename(is_val = TRIOS)
all_S3_semi <- bind_rows(hyp_S3A_semi, hyp_S3B_semi,
                         pro_S3A_semi, pro_S3B_semi,
                         tri_S3A_semi, tri_S3B_semi) |> 
  mutate(name = case_when(name == "Hyp" ~ "HYPERNETS",
                          name == "HYPERPRO" ~ "HyperPRO",
                          name == "TRIOS" ~ "So-Rad", TRUE ~ name)) |> 
  mutate(name = factor(name, 
                       levels = c("HYPERNETS", "HyperPRO", "So-Rad", 
                                  "S3A hm", "S3A v3.0", "S3A v4.0",
                                  "S3B hm", "S3B v3.0", "S3B v4.0")))

# Plot scatterplots
S3_scatter_plot <- all_S3_semi |> 
  ggplot(aes(x = is_val, y = value)) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid") +
  geom_smooth(method = "lm", formula = y ~ x, linewidth = 1.5, linetype = "solid", se = FALSE, aes(colour = name)) +
  geom_point(aes(colour = name), size = 3) +
  facet_grid(sat~is) +
  labs(x = "Rhow [in-situ]", y = "Rhow [satellite]", colour = "Platform /\n Version",
      title = "Comparisons of matchups per wavelength",
      subtitle = "Coloured lines show slope per satellite version") +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA, colour = "black"))
ggsave("figures/test_OLCI_scatter.png", S3_scatter_plot, width = 12, height = 8)

