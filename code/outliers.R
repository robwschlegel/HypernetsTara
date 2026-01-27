# code/outliers.R
# Convenience functions to rapidly visualise outliers


# Setup -------------------------------------------------------------------

source("code/functions.R")


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


# Outlier hunting ---------------------------------------------------------

## ED ----------------------------------------------------------------------

# Load processed in situ matchups
matchup_ED_in_situ <- read_csv("output/matchup_stats_ED_in_situ.csv") |> 
  mutate(comp_sensors = paste0(sensor_X," vs ",sensor_Y)) |> 
  filter(comp_sensors %in% c("Hyp vs TRIOS", "Hyp vs HYPERPRO", "TRIOS vs HYPERPRO"))

# Load base W_nm matchup values
base_ED_in_situ <- plyr::ldply(list.files(c(file_path_build("ED", "Hypernets", "Trios"), 
                                            file_path_build("ED", "Hypernets", "Hyperpro"),
                                            file_path_build("ED", "Trios", "Hyperpro")),
                                          pattern = "*.csv", full.names = TRUE), 
                               load_matchup_long, .parallel = TRUE)

# Filter all by MAPE to get an initial idea of the issues
filter_ED_in_situ <- filter(matchup_ED_in_situ, MAPE >= 20) |> mutate(val_filter = "MAPE >= 20")

# Join for full range of stats
join_ED_in_situ <- left_join(base_ED_in_situ, matchup_ED_in_situ, by = join_by(file_name))
filter_join_ED_in_situ <- right_join(base_ED_in_situ, filter_ED_in_situ)

# Plot all wavelength matchups
plot_matchup_nm(join_ED_in_situ, "Ed", "Hyp", "TRIOS")
plot_matchup_nm(filter_join_ED_in_situ, "Ed", "Hyp", "TRIOS")
plot_matchup_nm(join_ED_in_situ, "Ed", "TRIOS", "HYPERPRO")
plot_matchup_nm(filter_join_ED_in_situ, "Ed", "TRIOS", "HYPERPRO")

# Plot matchups by date
plot_matchup_date(filter_join_ED_in_situ, "Ed", "Hyp", "TRIOS")


## LD ----------------------------------------------------------------------

# Load processed in situ matchups
matchup_LD_in_situ <- read_csv("output/matchup_stats_LD_Hyp_vs_Trios.csv") |> 
  mutate(comp_sensors = paste0(sensor_X," vs ",sensor_Y)) |> 
  filter(comp_sensors %in% c("Hyp vs TRIOS", "Hyp vs HYPERPRO", "TRIOS vs HYPERPRO"))

# Load base W_nm matchup values
base_LD_in_situ <- plyr::ldply(list.files(c(file_path_build("LD", "Hypernets", "Trios"), 
                                            file_path_build("LD", "Hypernets", "Hyperpro"),
                                            file_path_build("LD", "Trios", "Hyperpro")),
                                          pattern = "*.csv", full.names = TRUE), 
                               load_matchup_long, .parallel = TRUE)

# Filter all by MAPE to get an initial idea of the issues
filter_LD_in_situ <- filter(matchup_LD_in_situ, Bias <= -50) |> mutate(val_filter = "Bias <= -50")

# Join for full range of stats
join_LD_in_situ <- left_join(base_LD_in_situ, filter_LD_in_situ, by = join_by(file_name))
filter_join_LD_in_situ <- right_join(base_LD_in_situ, filter_LD_in_situ)

# Plot all wavelength matchups
plot_matchup_nm(join_LD_in_situ, "LD", "Hyp", "TRIOS")
plot_matchup_nm(filter_join_LD_in_situ, "LD", "Hyp", "TRIOS")

# Plot matchups by date
plot_matchup_date(filter_join_LD_in_situ, "LD", "Hyp", "TRIOS")


## LU ----------------------------------------------------------------------

# Load processed in situ matchups
matchup_LU_in_situ <- read_csv("output/matchup_stats_LU_in_situ.csv") |> 
  mutate(comp_sensors = paste0(sensor_X," vs ",sensor_Y)) |> 
  filter(comp_sensors %in% c("Hyp vs TRIOS", "Hyp vs HYPERPRO", "TRIOS vs HYPERPRO"))

# Load base W_nm matchup values
base_LU_in_situ <- plyr::ldply(list.files(c(file_path_build("LU", "Hypernets", "Trios"), 
                                            file_path_build("LU", "Hypernets", "Hyperpro"),
                                            file_path_build("LU", "Trios", "Hyperpro")),
                                          pattern = "*.csv", full.names = TRUE), 
                               load_matchup_long, .parallel = TRUE)

# Filter all by MAPE to get an initial idea of the issues
filter_LU_in_situ <- filter(matchup_LU_in_situ, MAPE >= 20) |> mutate(val_filter = "MAPE >= 20")

# Join for full range of stats
join_LU_in_situ <- left_join(base_LU_in_situ, matchup_LU_in_situ, by = join_by(file_name))
filter_join_LU_in_situ <- right_join(base_LU_in_situ, filter_LU_in_situ)

# Plot all wavelength matchups
plot_matchup_nm(join_LU_in_situ, "LU", "Hyp", "TRIOS")
plot_matchup_nm(filter_join_LU_in_situ, "LU", "Hyp", "TRIOS")

# Plot matchups by date
plot_matchup_date(filter_join_LU_in_situ, "LU", "Hyp", "TRIOS")


## LW ----------------------------------------------------------------------

# Load processed in situ matchups
matchup_LW_in_situ <- read_csv("output/matchup_stats_LW_in_situ.csv") |> 
  mutate(comp_sensors = paste0(sensor_X," vs ",sensor_Y)) |> 
  filter(comp_sensors %in% c("Hyp vs TRIOS", "Hyp vs HYPERPRO", "TRIOS vs HYPERPRO"))

# Load base W_nm matchup values
base_LW_in_situ <- plyr::ldply(list.files(c(file_path_build("LW", "Hypernets", "Trios"), 
                                            file_path_build("LW", "Hypernets", "Hyperpro"),
                                            file_path_build("LW", "Trios", "Hyperpro")),
                                          pattern = "*.csv", full.names = TRUE), 
                               load_matchup_long, .parallel = TRUE)

# Filter all by MAPE to get an initial idea of the issues
filter_LW_in_situ <- filter(matchup_LW_in_situ, Bias <= -50) |> mutate(val_filter = "Bias <= -50")

# Join for full range of stats
join_LW_in_situ <- left_join(base_LW_in_situ, matchup_LW_in_situ, by = join_by(file_name))
filter_join_LW_in_situ <- right_join(base_LW_in_situ, filter_LW_in_situ)
length(unique(join_LW_in_situ$file_name))
length(unique(filter_LW_in_situ$file_name))

# Plot all wavelength matchups
plot_matchup_nm(join_LW_in_situ, "LW", "Hyp", "TRIOS")
plot_matchup_nm(filter_join_LW_in_situ, "LW", "Hyp", "TRIOS")

# Plot matchups by date
plot_matchup_date(filter_join_LW_in_situ, "LW", "Hyp", "TRIOS")


## Rhow --------------------------------------------------------------------

# Load processed in situ matchups
matchup_RHOW_in_situ <- read_csv("output/matchup_stats_RHOW_in_situ.csv") |> 
  mutate(comp_sensors = paste0(sensor_X," vs ",sensor_Y)) |> 
  filter(comp_sensors %in% c("Hyp vs TRIOS", "Hyp vs HYPERPRO", "TRIOS vs HYPERPRO"))

# Load base W_nm matchup values
base_RHOW_in_situ <- plyr::ldply(list.files(c(file_path_build("RHOW", "Hypernets", "Trios"), 
                                              file_path_build("RHOW", "Hypernets", "Hyperpro"),
                                              file_path_build("RHOW", "Trios", "Hyperpro")),
                                            pattern = "*.csv", full.names = TRUE), 
                                 load_matchup_long, .parallel = TRUE)

# Filter all by MAPE to get an initial idea of the issues
filter_RHOW_in_situ <- filter(matchup_RHOW_in_situ, MAPE >= 50) |> mutate(val_filter = "MAPE >= 20")

# Join for full range of stats
join_RHOW_in_situ <- left_join(base_RHOW_in_situ, matchup_RHOW_in_situ, by = join_by(file_name))
filter_join_RHOW_in_situ <- right_join(base_RHOW_in_situ, filter_RHOW_in_situ)
length(unique(join_RHOW_in_situ$file_name))
length(unique(filter_RHOW_in_situ$file_name))

# Plot all wavelength matchups
plot_matchup_nm(join_RHOW_in_situ, "RHOW", "Hyp", "TRIOS")
plot_matchup_nm(filter_join_RHOW_in_situ, "RHOW", "Hyp", "TRIOS")

# Plot matchups by date
plot_matchup_date(filter_join_RHOW_in_situ, "RHOW", "Hyp", "TRIOS")


## Combine in situ outliers ------------------------------------------------

# Stack all filtered data.frames with file names that appear to be outliers
in_situ_outliers <- rbind(filter_ED_in_situ, filter_LD_in_situ, filter_LU_in_situ, 
                          filter_LW_in_situ, filter_RHOW_in_situ) |> 
  dplyr::select(file_name, var_name, sensor_X, sensor_Y, dist, diff_time,MAPE, Bias)
write_csv(in_situ_outliers, "meta/in_situ_outliers.csv")

# Scatterplots of difftime and distance for MAPE and Bias
plot_matchup_scatter(matchup_ED_in_situ, "ED")
plot_matchup_scatter(matchup_LD_in_situ, "LD")
plot_matchup_scatter(matchup_LU_in_situ, "LU")
plot_matchup_scatter(matchup_LW_in_situ, "LW")
plot_matchup_scatter(matchup_RHOW_in_situ, "RHOW")


## VIIRS_J1 ---------------------------------------------------------------

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


## S3A errors --------------------------------------------------------------

# Load matchup
S3A_match <- read_delim("~/Downloads/TRIOS_vs_HYPERPRO_vs_S3A_vs_20240814T082814_RHOW.csv", delim = ";", col_types = "cccc")
colnames(S3A_match)[1] <- "sensor"

# Get coordinates
# ncdump::NetCDF("~/Downloads/S3A_OL_2_WFR____20240814T093459_20240814T093759_20240815T155154_0179_115_364_2340_MAR_O_NT_003.SEN3/geo_coordinates.nc")
S3A_coords <- tidync::tidync("~/Downloads/S3A_OL_2_WFR____20240814T093459_20240814T093759_20240815T155154_0179_115_364_2340_MAR_O_NT_003.SEN3/geo_coordinates.nc") |> 
  tidync::hyper_tibble()

# Load one netcdf
# ncdump::NetCDF("~/Downloads/S3A_OL_2_WFR____20240814T093459_20240814T093759_20240815T155154_0179_115_364_2340_MAR_O_NT_003.SEN3/Oa01_reflectance.nc")
# info_var <- ncdump::NetCDF("~/Downloads/S3A_OL_2_WFR____20240814T093459_20240814T093759_20240815T155154_0179_115_364_2340_MAR_O_NT_003.SEN3/Oa01_reflectance.nc")$variable
S3A_band_1 <- tidync::tidync("~/Downloads/S3A_OL_2_WFR____20240814T093459_20240814T093759_20240815T155154_0179_115_364_2340_MAR_O_NT_003.SEN3/Oa01_reflectance.nc") |> 
  tidync::hyper_tibble() |> left_join(S3A_coords, by = join_by(columns, rows))
S3A_band_2 <- tidync::tidync("~/Downloads/S3A_OL_2_WFR____20240814T093459_20240814T093759_20240815T155154_0179_115_364_2340_MAR_O_NT_003.SEN3/Oa02_reflectance.nc") |> 
  tidync::hyper_tibble() |> left_join(S3A_coords, by = join_by(columns, rows))
S3A_band_3 <- tidync::tidync("~/Downloads/S3A_OL_2_WFR____20240814T093459_20240814T093759_20240815T155154_0179_115_364_2340_MAR_O_NT_003.SEN3/Oa02_reflectance.nc") |> 
  tidync::hyper_tibble() |> left_join(S3A_coords, by = join_by(columns, rows))

# Get nearest pixels
target_lat <- S3A_match[8,]$latitude; target_lon <- S3A_match[8,]$longitude
S3A_band_1_5 <- get_nearest_pixels(S3A_band_1, target_lat, target_lon, 5)
mean(S3A_band_1_5$Oa01_reflectance, na.rm = TRUE)
S3A_band_2_5 <- get_nearest_pixels(S3A_band_2, target_lat, target_lon, 5)
mean(S3A_band_2_5$Oa02_reflectance, na.rm = TRUE)
S3A_band_2_5 <- get_nearest_pixels(S3A_band_2, target_lat, target_lon, 5)
mean(S3A_band_2_5$Oa02_reflectance, na.rm = TRUE)

# Map each band
S3A_plot <- S3A_band_1 |> 
  mutate(lon_coarse = round(longitude, 2),
         lat_coarse = round(latitude, 2)) |> 
  summarise(reflectance = mean(Oa01_reflectance, na.RM = TRUE), .by = c("lon_coarse", "lat_coarse")) |> 
  ggplot() +
  annotation_borders(fill = "grey80") +
  # geom_point(aes(x = longitude, y = latitude, colour = Oa01_reflectance), size = 5) +
  geom_tile(aes(x = lon_coarse, y = lat_coarse, fill = reflectance)) +
  geom_point(data = S3A_match[8,], aes(x = longitude, y = latitude), colour = "red") +
  scale_fill_viridis_c() +
  labs(x = "Longitude (°E)", y = "Latitude (°N)", 
       fill = "S3A band 1 (Rrs)") +
  coord_quickmap(xlim = c(7, 25), ylim = c(35, 42)) +
  theme(legend.position = "bottom")
  # coord_quickmap(xlim = c(S3A_match[8,]$longitude-0.01, S3A_match[8,]$longitude+0.01), 
                 # ylim = c(S3A_match[8,]$latitude-0.01, S3A_match[8,]$latitude+0.01))
ggsave("figures/S3A_error.png", S3A_plot, height = 6, width = 9)

