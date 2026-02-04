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

# Join for full range of stats
join_ED_in_situ <- left_join(base_ED_in_situ, matchup_ED_in_situ, by = join_by(file_name))

# Plot matchup by MAPE + Bias
plot_matchup_MAPE_Bias(join_ED_in_situ, "Ed", "Hyp", "TRIOS") # MAPE > 20
plot_matchup_MAPE_Bias(join_ED_in_situ, "Ed", "HYPERPRO", "Hyp") # OK
plot_matchup_MAPE_Bias(join_ED_in_situ, "Ed", "HYPERPRO", "TRIOS") # OK

# Filter all by MAPE or Bias to get an initial idea of the issues
filter_ED_in_situ <- filter(matchup_ED_in_situ, MAPE >= 20) |> mutate(val_filter = "MAPE >= 20")
filter_join_ED_in_situ <- right_join(base_ED_in_situ, filter_ED_in_situ, by = join_by(file_name))
clean_join_ED_in_situ <- anti_join(base_ED_in_situ, filter_ED_in_situ, by = join_by(file_name))

# Plot all wavelength matchups
plot_matchup_nm(join_ED_in_situ, "Ed", "Hyp", "TRIOS")
plot_matchup_nm(filter_join_ED_in_situ, "Ed", "Hyp", "TRIOS")
plot_matchup_nm(clean_join_ED_in_situ, "Ed", "Hyp", "TRIOS")

# Plot matchups by date
plot_matchup_date(filter_join_ED_in_situ, "Ed", "Hyp", "TRIOS")


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
join_LD_in_situ <- left_join(base_LD_in_situ, matchup_LD_in_situ, by = join_by(file_name))

# Plot matchup by MAPE + Bias
# NB: Only HypStar vs Trios
plot_matchup_MAPE_Bias(join_LD_in_situ, "Ld", "Hyp", "TRIOS") # Bias < -50

# Filter all by MAPE to get an initial idea of the issues
filter_LD_in_situ <- filter(matchup_LD_in_situ, Bias <= -50) |> mutate(val_filter = "Bias <= -50")
filter_join_LD_in_situ <- right_join(base_LD_in_situ, filter_LD_in_situ, by = join_by(file_name))
clean_join_LD_in_situ <- anti_join(base_LD_in_situ, filter_LD_in_situ, by = join_by(file_name))

# Plot all wavelength matchups
plot_matchup_nm(join_LD_in_situ, "LD", "Hyp", "TRIOS")
plot_matchup_nm(filter_join_LD_in_situ, "LD", "Hyp", "TRIOS")
plot_matchup_nm(clean_join_LD_in_situ, "LD", "Hyp", "TRIOS")

# Plot matchups by date
plot_matchup_date(filter_join_LD_in_situ, "LD", "Hyp", "TRIOS")


## LU ----------------------------------------------------------------------

# Load processed in situ matchups
matchup_LU_in_situ <- read_csv("output/matchup_stats_LU_in_situ.csv") |> 
  mutate(comp_sensors = paste0(sensor_X," vs ",sensor_Y)) |> 
  filter(comp_sensors %in% c("Hyp vs TRIOS", "HYPERPRO vs Hyp", "HYPERPRO vs TRIOS"))

# Load base W_nm matchup values
base_LU_in_situ <- plyr::ldply(list.files(c(file_path_build("LU", "Hypernets", "Trios"), 
                                            file_path_build("LU", "Hypernets", "Hyperpro"),
                                            file_path_build("LU", "Trios", "Hyperpro")),
                                          pattern = "*.csv", full.names = TRUE), 
                               load_matchup_long, .parallel = TRUE)

# Join for full range of stats
join_LU_in_situ <- left_join(base_LU_in_situ, matchup_LU_in_situ, by = join_by(file_name))

# Plot matchup by MAPE + Bias
# NB: Only HypStar vs Trios
plot_matchup_MAPE_Bias(join_LU_in_situ, "Lu", "Hyp", "TRIOS") # Bias < -50

# Filter all by MAPE to get an initial idea of the issues
filter_LU_in_situ <- filter(matchup_LU_in_situ, Bias <= -50) |> mutate(val_filter = "Bias <= -50")
filter_join_LU_in_situ <- right_join(base_LU_in_situ, filter_LU_in_situ)
clean_join_LU_in_situ <- anti_join(base_LU_in_situ, filter_LU_in_situ)

# Plot all wavelength matchups
plot_matchup_nm(join_LU_in_situ, "LU", "Hyp", "TRIOS")
plot_matchup_nm(filter_join_LU_in_situ, "LU", "Hyp", "TRIOS")
plot_matchup_nm(clean_join_LU_in_situ, "LU", "Hyp", "TRIOS")
ggsave("figures/global_scatter_LU_hyp_trios.png", height = 7, width = 6)

# Plot matchups by date
plot_matchup_date(filter_join_LU_in_situ, "LU", "Hyp", "TRIOS")

# Plot HyperPRO wavelength matchups for interest
plot_matchup_nm(join_LU_in_situ, "LU", "HYPERPRO", "Hyp")
plot_matchup_nm(clean_join_LU_in_situ, "LU", "HYPERPRO", "Hyp")
ggsave("figures/global_scatter_LU_hyperpro_hypstar.png", height = 7, width = 6)
plot_matchup_nm(join_LU_in_situ, "LU", "HYPERPRO", "TRIOS")
plot_matchup_nm(clean_join_LU_in_situ, "LU", "HYPERPRO", "TRIOS")
ggsave("figures/global_scatter_LU_hyperpro_trios.png", height = 7, width = 6)


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

# List of files with negative HypStar values
neg_LW_hypstar <- base_LW_in_situ |> 
  filter(Hyp < 0) |> 
  distinct(file_name) |> 
  left_join(matchup_LW_in_situ) |> 
  mutate(val_filter = "Negative HypStar values")

# Join for full range of stats
join_LW_in_situ <- left_join(base_LW_in_situ, matchup_LW_in_situ, by = join_by(file_name)) |> 
  anti_join(neg_LW_hypstar, by = join_by(file_name))

# Plot matchup by MAPE + Bias
plot_matchup_MAPE_Bias(join_LW_in_situ, "Lw", "Hyp", "TRIOS") # Bias < -100
plot_matchup_MAPE_Bias(join_LW_in_situ, "Lw", "HYPERPRO", "Hyp") # Bias > 50
plot_matchup_MAPE_Bias(join_LW_in_situ, "Lw", "HYPERPRO", "TRIOS") # OK

# Filter all by MAPE to get an initial idea of the issues
filter_LW_in_situ <- filter(matchup_LW_in_situ, Bias <= -100 | Bias >= 50) |> mutate(val_filter = "Bias <= -100 | Bias >= 50")
filter_join_LW_in_situ <- right_join(base_LW_in_situ, filter_LW_in_situ, by = join_by(file_name))
clean_join_LW_in_situ <- anti_join(base_LW_in_situ, filter_LW_in_situ, by = join_by(file_name))

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
plot_matchup_date(filter_join_LW_in_situ, "LW", "Hyp", "TRIOS")
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

# List of files with negative HypStar values
neg_RHOW_hypstar <- base_RHOW_in_situ |> 
  filter(Hyp < 0) |> 
  distinct(file_name) |> 
  left_join(matchup_RHOW_in_situ) |> 
  mutate(val_filter = "Negative HypStar values")

# Join for full range of stats
join_RHOW_in_situ <- left_join(base_RHOW_in_situ, matchup_RHOW_in_situ, by = join_by(file_name)) |> 
  anti_join(neg_RHOW_hypstar, by = join_by(file_name))

# Plot matchup by MAPE + Bias
plot_matchup_MAPE_Bias(join_RHOW_in_situ, "RHOW", "Hyp", "TRIOS") # MAPE > 50
plot_matchup_MAPE_Bias(join_RHOW_in_situ, "RHOW", "HYPERPRO", "Hyp") # MAPE > 50
plot_matchup_MAPE_Bias(join_RHOW_in_situ, "RHOW", "HYPERPRO", "TRIOS") # MAPE > 50

# Filter all by MAPE to get an initial idea of the issues
filter_RHOW_in_situ <- filter(matchup_RHOW_in_situ, MAPE >= 50) |> mutate(val_filter = "MAPE >= 50")
filter_join_RHOW_in_situ <- right_join(base_RHOW_in_situ, filter_RHOW_in_situ)
clean_join_RHOW_in_situ <- anti_join(base_RHOW_in_situ, filter_RHOW_in_situ)

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
  dplyr::select(file_name, var_name, sensor_X, sensor_Y, dist, diff_time,MAPE, Bias)
write_csv(in_situ_outliers, "meta/in_situ_outliers.csv")

# Scatterplots of difftime and distance for MAPE and Bias
plot_matchup_scatter(matchup_ED_in_situ, "ED")
plot_matchup_scatter(matchup_LD_in_situ, "LD")
plot_matchup_scatter(matchup_LU_in_situ, "LU")
plot_matchup_scatter(matchup_LW_in_situ, "LW")
plot_matchup_scatter(matchup_RHOW_in_situ, "RHOW")


# Satellite Outliers -------------------------------------------------------

## OLCI --------------------------------------------------------------------

# Load processed in situ matchups
matchup_OLCI <- read_csv("output/matchup_stats_RHOW_OLCI.csv") |> 
  filter(sensor_X %in% c("Hyp", "HYPERPRO", "TRIOS")) |> 
  mutate(comp_sensors = paste0(sensor_X," vs ",sensor_Y))

# OLCI files
file_list_OLCI <- list.files(dir("~/pCloudDrive/Documents/OMTAB/HYPERNETS/tara_matchups_results_20260203", 
                                 pattern = "S3", full.names = TRUE), pattern = "*.csv", full.names = TRUE)

# Load base W_nm matchup values
base_OLCI <- plyr::ldply(file_list_OLCI, load_matchup_long, .parallel = TRUE)

# Join for full range of stats
join_OLCI <- left_join(base_OLCI, matchup_OLCI, by = join_by(file_name))

# Check satellite variance in files
sat_var_OLCI <- plyr::ldply(file_list_OLCI, sat_var_check, .parallel = TRUE)

# Plot matchup by MAPE + Bias
# NB: There are more S3A matchups, so using that sensor for analysis
plot_matchup_MAPE_Bias(join_OLCI, "Rhow", "Hyp", "S3A") # The MAPE > 200 does not appear to be an outlier
plot_matchup_MAPE_Bias(join_OLCI, "Rhow", "TRIOS", "S3A") # MAPE > 300
plot_matchup_MAPE_Bias(join_OLCI, "Rhow", "HYPERPRO", "S3A") # OK

# Filter all by MAPE or Bias to get an initial idea of the issues
filter_OLCI <- filter(matchup_OLCI, MAPE >= 300) |> mutate(val_filter = "MAPE >= 300") |> 
  filter(file_name != "TRIOS_vs_S3B_vs_20240813T101805_RHOW.csv") # Manually checked, not an outlier, just a poor matchup
filter_join_OLCI <- right_join(base_OLCI, filter_OLCI, by = join_by(file_name))
clean_join_OLCI <- anti_join(base_OLCI, filter_OLCI, by = join_by(file_name))

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


## VIIRS -------------------------------------------------------------------

# Load processed in situ matchups
matchup_VIIRS <- read_csv("output/matchup_stats_RHOW_VIIRS.csv") |> 
  filter(sensor_X %in% c("Hyp", "HYPERPRO", "TRIOS")) |> 
  mutate(comp_sensors = paste0(sensor_X," vs ",sensor_Y))

# File list
file_list_VIIRS <- list.files(dir("~/pCloudDrive/Documents/OMTAB/HYPERNETS/tara_matchups_results_20260203", 
                                  pattern = "VIIRS", full.names = TRUE), pattern = "*.csv", full.names = TRUE)

# Load base W_nm matchup values
base_VIIRS <- plyr::ldply(file_list_VIIRS, load_matchup_long, .parallel = TRUE)

# Join for full range of stats
join_VIIRS <- left_join(base_VIIRS, matchup_VIIRS, by = join_by(file_name))

# Check satellite variance in files
sat_var_VIIRS <- plyr::ldply(file_list_VIIRS, sat_var_check, .parallel = TRUE)

# Plot matchup by MAPE + Bias
# NB: VIIRS_N is visually the least similar, so using this for base reference
plot_matchup_MAPE_Bias(join_VIIRS, "Rhow", "Hyp", "VIIRS_N") # MAPE > 100
plot_matchup_MAPE_Bias(join_VIIRS, "Rhow", "TRIOS", "VIIRS_N") # MAPE > 100
plot_matchup_MAPE_Bias(join_VIIRS, "Rhow", "HYPERPRO", "VIIRS_N") # MAPE > 100

# Filter all by MAPE or Bias to get an initial idea of the issues
filter_VIIRS <- filter(matchup_VIIRS, MAPE >= 100) |> mutate(val_filter = "MAPE >= 100") |>
  right_join(sat_var_VIIRS, by = join_by(file_name)) |> 
  filter(!is.na(var_name))
filter_join_VIIRS <- right_join(base_VIIRS, filter_VIIRS, by = join_by(file_name))
clean_join_VIIRS <- anti_join(base_VIIRS, filter_VIIRS, by = join_by(file_name))

# Plot matchups by date
plot_matchup_date(filter_join_VIIRS, "Rhow", "Hyp", "VIIRS_N") # OK
plot_matchup_date(filter_join_VIIRS, "Rhow", "TRIOS", "VIIRS_N")
plot_matchup_date(filter_join_VIIRS, "Rhow", "HYPERPRO", "VIIRS_N")

# Plot all wavelength matchups
plot_matchup_nm(join_VIIRS, "Rhow", "Hyp", "VIIRS_N")
plot_matchup_nm(filter_join_VIIRS, "Rhow", "Hyp", "VIIRS_N") # OK
plot_matchup_nm(clean_join_VIIRS, "Rhow", "Hyp", "VIIRS_N")
plot_matchup_nm(join_VIIRS, "Rhow", "TRIOS", "VIIRS_N")
plot_matchup_nm(filter_join_VIIRS, "Rhow", "TRIOS", "VIIRS_N")
plot_matchup_nm(clean_join_VIIRS, "Rhow", "TRIOS", "VIIRS_N")
plot_matchup_nm(join_VIIRS, "Rhow", "HYPERPRO", "VIIRS_N")
plot_matchup_nm(filter_join_VIIRS, "Rhow", "HYPERPRO", "VIIRS_N")
plot_matchup_nm(clean_join_VIIRS, "Rhow", "HYPERPRO", "VIIRS_N")


## MODIS -------------------------------------------------------------------

# Load processed in situ matchups
matchup_MODIS <- read_csv("output/matchup_stats_RHOW_MODIS.csv") |> 
  filter(sensor_X %in% c("Hyp", "HYPERPRO", "TRIOS")) |> 
  mutate(comp_sensors = paste0(sensor_X," vs ",sensor_Y))

# File list
file_list_MODIS <- list.files(dir("~/pCloudDrive/Documents/OMTAB/HYPERNETS/tara_matchups_results_20260203", 
                                  pattern = "AQUA", full.names = TRUE), pattern = "*.csv", full.names = TRUE)

# Load base W_nm matchup values
base_MODIS <- plyr::ldply(file_list_MODIS, load_matchup_long, .parallel = TRUE)

# Join for full range of stats
join_MODIS <- left_join(base_MODIS, matchup_MODIS, by = join_by(file_name))

# Check satellite variance in files
sat_var_MODIS <- plyr::ldply(file_list_MODIS, sat_var_check, .parallel = TRUE)

# Plot matchup by MAPE + Bias
plot_matchup_MAPE_Bias(join_MODIS, "Rhow", "Hyp", "AQUA") # MAPE > 200
plot_matchup_MAPE_Bias(join_MODIS, "Rhow", "TRIOS", "AQUA") # MAPE > 300
plot_matchup_MAPE_Bias(join_MODIS, "Rhow", "HYPERPRO", "AQUA") # MAPE > 150

# Filter all by MAPE or Bias to get an initial idea of the issues
# No need to filter by satellite variance, there is one bad AQUA matchup across all sensors and datetimes
filter_MODIS <- filter(matchup_MODIS, MAPE >= 150) |> mutate(val_filter = "MAPE >= 150")
filter_join_MODIS <- right_join(base_MODIS, filter_MODIS, by = join_by(file_name))
clean_join_MODIS <- anti_join(base_MODIS, filter_MODIS, by = join_by(file_name))

# Plot matchups by date
plot_matchup_date(filter_join_MODIS, "Rhow", "Hyp", "AQUA")
plot_matchup_date(filter_join_MODIS, "Rhow", "TRIOS", "AQUA")
plot_matchup_date(filter_join_MODIS, "Rhow", "HYPERPRO", "AQUA")

# Plot all wavelength matchups
plot_matchup_nm(join_MODIS, "Rhow", "Hyp", "AQUA")
plot_matchup_nm(filter_join_MODIS, "Rhow", "Hyp", "AQUA")
plot_matchup_nm(clean_join_MODIS, "Rhow", "Hyp", "AQUA")
plot_matchup_nm(join_MODIS, "Rhow", "TRIOS", "AQUA")
plot_matchup_nm(filter_join_MODIS, "Rhow", "TRIOS", "AQUA")
plot_matchup_nm(clean_join_MODIS, "Rhow", "TRIOS", "AQUA")
plot_matchup_nm(join_MODIS, "Rhow", "HYPERPRO", "AQUA")
plot_matchup_nm(filter_join_MODIS, "Rhow", "HYPERPRO", "AQUA")
plot_matchup_nm(clean_join_MODIS, "Rhow", "HYPERPRO", "AQUA")


## OCI ---------------------------------------------------------------------

# Load processed in situ matchups
matchup_OCI <- read_csv("output/matchup_stats_RHOW_OCI.csv") |> 
  filter(sensor_X %in% c("Hyp", "HYPERPRO", "TRIOS")) |> 
  mutate(comp_sensors = paste0(sensor_X," vs ",sensor_Y))

# File list
file_list_OCI <- list.files(dir("~/pCloudDrive/Documents/OMTAB/HYPERNETS/tara_matchups_results_20260203", 
                                  pattern = "PACE", full.names = TRUE), pattern = "*.csv", full.names = TRUE)
file_list_OCI <- file_list_OCI[!grepl("RHOW_PACE_V2_vs_PACE_V30_vs_PACE_V31", file_list_OCI)] # Remove self-comparisons

# Load base W_nm matchup values
base_OCI <- plyr::ldply(file_list_OCI, load_matchup_long, .parallel = TRUE)

# Join for full range of stats
join_OCI <- left_join(base_OCI, matchup_OCI, by = join_by(file_name))

# Check satellite variance in files
sat_var_OCI <- plyr::ldply(file_list_OCI, sat_var_check, .parallel = TRUE)

# Plot matchup by MAPE + Bias
# NB: PACE_v30 is visually the least similar, so using this for base reference
# NB: There are many PACE files with negative values
plot_matchup_MAPE_Bias(join_OCI, "Rhow", "Hyp", "PACE_V30") # MAPE > 100
plot_matchup_MAPE_Bias(join_OCI, "Rhow", "TRIOS", "PACE_V30") # MAPE > 100
plot_matchup_MAPE_Bias(join_OCI, "Rhow", "HYPERPRO", "PACE_V30") # MAPE > OK

# Filter all by MAPE or Bias to get an initial idea of the issues
filter_OCI <- filter(matchup_OCI, MAPE >= 100) |> mutate(val_filter = "MAPE >= 100") |>
  right_join(sat_var_OCI, by = join_by(file_name)) |> 
  filter(!is.na(var_name))
filter_join_OCI <- right_join(base_OCI, filter_OCI, by = join_by(file_name))
clean_join_OCI <- anti_join(base_OCI, filter_OCI, by = join_by(file_name))

# Plot matchups by date
plot_matchup_date(filter_join_OCI, "Rhow", "Hyp", "PACE_V30")
plot_matchup_date(filter_join_OCI, "Rhow", "TRIOS", "PACE_V30")
plot_matchup_date(filter_join_OCI, "Rhow", "HYPERPRO", "PACE_V30") # OK

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
  dplyr::select(file_name, var_name, sensor_X, sensor_Y, dist, diff_time,MAPE, Bias)
write_csv(satellite_outliers, "meta/satellite_outliers.csv")


# S3A errors --------------------------------------------------------------

# This section of code investigates a specific S3A error to demonstrate a visual inspection of the data

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

