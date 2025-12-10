# code/map.R
# Code used to create the map of the Tara voyage, sampling sites, and a PACE overhead pass


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ncdf4)
library(patchwork)


# Functions ---------------------------------------------------------------

map_PACE <- function(df){
  df |> 
    filter(!is.na(Rrs)) |> 
    ggplot() +
    borders(fill = "grey80") +
    geom_tile(aes(x = longitude, y = latitude, fill = Rrs)) +
    scale_colour_viridis_c(aesthetics = c("colour", "fill")) +
    labs(x = NULL, y = NULL) +
    coord_quickmap(xlim = c(min(df$longitude), max(df$longitude)),
                   ylim = c(min(df$latitude), max(df$latitude)))
}

# Map of Tara mission -----------------------------------------------------

# Load HyperPRO sampling stations
station_HP <- read_csv("meta/all_in-situ_dm_10_RHOW_stations.csv") |> 
  filter(sensor != "Hyp_nosc") |> 
  mutate(date = parse_date(as.character(day), format = "%Y%m%d"))

# Get count of instruments per sample
## Doesn't work great...
station_count <- station_HP |> 
  mutate(longitude = round(longitude, 2),
         latitude = round(latitude, 2)) |> 
  summarise(count_n = n(), .by = c("longitude", "latitude"))

# Load one slice of PACE v3.1 data at 413 nm
## Visualise all five days to pick the best coverage
# map_PACE(read_csv("data/PACE_OCI.20240809T105059.L2.OC_AOP.V3_1_rrs_413.csv"))
# map_PACE(read_csv("data/PACE_OCI.20240812T105611.L2.OC_AOP.V3_1_rrs_413.csv"))
# map_PACE(read_csv("data/PACE_OCI.20240813T113041.L2.OC_AOP.V3_1_rrs_413.csv")) # Winner
# map_PACE(read_csv("data/PACE_OCI.20240814T120512.L2.OC_AOP.V3_1_rrs_413.csv"))
# map_PACE(read_csv("data/PACE_OCI.20240815T110624.L2.OC_AOP.V3_1_rrs_413.csv"))
PACE_swath <- read_csv("data/PACE_OCI.20240813T113041.L2.OC_AOP.V3_1_rrs_413.csv") |> 
  filter(!is.na(Rrs))

# Map
pl_map <- ggplot(data = station_HP) +
  borders(fill = "grey80") +
  geom_tile(data = PACE_swath, aes(x = longitude, y = latitude, fill = Rrs)) +
  geom_point(aes(x = longitude, y = latitude), colour = "black", size = 5.5) +
  geom_point(aes(x = longitude, y = latitude, colour = date), size = 5) +
  geom_point(data = filter(station_HP, sensor == "HYPERPRO"),
             aes(x = longitude, y = latitude), colour = "maroon", size = 7, shape = 5, stroke = 2) +
  # geom_point(data = filter(station_HP, sensor == "HYPERPRO"),
  #            aes(x = longitude, y = latitude, colour = date), size = 6, shape = 22) +
  scale_colour_date(guide = "legend", low = "pink", high = "purple") +
  # scale_colour_viridis_c(option = "A", guide = "legend") +
  scale_fill_viridis_c() +
  labs(x = "Longitude (°E)", y = "Latitude (°N)", 
       colour = "Sampling date", fill = "Water Leaving Reflectance (Rhow; 413 nm)") +
  coord_quickmap(xlim = c(7, 25), ylim = c(35, 42)) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "top")
ggsave("figures/fig_1.png", pl_map, height = 9, width = 16)


# PACE --------------------------------------------------------------------

# Load the full spectra for one point
v_all_spectra <- read_delim("data/csv_pour_générer_spectres_pace_V20_V30_V31/PACE_20240809T0900.csv", delim = ";")
colnames(v_all_spectra)[1] <- "version"
v_all_spectra_long <- v_all_spectra |> 
  pivot_longer(`356`:`718`, names_to = "nm") |> 
  mutate(nm = as.integer(nm))

# Load data extracted via Python script
v2_413 <- read_csv("data/PACE_OCI.20240809T105059.L2.OC_AOP.V2_0.NRT_rrs_413.csv") |> 
  filter(!is.na(Rrs)) |> 
  mutate(longitude = plyr::round_any(longitude, 0.02),
         latitude = plyr::round_any(latitude, 0.02)) |>
  summarise(Rrs = mean(Rrs, na.rm = TRUE), .by = c("longitude", "latitude")) |> 
  dplyr::rename(v2_Rrs = Rrs)
v3_413 <- read_csv("data/PACE_OCI.20240809T105059.L2.OC_AOP.V3_0_rrs_413.csv") |> 
  filter(!is.na(Rrs)) |> 
  mutate(longitude = plyr::round_any(longitude, 0.02),
         latitude = plyr::round_any(latitude, 0.02)) |>
  summarise(Rrs = mean(Rrs, na.rm = TRUE), .by = c("longitude", "latitude")) |> 
  dplyr::rename(v3_Rrs = Rrs)
v31_413 <- read_csv("data/PACE_OCI.20240809T105059.L2.OC_AOP.V3_1_rrs_413.csv") |> 
  filter(!is.na(Rrs)) |> 
  mutate(longitude = plyr::round_any(longitude, 0.02),
         latitude = plyr::round_any(latitude, 0.02)) |>
  summarise(Rrs = mean(Rrs, na.rm = TRUE), .by = c("longitude", "latitude")) |> 
  dplyr::rename(v31_Rrs = Rrs)

# Combine and compare
vall_413 <- left_join(v2_413, v3_413, by = join_by(latitude, longitude)) |> 
  left_join(v31_413, by = join_by(latitude, longitude)) |> 
  mutate(v2_v3 = (v2_Rrs / v3_Rrs)*100,
         v2_v31 = (v2_Rrs / v31_Rrs)*100,
         v3_v31 = (v3_Rrs / v31_Rrs)*100) |> 
  # mutate(v2_v3 = ifelse(v2_v3 > 2, 2, v2_v3),
  #        v2_v31 = ifelse(v2_v31 > 2, 2, v2_v3),
  #        v3_v31 = ifelse(v3_v31 > 2, 2, v2_v3),
  #        v2_v3 = ifelse(v2_v3 < -2, -2, v2_v3),
  #        v2_v31 = ifelse(v2_v31 < -2, -2, v2_v3),
  #        v3_v31 = ifelse(v3_v31 < -2, -2, v2_v3)) |>
  # mutate(v2_v3 = cut(v2_v3, seq(-200, 200, 50)),
  #        v2_v31 = cut(v2_v31, seq(-200, 200, 50)),
  #        v3_v31 = cut(v3_v31, seq(-200, 200, 50)))
  mutate(v2_v3 = cut(v2_v3, c(-200, -100, 0, 50, 80, 90, 100, 110, 120, 150, 200)),
         v2_v31 = cut(v2_v31, c(-200, -100, 0, 50, 80, 90, 100, 110, 120, 150, 200)),
         v3_v31 = cut(v3_v31, c(-200, -100, 0, 50, 80, 90, 100, 110, 120, 150, 200)))

# Pivot longer for plotting
v_comp_long <- vall_413 |> 
  dplyr::select(longitude, latitude, v2_v3:v3_v31) |> 
  pivot_longer(v2_v3:v3_v31, names_to = "ver") |> 
  mutate(ver = factor(ver, levels = c("v2_v3", "v2_v31", "v3_v31"),
                      labels = c("v2 / v3", "v2 / v3.1", "v3 / v3.1"))) |> 
  filter(!is.na(value))

# Plot map differences
pl_top <- ggplot(data = v_comp_long) +
  # borders(fill = "grey80") +
  geom_tile(aes(x = longitude, y = latitude, fill = value)) +
  annotate(geom = "point", x = v_all_spectra$longitude[1], y = v_all_spectra$latitude[1]) +
  # geom_contour(aes(x = longitude, y = latitude, z = value),
  #              breaks = c(1.0), color = "black") +
  # scale_colour_viridis_c(aesthetics = c("colour", "fill")) +
  scale_fill_viridis_d() +
  labs(x = NULL, y = NULL, fill = "Difference (%)") +
  facet_wrap(~ver) +
  coord_quickmap(xlim = c(min(v_comp_long$longitude), max(v_comp_long$longitude)),
                 ylim = c(min(v_comp_long$latitude), max(v_comp_long$latitude))) +
  theme(panel.background = element_rect(colour = "black", fill = "grey90"),
        legend.position = "top")

# Plot percent difference as non-map
pl_left <- v_comp_long |> 
  summarise(cut_n = n(), .by = c("ver", "value")) |> 
  # complete(ver, value)
  ggplot() +
  geom_col(aes(x = value, y = cut_n, fill = ver), 
           position = "dodge", colour = "black") +
  scale_y_continuous(expand = c(0, 2000), 
                     breaks = c(0, 50000, 100000, 150000, 200000, 250000),
                     labels = c("0", "50K", "100K", "150K", "200K", "250K")) +
  scale_fill_brewer(palette = "Accent") +
  # scale_fill_viridis_d(option = "A") + # yuck
  labs(x = "Difference (%)", y = "Pixel count (n)", fill = "Comparison") +
  theme(panel.background = element_rect(colour = "black", fill = "grey90"),
        legend.position = "bottom")

# Plot spectra differences
pl_right <- ggplot(v_all_spectra_long) +
  geom_path(aes(x = nm, y = value, colour = version), 
            linewidth = 2, alpha = 0.8) +
  geom_vline(xintercept = 413) +
  labs(x = "Wavelength (nm)", y = "Water Leaving Reflectance (Rhow)") +
  scale_color_brewer(palette = "YlGnBu") +
  # scale_colour_viridis_d(option = "B") + # yuck
  scale_y_continuous(expand = c(-0.1, 0.005)) +
  theme(panel.background = element_rect(colour = "black", fill = "grey90"),
        legend.position = "bottom")

# Put it all together
pl_all <- pl_top / (pl_left + pl_right)
ggsave("figures/fig_S1.png", pl_all, height = 9, width = 14)

