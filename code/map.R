# code/map.R
# Code used to create the map of the Tara voyage, sampling sites, and a PACE overhead pass


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ncdf4)



# Functions ---------------------------------------------------------------

map_PACE <- function(df){
  ggplot(data = df) +
    borders(fill = "grey80") +
    geom_tile(aes(x = longitude, y = latitude, fill = Rrs)) +
    scale_colour_viridis_c(aesthetics = c("colour", "fill")) +
    labs(x = NULL, y = NULL) +
    coord_quickmap(xlim = c(min(df$longitude), max(df$longitude)),
                   ylim = c(min(df$latitude), max(df$latitude))) +
    theme(plot.background = element_rect(colour = "black", fill = NA))
}

# Map of Tara mission -----------------------------------------------------

# Create a function that extracts all lon/lat coordinates for all samples and create the base map. Add PACE if not a pain. 


# PACE --------------------------------------------------------------------

# Load the full spectra for one point
v_all_spectra <- read_delim("data/csv_pour_générer_spectres_pace_V20_V30_V31/PACE_20240809T0900.csv", delim = ";")
colnames(v_all_spectra)[1] <- "version"
v_all_spectra_long <- v_all_spectra |> 
  pivot_longer(`356`:`718`, names_to = "nm") |> 
  mutate(nm = as.integer(nm))

# Plot
ggplot(v_all_spectra_long) +
  geom_path(aes(x = nm, y = value, colour = version))+
  geom_vline(xintercept = 413) +
  labs(x = "Wavelength (nm)", y = "Water Leaving Reflectance (Rhow)") +
  theme(panel.background = element_rect(colour = "black", fill = "grey90"))

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
  mutate(v2_v3 = v2_Rrs / v3_Rrs,
         v2_v31 = v2_Rrs / v31_Rrs,
         v3_v31 = v3_Rrs / v31_Rrs) |> 
  # mutate(v2_v3 = ifelse(v2_v3 > 2, 2, v2_v3),
  #        v2_v31 = ifelse(v2_v31 > 2, 2, v2_v3),
  #        v3_v31 = ifelse(v3_v31 > 2, 2, v2_v3),
  #        v2_v3 = ifelse(v2_v3 < -2, -2, v2_v3),
  #        v2_v31 = ifelse(v2_v31 < -2, -2, v2_v3),
  #        v3_v31 = ifelse(v3_v31 < -2, -2, v2_v3)) |> 
  mutate(v2_v3 = cut(v2_v3, seq(-2, 2, 0.5)),
         v2_v31 = cut(v2_v31, seq(-2, 2, 0.5)),
         v3_v31 = cut(v3_v31, seq(-2, 2, 0.5)))

# Plot differences
v_comp_long <- vall_413 |> 
  dplyr::select(longitude, latitude, v2_v3:v3_v31) |> 
  pivot_longer(v2_v3:v3_v31, names_to = "ver") |> 
  mutate(ver = factor(ver, levels = c("v2_v3", "v2_v31", "v3_v31"))) |> 
  filter(!is.na(value))

ggplot(data = v_comp_long) +
  # borders(fill = "grey80") +
  geom_tile(aes(x = longitude, y = latitude, fill = value)) +
  annotate(geom = "point", x = v_all_spectra$longitude[1], y = v_all_spectra$latitude[1]) +
  # geom_contour(aes(x = longitude, y = latitude, z = value),
  #              breaks = c(1.0), color = "black") +
  # scale_colour_viridis_c(aesthetics = c("colour", "fill")) +
  scale_fill_discrete() +
  labs(x = NULL, y = NULL) +
  facet_wrap(~ver) +
  coord_quickmap(xlim = c(min(v_comp_long$longitude), max(v_comp_long$longitude)),
                 ylim = c(min(v_comp_long$latitude), max(v_comp_long$latitude))) +
  theme(panel.background = element_rect(colour = "black", fill = NA))

# Extract and compare the different PACE versions
pl_v2 <- map_PACE(v2_413)
pl_v3 <- map_PACE(v3_413)
pl_v31 <- map_PACE(v31_413)

pl_all <- ggpubr::ggarrange(pl_v2, pl_v3, pl_v31, nrow = 1, ncol = 3)
pl_all
