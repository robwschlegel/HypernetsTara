# code/map.R
# Code used to create the map of the Tara voyage, sampling sites, and a PACE overhead pass


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ncdf4)


# Map of Tara mission -----------------------------------------------------

# Create a function that extracts all lon/lat coordinates for all samples and create the base map. Add PACE if not a pain. 


# PACE --------------------------------------------------------------------

# Extract and compare the different PACE versions

# Load data extracted via Python script
v2_all <- read_csv("data/rrs.csv")
v2_all_full <- v2_all |> 
  # dplyr::select(wavelength_3d) |> 
  # distinct()
  # filter(wavelength_3d == 380) |> 
  filter(!is.na(Rrs)) |> 
  filter(Rrs != 0) #|> 
  # mutate(longitude = plyr::round_any(longitude, 0.05), 
         # latitude = plyr::round_any(latitude, 0.05)) |> 
  # summarise(Rrs = mean(Rrs, na.rm = TRUE), .by = c("longitude", "latitude"))

v2_480 <- read_csv("data/rrs_480.csv")
v2_480_full <- filter(v2_480, !is.na(Rrs)) |> 
  mutate(longitude = plyr::round_any(longitude, 0.05), 
         latitude = plyr::round_any(latitude, 0.05)) |> 
  summarise(Rrs = mean(Rrs, na.rm = TRUE), .by = c("longitude", "latitude"))

# Plot
ggplot(data = v2_all_full) +
  borders() +
  # geom_point(aes(x = longitude, y = latitude, colour = Rrs)) +
  geom_tile(aes(x = longitude, y = latitude, fill = Rrs)) +
  scale_colour_viridis_c(aesthetics = c("colour", "fill")) +
  coord_quickmap(xlim = c(min(v2_all_full$longitude), max(v2_all_full$longitude)),
                 ylim = c(min(v2_all_full$latitude), max(v2_all_full$latitude)))
