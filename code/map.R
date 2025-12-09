# code/map.R
# Code used to create the map of the Tara voyage, sampling sites, and a PACE overhead pass


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ncdf4)



# Functions ---------------------------------------------------------------

map_PACE <- function(df){
  ggplot(data = df) +
    borders() +
    # geom_point(aes(x = longitude, y = latitude, colour = Rrs)) +
    geom_tile(aes(x = longitude, y = latitude, fill = Rrs)) +
    scale_colour_viridis_c(aesthetics = c("colour", "fill")) +
    coord_quickmap(xlim = c(min(df$longitude), max(df$longitude)),
                   ylim = c(min(df$latitude), max(df$latitude)))
}

#|> 
# mutate(longitude = plyr::round_any(longitude, 0.05), 
# latitude = plyr::round_any(latitude, 0.05)) |> 
# summarise(Rrs = mean(Rrs, na.rm = TRUE), .by = c("longitude", "latitude"))

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

v2_413 <- read_csv("data/PACE_OCI.20240809T105059.L2.OC_AOP.V2_0.NRT_rrs_413.csv") |> filter(!is.na(Rrs))
v2_450 <- read_csv("data/PACE_OCI.20240809T105059.L2.OC_AOP.V2_0.NRT_rrs_450.csv") |> filter(!is.na(Rrs))
v2_480 <- read_csv("data/PACE_OCI.20240809T105059.L2.OC_AOP.V2_0.NRT_rrs_480.csv") |> filter(!is.na(Rrs)) 

map_PACE(v2_413)
map_PACE(v2_450)
map_PACE(v2_480)
