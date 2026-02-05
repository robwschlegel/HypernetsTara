# code/map.R
# Code used to create the map of the Tara voyage, sampling sites, and a PACE overhead pass


# Setup -------------------------------------------------------------------

source("code/functions.R")

# Luna package is used to access large spatial data products
# install.packages('luna', repos = 'https://rspatial.r-universe.dev')
library(terra)
library(luna)


# Station data ------------------------------------------------------------

# Get file lists for all in situ measurements
file_list_Hyp <- list.files(dir("~/pCloudDrive/Documents/OMTAB/HYPERNETS/tara_matchups_results_20260203", 
                                pattern = "HYPERNETS", full.names = TRUE), pattern = "*.csv", full.names = TRUE)
file_list_Hyp <- file_list_Hyp[!grepl("HYPERNETS_vs_TRIOS_vs_HYPERPRO", file_list_Hyp)]
file_list_Trios <- list.files(dir("~/pCloudDrive/Documents/OMTAB/HYPERNETS/tara_matchups_results_20260203", 
                                pattern = "TRIOS", full.names = TRUE), pattern = "*.csv", full.names = TRUE)
file_list_Trios <- file_list_Trios[!grepl("HYPERNETS_vs_TRIOS_vs_HYPERPRO", file_list_Trios)]
file_list_Pro <- list.files(dir("~/pCloudDrive/Documents/OMTAB/HYPERNETS/tara_matchups_results_20260203", 
                                pattern = "HYPERPRO", full.names = TRUE), pattern = "*.csv", full.names = TRUE)
file_list_Pro <- file_list_Pro[!grepl("HYPERNETS_vs_TRIOS_vs_HYPERPRO", file_list_Pro)]

# Load base HYPERNETS values
base_Hyp <- plyr::ldply(file_list_Hyp, load_matchup_mean, .parallel = TRUE)
base_Trios <- plyr::ldply(file_list_Trios, load_matchup_mean, .parallel = TRUE)
base_Pro <- plyr::ldply(file_list_Pro, load_matchup_mean, .parallel = TRUE)

# Filter out satellite coordinates and reduce to unique measurements
station_in_situ <- bind_rows(base_Hyp, base_Trios, base_Pro) |> 
  dplyr::select(sensor:longitude) |> 
  filter(sensor %in% c("Hyp", "HYPERPRO", "TRIOS")) |> 
  distinct() |> 
  mutate(date = parse_date(day, format = "%Y%m%d", locale = locale(tz = "UTC")),
         dateTime = as.POSIXct(paste0(day," ",time), format = "%Y%m%d %H%M%S", locale = locale(tz = "UTC")), .before = "latitude")
write_csv(station_in_situ, "meta/station_in_situ.csv")

# Get count of samples per ~1 km lon/lat
station_count <- base_in_situ |> 
  mutate(longitude = round(longitude, 2),
         latitude = round(latitude, 2)) |> 
  summarise(count_n = n(), .by = c("longitude", "latitude"))


# MODIS data --------------------------------------------------------------

# Load username and password
earth_up <- read_csv("~/pCloudDrive/Documents/info/earthdata_pswd.csv")

# Lists all products that are currently searchable
# https://lpdaac.usgs.gov/documents/925/MOD09_User_Guide_V61.pdf
MODIS_prod <- luna::getProducts()
# productInfo("MODISA_L3m_RRS")

# MODIS/Aqua Surface Reflectance Daily L2G Global 250m SIN Grid V061
productInfo("MYD09GQ")

# Niveau 1
productInfo("MYD01")

# MODIS/Aqua Surface Reflectance 8-Day L3 Global 250m SIN Grid V006
# productInfo("MYD09Q1")

# MODIS/Aqua Surface Reflectance 8-Day L3 Global 500m SIN Grid V006
# moproductInfo("MYD09A1")

# MODIS/Terra Land Water Mask Derived from MODIS and SRTM L3 Global 250m SIN Grid V061
# https://lpdaac.usgs.gov/documents/1915/MOD44W_User_Guide_ATBD_V61.pdf
# productInfo("MOD44W")

# Coords for bbox
coords <- matrix(c(
  3, 32,  # Bottom-left corner
  27, 32, # Bottom-right corner
  27, 45, # Top-right corner
  3, 45,  # Top-left corner
  3, 32   # Close the polygon (same as first point)
), ncol = 2, byrow = TRUE)

# Create a SpatVector object
bbox <- vect(coords, crs = "EPSG:4326", type = "polygons")

# Print the object to verify
print(bbox)
plot(bbox)

# Chosen start and end dates for downloading
dl_start <- "2024-08-09"; dl_end <- "2024-08-16"

# Download data
MODIS_dl("MYD09A1", dl_start, bbox, earth_up$usrname, earth_up$psswrd)

# Set file pathways
mask_files <- list.files(path = "data/MODIS", pattern = "MOD", full.names = TRUE)
rast_files <- list.files(path = "data/MODIS", pattern = "MYD", full.names = TRUE)

# Run on all of them
# MODIS_mask <- MODIS_proc(mask_files, bbox = bbox, water_mask = TRUE)
# writeRaster(MODIS_mask, "data/MODIS/study_area_mask.tif", overwrite = TRUE)
MODIS_mask <- rast("data/MODIS/study_area_mask.tif")
plot(MODIS_mask)

# Prep one day of MODIS data
# MODIS_rast <- MODIS_proc(rast_files, bbox = bbox)
# writeRaster(MODIS_rast, "data/MODIS/study_area_rast.tif", overwrite = TRUE)
MODIS_rast <- rast("data/MODIS/study_area_rast.tif")
plot(MODIS_rast)

# Projected the 250 m mask to the same grid as the 500 m raster data
MODIS_mask_proj <- project(MODIS_mask, MODIS_rast)

# Mask the raster data
MODIS_water <- mask(MODIS_rast, MODIS_mask_proj)
plot(MODIS_water)

# Convert to data.frame for easy plotting
MODIS_water_df <- as.data.frame(MODIS_water, xy = TRUE, na.rm = TRUE)


# Map of Tara mission -----------------------------------------------------

# TODO: Constrain pixel values from 0 - 0.1
# Change sampling date legend to show all values and change legend title
# Triple check lon/lat points

# Map
pl_map <- ggplot(data = station_in_situ) +
  borders(fill = "grey80") +
  # geom_tile(data = MODIS_water_df, aes(x = x, y = y, fill = sur_refl_b01)) +
  geom_point(aes(x = longitude, y = latitude), colour = "black", size = 5.5) +
  geom_point(aes(x = longitude, y = latitude, colour = as.factor(date)), size = 5) +
  geom_point(data = filter(station_in_situ, sensor == "HYPERPRO"),
             aes(x = longitude, y = latitude), colour = "maroon", size = 7, shape = 5, stroke = 2) +
  # geom_point(data = filter(station_HP, sensor == "HYPERPRO"),
  #            aes(x = longitude, y = latitude, colour = date), size = 6, shape = 22) +
  # scale_colour_date(guide = "colourbar", low = "red", high = "blue", breaks = unique(station_in_situ$date)) +
  # scale_colour_viridis_c(option = "A", guide = "legend") +
  # scale_fill_viridis_c() +
  guides(fill = guide_colorbar(barwidth = 20, barheight = 2)) +
  labs(x = "Longitude (°E)", y = "Latitude (°N)", 
       colour = "Measurement\ndate", fill = "Surface reflectance (459-479 nm) ") +
  coord_quickmap(xlim = c(7, 25), ylim = c(35, 42)) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        # legend.position = "top", 
        # legend.box = "vertical",
        legend.key.height = unit(1, "cm"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18))
# pl_map
ggsave("figures/fig_1.png", pl_map, height = 9, width = 18)

