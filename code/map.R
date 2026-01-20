# code/map.R
# Code used to create the map of the Tara voyage, sampling sites, and a PACE overhead pass

# TODO: Replace PACE data with MODIS, or something that covers the full mapped region


# Setup -------------------------------------------------------------------

source("code/functions.R")

# Luna package is used to access large spatial data products
# install.packages('luna', repos = 'https://rspatial.r-universe.dev')
library(terra)
library(luna)


# Station data ------------------------------------------------------------

# Load HyperPRO sampling stations
station_HP <- read_csv("meta/all_in-situ_RHOW_stations.csv") |> 
  filter(sensor != "Hyp_nosc") |> 
  mutate(date = parse_date(as.character(day), format = "%Y%m%d", locale = locale(tz = "UTC")))

# Get count of instruments per sample
## Doesn't work great...
station_count <- station_HP |> 
  mutate(longitude = round(longitude, 2),
         latitude = round(latitude, 2)) |> 
  summarise(count_n = n(), .by = c("longitude", "latitude"))


# MODIS data --------------------------------------------------------------

# Load username and password
earth_up <- read_csv("~/pCloudDrive/Documents/info/earthdata_pswd.csv")

# Lists all products that are currently searchable
# https://lpdaac.usgs.gov/documents/925/MOD09_User_Guide_V61.pdf
MODIS_prod <- luna::getProducts()
productInfo("MODISA_L3m_RRS")

# MODIS/Aqua Surface Reflectance Daily L2G Global 250m SIN Grid V061
productInfo("MYD09GQ")

# MODIS/Aqua Surface Reflectance 8-Day L3 Global 250m SIN Grid V006
productInfo("MYD09Q1")

# MODIS/Aqua Surface Reflectance 8-Day L3 Global 500m SIN Grid V006
productInfo("MYD09A1")

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

# Get possible MODIS files
luna::getNASA("MYD09A1", dl_start, dl_start, aoi = bbox, download = FALSE)

# Download the files
luna::getNASA("MYD09A1", dl_start, dl_start, aoi = bbox, download = TRUE, overwrite = FALSE,
              path = "data", username = earth_up$usrname, password = earth_up$psswrd)

# Load a file as a raster to look at the specifics
mf1 <- terra::rast("data/MYD09A1.A2024217.h18v04.061.2024249185906.hdf")
mf1
terra::names(mf1)
plot(mf1[[3]])
# plotRGB(mf1, r = 1, g = 4, b = 3, stretch="lin")

# Create a quality mask
# https://rspatial.org/modis/4-quality.html
from <- c(3)
to <- c(5)
reject <- c("001")
qa_bits <- cbind(from, to, reject)
qa_bits
qc <- mf1[[12]]
plot(qc, main = "Quality")
quality_mask <- modis_mask(qc, 16, qa_bits)
plot(quality_mask, main = "Quality mask")
hist(quality_mask)
mf1_mask <- mask(mf1, quality_mask)
plot(mf1_mask[[3]])

# List all HDF files in a directory
MODIS_files <- list.files(path = "data", pattern = "MYD", full.names = TRUE)

# Read each file individually and store in a list
MODIS_list <- lapply(MODIS_files, rast, subds = 3) # 459-479 nm

# Mosaic all rasters into one
MODIS_mosaic <- do.call(merge, MODIS_list)
MODIS_mosaic_proj <- project(MODIS_mosaic, y = "EPSG:4326")

# Crop the mosaic raster
MODIS_cropped <- crop(MODIS_mosaic_proj, bbox)
plot(MODIS_cropped)

# Filter all values above 0.03
MODIS_filtered <- terra::ifel(MODIS_cropped > 0.03, NA, MODIS_cropped)
plot(MODIS_filtered)


# Map of Tara mission -----------------------------------------------------

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
  guides(fill = guide_colorbar(barwidth = 20, barheight = 2)) +
  labs(x = "Longitude (°E)", y = "Latitude (°N)", 
       colour = "Sampling date", fill = "Remote sensing reflectance\n(Rrs; 413 nm)") +
  coord_quickmap(xlim = c(7, 25), ylim = c(35, 42)) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "top", 
        legend.box = "vertical",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18))
ggsave("figures/fig_1.png", pl_map, height = 9, width = 14)


