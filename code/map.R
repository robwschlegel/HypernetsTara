# code/map.R
# Code used to create the map of the Tara voyage, sampling sites, and a PACE overhead pass


# Libraries ---------------------------------------------------------------

library(tidyverse)
# library(tidync) # Not able to open PACE file structure
library(ncdf4)


# Map of Tara mission -----------------------------------------------------

# Create a function that extracts all lon/lat coordinates for all samples and create the base map. Add PACE if not a pain. 


# PACE --------------------------------------------------------------------

# Extract and compare the different PACE versions

# Basic structure
info_PACE <- ncdump::NetCDF("~/Downloads/Netcdf_PACE/V2.0/PACE_OCI.20240809T105059.L2.OC_AOP.V2_0.NRT.nc")

# Variables
list_vars <- ncdump::NetCDF("~/Downloads/Netcdf_PACE/V2.0/PACE_OCI.20240809T105059.L2.OC_AOP.V2_0.NRT.nc")$variable

# v2.0
# PACE_v2 <- tidync("~/Downloads/Netcdf_PACE/V2.0/PACE_OCI.20240809T105059.L2.OC_AOP.V2_0.NRT.nc") |> 
  # hyper_tibble()
nc_data <- nc_open("~/Downloads/Netcdf_PACE/V2.0/PACE_OCI.20240809T105059.L2.OC_AOP.V2_0.NRT.nc")
print(nc_data$var)
nm_3d <- nc_data$dim[["wavelength_3d"]]
nm_3d <- ncvar_get(nc_data, "wavelength_3d", verbose = TRUE)
# nm_data <- ncvar_get(nc_data, "sensor_band_parameters/wavelength_3d")
nm_data <- ncvar_get(nc_data, "sensor_band_parameters/wavelength")
aw_data <- ncvar_get(nc_data, "sensor_band_parameters/aw")
rrs_data <- ncvar_get(nc_data, "geophysical_data/Rrs")
rrs_l2f_data <- ncvar_get(nc_data, "geophysical_data/l2_flags")
lon_data <- ncvar_get(nc_data, "navigation_data/longitude")
lat_data <- ncvar_get(nc_data, "navigation_data/latitude")

# test for wavelengths
rrs_data_1 <- as.vector(rrs_data[28,,])
range(rrs_data_1, na.rm = TRUE)
