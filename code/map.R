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
ncdump::NetCDF("~/Downloads/Netcdf_PACE/V2.0/PACE_OCI.20240809T105059.L2.OC_AOP.V2_0.NRT.nc")

# Variables
list_vars <- ncdump::NetCDF("~/Downloads/Netcdf_PACE/V2.0/PACE_OCI.20240809T105059.L2.OC_AOP.V2_0.NRT.nc")$variable

# v2.0
# PACE_v2 <- tidync("~/Downloads/Netcdf_PACE/V2.0/PACE_OCI.20240809T105059.L2.OC_AOP.V2_0.NRT.nc") |> 
  # hyper_tibble()
nc_data <- nc_open("~/Downloads/Netcdf_PACE/V2.0/PACE_OCI.20240809T105059.L2.OC_AOP.V2_0.NRT.nc")
print(nc_data)
nm_data <- ncvar_get(nc_data, "sensor_band_parameters/wavelength")
aw_data <- ncvar_get(nc_data, "sensor_band_parameters/aw")

