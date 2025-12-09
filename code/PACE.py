
# Tutorials
# https://oceancolor.gsfc.nasa.gov/resources/docs/tutorials/
# https://oceancolor.gsfc.nasa.gov/resources/docs/tutorials/notebooks/oci-file-structure/
# https://nasa.github.io/VITALS/python/Exploring_PACE_OCI_L2_SFRFL.html

# Import required libraries
import csv
import math
from io import BytesIO

import netCDF4
import cf_xarray
import earthaccess
import rasterio as rio
import xarray as xr
import cartopy
import cartopy.crs as ccrs

import numpy as np
import pandas as pd

import matplotlib.pyplot as plt
import holoviews as hv
import geoviews as gv
import hvplot.xarray


datatree = xr.open_datatree("~/Downloads/Netcdf_PACE/V2.0/PACE_OCI.20240809T105059.L2.OC_AOP.V2_0.NRT.nc", decode_timedelta=False)
datatree

ds = xr.merge(
    (
        datatree.ds,
        datatree["geophysical_data"].ds[["Rrs", "l2_flags"]],
        datatree["sensor_band_parameters"].coords,
        datatree["navigation_data"].ds.set_coords(("longitude", "latitude")).coords,
    )
)
ds

# Check the wavelengths available for PACE 
ds["wavelength_3d"]

rrs_480 = ds["Rrs"].sel({"wavelength_3d": 480}, method="nearest")

plot = rrs_480.plot(x="longitude", y="latitude", cmap="viridis", vmin=0)

fig = plt.figure()
ax = plt.axes(projection=ccrs.PlateCarree())
ax.coastlines()
ax.gridlines(draw_labels={"left": "y", "bottom": "x"})
plot = rrs_480.plot(x="longitude", y="latitude", cmap="viridis", vmin=0, ax=ax)
plt.show()

rrs_480
