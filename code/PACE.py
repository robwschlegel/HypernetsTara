
# Tutorials
# https://oceancolor.gsfc.nasa.gov/resources/docs/tutorials/
# https://oceancolor.gsfc.nasa.gov/resources/docs/tutorials/notebooks/oci-file-structure/
# https://nasa.github.io/VITALS/python/Exploring_PACE_OCI_L2_SFRFL.html

# Import required libraries
# import csv
# import math
# from io import BytesIO
import pathlib

# import netCDF4
# import cf_xarray
# import earthaccess
import rasterio as rio
import xarray as xr
# import rioxarray
# import cartopy
# import cartopy.crs as ccrs

import numpy as np
# import pandas as pd

# import matplotlib.pyplot as plt
# import holoviews as hv
# import geoviews as gv
# import hvplot.xarray

# Process one wavelength from an L@ PACE NetCDF file
def process_pace_data(file_path, wavelength, print_nm = False):
    """
    Process PACE OCI data and extract reflectance at specified wavelength.
    
    Args:
        file_path: Path to the NetCDF PACE OCI file
        wavelength: Wavelength value to extract from wavelength_3d
    
    Returns:
        DataFrame with processed reflectance data
    """
    # Open and merge data
    datatree = xr.open_datatree(file_path, decode_timedelta=False)
    
    ds = xr.merge(
        (
            datatree.ds,
            datatree["geophysical_data"].ds[["Rrs", "l2_flags"]],
            datatree["sensor_band_parameters"].coords,
            datatree["navigation_data"].ds.set_coords(("longitude", "latitude")).coords,
        )
    )
    
    # Check the wavelengths available for PACE
    if print_nm:
        print(ds["wavelength_3d"])

    # Extract wavelength
    rrs = ds["Rrs"].sel({"wavelength_3d": wavelength}, method="nearest")
    
    # Grid the data
    sr_src = rrs.rio.set_spatial_dims("pixels_per_line", "number_of_lines").rio.write_crs("epsg:4326")
    sr_dst = sr_src.rio.reproject(
        dst_crs=sr_src.rio.crs,
        src_geoloc_array=(
            sr_src.coords["longitude"],
            sr_src.coords["latitude"],
        ),
        nodata=np.nan,
        resampling=rio.warp.Resampling.nearest,
    ).rename({'y': 'latitude', 'x': 'longitude'})
    
    # Convert to DataFrame
    sr_df = sr_dst.to_dataframe().reset_index()
    
    # Generate output filename based on input file path
    base_name = pathlib.Path(file_path).stem
    output_file = f'data/{base_name}_rrs_{wavelength}.csv'
    sr_df.to_csv(output_file, index=False)
    
    return


# Process data
# V2.0
### Rrs 413 gives the most contrasted image. Using this for the other versions.
process_pace_data("~/Downloads/Netcdf_PACE/V2.0/PACE_OCI.20240809T105059.L2.OC_AOP.V2_0.NRT.nc", 413)
process_pace_data("~/Downloads/Netcdf_PACE/V2.0/PACE_OCI.20240809T105059.L2.OC_AOP.V2_0.NRT.nc", 450)
process_pace_data("~/Downloads/Netcdf_PACE/V2.0/PACE_OCI.20240809T105059.L2.OC_AOP.V2_0.NRT.nc", 480)
# V3.0
process_pace_data("~/Downloads/Netcdf_PACE/V3.0/PACE_OCI.20240809T105059.L2.OC_AOP.V3_0.nc", 413)
# V3.1
process_pace_data("~/Downloads/Netcdf_PACE/V3.1/PACE_OCI.20240809T105059.L2.OC_AOP.V3_1.nc", 413)