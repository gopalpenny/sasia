# %%
import sys
sys.path.insert(1, '/Users/gopal/Projects/ml/downloadGEErasters')
import rs
import ee
ee.Initialize()
# %%
def prep_tm5_ic(ee_geometry):
    """ Prepare Landsat 5 image collection
    """
    
    tm5_output_bands_orig = ['SR_B7','SR_B5','SR_B4','SR_B3','SR_B2','SR_B1','QA_PIXEL']
    tm5_output_bands = ['swir2','swir1','nir','red','green','blue','QA_PIXEL']
    tm5_ic = ee.ImageCollection("LANDSAT/LT05/C02/T1_L2") \
      .filterBounds(ee_geometry)
    
    return tm5_ic.select(tm5_output_bands_orig, tm5_output_bands)

# %% 
def prep_etm7_ic(ee_geometry):
    """ Prepare Landsat 7 image collection
    """
    
    etm7_output_bands_orig = ['SR_B7','SR_B5','SR_B4','SR_B3','SR_B2','SR_B1','QA_PIXEL']
    etm7_output_bands = ['swir2','swir1','nir','red','green','blue','QA_PIXEL']
    etm7_ic = ee.ImageCollection("LANDSAT/LE07/C02/T1_L2") \
      .filterBounds(ee_geometry)
      
    return etm7_ic.select(etm7_output_bands_orig, etm7_output_bands)


# %%
def prep_oli8_ic(ee_geometry):
    """ Prepare Landsat 8 image collection
    """
    
    oli8_output_bands_orig = ['SR_B7','SR_B6','SR_B5','SR_B4','SR_B3','SR_B2','QA_PIXEL']
    oli8_output_bands = ['swir2','swir1','nir','red','green','blue','QA_PIXEL']
    oli8_ic = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2') \
        .filterBounds(ee_geometry)

    return oli8_ic.select(oli8_output_bands_orig, oli8_output_bands)
    # Get oli8 pixel timeseries

# %%
ee_geometry = ee.FeatureCollection("users/gopalpenny/cauvery/Cauvery_boundary5")


# %%
tm5 = prep_tm5_ic(ee_geometry) 
etm7 = prep_etm7_ic(ee_geometry)
oli8 = prep_oli8_ic(ee_geometry) 

# %%
def prep_landsat_clouds(landsat_ic):
    """ Prepare Landsat image collection with cloud and shadow masks
    Example:

    tm5 = prep_tm5_ic(ee_geometry) 
    etm7 = prep_etm7_ic(ee_geometry)
    oli8 = prep_oli8_ic(ee_geometry) 
    landsat = tm5.merge(etm7).merge(oli8) \
        .filterMetadata('CLOUD_COVER', 'less_than', 50)
    landsat_clouds = prep_landsat_clouds(landsat)
    """

    landsat_output_bands = ['swir2','swir1','nir','red','green','blue','clouds','shadows','clouds_shadows']
    get_qaband_clouds_shadows = rs.get_qaband_clouds_shadows_func(
          qa_bandname = 'QA_PIXEL', 
          cloud_bit = 3, 
          shadow_bit = 4,
          keep_orig_bands = True) 
    landsat_clouds_ic = (landsat_ic
      .map(get_qaband_clouds_shadows))
    
    return landsat_clouds_ic.select(landsat_output_bands)

# %%

