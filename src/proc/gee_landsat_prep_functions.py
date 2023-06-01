
def prep_tm5_ic(ee_geometry):
    """ Prepare Landsat 5 image collection
    """
    
    tm5_output_bands_orig = ['SR_B7','SR_B5','SR_B4','SR_B3','SR_B2','SR_B1','clouds','shadows','clouds_shadows']
    tm5_output_bands = ['swir2','swir1','nir','red','green','blue','clouds','shadows','clouds_shadows']
    tm5_ic = ee.ImageCollection("LANDSAT/LT05/C02/T1_L2") \
      .filterBounds(ee_geometry)
      
    get_qaband_clouds_shadows = rs.get_qaband_clouds_shadows_func(
          qa_bandname = 'QA_PIXEL', 
          cloud_bit = 3, 
          shadow_bit = 4,
          keep_orig_bands = True) 
    tm5_clouds_ic = (tm5_ic
      .map(get_qaband_clouds_shadows))
      # .map(lambda im: im.addBands(im.expression('im.clouds | im.clouds_shadows', {'im' : im}).rename('cloudmask'))))
      
    return tm5_clouds_ic.select(tm5_output_bands_orig, tm5_output_bands)

# %% 
def prep_etm7_ic(ee_geometry):
    """ Prepare Landsat 7 image collection
    """
    
    etm7_output_bands_orig = ['SR_B7','SR_B5','SR_B4','SR_B3','SR_B2','SR_B1','clouds','shadows','clouds_shadows']
    etm7_output_bands = ['swir2','swir1','nir','red','green','blue','clouds','shadows','clouds_shadows']
    etm7_ic = ee.ImageCollection("LANDSAT/LE07/C02/T1_L2") \
      .filterBounds(ee_geometry)
      
    get_qaband_clouds_shadows = rs.get_qaband_clouds_shadows_func(
          qa_bandname = 'QA_PIXEL', 
          cloud_bit = 3, 
          shadow_bit = 4,
          keep_orig_bands = True) 
    etm7_clouds_ic = (etm7_ic
      .map(get_qaband_clouds_shadows))
      # .map(lambda im: im.addBands(im.expression('im.clouds | im.clouds_shadows', {'im' : im}).rename('cloudmask'))))
      
    return etm7_clouds_ic.select(etm7_output_bands_orig, etm7_output_bands)


# %%
def prep_oli8_ic(ee_geometry):
    """ Prepare Landsat 8 image collection
    """
    
    oli8_output_bands = ['SR_B7','SR_B6','SR_B5','SR_B4','SR_B3','SR_B2','clouds','shadows','clouds_shadows']
    oli8_ic = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2') \
      .filterBounds(ee_geometry)
      
    get_qaband_clouds_shadows = rs.get_qaband_clouds_shadows_func(
          qa_bandname = 'QA_PIXEL', 
          cloud_bit = 3, 
          shadow_bit = 4,
          keep_orig_bands = True) 
    oli8_clouds_ic = (oli8_ic
      .map(get_qaband_clouds_shadows))
      # .map(lambda im: im.addBands(im.expression('im.clouds | im.clouds_shadows', {'im' : im}).rename('cloudmask'))))
      
    return oli8_clouds_ic.select(oli8_output_bands)
    # Get oli8 pixel timeseries