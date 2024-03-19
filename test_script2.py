#!/usr/bin/env python
# coding: utf-8

## MSC_Thesis 
## Jacotte Monroe 
## 19/03/24

## Script that reads the spatial and temporal extents and retrieves the matching MODIS 250m images. 
## MODIS dataset is cloudmasked using a bitmask from MODIS 500m, then gap filled using temporal linear interpolation. 
## Each image is reprojected to EPSG 32733 (Namibia projection) and clipped to study area. 
## NDVI is calculated. 
## Script inputs: 
##       0_single_run_settings.csv
##       2_a1_step_extents.csv
## Script outputs: 
##       3_a1_modis_images (folder) > [daily_MODIS_NDVI_250m_date].tif (intermediate output) & 3_a2_modis_ndvi.tif (intermediate output)



###########
## Script set-up and loading necessary libraries 
###########

import ee
ee.Authenticate()

import geemap
import os
import pandas as pd
import math 

ee.Initialize()



###########
## Functions needed to run the script 
###########

# function to extract and combined relevant QA band bits/flags 
# source: https://mygeoblog.com/2017/09/08/modis-cloud-masking/
def getQABits(image, start_bit, end_bit, band_name):
    pattern = 0
    
    # for each bit/flag of the QA band --> assign new value to its bits 
    # so each flag will have a different value and the pixel will be the sum of all flags 
    for i in range(start_bit, end_bit):
        pattern += math.pow(2,i)
    # return single band image of extracted QA bits
    # source: https://developers.google.com/earth-engine/apidocs/ee-image-bitwiseand
    return image.select([0], [band_name]) \
                .bitwiseAnd(pattern) \
                .rightShift(start_bit)



# function to mask out cloudy pixels 
# source: https://gis.stackexchange.com/questions/308456/get-a-mask-for-modis-250m-mod09gq-using-modis-500m-mod09ga-in-google-earth
# source: https://mygeoblog.com/2017/09/08/modis-cloud-masking/
def maskClouds(image):
    # selects the MODIS 500m QA band 
    QA = image.select('state_1km')

    # creates a cloud&shadow flag from specified bits --> in this case: 'Cloud state' and 'Cloud shadow'
    pixelQuality = getQABits(QA, 0, 2, 'cloud_and_shadow_quality_flag')

    # returns image masking out cloudy pixels 
    return image.updateMask(pixelQuality.eq(0))



# function to add timestamp band to image
# source: https://spatialthoughts.com/2021/11/08/temporal-interpolation-gee/
def addTimestamp(image): 
    # create new image where pixel value = time of original image
    timeImage = image.metadata('system:time_start').rename('timestamp')

    # mask new time image with original image to remove cloudmasked pixels
    timeImageMasked = timeImage.updateMask(image.mask().select(0))

    # return original image with time image as new band 
    return image.addBands(timeImageMasked)



# function that takes image and replaces masked pixels with linearly interpolated values from bef/aft images
def interpolateImage(image):
    image = ee.Image(image)

    # get list of before/after images from image property
    beforeImages = ee.List(image.get('before'))
    afterImages = ee.List(image.get('after'))

    # create image collection of before/after images
    # mosaic() combines images into one image accordint to their position in collection 
    #  image first has all pixels from last image in collection 
    #  gaps filled with second to last image from collection ...
    beforeMosaic = ee.ImageCollection.fromImages(beforeImages).mosaic()
    afterMosaic = ee.ImageCollection.fromImages(afterImages).mosaic()

    # rename time band of images 
    time_bef = beforeMosaic.select('timestamp').rename('time_bef')
    time_aft = afterMosaic.select('timestamp').rename('time_aft')
    time0 = image.metadata('system:time_start').rename('time0')

    # combine all three single band time images into one image with three time bands 
    timeImage = ee.Image.cat([time_bef, time_aft, time0])

    # compute image of interpolated surface reflectance values 
    timeRatio = timeImage.expression('(time0 - time_bef) / (time_aft - time_bef)', \
                    {'time0': timeImage.select('time0'), 
                     'time_bef': timeImage.select('time_bef'), 
                     'time_aft': timeImage.select('time_aft')})

    interpolated = beforeMosaic.add((afterMosaic.subtract(beforeMosaic).multiply(timeRatio)))

    # replace masked pixels in current image with pixels from interpolated mosaic
    result = image.unmask(interpolated)

    # return gap-filled image
    return result.copyProperties(image, ['system:time_start'])



# function to reproject (elephant fixes reprojected from 4326 to 32733 same needs to be done to images) 
def reprojectModis(image):
    return image.reproject('EPSG:32733', None, 250)



# function to clip image to study area
# have to include nested function because can't use map() with more than one argument 
# source: https://gis.stackexchange.com/questions/473500/mapping-function-with-multiple-arguments-in-google-earth-engine
def clipToAOI(bbox): 
    def withBbox(image):
        return image.clip(bbox).copyProperties(image, ['system:id'])
    return withBbox



# function to calculate NDVI 
def addNDVI(image): 
    ndvi = image.normalizedDifference(['nir', 'red']).rename('NDVI')
    return image.addBands(ndvi)



def test_function(): 
    # source: https://stackoverflow.com/questions/44116194/import-a-function-from-another-ipynb-file
    #from ipynb.fs.defs.pythonFunctions import getQABits, maskClouds, addTimestamp, interpolateImage, reprojectMODIS, clipToAOI, addNDVI
    
    ###########
    ## Read run settings 
    ###########
    run_settings_table = pd.read_csv('data/single_run_settings.csv', index_col = 0, header = None) #.iloc[-1]

    ID = run_settings_table.iloc[0,0]
    week = str(run_settings_table.iloc[0,1])
    random_data_method = run_settings_table.iloc[0,2]



    ###########
    ## Load step extents LUT
    ###########
    extents_lut = pd.read_csv('data/' + ID + '/' + week + '/' + 'step_extents_LUT_' + random_data_method + '.csv')



    ###########
    ## Create region (largest extent) geometry and define first and last dates
    ###########
    large_extent = extents_lut.iloc[-1]

    large_extent_coords = [[[large_extent.loc['xmin'], large_extent.loc['ymin']],
                            [large_extent.loc['xmin'], large_extent.loc['ymax']],
                            [large_extent.loc['xmax'], large_extent.loc['ymax']],
                            [large_extent.loc['xmax'], large_extent.loc['ymin']]]]
    
    large_region = ee.Geometry.Polygon(large_extent_coords, proj = 'EPSG:32733', evenOdd = False)
    
    first_date = ee.Date(large_extent.loc['start_date_prev_week'], 'Africa/Maputo')
    week_start_date = ee.Date(large_extent.loc['start_date'], 'Africa/Maputo')
    last_date = ee.Date(large_extent.loc['end_date'], 'Africa/Maputo')


    
    ###########
    ## Load feature of Etosha National Park (Namibia) and transform into geometry
    ###########
    # load Etosha National Park study area
    enp = ee.FeatureCollection('WCMC/WDPA/current/polygons') \
            .filter(ee.Filter.eq('ORIG_NAME', 'Etosha'))
    
    # turn ENP study area into a geometry
    enp_geom = enp.geometry()


    
    ###########
    ## Set interpolation parameters 
    ###########
    # set time-window for interpolation (how far will interpolate)
    # source: https://spatialthoughts.com/2021/11/08/temporal-interpolation-gee/
    days = ee.Number(30) 
    
    # convert to milliseconds (for gap filling step)
    millis = days.multiply(1000*60*60*24)


    
    ###########
    ## Define path of output directory 
    ###########
    # define output folder paths for current MODIS images and images from week prior
    out_dir = 'data/' + ID + '/' + week + '/' + 'modis_images_' + random_data_method + '/'
    
    
    
    ###########
    ## Retrieve MODIS datasets and combine into one dataset
    ###########
    # load MODIS 250m dataset 
    # take larger time range to include images for gap filling of cloudmasked pixels 
    # source: https://developers.google.com/earth-engine/apidocs/ee-date-advance#colab-python
    modis_250 = ee.ImageCollection('MODIS/061/MOD09GQ') \
            .filterDate(first_date.advance(days.multiply(-1), 'day'), last_date.advance(days, 'day')) \
            .filterBounds(enp_geom)
    
    # import MODIS 500m dataset (contains information for cloudmasking)
    modis_500 = ee.ImageCollection('MODIS/006/MOD09GA') \
            .filterDate(first_date.advance(days.multiply(-1), 'day'), last_date.advance(days, 'day'))  \
            .filterBounds(enp_geom)
    
    # combine datasets
    # pixel with certain MODIS 500m value can be selected for MODIS 250m 
    # source: https://gis.stackexchange.com/questions/308456/get-a-mask-for-modis-250m-mod09gq-using-modis-500m-mod09ga-in-google-earth
    modis_combined = modis_500.combine(modis_250)
    
    # new name of MODIS 250m bands (to avoid having two of similar name)
    modisBands = ['sur_refl_b01_1', 'sur_refl_b02_1']
    renamedBands = ['red', 'nir']



    ###########
    ## Create cloud free MODIS 250m dataset
    ###########
    # source: https://mygeoblog.com/2017/09/08/modis-cloud-masking/ 
    # source: https://gis.stackexchange.com/questions/308456/get-a-mask-for-modis-250m-mod09gq-using-modis-500m-mod09ga-in-google-earth
    
    modis_cloudFree = modis_combined.map(maskClouds).select(modisBands, renamedBands)

    
    ###########
    ## Image gap filling - Linear interpolation of missing pixels 
    ###########
    # add timestamp band to each image 
    modis_cloudFree_withTime = modis_cloudFree.map(addTimestamp)
    
    # following tutorial: https://spatialthoughts.com/2021/11/08/temporal-interpolation-gee/
    # define filter to only retrieve images within the specified time-window
    maxDiffFilter = ee.Filter.maxDifference(difference = millis, 
                                            leftField = 'system:time_start', 
                                            rightField = 'system:time_start')
    
    # define filters that compare given image timestamp against other image timestamps
    # NOTE: leftField compared against rightField --> so in first filter = leftField smaller than rightField
    greaterEqFilter = ee.Filter.greaterThanOrEquals(leftField = 'system:time_start', 
                                                    rightField = 'system:time_start')
    
    lessEqFilter = ee.Filter.lessThanOrEquals(leftField = 'system:time_start', 
                                              rightField = 'system:time_start')
    
    # combined filters --> find all images before/after image that are within time-window
    filter_before = ee.Filter.And(maxDiffFilter, greaterEqFilter)
    filter_after = ee.Filter.And(maxDiffFilter, lessEqFilter)
    
    # set join parameters
    # pairs each image to group of matching elements from the second collection 
    # saves the matching elements as new property with matchesKey name, ordered by date
    # order set to have the closest image as last element in list
    # source: https://spatialthoughts.com/2021/11/08/temporal-interpolation-gee/
    # source: https://developers.google.com/earth-engine/apidocs/ee-join-saveall
    join_before = ee.Join.saveAll(
        matchesKey = 'before', 
        ordering = 'system:time_start', 
        ascending = True)
    
    join_after = ee.Join.saveAll(
        matchesKey = 'after', 
        ordering = 'system:time_start', 
        ascending = False)
    
    # join the cloudfree MODIS 250m collection with itself to get all previous/post images within time-window
    # results in image collection where each image has a property that lists all images preceeding/following image within time-window
    # source: https://spatialthoughts.com/2021/11/08/temporal-interpolation-gee/
    modis_joined_temp = join_before.apply(
        primary = modis_cloudFree_withTime, 
        secondary = modis_cloudFree_withTime, 
        condition = filter_before)
    
    modis_joined = join_after.apply(
        primary = modis_joined_temp, 
        secondary = modis_joined_temp, 
        condition = filter_after)
    
    # interpolate all images from MODIS 250m image collection
    modis_interpolated = ee.ImageCollection(modis_joined.map(interpolateImage))
    
    
    
    ###########
    ## Look over each step extent entry in the LUT to export corresponding image 
    ###########
    # 1. get dates
    # 2. get coordinates 
    # 3. turn into geometry
    # 4. get MODIS image
    # 5. reproject 
    # 6. clip
    # 7. calculate NDVI
    # 8. export image
    
    for i in range(len(extents_lut)-1):
        # select entry
        extent = extents_lut.iloc[i]
        
        # retrieve extent dates 
        start_date = extent.loc['start_date']
        end_date = extent.loc['end_date']
        
        start_date_prev_week = extent.loc['start_date_prev_week']
        end_date_prev_week = extent.loc['end_date_prev_week']
        
        # retrieve extent coordinates 
        # source: https://sparkbyexamples.com/pandas/pandas-get-cell-value-from-dataframe/?utm_content=cmp-true
        # source: https://stackoverflow.com/questions/75203044/how-to-create-polygon-from-bbox-data-in-pythonv
        coords = [[[extent.loc['xmin'], extent.loc['ymin']],
                  [extent.loc['xmin'], extent.loc['ymax']],
                  [extent.loc['xmax'], extent.loc['ymax']],
                  [extent.loc['xmax'], extent.loc['ymin']]]]
    
        # create geometry from coordinates
        bbox = ee.Geometry.Polygon(coords, proj = 'EPSG:32733', evenOdd = False)
        
        # retrieve MODIS 250m image for geometry 
        modis = modis_interpolated \
            .filterDate(start_date, end_date) \
            .filterBounds(enp_geom) \
            .select(['nir', 'red']) 
        
        modis_prev_week = modis_interpolated \
            .filterDate(start_date_prev_week, end_date_prev_week) \
            .filterBounds(enp_geom) \
            .select(['nir', 'red']) 
        
        # reproject image
        modis_reproj = modis.map(reprojectModis)
        
        modis_prev_week_reproj = modis_prev_week.map(reprojectModis)
        
        # clip image
        modis_clipped = modis_reproj.map(clipToAOI(bbox))
        
        modis_prev_week_clipped = modis_prev_week_reproj.map(clipToAOI(bbox))
    
        # calculate NDVI 
        modis_ndvi = modis_clipped.map(addNDVI).select(['NDVI'])
    
        modis_ndvi_prev_week = modis_prev_week_clipped.map(addNDVI).select(['NDVI'])
        
        # export MODIS image to local repository 
        # source: https://github.com/gee-community/geemap/blob/master/examples/notebooks/11_export_image.ipynb
        geemap.ee_export_image_collection(modis_ndvi, out_dir = out_dir, region = large_region)
        
        geemap.ee_export_image_collection(modis_ndvi_prev_week, out_dir = out_dir, region = large_region)
    
    
    
    ###########
    ## Create and export mean NDVI image for large extent 
    ###########
    # This image will be used as basemap for visualization (no analysis done with it)
    # filter dataset to dates
    modis_week = modis_interpolated \
        .filterDate(week_start_date, last_date) \
        .filterBounds(enp_geom).select(['nir', 'red']) 
    
    # calculate NDVI 
    modis_week_ndvi = modis_week.map(addNDVI).select(['NDVI'])
    
    # create mean and standard deviation NDVI rasters
    # source: https://developers.google.com/earth-engine/guides/ic_reducing
    modis_ndvi_mean = modis_week_ndvi.mean()
    
    # reproject image
    modis_ndvi_mean = modis_ndvi_mean.reproject('EPSG:32733', None, 250)
    
    # clip image
    modis_ndvi_mean = modis_ndvi_mean.clip(large_region)
    
    # export image
    geemap.ee_export_image(modis_ndvi_mean, filename = os.path.join(out_dir, "mean_ndvi.tif"), region = large_region, scale = 250)

    return print('(DONE) Retrieving cloudmasked and filled MODIS 250m NDVI images corresponding to the movement steps.')
    #print('Now extracting covariates...')


# test 
#retrieveMODISImages()
test_function()
