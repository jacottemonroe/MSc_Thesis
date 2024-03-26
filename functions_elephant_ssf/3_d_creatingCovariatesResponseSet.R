## MSc Thesis 
## Jacotte Monroe 
## Downscaling script 

## Function that loads a MODIS 250m raster image and its corresponding Landsat 8 30m image.
## The outliers in L8 surface reflectance and MODIS NDVI (negative values) are masked or set to 0.
## Then the Landsat image is upscaled to 250m to match the MODIS resolution. 
## Band ratios are calculated for the Landsta 8 raster and returned in addition to the original bands as covariates. 
## Input: the filepaths for the Landsat and MODIS images and an optional output name for covariates/response dataset. Also ID, week, and method.
## Output: a SpatRaster dataset containing the L8 bands at 250m (covariates) and the MODIS NDVI as a layer.



createCovariatesResponseSet <- function(modis_filepath, landsat_filepath, ID, week, mods_date, 
                                            output_directory = 'data/', output_filename_suffix = ''){
  
  # read modis and landsat images 
  modis_250 <- rast(paste0(modis_filepath, LUT$modis_image))
  l_30 <- rast(paste0(landsat_filepath, LUT$closest_landsat_image))
  
  # rename layer names to corresponding bands 
  names(modis_250) <- c('B1', 'B2', 'NDVI')
  names(l_30) <- c('B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7')
  
  # removing outlier pixel values --> negative NDVI or reflectance values 
  # all pixels with value below 0 changed to 0 (threshold)
  l_30[l_30 <= 0] <- 0
  modis_250$NDVI[modis_250$NDVI < 0] <- 0
  
  # since the landsat and modis images have slightly different extents --> set modis (larger) extent to landsat (smaller) extent
  # this prevents having a boundary of NA values 
  ext(modis_250) <- ext(l_30)
  
  # upscale landsat 30m image to 250m
  # source: https://www.pmassicotte.com/posts/2022-04-28-changing-spatial-resolution-of-a-raster-with-terra/#resampling
  l_250 <- resample(l_30, modis_250, method = 'average')
  
  # create dataset for running model 
  dataset <- l_250
  dataset$ndvi <- modis_250$NDVI
  
  # get all NA values in same place (if any)
  # source: https://stackoverflow.com/questions/73719011/mask-layer-of-a-raster-stack-using-other-layer-using-terra
  mask <- any(is.na(dataset))
  dataset <- mask(dataset, mask, maskvalues = T)
  
  # get dataframe of all band combinations
  # source: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/expand.grid
  band_combinations <- expand.grid(bandA = names(dataset['B.']), bandB = names(dataset['B.']), KEEP.OUT.ATTRS = F, stringsAsFactors = F)
  
  # calculate landsat band ratios 
  for(i in 1:nrow(band_combinations)){
    # get band name from combination dataframe 
    bandA <- band_combinations[i,1]
    bandB <- band_combinations[i,2]
    
    # get band number 
    # source: https://www.statology.org/r-extract-number-from-string/
    bandA_number <- as.numeric(gsub("\\D", "", bandA))
    bandB_number <- as.numeric(gsub("\\D", "", bandB))
    
    if(bandA_number>bandB_number){
      # calculate ratio 
      ratio <- (dataset[[bandA]]-dataset[[bandB]])/(dataset[[bandA]]+dataset[[bandB]])
      
      # add band ratio to dataset
      layer_name <- paste0(bandA,'.',bandB)
      dataset[[layer_name]] <- ratio
    }
  }
  
  # create output filepath 
  output_filepath <- paste0(output_directory, ID, '/', week, '/')
  
  # create data directory if it does not yet exist
  if(!dir.exists(output_filepath)){dir.create(output_filepath, recursive = T)}
  
  # save dataset
  writeRaster(dataset, paste0(output_filepath,'3_d1_', modis_date, '_dataset', output_filename_suffix, '.tif'), overwrite = T)
  
  
}

