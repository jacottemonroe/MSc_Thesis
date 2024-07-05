## MSc Thesis 
## Jacotte Monroe 

## Function to predict MODIS NDVI 30m 
## Input: name of modis image (from LUT), date of modis image (from LUT), name of landsat image 
##        (from LUT), model type, any bands and/or covariates to remove 
## Output: saved tiff of predicted image, saved png comparing with other images, saved
##         png of error check 


predictMODIS30 <- function(modis_image, modis_date, landsat_image, model_type = 'rf', 
                           bands_to_remove_list = NULL, covariates_to_remove_list = NULL, 
                           input_modis_directory = 'data/modis/', input_landsat_directory = 'data/l8/', 
                           input_model_directory = 'output/models/', 
                           output_directory = 'output/30m_images/'){
  
  print(paste('Starting processing of image'), str(modis_date))
  
  # retrieve MODIS image for later error check
  modis_250 <- rast(paste0(input_modis_directory,modis_image))
  names(modis_250) <- c('B2', 'B1')
  
  # retrieve downscaling model for corresponding date 
  model_path <- paste0(input_model_directory,modis_date,'_',model_type,'_model.RData')
  model <- readRDS(model_path)
  model <- model$RF_model
  
  print(paste('Model is:', model_type))
  print(paste('Model R²:', model$r.squared))
  print(paste('Model MSE:', model$prediction.error))
  
  # retrieve landsat 30m data
  l_30 <- rast(paste0(input_landsat_directory,landsat_image))
  names(l_30) <- c('B2', 'B3', 'B4', 'B5', 'B6', 'B7')
  
  # remove bands (specified in user input)
  source('functions_downscaling/removingSpatRasterLayers.R')
  l_30 <- removeSpatRasterLayers(l_30, bands_to_remove_list)
  
  # remove faulty values (pixels with value 0)
  l_30[l_30 == 0] <- NA
  
  # preprocess landsat 30 image (NA values in same place, generate all band ratios)
  source('functions_downscaling/generatingCovariatesResponseSet.R')
  
  l_30_covs <- getCovariatesResponseSet(l_30)
  
  # remove highly correlated covariates based on multicolinearity check (based on user input)
  source('functions_downscaling/removingSpatRasterLayers.R')
  
  l_30_covs <- removeSpatRasterLayers(l_30_covs, covariates_to_remove_list)
  
  ## predict modis 30m 
  source('functions_downscaling/predictingModelOnData.R')
  
  print('Starting prediction of MODIS NDVI 30m')
  
  # random forest regression
  modis_30_pred <- predictModelOnData(l_30_covs, model)
  
  # save predicted modis 30m ndvi 
  writeRaster(modis_30_pred, paste0(output_directory, modis_date, '_ndvi_30m.tif'), overwrite = T)
  
  ## compare predictions
  # calc ndvi
  modis_250_ndvi <- (modis_250$B2 - modis_250$B1)/(modis_250$B2 + modis_250$B1)
  
  # calculate landsat 8 ndvi 
  l_ndvi <- (l_30$B5 - l_30$B4)/(l_30$B5 + l_30$B4)
  
  # visualize 
  png(paste0(output_directory, modis_date, '_raster_comparison.png'))
  par(mfrow = c(1,3))
  plot(l_ndvi, main = 'True Landsat 8 NDVI 30m')
  plot(modis_30_pred, main = 'RF Predicted MODIS NDVI 30m')
  plot(modis_250_ndvi, main = 'True MODIS NDVI 250m')
  dev.off()
  
  ## error estimation
  # upscale result 
  modis_250_pred <- resample(modis_30_pred, modis_250_ndvi, method = 'average')
  
  # mask any NAs 
  modis_250_pred <- mask(modis_250_pred, any(is.na(modis_250_ndvi)), maskvalues = T)
  
  # calculate error
  error <- modis_250_ndvi - modis_250_pred
  abs_error <- abs(modis_250_ndvi- modis_250_pred)
  names(abs_error) <- c('absolute_error')
  
  # visualize
  png(paste0(output_directory, modis_date, '_errors.png'))
  par(mfrow = c(1,2))
  plot(error, main = 'Error Between True and Predicted NDVI')
  plot(abs_error, main = 'Absolute Error Between True and Predicted NDVI')
  dev.off()
  
  print(summary(abs_error))
}
