## MSc_Thesis
## Jacotte Monroe 
## Downscaling function 

## Function takes a raster of covariates and a model. 
## It predicts MODIS 30m from the Landsat covariate raster using the specified model.
## Input: filepaths, ID, week, and entry from MODIS-Landsat look up table, specify model type (string), 
##        any input or output suffix and the output directory.
## Output: The predicted MODIS NDVI 30m raster, as well as some error/accuracy metrics and statistics. 


predictDownscaledModis <- function(input_filepath, modis_filepath, ID, week, LUT_entry, model_type, 
                                   input_suffix = '', output_directory = 'data/', output_suffix = ''){
  
  # retrieve landsat date from LUT
  l_name <- LUT_entry$closest_landsat_image
  l_date <- sub('LC.*4_', '', sub('_stitched.tif', '', l_name))
  
  # load Landsat covariate raster 
  l_30 <- rast(paste0(input_filepath, '3_d2_', l_date, '_prediction_covariates.tif'))
  
  # define date and name of downscaling model
  m_date <- LUT_entry$modis_date
  
  # load downscaling model 
  model <- readRDS(paste0(input_filepath, '3_f1_', m_date, '_', model_type, '_model', input_suffix, '.RDS'))
  
  # check that covariates match 
  if(!identical(names(l_30), model$finalModel$xNames)){stop('The covariates are not matching! 
    Make sure that the prediction covariate raster has the same predictors as the ones used to train the model')}
  
  # predict MODIS 30m using the selected Landsat 30m dataset and model
  modis_30 <- predict(l_30, model)
  
  # define output filepath 
  output_filepath1 <- paste0(output_directory, ID, '/', week, '/', '3_g1_downscaled_modis_images_30m_', model_type, output_suffix, '/')
  
  # create output filepath if it does not yet exist
  if(!dir.exists(output_filepath1)){dir.create(output_filepath1, recursive = T)}
  
  # save predicted raster
  writeRaster(modis_30, paste0(output_filepath1, LUT_entry$modis_image), overwrite = T)
              
  # load modis 250m 
  modis_250 <- rast(paste0(modis_filepath, LUT_entry$modis_image))
  
  # upscale predicted modis raster to 250m for comparison 
  modis_250_pred <- resample(modis_30, modis_250)
  
  # compare modis 30m with modis 250m
  num_obs <- ncol(modis_250_pred) * nrow(modis_250_pred)
  
  # create error raster 
  # source: https://gis.stackexchange.com/questions/4802/rmse-between-two-rasters-step-by-step
  error <- modis_250_pred - modis_250
  
  # calculate accuracy metrics
  # source: https://rdrr.io/cran/terra/man/global.html#google_vignette
  mse <- global(error**2, 'sum', na.rm = T)[[1]]/num_obs
  rmse <- sqrt(mse)
  mae <- global(abs(error), 'sum', na.rm = T)[[1]]/num_obs
  
  # source: https://stackoverflow.com/questions/63335671/correct-way-of-determining-r2-between-two-rasters-in-r#:~:text=R2%20%3D%20r%20*%20r.,of%202%20to%20get%20R2.
  r2 <- cor(values(modis_250_pred), values(modis_250), use="complete.obs", method = 'pearson')^2
  r2 <- r2[[1]]
  
  # store results in dataframes 
  results <- data.frame(MSE = mse, RMSE = rmse, MAE = mae, R2 = r2)
  errors <- data.frame(summary(error))[,3]
  abs_errors <- data.frame(summary(abs(error)))[,3]
  
  # source: https://stackoverflow.com/questions/41056639/r-get-value-from-summary
  summary_errors <- data.frame('Label' = sub(':.*', '', errors), 'Error' = as.numeric(sub('.*:', '', errors)), 
                               'Abs(Error)' = as.numeric(sub('.*:', '', abs_errors)))
  
  # define output filepath 
  output_filepath2 <- paste0(output_directory, ID, '/', week, '/')
  
  # create output filepath if it does not yet exist
  if(!dir.exists(output_filepath2)){dir.create(output_filepath2, recursive = T)}
  
  # save outputs 
  write.csv(results, paste0(output_filepath2, '3_g2_', m_date, '_', model_type, '_predNDVI_30m_results', output_suffix, '.csv'))
  write.csv(summary_errors, paste0(output_filepath2, '3_g3_', m_date, '_', model_type, '_predNDVI_30m_error_summary', output_suffix, '.csv'))
  png(paste0(output_filepath2, '3_g4_', m_date, '_', model_type, '_predNDVI_30m_errors', output_suffix, '.png'))
  par(mfrow = c(1, 3))
  plot(error)
  plot(error < 0)
  plot(abs(error))
  dev.off()
}



# Function that creates a mean raster from all images in directory 

createMeanRaster <- function(modis_filepath, output_filename = 'mean_ndvi.tif'){
  
  # load all MODIS images into one stacked raster 
  modis_raster <- rast(list.files(modis_filepath, full.names = T))
  
  # average all raster layers into one layer
  mean_raster <- mean(modis_raster)
  
  # save raster 
  writeRaster(mean_raster, paste0(modis_filepath, output_filename), overwrite = T)
}