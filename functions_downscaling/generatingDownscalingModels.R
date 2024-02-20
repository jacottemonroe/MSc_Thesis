## MSc Thesis 
## Jacotte Monroe 

## Function to generate models 
## Input: filename for modis and landsat images, date of modis image, specify landsat bands to remove (will also not include 
##        corresponding band ratios), specify covariates to remove (landsat band/band ratios), boolean for 
##        checking and returning multicolinearity, value for multicolinearity threshold (only applicable if 
##        multicol check is true), booleans for fitting linear/cubist/RF regressions
## Output: saved as RDS --> can include: dataset, multicolinearity check, regression models and validations


generateDownscalingModels <- function(modis_filename, modis_date, landsat_filename, bands_to_remove_list = NULL, 
                                        covariates_to_remove_list = NULL, check_multicolinearity = F, 
                                        multicolinearity_threshold = 0.8, fit_linear_regression = F, 
                                        fit_cubist_regression = F, fit_rf_regression = F, output_directory = 'output/models/'){
  
  
  ####################### read datasets #####################
  
  modis_250 <- rast(modis_filename)
  names(modis_250) <- c('B2', 'B1')
  
  l_30 <- rast(landsat_filename)
  names(l_30) <- c('B2', 'B3', 'B4', 'B5', 'B6', 'B7')
  
  # remove bands (specified in user input)
  source('functions_downscaling/removingSpatRasterLayers.R')
  l_30 <- removeSpatRasterLayers(l_30, bands_to_remove_list)
  
  # reproject datasets to EPSG for Namibia [not needed anymore since reproject in GEE directly]
  # source: https://epsg.io/?q=Namibia&page=2#:~:text=WGS%2084%20%2F%20UTM%20zone%2033S
  #modis_250 <- project(modis_250, 'EPSG:32733')
  #l_30 <- project(l_30, 'EPSG:32733')
  
  
  ################### upscale landsat to 250m ###############
  
  # source: https://www.pmassicotte.com/posts/2022-04-28-changing-spatial-resolution-of-a-raster-with-terra/#resampling
  l_250 <- resample(l_30, modis_250, method = 'average')
  
  # all pixels with value 0 changed to NA (because originally masked pixels in GEE)
  l_250[l_250 == 0] <- NA
  
  
  ################# generate covariates and response variable ###############
  
  source('functions_downscaling/generatingCovariatesResponseSet.R')
  
  dataset <- getCovariatesResponseSet(l_250, modis_250)
  
  # remove highly correlated covariates based on multicolinearity check (based on user input)
  source('functions_downscaling/removingSpatRasterLayers.R')
  
  dataset <- removeSpatRasterLayers(dataset, covariates_to_remove_list)
  
  #outputs[['cov_resp_dataset']] <- c(outputs[['cov_resp_dataset']], dataset)
  saveRDS(dataset, paste0(output_directory,modis_date,'_dataset.RData'))
  
  
  ############### check for multicolinearity ####################
  
  if(check_multicolinearity == T){
    
    source('functions_downscaling/checkingMulticolinearity.R')
    
    multicol_results <- checkMulticolinearity(dataset, multicolinearity_threshold)
    
    #outputs[['multicol_check']] <- c(outputs[['multicol_check']], multicol_results)
    saveRDS(multicol_results, paste0(output_directory,modis_date,'_multicol_check.RData'))
  }
  
  
  ############### create training and validation datasets #################
  
  source('functions_downscaling/spatialSplittingTrainValidationSets.R')
  
  train_val_sets <- spatialSplitTrainValidationSets(dataset, 0.7)
  
  d_train <- train_val_sets$trainingset
  d_test <- train_val_sets$validationset
  
  
  ############### fit linear regression #######################
  
  if(fit_linear_regression == T){
    # train model
    source('functions_downscaling/fittingLinearRegression.R')
    
    lr <- fitLinearRegression(d_train, full_model = F, stepwise_AIC = T)
    
    # validate model
    source('functions_downscaling/predictingModelOnData.R')
    
    lr_outputs <- predictModelOnData(d_test, lr, validation = T)
    
    #outputs[['lr_model']] <- c(outputs[['lr_model']], lr)
    #outputs[['lr_validation']] <- c(outputs[['lr_validation']], lr_outputs)
    saveRDS(lr, paste0(output_directory,modis_date,'_lr_model.RData'))
    saveRDS(lr_outputs, paste0(output_directory,modis_date,'_lr_validation.RData'))
  }
  
  
  ################ fit cubist regression #####################
  
  if(fit_cubist_regression == T){
    # train model
    source('functions_downscaling/fittingCubistRegression.R')
    
    cr <- fitCubistRegression(d_train)
    
    # validate model
    source('functions_downscaling/predictingModelOnData.R')
    
    cr_outputs <- predictModelOnData(d_test, cr, validation = T)
    
    #outputs[['cr_model']] <- c(outputs[['cr_model']], cr)
    #outputs[['cr_validation']] <- c(outputs[['cr_validation']], cr_outputs)
    saveRDS(cr, paste0(output_directory,modis_date,'_cr_model.RData'))
    saveRDS(cr_outputs, paste0(output_directory,modis_date,'_cr_validation.RData'))
  }
  
  
  ############### fit random forest regression ###############
  
  if(fit_rf_regression == T){
    # train model
    source('functions_downscaling/fittingRFRegression.R')
    
    rf <- fitRandomForestRegression(d_train) #specify if want to turn off importance with importance = 'none'
    
    # validate model
    source('functions_downscaling/predictingModelOnData.R')
    
    rf_outputs <- predictModelOnData(d_test, rf$RF_model, validation = T)
    
    #outputs[['rf_model']] <- c(outputs[['rf_model']], rf)
    #outputs[['rf_validation']] <- c(outputs[['rf_validation']], rf_outputs)
    saveRDS(rf, paste0(output_directory,modis_date,'_rf_model.RData'))
    saveRDS(rf_outputs, paste0(output_directory,modis_date,'_rf_validation.RData'))
  }
}