## MSc Thesis 
## Jacotte Monroe 
## Function SSF script 

# Function that loads the NDVI data (rasters), retrieves and stores the covariates in a table for each step.
# Note: Downscaling term makes the distinction between MODIS dataset that was retrieved without running any downscaling scripts (downscaling = NULL), 
#         MODIS 250m retrieved through downscaling JN script (downscaling = F), MODIS 30m generated through downscaling in R (downscaling = T).
# Note: Specify downscaling model if downscaling = T --> should match last section of name of MODIS 30m folder 
# Input: filename of elephant step dataset, directory name where NDVI data stored,
#   specified lag (covariate calculated from two images, specify lag between two images), 
#   output data frame filename.
#   
# Output: saves covariates table as csv.

 
loadAndExtractCovariates <- function(input_directory, ID, week, ndvi_rate_lag = 7, random_data_method, 
                                     downscaling = 'NULL', downscaling_model = 'ranger_full_selection', 
                                     input_suffix = '', output_directory = 'data/', output_suffix = ''){ 
  
  # load step dataset RDS 
  step_dataset <- readRDS(paste0(input_directory, '1_b1_all_steps_', random_data_method, input_suffix, '.RDS'))
  
  # add empty columns for covariates 
  # source: https://sparkbyexamples.com/r-programming/add-empty-column-to-dataframe-in-r/
  empty_cols <- c('ndvi_10', 'ndvi_50', 'ndvi_90', 'ndvi_sd', 'ndvi_rate_10', 'ndvi_rate_50', 'ndvi_rate_90', 'ndvi_rate_sd')
  step_dataset[, empty_cols] <- NA
  
  # retrieve and stack all generated MODIS images together
  if(downscaling == 'NULL'){
    modis_directory <- paste0(input_directory, '3_a1_modis_images_', random_data_method, input_suffix, '/')
    
  }else if(downscaling == T){
    modis_directory <- paste0(input_directory, '3_g1_downscaled_modis_images_30m_', downscaling_model, input_suffix, '/')
    
    output_suffix <- paste0('_downscaling_modis_30m', output_suffix)

  }else if(downscaling == F){
    modis_directory <- paste0(input_directory, '3_b1_modis_images_downscaling_', random_data_method, input_suffix, '/')
    
    output_suffix <- paste0('_downscaling_modis_250m', output_suffix)
    
  }else{stop('Incorrect term set for downscaling parameter. Should be one of the following: NULL, T, F.')}

  modis_images <- rast(list.files(modis_directory, pattern = glob2rx('2*.tif'), full.names = T))
  
  # rename the raster layers as the dates using the filenames 
  names(modis_images) <- sub('/.*/', '', sub('.tif', '', sources(modis_images)))
  
  # add a time (date) attribute to the spatraster --> daily interval 
  # source: https://rdrr.io/github/rspatial/terra/man/time.html
  # source: https://stackoverflow.com/questions/73259623/how-to-index-individual-layers-from-a-spatraster-object-by-time
  time(modis_images, tstep = 'days') <- as.Date(names(modis_images), format = '%Y_%m_%d', tz = 'Africa/Maputo')
  
  # extract covariates from corresponding MODIS image
  for(i in 1:nrow(step_dataset)){
    
    # retrieve MODIS image (SpatRaster layer) by matching date with step
    # source: https://stackoverflow.com/questions/73259623/how-to-index-individual-layers-from-a-spatraster-object-by-time
    step_modis <- modis_images[[time(modis_images) == as.Date(step_dataset$t1_[i], tz = 'Africa/Maputo')]]
    
    # extract NDVI for all pixels along step on day of passage
    ndvi_along <- unlist(extract_covariates_along(step_dataset[i,], step_modis, name_covar = 'ndvi_along'))
    
    # calculate percentile values of NDVI along step
    # source: https://www.statology.org/how-to-fix-in-r-error-in-sort-intx-na-last-decreasing-x-must-be-atomic/
    step_dataset$ndvi_10[i] <- quantile(ndvi_along, probs = 0.1, names = F, na.rm = T)
    step_dataset$ndvi_50[i] <- quantile(ndvi_along, probs = 0.5, names = F, na.rm = T)
    step_dataset$ndvi_90[i] <- quantile(ndvi_along, probs = 0.9, names = F, na.rm = T)
    step_dataset$ndvi_sd[i] <- sd(ndvi_along, na.rm = T)
    
    # retrieve MODIS image from prior to passage (*tested on step 43, had to take 6 days instead of 7 days because of cloudcover)
    step_modis_prior <- modis_images[[time(modis_images) == as.Date(step_dataset$t1_[i], tz = 'Africa/Maputo')-ndvi_rate_lag]]
    
    # extract NDVI for all pixels along step for prior passage
    ndvi_along_prior <- extract_covariates_along(step_dataset[i,], step_modis_prior, name_covar = 'ndvi_along_prior')
    
    # calculate rate of NDVI change for all pixels along step for prior to passage
    # note: have to retrieve vector inside list if want to do arithmatics
    ndvi_rate_along <- (ndvi_along[[1]] - ndvi_along_prior[[1]])/ndvi_rate_lag
    
    # calculate percentile values of NDVI change rate along step
    # source: https://www.statology.org/how-to-fix-in-r-error-in-sort-intx-na-last-decreasing-x-must-be-atomic/
    step_dataset$ndvi_rate_10[i] <- quantile(ndvi_rate_along, probs = 0.1, names = F, na.rm = T)
    step_dataset$ndvi_rate_50[i] <- quantile(ndvi_rate_along, probs = 0.5, names = F, na.rm = T)
    step_dataset$ndvi_rate_90[i] <- quantile(ndvi_rate_along, probs = 0.9, names = F, na.rm = T)
    step_dataset$ndvi_rate_sd[i] <- sd(ndvi_rate_along, na.rm = T)
    
  }
  
  # change all NA values in sd columns to 0 --> sd = 0 when sample size = 1 but here it returns sd as NA so manually change to 0
  # source: https://stackoverflow.com/questions/19379081/how-to-replace-na-values-in-a-table-for-selected-columns
  step_dataset[c('ndvi_sd', 'ndvi_rate_sd')][is.na(step_dataset[c('ndvi_sd', 'ndvi_rate_sd')])] <- 0
  
  # add columns of normalized data
  step_dataset$ndvi_10_scaled <- scale(step_dataset$ndvi_10)
  step_dataset$ndvi_50_scaled <- scale(step_dataset$ndvi_50)
  step_dataset$ndvi_90_scaled <- scale(step_dataset$ndvi_90)
  step_dataset$ndvi_sd_scaled <- scale(step_dataset$ndvi_sd)
  step_dataset$ndvi_rate_10_scaled <- scale(step_dataset$ndvi_rate_10)
  step_dataset$ndvi_rate_50_scaled <- scale(step_dataset$ndvi_rate_50)
  step_dataset$ndvi_rate_90_scaled <- scale(step_dataset$ndvi_rate_90)
  step_dataset$ndvi_rate_sd_scaled <- scale(step_dataset$ndvi_rate_sd)
  
  # create output filepath 
  output_filepath <- paste0(output_directory, ID, '/', week, '/')
  
  # create data directory if it does not yet exist
  if(!dir.exists(output_filepath)){dir.create(output_filepath, recursive = T)}
  
  # save this dataframe for now since took so long to generate 
  #write.csv(step_dataset, paste0('output/elephant_etosha/LA2_', as.character(w), '_step_dataset.csv'))
  write.csv(step_dataset, paste0(output_filepath, '4_a1_cov_resp_dataset_', random_data_method, output_suffix, '.csv'))
}





loadAndExtractCovariatesMeanSD <- function(input_directory, ID, week, ndvi_rate_lag = 7, random_data_method, 
                                     downscaling = 'NULL', downscaling_model = 'ranger_full_selection', 
                                     input_suffix = '', output_directory = 'data/', output_suffix = ''){ 
  
  # load step dataset RDS 
  step_dataset <- readRDS(paste0(input_directory, '1_b1_all_steps_', random_data_method, input_suffix, '.RDS'))
  
  # add empty columns for covariates 
  # source: https://sparkbyexamples.com/r-programming/add-empty-column-to-dataframe-in-r/
  empty_cols <- c('ndvi_mean', 'ndvi_sd', 'ndvi_rate_mean', 'ndvi_rate_sd')
  step_dataset[, empty_cols] <- NA
  
  # retrieve and stack all generated MODIS images together
  if(downscaling == 'NULL'){
    modis_directory <- paste0(input_directory, '3_a1_modis_images_', random_data_method, input_suffix, '/')
    
  }else if(downscaling == T){
    modis_directory <- paste0(input_directory, '3_g1_downscaled_modis_images_30m_', downscaling_model, input_suffix, '/')
    
    output_suffix <- paste0('_downscaling_modis_30m', output_suffix)
    
  }else if(downscaling == F){
    modis_directory <- paste0(input_directory, '3_b1_modis_images_downscaling_', random_data_method, input_suffix, '/')
    
    output_suffix <- paste0('_downscaling_modis_250m', output_suffix)
    
  }else{stop('Incorrect term set for downscaling parameter. Should be one of the following: NULL, T, F.')}
  
  modis_images <- rast(list.files(modis_directory, pattern = glob2rx('2*.tif'), full.names = T))
  
  # rename the raster layers as the dates using the filenames 
  names(modis_images) <- sub('/.*/', '', sub('.tif', '', sources(modis_images)))
  
  # add a time (date) attribute to the spatraster --> daily interval 
  # source: https://rdrr.io/github/rspatial/terra/man/time.html
  # source: https://stackoverflow.com/questions/73259623/how-to-index-individual-layers-from-a-spatraster-object-by-time
  time(modis_images, tstep = 'days') <- as.Date(names(modis_images), format = '%Y_%m_%d', tz = 'Africa/Maputo')
  
  # extract covariates from corresponding MODIS image
  for(i in 1:nrow(step_dataset)){
    
    # retrieve MODIS image (SpatRaster layer) by matching date with step
    # source: https://stackoverflow.com/questions/73259623/how-to-index-individual-layers-from-a-spatraster-object-by-time
    step_modis <- modis_images[[time(modis_images) == as.Date(step_dataset$t1_[i], tz = 'Africa/Maputo')]]
    
    # extract NDVI for all pixels along step on day of passage
    ndvi_along <- unlist(extract_covariates_along(step_dataset[i,], step_modis, name_covar = 'ndvi_along'))
    
    # calculate percentile values of NDVI along step
    # source: https://www.statology.org/how-to-fix-in-r-error-in-sort-intx-na-last-decreasing-x-must-be-atomic/
    step_dataset$ndvi_mean[i] <- mean(ndvi_along, na.rm = T)
    step_dataset$ndvi_sd[i] <- sd(ndvi_along, na.rm = T)
    
    # retrieve MODIS image from prior to passage (*tested on step 43, had to take 6 days instead of 7 days because of cloudcover)
    step_modis_prior <- modis_images[[time(modis_images) == as.Date(step_dataset$t1_[i], tz = 'Africa/Maputo')-ndvi_rate_lag]]
    
    # extract NDVI for all pixels along step for prior passage
    ndvi_along_prior <- extract_covariates_along(step_dataset[i,], step_modis_prior, name_covar = 'ndvi_along_prior')
    
    # calculate rate of NDVI change for all pixels along step for prior to passage
    # note: have to retrieve vector inside list if want to do arithmatics
    ndvi_rate_along <- (ndvi_along[[1]] - ndvi_along_prior[[1]])/ndvi_rate_lag
    
    # calculate percentile values of NDVI change rate along step
    # source: https://www.statology.org/how-to-fix-in-r-error-in-sort-intx-na-last-decreasing-x-must-be-atomic/
    step_dataset$ndvi_rate_mean[i] <- mean(ndvi_rate_along, na.rm = T)
    step_dataset$ndvi_rate_sd[i] <- sd(ndvi_rate_along, na.rm = T)
    
  }
  
  # change all NA values in sd columns to 0 --> sd = 0 when sample size = 1 but here it returns sd as NA so manually change to 0
  # source: https://stackoverflow.com/questions/19379081/how-to-replace-na-values-in-a-table-for-selected-columns
  step_dataset[c('ndvi_sd', 'ndvi_rate_sd')][is.na(step_dataset[c('ndvi_sd', 'ndvi_rate_sd')])] <- 0
  
  # add columns of normalized data
  step_dataset$ndvi_mean_scaled <- scale(step_dataset$ndvi_mean)
  step_dataset$ndvi_sd_scaled <- scale(step_dataset$ndvi_sd)
  step_dataset$ndvi_rate_mean_scaled <- scale(step_dataset$ndvi_rate_mean)
  step_dataset$ndvi_rate_sd_scaled <- scale(step_dataset$ndvi_rate_sd)
  
  # create output filepath 
  output_filepath <- paste0(output_directory, ID, '/', week, '/')
  
  # create data directory if it does not yet exist
  if(!dir.exists(output_filepath)){dir.create(output_filepath, recursive = T)}
  
  # save this dataframe for now since took so long to generate 
  #write.csv(step_dataset, paste0('output/elephant_etosha/LA2_', as.character(w), '_step_dataset.csv'))
  write.csv(step_dataset, paste0(output_filepath, '4_a1_cov_resp_dataset_', random_data_method, output_suffix, '.csv'))
}