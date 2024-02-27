## MSc Thesis 
## Jacotte Monroe 
## Function SSF script 

# Function that loads the NDVI data (rasters), retrieves and stores the covariates in a table for each step.
# 
# Input: filename of elephant step dataset, directory name where NDVI data stored, 
#   specified lag (covariate calculated from two images, specify lag between two images), 
#   output data frame filename.
#   
# Output: saves covariates table as csv.


loadAndExtractCovariates <- function(input_filename = 'data/temp_eleph_path.csv', modis_image_directory, ndvi_rate_lag = 7, output_filename){
  
  # get elephant step dataset --> CHECK THAT IT IS STEP_XYT FORMAT OTHERWISE HAVE TO TRANSFORM AGAIN?!
  step_dataset <- read.csv(input_filename)
  
  # add columns for covariates 
  # source: https://sparkbyexamples.com/r-programming/add-empty-column-to-dataframe-in-r/#:~:text=Use%20%24%20operator%2C%20square%20bracket%20%5B%5D,()%20function%20from%20tidyverse%20package.
  step_dataset <- cbind(step_dataset, step_dataset$ndvi_10=NA, step_dataset$ndvi_50=NA,
                        step_dataset$ndvi_90=NA, step_dataset$ndvi_sd=NA, step_dataset$ndvi_rate_10=NA,
                        step_dataset$ndvi_rate_50=NA, step_dataset$ndvi_rate_90=NA, step_dataset$ndvi_rate_sd=NA)
  
  # retrieve and stack all generated MODIS images together
  modis_images <- rast(list.files(modis_image_directory, pattern = glob2rx('*.tif'), full.names = T))
  
  # add a time (date) attribute to the spatraster --> daily interval 
  # source: https://rdrr.io/github/rspatial/terra/man/time.html
  # source: https://stackoverflow.com/questions/73259623/how-to-index-individual-layers-from-a-spatraster-object-by-time
  time(modis_images, tstep = 'days') <- as.Date(names(modis_images)[1], format = '%Y_%m_%d', 
                                                tz = 'Africa/Maputo') + 0:(nlyr(modis_images)-1)
  
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
  
  # save this dataframe for now since took so long to generate 
  #write.csv(step_dataset, paste0('output/elephant_etosha/LA2_', as.character(w), '_step_dataset.csv'))
  write.csv(step_dataset, output_filename)
  
}