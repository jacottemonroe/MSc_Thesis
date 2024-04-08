## MSc_Thesis
## Jacotte Monroe 
## 19/03/24

## R script that runs function to extract covarites from the movement steps and MODIS images. 
## Script takes the run settings to retrieve the step_xyt dataset and the MODIS images. 
## The images are matched to each step. The NDVI is retrieved along each step and the percentiles and standard deviation values are calculated. 
## The change in NDVI between the image at the time of movement and one week prior is also calculated, the percentiles and stdev are derived. 
## The extracted covariates are stored as additional columns in the step dataset. 
## The final dataframe therefore contains the response variable and predictors for running a model.
## Script outputs: 
##    4_a1: The movement step dataset with retrieved covariates, saved as csv. (intermediate result)

run_settings <- read.csv('data/run_settings_moreRQ1.csv', row.names = 1)[6,]
# write.csv(run_settings, 'data/run_settings_moreRQ1_2.csv')

###########
## Read run settings 
###########

run_settings <- read.csv('data/single_run_settings.csv', header = F, row.names = 1)

# define elephant ID
ID <- run_settings[[1]]

# define week to test 
week <- run_settings[[2]]

# define pseudo-absence path generator method --> Choices are 1) random_path_custom_distr 2) random_path_buffer_point 3) random_step
pseudo_abs_method <- run_settings[[3]]

# set downscaling parameter 
downscaling_setting <- run_settings[[4]]
downscaling_model <- run_settings[[5]]

# define run filepath 
run_filepath <- paste0('data/', ID, '/', week, '/')

print(paste('Now extracting covariates for elephant', ID, 'of week', week, '...'))

###########
## Extract covariates or each step from the MODIS images 
###########
# load function
source('functions_elephant_ssf/4_a_extractingCovariates.R')

# load necessary packages 
if(!('terra') %in% installed.packages()){install.packages('terra')}
library(terra)
if(!('amt') %in% installed.packages()){install.packages('amt')}
library(amt)

# run function
# Note: Downscaling term makes the distinction between MODIS dataset that was retrieved without running any downscaling scripts (downscaling = NULL), 
#         MODIS 250m retrieved through downscaling JN script (downscaling == F), MODIS 30m generated through downscaling in R (downscaling == T).
# Note: Specify downscaling model if downscaling = T --> should match last section of name of MODIS 30m folder 
loadAndExtractCovariates(run_filepath, ID, week, random_data_method = pseudo_abs_method, 
                         downscaling = downscaling_setting, downscaling_model = downscaling_model, output_directory = 'data/')

print(paste('(DONE) Extracting covariates for elephant', ID, 'of week', week))







input_directory <- run_filepath
random_data_method <- pseudo_abs_method
downscaling <- downscaling_setting
output_directory <- 'data/'
# load step dataset RDS 
step_dataset <- readRDS(paste0(input_directory, '1_b1_all_steps_', random_data_method, '.RDS'))

# add empty columns for covariates 
# source: https://sparkbyexamples.com/r-programming/add-empty-column-to-dataframe-in-r/
empty_cols <- c('ndvi_10', 'ndvi_50', 'ndvi_90', 'ndvi_sd', 'ndvi_rate_10', 'ndvi_rate_50', 'ndvi_rate_90', 'ndvi_rate_sd')
step_dataset[, empty_cols] <- NA

# retrieve and stack all generated MODIS images together
if(downscaling == 'NULL'){
  modis_directory <- paste0(input_directory, '3_a1_modis_images_', random_data_method, '/')
  
  output_suffix <- ''
  
}else if(downscaling == T){
  modis_directory <- paste0(input_directory, '3_g1_downscaled_modis_images_30m_', downscaling_model, '/')
  
  output_suffix <- '_downscaling_modis_30m'
  
}else if(downscaling == F){
  modis_directory <- paste0(input_directory, '3_b1_modis_images_downscaling_', random_data_method, '/')
  
  output_suffix <- '_downscaling_modis_250m'
  
}else{stop('Incorrect term set for downscaling parameter. Should be one of the following: NULL, T, F.')}

modis_images <- rast(list.files(modis_directory, pattern = glob2rx('2*.tif'), full.names = T))
modis_images
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

# create output filepath 
output_filepath <- paste0(output_directory, ID, '/', week, '/')

# create data directory if it does not yet exist
if(!dir.exists(output_filepath)){dir.create(output_filepath, recursive = T)}

# save this dataframe for now since took so long to generate 
#write.csv(step_dataset, paste0('output/elephant_etosha/LA2_', as.character(w), '_step_dataset.csv'))
write.csv(step_dataset, paste0(output_filepath, '4_a1_cov_resp_dataset_', random_data_method, output_suffix, '.csv'))