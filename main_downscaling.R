## MSc Thesis 
## Jacotte Monroe 

## Run main steps and functions

# Packages 
if(!('terra') %in% installed.packages()){install.packages('terra')}
library(terra)
if(!('sf') %in% installed.packages()){install.packages('sf')} # to read rasters
library(sf)
if(!('CAST') %in% installed.packages()){install.packages('CAST')} # to read rasters
library(CAST)
if(!('caret') %in% installed.packages()){install.packages('caret')} # to read rasters
library(caret)
if(!('car') %in% installed.packages()){install.packages('car')} 
library(car)
if(!('ranger') %in% installed.packages()){install.packages('ranger')} # fit Random Forest regression 
library(ranger)

if(!('ggplot2') %in% installed.packages()){install.packages('ggplot2')} # to plot timeseries
library(ggplot2)

library(tidyterra)




# read run settings 
run_table <- read.csv('data/run_settings_downscaling.csv', row.names = 1) #[-4,]
#row.names(run_table) <- 1:nrow(run_table)

# ID <- run_table$ID[i]
# week <- run_table$week[i]
# pseudo_abs_method <- run_table$pseudo_abs_method[i]

for(i in 1:nrow(run_table)){
  
  ###########
  ## Read run settings 
  ###########
  
  ## mosaic the landsat scenes together to create the large extent image
  # will need to loop through dates (could do this after creating the LUT?)
  
  # enter settings
  ID <- run_table$ID[i]
  week <- run_table$week[i]
  pseudo_abs_method <- run_table$pseudo_abs_method[i]
  
  # define run filepath 
  run_filepath <- paste0('data/', ID, '/', week, '/')
  
  # define landsat filepath 
  landsat_filepath <- paste0(run_filepath, '3_b2_landsat_images_downscaling_', pseudo_abs_method, '/')
  
  # define modis filepath 
  modis_filepath <- paste0(run_filepath, '3_b1_modis_images_downscaling_', pseudo_abs_method, '/')
  
  # suffix 
  suffix <- '_selection'
  
  
  print(paste('(START) Starting to run', ID, week))
  
  
  ###########
  ## Stitch Landsat scenes back together 
  ###########
  
  # load function 
  source('functions_elephant_ssf/3_b_stitchingScenes.R')
  
  # necessary packages 
  # if(!('terra') %in% installed.packages()){install.packages('terra')}
  # library(terra)
  
  # list files in landsat directory 
  l_files <- list.files(landsat_filepath, pattern = glob2rx('*_stitched.tif'))
  
  # run function if stitched Landsat images don't already exist (only stitch once)
  if(length(l_files) == 0){
    stitchScenes(run_filepath)
  }
  
  
  print(paste('(DONE) Stitching scenes for', ID, week))
  
  
  ###########
  ## create LUT to match each MODIS image to the closest Landsat image 
  ###########
  
  # load function 
  source('functions_downscaling/creatingLUT.R')
  
  # necessary packages 
  # if(!('terra') %in% installed.packages()){install.packages('terra')}
  # library(terra)
  
  # run function 
  createLUT(modis_filepath, landsat_filepath, ID, week, output_directory = 'data/')
  
  
  # load LUT
  LUT <- readRDS(paste0(run_filepath, '3_c1_MODISLandsatLUT.RData'))
  
  
  print(paste('(DONE) Creating LUT for', ID, week))
  
  
  
  
  ###########
  ## create covariates and response dataset (combined Landsat bands at 250m with MODIS NDVI 250m)
  ###########
  
  # load function 
  source('functions_elephant_ssf/3_d_creatingCovariatesSets.R')
  
  # necessary packages 
  # if(!('terra') %in% installed.packages()){install.packages('terra')}
  # library(terra)
  
  # define band combinations that want for band ratios 
  b_ratios <- list(c('B3', 'B2'), c('B5', 'B4'), c('B7', 'B6'))
  
  # run function for each image from the LUT
  for(i in 1:nrow(LUT)){
    createCovariatesResponseSet(modis_filepath, landsat_filepath, ID, week, LUT[i,], 
                                band_combinations = b_ratios, output_filename_suffix = suffix)
  }
  
  
  print(paste('(DONE) Creating covariate training set for', ID, week))
  
  
  
  ###########
  ## sample points for training the model
  ###########
  
  # load function 
  source('functions_elephant_ssf/3_e_samplingTrainingPoints.R')
  
  # necessary packages 
  # if(!('terra') %in% installed.packages()){install.packages('terra')}
  # library(terra)
  # if(!('sf') %in% installed.packages()){install.packages('sf')} # to read rasters
  # library(sf)
  # if(!('CAST') %in% installed.packages()){install.packages('CAST')} # to read rasters
  # library(CAST)
  
  # run function for each image from the LUT
  for(i in 1:nrow(LUT)){
    modis_date <- LUT$modis_date[i]
    sampleTrainingPoints(run_filepath, ID, week, modis_date, input_suffix = suffix, output_suffix = suffix)
  }
  
  
  print(paste('(DONE) Samling training points for', ID, week))
  
  
  
  ###########
  ## fit regressions with k-fold cross validation 
  ###########
  
  # load function 
  source('functions_elephant_ssf/3_f_fittingRegressions.R')
  
  # # necessary packages 
  # if(!('caret') %in% installed.packages()){install.packages('caret')} # to read rasters
  # library(caret)
  # if(!('car') %in% installed.packages()){install.packages('car')} 
  # library(car)
  # if(!('terra') %in% installed.packages()){install.packages('terra')}
  # library(terra)
  # if(!('ranger') %in% installed.packages()){install.packages('ranger')} # fit Random Forest regression 
  # library(ranger)
  # if(!('CAST') %in% installed.packages()){install.packages('CAST')} # to read rasters
  # library(CAST)
  
  # run function to generate full models
  for(i in 1:nrow(LUT)){
    modis_date <- LUT$modis_date[i]
    fitRegression(run_filepath, ID, week, modis_date, input_suffix = suffix, subset_predictors = NULL, 
                  feature_selection = F, regression_type = 'ranger', output_suffix = suffix)
  }
  
  
  print(paste('(DONE) Fitting Random Forest regression for', ID, week))
  
  
  
  ###########
  ## Create covariate (Landsat8) raster for predicting MODIS 30m
  ###########
  
  # load function 
  source('functions_elephant_ssf/3_d_creatingCovariatesSets.R')
  
  # necessary packages 
  # if(!('terra') %in% installed.packages()){install.packages('terra')}
  # library(terra)
  
  # get list of landsat image names 
  landsat_image_list <- unique(LUT$closest_landsat_image)
  
  # define band combinations that want for band ratios 
  b_ratios <- list(c('B3', 'B2'), c('B5', 'B4'), c('B7', 'B6'))
  
  # run function for each landsat image 
  for(file in landsat_image_list){
    createPredictionCovariatesSet(landsat_filepath, file, ID, week, b_ratios)
  }
  
  
  print(paste('(DONE) Creating covariate prediction set for', ID, week))
  
  
  
  ###########
  ## Predict MODIS 30m
  ###########
  
  # load function 
  source('functions_elephant_ssf/3_g_predictingDownscaledModis.R')
  
  # necessary packages
  # if(!('terra') %in% installed.packages()){install.packages('terra')}
  # library(terra)
  # if(!('ggplot2') %in% installed.packages()){install.packages('ggplot2')}
  # library(ggplot2)
  
  # specify model type and suffix if any 
  model_type <- 'ranger_full'
  
  # run the function for each entry from LUT 
  for (i in 1:nrow(LUT)) {
    entry <- LUT[i,]
    predictDownscaledModis(run_filepath, modis_filepath, ID, week, entry, model_type, 
                           input_suffix = suffix, output_suffix = suffix)
  }
  
  
  print(paste('(DONE) Predicting MODIS 30m for', ID, week))
  
  
  
  ###########
  ## Create mean MODIS NDVI 30m raster 
  ###########
  
  # load function 
  source('functions_elephant_ssf/3_g_predictingDownscaledModis.R')
  
  # necessary packages 
  # if(!('terra') %in% installed.packages()){install.packages('terra')}
  # library(terra)
  # 
  # define modis 30 filepath 
  modis_30_filepath <- paste0(run_filepath, '3_g1_downscaled_modis_images_30m_ranger_full_selection/')
  
  # run function 
  createMeanRaster(modis_30_filepath)
  
  print(paste('(DONE) Creating a mean MODIS 30m NDVI raster for', ID, week))
  print(paste('(COMPLETE) Done running', ID, week, '!'))
  
  
}

















###########
## Read run settings 
###########

## mosaic the landsat scenes together to create the large extent image
# will need to loop through dates (could do this after creating the LUT?)

# enter settings
ID <- 'LA14'
week <- 2260
pseudo_abs_method <- 'random_path_custom_distr'

# define run filepath 
run_filepath <- paste0('data/', ID, '/', week, '/')

# define landsat filepath 
landsat_filepath <- paste0(run_filepath, '3_b2_landsat_images_downscaling_', pseudo_abs_method, '/')

# define modis filepath 
modis_filepath <- paste0(run_filepath, '3_b1_modis_images_downscaling_', pseudo_abs_method, '/')

# load LUT
LUT <- readRDS(paste0(run_filepath, '3_c1_MODISLandsatLUT.RData'))
LUT <- LUT[2:14,]





###########
## Stitch Landsat scenes back together 
###########

# load function 
source('functions_elephant_ssf/3_b_stitchingScenes.R')

# necessary packages 
if(!('terra') %in% installed.packages()){install.packages('terra')}
library(terra)

# list files in landsat directory 
l_files <- list.files(landsat_filepath, pattern = glob2rx('*_stitched.tif'))

# run function if stitched Landsat images don't already exist (only stitch once)
if(length(l_files) == 0){
  stitchScenes(landsat_filepath)
}




###########
## create LUT to match each MODIS image to the closest Landsat image 
###########

# load function 
source('functions_downscaling/creatingLUT.R')

# necessary packages 
if(!('terra') %in% installed.packages()){install.packages('terra')}
library(terra)

# run function 
createLUT(modis_filepath, landsat_filepath, ID, week, output_directory = 'data/')




###########
## create covariates and response dataset (combined Landsat bands at 250m with MODIS NDVI 250m)
###########

# load function 
source('functions_elephant_ssf/3_d_creatingCovariatesSets.R')

# necessary packages 
if(!('terra') %in% installed.packages()){install.packages('terra')}
library(terra)

# define band combinations that want for band ratios 
b_ratios <- list(c('B3', 'B2'), c('B5', 'B4'), c('B7', 'B6'))

# run function for each image from the LUT
for(i in 1:nrow(LUT)){
  createCovariatesResponseSet(modis_filepath, landsat_filepath, ID, week, LUT[i,], 
                              band_combinations = b_ratios, output_filename_suffix = '_selection')
}



###########
## sample points for training the model
###########

# load function 
source('functions_elephant_ssf/3_e_samplingTrainingPoints.R')

# necessary packages 
if(!('terra') %in% installed.packages()){install.packages('terra')}
library(terra)
if(!('sf') %in% installed.packages()){install.packages('sf')} # to read rasters
library(sf)
if(!('CAST') %in% installed.packages()){install.packages('CAST')} # to read rasters
library(CAST)

# run function for each image from the LUT
for(i in 1:nrow(LUT)){
  modis_date <- LUT$modis_date[i]
  sampleTrainingPoints(run_filepath, ID, week, modis_date, input_suffix = '_selection', output_suffix = '_selection')
}

# extra (visualize sample points distribution vs dataset distribution)
# hist(dataset$ndvi, breaks = 30)
#hist(sample_points$ndvi, breaks = 30)
#max(sample_points$ndvi, na.rm = T)
#plot(dataset$ndvi)
#plot(sample_points, add = T)
# dev.off()




###########
## fit regressions with k-fold cross validation 
###########

# load function 
source('functions_elephant_ssf/3_f_fittingRegressions.R')

# necessary packages 
if(!('caret') %in% installed.packages()){install.packages('caret')} # to read rasters
library(caret)
if(!('car') %in% installed.packages()){install.packages('car')} 
library(car)
if(!('terra') %in% installed.packages()){install.packages('terra')}
library(terra)
if(!('ranger') %in% installed.packages()){install.packages('ranger')} # fit Random Forest regression 
library(ranger)
if(!('CAST') %in% installed.packages()){install.packages('CAST')} # to read rasters
library(CAST)

# run function to generate full models
for(i in 1:nrow(LUT)){
  modis_date <- LUT$modis_date[i]
  # fitRegression(run_filepath, ID, week, modis_date, subset_predictors = NULL, 
  #               feature_selection = F, regression_type = 'lm')
  # fitRegression(run_filepath, ID, week, modis_date, subset_predictors = NULL, 
  #               feature_selection = F, regression_type = 'cubist')
  fitRegression(run_filepath, ID, week, modis_date, input_suffix = '_selection', subset_predictors = NULL, 
                feature_selection = F, regression_type = 'ranger', output_suffix = '_selection')
}


# # run function to generate FFS models
# for(i in 1:nrow(LUT)){
#   modis_date <- LUT$modis_date[i]
#   fitRegression(run_filepath, ID, week, modis_date, subset_predictors = NULL,
#                 feature_selection = T, regression_type = 'lm')
#   #fitRegression(run_filepath, ID, week, modis_date, input_dataset_suffix = '_noNegs',
#    #             subset_predictors = NULL, feature_selection = T, regression_type = 'cubist')
#   # note: can't run FFS with RF --> takes too long
#   #fitRegression(run_filepath, ID, week, modis_date, input_dataset_suffix = '_noNegs',
#    #             subset_predictors = NULL, feature_selection = T, regression_type = 'ranger')
# }




###########
## result analysis and interpretation 
###########

# see script for that (3bis_DownscalingDataExploration) --> not running it atm because its more exploration of results 
# change this later dep on what want to show 




###########
## Create covariate (Landsat8) raster for predicting MODIS 30m
###########

# load function 
source('functions_elephant_ssf/3_d_creatingCovariatesSets.R')

# necessary packages 
if(!('terra') %in% installed.packages()){install.packages('terra')}
library(terra)

# get list of landsat image names 
landsat_image_list <- unique(LUT$closest_landsat_image)

# define band combinations that want for band ratios 
b_ratios <- list(c('B3', 'B2'), c('B5', 'B4'), c('B7', 'B6'))

# run function for each landsat image 
for(file in landsat_image_list){
  createPredictionCovariatesSet(landsat_filepath, file, ID, week, b_ratios)
}




###########
## Predict MODIS 30m
###########

# load function 
source('functions_elephant_ssf/3_g_predictingDownscaledModis.R')

# necessary packages
if(!('terra') %in% installed.packages()){install.packages('terra')}
library(terra)
if(!('ggplot2') %in% installed.packages()){install.packages('ggplot2')}
library(ggplot2)

# specify model type and suffix if any 
model_type <- 'ranger_full'
suffix <- '_selection'

# run the function for each entry from LUT 
for (i in 1:nrow(LUT)) {
  entry <- LUT[i,]
  predictDownscaledModis(run_filepath, modis_filepath, ID, week, entry, model_type, 
                         input_suffix = suffix, output_suffix = suffix)
}




###########
## Create mean MODIS NDVI 30m raster 
###########

# load function 
source('functions_elephant_ssf/3_g_predictingDownscaledModis.R')

# necessary packages 
if(!('terra') %in% installed.packages()){install.packages('terra')}
library(terra)

# define modis 30 filepath 
modis_30_filepath <- paste0(run_filepath, '3_g1_downscaled_modis_images_30m_ranger_full_selection/')

# run function 
createMeanRaster(modis_30_filepath)































































# create downscaling model 
source('functions_downscaling/generatingDownscalingModels.R')

for(i in 1:nrow(LUT)){
  generateDownscalingModels(paste0(path_modis, '/', LUT$modis_image[i]), LUT$modis_date[i], 
                            paste0(path_landsat, '/', LUT$closest_landsat_image[i]), 
                            check_multicolinearity = F, multicolinearity_threshold = 0.8, 
                            fit_rf_regression = T, output_directory = 'output/models/2300/8d/')
}







list_images <- landsat_filenames_list[grep('20130504', landsat_filenames_list)]
list_images
t <- rast(paste0(landsat_filepath, 'LC08_179073_4_20130504.tif'))
t
plot(t[[4]])


m <- rast('data/LA14/2260/3_b1_modis_images_downscaling_random_path_custom_distr/2013_04_18.tif')
m
plot(m[[4]])

m2 <- rast('data/modis/2300/2014_01_23.tif')
m2
plot(is.na(m2[[2]]))

m3 <- rast('data/modis/8d/2300/2014_01_17.tif')
m3
plot(m3[[2]])

l2 <- rast('data/LA26/2260/3_b2_landsat_images_downscaling_random_path_custom_distr/TL_TL/LC08_179074_20130504.tif')
l2
plot(is.na(l2[[4]]))

tl <- rast(paste0('data/LA14/2260/3_b2_landsat_images_downscaling_random_path_custom_distr/', 'TL_TL/', landsat_filename))

tr <- rast(paste0('data/LA14/2260/3_b2_landsat_images_downscaling_random_path_custom_distr/', 'top_right_image/', landsat_filename))
br <- rast(paste0('data/LA14/2260/3_b2_landsat_images_downscaling_random_path_custom_distr/', 'bottom_right_image/', landsat_filename))
bl <- rast(paste0('data/LA14/2260/3_b2_landsat_images_downscaling_random_path_custom_distr/', 'bottom_left_image/', landsat_filename))


l <- rast('data/LA14/2260/3_b2_landsat_images_downscaling_random_path_custom_distr/LC08_179073_20130418.tif')
l
plot(is.na(l[[1]]))
plot(l[[1]])

ndvi_data <- l[[4]]
names(ndvi_data) <- 'ndvi'
ID <- 'LA14'
week <- 2260

ndvi_data

modis_ndvi_map <- ggplot() +
  geom_spatraster(data = ndvi_data, aes(fill = ndvi), show.legend = T) +
  scale_fill_terrain_c(name = 'NDVI')

# read step dataset
dat <- readRDS('data/LA14/2260/1_b1_all_steps_random_path_custom_distr.RDS')
write.csv(dat, 'data/LA14/2260/temp_all_steps.csv')
dat <- read.csv('data/LA14/2260/temp_all_steps.csv', row.names = 1)
dat$random_id_ <- as.factor(dat$random_id_)

# add title and theme to plot
mov_map <- modis_ndvi_map +
  labs(title = 'Elephant Movement on NDVI', subtitle = paste(ID, week), x = "Longitude", y = "Latitude") +
  theme_minimal()

# plot presence and pseudo-absence paths on basemap by looping for each path 
# source: https://stackoverflow.com/questions/9206110/using-png-function-not-working-when-called-within-a-function
for(i in 1:length(unique(dat$burst_))){
  for(j in 1:length(unique(dat$random_id_))){
    # source: https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html
    mov_map <- mov_map + geom_path(data = dat[dat$case_ == F & dat$burst_ == i & dat$random_id_ == j,], aes(x = x1_, y = y1_), color = 'grey40', linewidth = 0.4, linetype = 2, show.legend = F)
  }
  mov_map <- mov_map + geom_path(data = dat[dat$case_ == T & dat$burst_ == i,], aes(x = x1_, y = y1_), color = 'black', linewidth = 0.5, show.legend = F)
} 

png('data/LA14/2260/random_image.png')
mov_map
dev.off()




############################################################################
############### generate Landsat and MODIS images in JN ####################
############## once have images i can start this script ####################
############################################################################
ID <- 'LA14'
week <- 2260

# define run filepath 
run_filepath <- paste0('data/', ID, '/', week, '/')

path_modis <- paste0(run_filepath, '3_b1_modis_images_downscaling_random_path_custom_distr/')
path_landsat <- paste0(run_filepath, '3_b2_landsat_images_downscaling_random_path_custom_distr/')

# create LUT from Landsat and MODIS images in data folders
# matches a Landsat image to each MODIS image (nearest Landsat image)
source('functions_downscaling/creatingLUT.R')

createLUT(path_modis, path_landsat, ID, week, output_directory = 'data/')




# load LUT
LUT <- readRDS(paste0(run_filepath, '3_c1_MODISLandsatLUT.RData'))
#LUT <- LUT[1:30,]

# create downscaling model 
source('functions_downscaling/generatingDownscalingModels.R')

for(i in 1:nrow(LUT)){
  generateDownscalingModels(paste0(path_modis, '/', LUT$modis_image[i]), LUT$modis_date[i], 
                            paste0(path_landsat, '/', LUT$closest_landsat_image[i]), 
                            check_multicolinearity = F, multicolinearity_threshold = 0.8, 
                            fit_rf_regression = T, output_directory = 'output/models/2300/8d/')
}

# predict modis ndvi 30m
source('functions_downscaling/predictingDownscaledModis.R')

for(i in 1:nrow(LUT)){
  predictMODIS30(LUT$modis_image[i], LUT$modis_date[i], LUT$closest_landsat_image[i], model_type = 'rf', 
                 input_modis_directory = path_modis, input_landsat_directory = path_landsat, 
                 input_model_directory = 'output/models/2300/8d/', output_directory = paste0('output/30m_images/8d/', week, '/'))
}

# calculate MODIS NDVI 250m [other functions should be adapted so that they retrieve the layer]
for(i in 1:nrow(LUT)){
  # load
  modis_250 <- rast(paste0('data/modis/', LUT$modis_image[i]))
  # rename layers
  names(modis_250) <- c('B2', 'B1')
  # calculate ndvi
  modis_250_ndvi <- (modis_250$B2 - modis_250$B1)/(modis_250$B2 + modis_250$B1)
  # save raster
  writeRaster(modis_250_ndvi, paste0('data/modis/ndvi_250m/', LUT$modis_date[i], '_ndvi_250m.tif'), overwrite = T)
}

## create NDVI 250m and 30m timeseries 

# get list of files in each folder 
list_250 <- list.files('data/modis/ndvi_250m', full.names = T)[85:119]
list_30 <- list.files('output/30m_images/', pattern = glob2rx('2014*ndvi_30m.tif'), full.names = T) #[85:119]

# stack rasters into one for each resolution
modis_ndvi_250_stacked <- rast(list_250)
modis_ndvi_30_stacked <- rast(list_30)

# add dates to each layer of each raster
names(modis_ndvi_250_stacked) <- LUT$modis_date
names(modis_ndvi_30_stacked) <- LUT$modis_date

# retrieve average
# source: https://gis.stackexchange.com/questions/457402/how-to-calculate-mean-values-of-all-cells-of-raster-files-in-r
mean_250 <- global(modis_ndvi_250_stacked, 'mean', na.rm = T)
mean_30 <- global(modis_ndvi_30_stacked, 'mean', na.rm = T)

mean_250[mean_250 == 0] <- NA
mean_30[mean_30 == 0] <- NA

# retrieve standard deviation 
std_250 <- global(modis_ndvi_250_stacked, 'std', na.rm = T)
std_30 <- global(modis_ndvi_30_stacked, 'std', na.rm = T)

# calculate coef of variation 
cv_250 <- std_250/mean_250
names(cv_250) <- c('cv')

cv_30 <- std_30/mean_30
names(cv_30) <- c('cv')

# add column for name of dataset
mean_250$data <- 'ndvi_250'
mean_30$data <- 'ndvi_30'

cv_250$data <- 'ndvi_250'
cv_30$data <- 'ndvi_30'

# create dataframes of timeseries
ts <- rbind.data.frame(cv_250, cv_30)  #(mean_250, mean_30)

# add dates in date format
ts$date <- as.Date(rownames(ts))

# turn any pixel value of exactly 0 into NA
ts[ts == 0] <- NA
ts[is.na(ts)] <- NA

# plot timeseries 
# source: https://www.statology.org/plot-time-series-in-r/
# source: https://www.datanovia.com/en/blog/how-to-create-a-ggplot-with-multiple-lines/
# source: https://stackoverflow.com/questions/39201856/adding-legends-to-multiple-line-plots-with-ggplot
p <- ggplot(ts, aes(x = date, y = cv)) +
  geom_line(aes(color = data)) +
  scale_color_manual(values=c("turquoise4", "orange")) + 
  scale_x_date(date_labels = "%d %b %Y") + 
  labs(x = 'Date', y = 'Coefficient of Variation', title = 'Coefficient of Variation MODIS NDVI from Jan 14th to Feb 17th 2014', color = 'Datasets') +
  theme_minimal()
p




for(i in 1:nrow(LUT)){
  modis_date <- LUT$modis_date[i]
  
  # retrieve landsat 30m data
  l_30 <- rast(paste0('data/l8/',LUT$closest_landsat_image[i]))
  names(l_30) <- c('B2', 'B3', 'B4', 'B5', 'B6', 'B7')
  
  # remove faulty values (pixels with value 0)
  l_30[l_30 == 0] <- NA
  
  ## compare predictions
  # calc ndvi
  modis_250_ndvi <- rast(paste0('data/modis/ndvi_250m/', modis_date, '_ndvi_250m.tif'))
  
  # calculate landsat 8 ndvi 
  l_ndvi <- (l_30$B5 - l_30$B4)/(l_30$B5 + l_30$B4)
  
  modis_30_pred <- rast(paste0('output/30m_images/', modis_date, '_ndvi_30m.tif'))
  
  # visualize 
  png(paste0('output/30m_images/', modis_date, '_raster_comparison.png'))
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
  png(paste0('output/30m_images/', modis_date, '_errors.png'))
  par(mfrow = c(1,2))
  plot(error, main = 'Error Between True and Predicted NDVI')
  plot(abs_error, main = 'Absolute Error Between True and Predicted NDVI')
  dev.off()
  
}










modis_date <- '2013-04-19'


# retrieve landsat 30m data
l_30 <- rast('data/l8/LC08_179073_20130418.tif')
names(l_30) <- c('B2', 'B3', 'B4', 'B5', 'B6', 'B7')

# remove faulty values (pixels with value 0)
l_30[l_30 == 0] <- NA


## compare predictions
# calc ndvi
modis_250_ndvi <- rast('data/modis/ndvi_250m/2013-04-19_ndvi_250m.tif')

# calculate landsat 8 ndvi 
l_ndvi <- (l_30$B5 - l_30$B4)/(l_30$B5 + l_30$B4)

modis_30_pred <- rast('output/30m_images/2013-04-19_ndvi_30m.tif')

# visualize 
png(paste0('output/30m_images/', modis_date, '_raster_comparison.png'))
par(mfrow = c(1,1))
plot(l_ndvi, main = 'True Landsat 8 NDVI 30m')
plot(modis_30_pred, main = 'RF Predicted MODIS NDVI 30m')
plot(modis_250_ndvi, main = 'True MODIS NDVI 250m')
dev.off()

modis_250_pred <- resample(modis_30_pred, modis_250_ndvi, method = 'average')

# mask any NAs 
modis_250_pred <- mask(modis_250_pred, any(is.na(modis_250_ndvi)), maskvalues = T)

# calculate error
error <- modis_250_ndvi - modis_250_pred
abs_error <- abs(modis_250_ndvi- modis_250_pred)
names(abs_error) <- c('absolute_error')

# visualize
png(paste0('output/30m_images/', modis_date, '_errors.png'))
par(mfrow = c(1,2))
plot(error, main = 'Error Between True and Predicted NDVI')
plot(abs_error, main = 'Absolute Error Between True and Predicted NDVI')
dev.off()

library(ggplot2)
if(!('tidyterra') %in% installed.packages()){install.packages('tidyterra')} # for mapping spatraster
library(tidyterra)
ndvi_map <- ggplot() + 
  geom_spatraster(data = abs_error, aes(fill = absolute_error), show.legend = T) + 
  scale_fill_terrain_c(name = 'Abs Error') +
  labs(title = "Absolute Error Between True and Predicted NDVI", x = "Longitude", y = "Latitude") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, size = 20, margin=margin(0,0,-10,0)), 
        legend.box.margin=margin(-10,-10,-10,-10), 
        text = element_text(size=15))

ndvi_map





sssssssssssssssssmodis_ndvi_map


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
png(paste0('output/30m_images/', modis_date, '_errors.png'))
par(mfrow = c(1,2))
plot(error, main = 'Error Between True and Predicted NDVI')
plot(abs_error, main = 'Absolute Error Between True and Predicted NDVI')
dev.off()



rl <- rast()
