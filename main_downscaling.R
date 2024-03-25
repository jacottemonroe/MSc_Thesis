## MSc Thesis 
## Jacotte Monroe 

## Run main steps and functions

# Packages 
if(!('terra') %in% installed.packages()){install.packages('terra')} # to read rasters
library(terra)

if(!('ggplot2') %in% installed.packages()){install.packages('ggplot2')} # to plot timeseries
library(ggplot2)

library(tidyterra)



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




###########
## Stitch Landsat scenes back together 
###########

# define landsat filepath 
landsat_filepath <- paste0(run_filepath, '3_b2_landsat_images_downscaling_', pseudo_abs_method, '/')

# list directories of scenes 
scene_directories <- list.dirs(landsat_filepath, full.names = F)[-1]

# get filenames (from LUT?)
landsat_filenames_list <- list.files(paste0(landsat_filepath, 'BL_BL'))

# loop through the different landsat image dates 
for(landsat_filename in landsat_filenames_list){
  # loop through the directories to load the scenes for the corresponding image date and append them to a list
  scenes <- list()
  for(sc_folder in scene_directories){
    # load scene to align 
    sc <- rast(paste0(landsat_filepath, sc_folder, '/', landsat_filename))
    scenes <- append(scenes, list(sc))
  }
  
  # stitch the scenes together to make one large landsat 8 image
  # source: # source: https://gis.stackexchange.com/questions/224781/merge-rasters-with-different-origins-in-r
  # source: https://rdrr.io/cran/terra/man/merge.html
  l8_mosaic <- do.call(merge, scenes)
  
  # save new image 
  writeRaster(l8_mosaic, paste0(landsat_filepath, landsat_filename), overwrite = T)
  
}

# remove scene folders and files since now have composite image
# source: https://stackoverflow.com/questions/28097035/how-to-remove-a-directory-in-r
unlink(paste0(landsat_filepath, scene_directories), recursive = T)

# get all dates from file names 
# source: https://stackoverflow.com/questions/17215789/extract-a-substring-according-to-a-pattern
l_dates <- sub('.*_17.*_', '', landsat_filenames_list)
l_dates <- unique(sub('.tif', '', l_dates))

# select images for each date and mosaic them 
for(date in l_dates){
  # select file names containing the date 
  # source: https://stackoverflow.com/questions/69759984/how-can-i-subset-a-list-in-r-by-extracting-the-elements-that-contain-a-string
  list_images <- landsat_filenames_list[grep(date, landsat_filenames_list)]
  
  # create empty list to store the rasters
  scenes <- list()
  
  for(image in list_images){
    sc <- rast(paste0(landsat_filepath, image))
    scenes <- append(scenes, list(sc))
  }
  
  # stitch the scenes together to make one large landsat 8 image
  # source: # source: https://gis.stackexchange.com/questions/224781/merge-rasters-with-different-origins-in-r
  # source: https://rdrr.io/cran/terra/man/merge.html
  l8_mosaic <- do.call(merge, scenes)
  
  # save new image 
  writeRaster(l8_mosaic, paste0(landsat_filepath, 'LC08_179073_4_', date, '.tif'), overwrite = T)
}

# remove scene files since now have composite image
list_files <- list.files(landsat_filepath)
list_files <- list_files[grep('*3_4_2*', list_files, invert = T)]
list_files
file.remove(paste0(landsat_filepath, list_files))




###########
## create LUT to match each MODIS image to the closest Landsat image 
###########

modis_filepath <- paste0(run_filepath, '3_b1_modis_images_downscaling_random_path_custom_distr/')

# create LUT from Landsat and MODIS images in data folders
# matches a Landsat image to each MODIS image (nearest Landsat image)
source('functions_downscaling/creatingLUT.R')

createLUT(modis_filepath, landsat_filepath, ID, week, output_directory = 'data/')




###########
## create covariates and response dataset
###########

# load LUT
LUT <- readRDS(paste0(run_filepath, '3_c1_MODISLandsatLUT.RData'))

LUT <- LUT[1,]


# read modis and landsat images 
library(terra)
modis_250 <- rast(paste0(modis_filepath, LUT$modis_image))
l_30 <- rast(paste0(landsat_filepath, LUT$closest_landsat_image))

# rename layer names to corresponding bands 
names(modis_250) <- c('B1', 'B2', 'NDVI')
names(l_30) <- c('B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7')

# removing outlier pixel values --> negative NDVI or reflectance values 
# all pixels with value 0 changed to NA (because originally masked pixels in GEE)
# all modis ndvi pixels with value below 0 turned into NA
l_30[l_30 <= 0] <- NA
modis_250$NDVI[modis_250$NDVI < 0] <- 0

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

# save dataset
saveRDS(dataset, paste0(run_filepath,'3_c2_', LUT$modis_date,'_dataset_noNegs.RData'))



#dataset <- readRDS(paste0(run_filepath,'3_c2_', LUT$modis_date,'_dataset_noNegs.RData'))





################ set up cross validation strategy #############################

# create grid object from an empty raster
# source: https://gis.stackexchange.com/questions/431873/creating-regular-sampling-grid-with-specific-distance-between-points-using-r
grid <- rast(extent = ext(dataset), crs = crs(dataset), nrow = 3, ncol = 3)
values(grid) <- 1:9
names(grid) <- 'grid_ID'

# resample the grid raster to the resolution of the dataset
grid <- resample(grid, dataset, method = 'average')

# stack dataset and grid rasters to add the grid IDs to the dataset 
dataset <- c(dataset, grid)

# sample from the dataset raster 
# source: https://www.rdocumentation.org/packages/terra/versions/1.7-71/topics/spatSample
sample_points <- spatSample(dataset, size = 5000, method = 'regular', as.points = T) 

# compare distribution of NDVI values between raster dataset and sampled points
# hist(dataset$ndvi, breaks = 30)
hist(sample_points$ndvi, breaks = 30)
max(sample_points$ndvi, na.rm = T)
plot(dataset$ndvi)
plot(sample_points, add = T)
# dev.off()

# transform SpatVector object into sf object 
if(!('sf') %in% installed.packages()){install.packages('sf')} # to read rasters
library(sf)
sample_points <- st_as_sf(sample_points, crs = st_crs_crs(dataset))

# split the points into training and test sets 
# note: one index value/fold (ID = 9) is withheld for testing the trained model 
# training_points <- sample_points[sample_points$grid_ID < 9,]
# testing_points <- sample_points[sample_points$grid_ID == 9,]

# create a list of folds for cross-validation 
# each list has the indices of points to include or exclude for each CV iteration
# source: https://www.rdocumentation.org/packages/CAST/versions/0.2.1/topics/CreateSpacetimeFolds
if(!('CAST') %in% installed.packages()){install.packages('CAST')} # to read rasters
library(CAST)
cv_folds <- CreateSpacetimeFolds(sample_points, spacevar = 'grid_ID', k = 9)




###########
## fit linear regression with k-fold cross validation 
###########

# retrieve covariates from sample points and store in a new dataframe 
covs <- data.frame(sample_points)
covs <- covs[, grep('B.*', colnames(covs))]

# fit model with full range of predictors 
# source: https://www.statology.org/k-fold-cross-validation-in-r/
# source: https://cran.r-hub.io/web/packages/CAST/vignettes/CAST-intro.html
if(!('caret') %in% installed.packages()){install.packages('caret')} # to read rasters
library(caret)
lr_model_trained <- train(x = covs, y = sample_points$ndvi, method = 'lm', 
                          trControl = trainControl(method = 'cv', index = cv_folds$index))

# look at model results 
lr_model_trained
lr_model_trained$results
lr_model_trained$finalModel
lr_model_trained$resample

plot(varImp(lr_model_trained))

# check VIF 
# source: https://stackoverflow.com/questions/63251868/variance-inflation-vif-for-glm-caret-model-in-r
if(!('car') %in% installed.packages()){install.packages('car')} 
library(car)
v <- sort(vif(lr_model_trained$finalModel))
v

saveRDS(lr_model_trained, paste0(run_filepath, '3_c3_linear_model_full_temporary_noNegs_5k.RDS'))

# predict the full MODIS NDVI image using the trained linear regression model 
# covs_test <- data.frame(testing_points)
# covs_test <- covs_test[, grep('B.*', colnames(covs_test))]
dataset_covs <- dataset[[names(dataset) %in% colnames(covs)]]

dataset_predicted <- predict(dataset_covs, lr_model_trained)
names(dataset_predicted) <- 'ndvi_pred'
plot(dataset_predicted$ndvi_pred)
hist(dataset_predicted$ndvi_pred)
dataset_predicted[dataset_predicted < 0,] <- NA
plot(dataset_predicted$ndvi_pred)
hist(dataset_predicted$ndvi_pred)
plot(is.na(dataset_predicted))

ncol(dataset_predicted)
num_obs <- ncol(dataset_predicted) * nrow(dataset_predicted)

# source: https://gis.stackexchange.com/questions/4802/rmse-between-two-rasters-step-by-step
error <- dataset_predicted$ndvi_pred - dataset$ndvi
# source: https://rdrr.io/cran/terra/man/global.html#google_vignette
mse <- global(error**2, 'sum', na.rm = T)[[1]]/num_obs
rmse <- sqrt(mse)
mae <- global(abs(error), 'sum', na.rm = T)[[1]]/num_obs
# source: https://stackoverflow.com/questions/63335671/correct-way-of-determining-r2-between-two-rasters-in-r#:~:text=R2%20%3D%20r%20*%20r.,of%202%20to%20get%20R2.
r2 <- cor(values(dataset_predicted$ndvi_pred), values(dataset$ndvi), use="complete.obs", method = 'pearson')[[1]]

plot(error)
plot(abs(error))



# apply forward feature selection
# source: https://cran.r-hub.io/web/packages/CAST/vignettes/CAST-intro.html
lr_model_ffs <- ffs(predictors = covs, response = sample_points$ndvi, method = 'lm', 
                          trControl = trainControl(method = 'cv', index = cv_folds$index))

# read results
lr_model_ffs
lr_model_ffs$results
lr_model_ffs$finalModel
lr_model_ffs$resample

vif(lr_model_ffs$finalModel)

plot(varImp(lr_model_ffs))

saveRDS(lr_model_ffs, paste0(run_filepath, '3_c3_linear_model_ffs_temporary_noNegs_5k.RDS'))

lr_model_ffs <- readRDS(paste0(run_filepath, '3_c3_linear_model_ffs_temporary.RDS'))

# predict for whole image
dataset_covs_ffs <- dataset[[names(dataset) %in% lr_model_ffs$selectedvars]]

dataset_predicted_ffs <- predict(dataset_covs_ffs, lr_model_ffs)

dataset_predicted_ffs
names(dataset_predicted_ffs) <- 'ndvi_pred'
plot(dataset_predicted_ffs)
plot(dataset$ndvi)

# see results 
num_obs <- ncol(dataset_predicted_ffs) * nrow(dataset_predicted_ffs)
error <- dataset_predicted_ffs$ndvi_pred - dataset$ndvi
mse <- global(error**2, 'sum', na.rm = T)[[1]]/num_obs
rmse <- sqrt(mse)
mae <- global(abs(error), 'sum', na.rm = T)[[1]]/num_obs
r2 <- cor(values(dataset_predicted_ffs$ndvi_pred), values(dataset$ndvi), use="complete.obs", method = 'pearson')[[1]]
plot(error)
plot(abs(error))




# decide which predictors to remove

# predict with landsat 30m image 
l_30
# get dataframe of all band combinations
# source: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/expand.grid
band_combinations_30 <- expand.grid(bandA = names(l_30['B.']), bandB = names(l_30['B.']), KEEP.OUT.ATTRS = F, stringsAsFactors = F)

# calculate landsat band ratios 
for(i in 1:nrow(band_combinations_30)){
  # get band name from combination dataframe 
  bandA <- band_combinations_30[i,1]
  bandB <- band_combinations_30[i,2]
  
  # get band number 
  # source: https://www.statology.org/r-extract-number-from-string/
  bandA_number <- as.numeric(gsub("\\D", "", bandA))
  bandB_number <- as.numeric(gsub("\\D", "", bandB))
  
  if(bandA_number>bandB_number){
    # calculate ratio 
    ratio <- (l_30[[bandA]]-l_30[[bandB]])/(l_30[[bandA]]+l_30[[bandB]])
    
    # add band ratio to dataset
    layer_name <- paste0(bandA,'.',bandB)
    l_30[[layer_name]] <- ratio
  }
}

# save landsat 30 image with new bands
saveRDS(l_30, paste0(run_filepath,'3_c4_', LUT$modis_date,'_l30.RData'))


# predict MODIS at 30m
l_30 <- l_30[[names(l_30) %in% lr_model_ffs$selectedvars]]

modis_30 <- predict(l_30, lr_model_ffs)
names(modis_30) <- 'ndvi_pred'

plot(modis_30)

# see results 
m_30_250 <- resample(modis_30, modis_250)
num_obs <- ncol(m_30_250) * nrow(m_30_250)
error <- m_30_250$ndvi_pred - modis_250$NDVI
mse <- global(error**2, 'sum', na.rm = T)[[1]]/num_obs
rmse <- sqrt(mse)
mae <- global(abs(error), 'sum', na.rm = T)[[1]]/num_obs
r2 <- cor(values(m_30_250$ndvi_pred), values(modis_250$NDVI), use="complete.obs", method = 'pearson')[[1]]




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
