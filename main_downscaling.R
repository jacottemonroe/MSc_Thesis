## MSc Thesis 
## Jacotte Monroe 

## Run main steps and functions

# Packages 
if(!('terra') %in% installed.packages()){install.packages('terra')} # to read rasters
library(terra)

if(!('ggplot2') %in% installed.packages()){install.packages('ggplot2')} # to plot timeseries
library(ggplot2)

############################################################################
############### generate Landsat and MODIS images in JN ####################
############## once have images i can start this script ####################
############################################################################


# create LUT from Landsat and MODIS images in data folders
# matches a Landsat image to each MODIS image (nearest Landsat image)
source('functions_downscaling/creatingLUT.R')

createLUT('data/modis/cloudmasked', 'data/l8')

# load LUT
LUT <- readRDS('data/LUT.RData')
#LUT <- LUT[1:30,]

m <- rast('data/modis/cloudmasked/2014_01_19.tif')
m <- m[[2]]
plot(m[[m == 0]])
m[m == 0] <- NA
plot(m)
# create downscaling model 
source('functions/generatingDownscalingModels.R')

for(i in 1:nrow(LUT)){
  generateDownscalingModels(paste0('data/modis/cloudmasked', LUT$modis_image[i]), LUT$modis_date[i], 
                            paste0('data/l8/', LUT$closest_landsat_image[i]), 
                            check_multicolinearity = F, multicolinearity_threshold = 0.8, 
                            fit_rf_regression = T)
}

# predict modis ndvi 30m
source('functions/predictingDownscaledModis.R')

for(i in 1:nrow(LUT)){
  predictMODIS30(LUT$modis_image[i], LUT$modis_date[i], LUT$closest_landsat_image[i], model_type = 'rf')
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
