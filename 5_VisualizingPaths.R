## MSc_Thesis
## Jacotte Monroe 
## 19/03/24

## R script that runs function to visualize presence and absence elephant paths for week of interest. 
## A raster of the mean NDVI over that week is used as the basemap. 
## Script outputs: 
##    5_a1: A map of the elephant movement over the mean NDVI, saved as png. (final output)



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

# set downscaling parameters 
downscaling_setting <- run_settings[[4]]
downscaling_model <- run_settings[[5]]

# define run filepath 
run_filepath <- paste0('data/', ID, '/', week, '/')

print(paste('Now visualizing the movement paths for elephant', ID, 'of week', week, '...'))

###########
## Map elephant paths
###########
# load function
source('functions_elephant_ssf/5_a_visualizingPaths.R')

# load necessary packages
if(!('terra') %in% installed.packages()){install.packages('terra')}
library(terra)
if(!('ggplot2') %in% installed.packages()){install.packages('ggplot2')}
library(ggplot2)
if(!('tidyterra') %in% installed.packages()){install.packages('tidyterra')} # for mapping spatraster
library(tidyterra)

# run function 
visualizePaths(run_filepath, ID, week, pseudo_abs_method, downscaling= downscaling_setting, 
               downscaling_model = downscaling_model, title = "Elephant Movement on Mean NDVI", output_directory = 'output/')

print(paste('(DONE) Visualizing the movement paths for elephant', ID, 'of week', week))






ID <- 'LA14'
week <- 2260
input_filepath <- paste0('data/', ID, '/', week, '/')
random_data_method <- 'random_path_custom_distr'
downscaling = T
downscaling_model = 'ranger_full_selection'
title = 'Elephant movement on mean NDVI'
  
# define data directory and suffix
if(downscaling == 'NULL'){
  modis_directory <- paste0(input_filepath, '3_a1_modis_images_', random_data_method, '/')
  
  suffix <- ''
  
}else if(downscaling == T){
  modis_directory <- paste0(input_filepath, '3_g1_downscaled_modis_images_30m_', downscaling_model, '/')
  
  suffix <- '_downscaling_modis_30m'
  
}else if(downscaling == F){
  modis_directory <- paste0(input_filepath, '3_b1_modis_images_downscaling_', random_data_method, '/')
  
  suffix <- '_downscaling_modis_250m'
  
}else{stop('Incorrect term set for downscaling parameter. Should be one of the following: NULL, T, F.')}

# read NDVI dataset
ndvi_data <- rast(paste0(modis_directory, 'mean_ndvi.tif'))
names(ndvi_data) <- 'ndvi'



if(!('ggspatial') %in% installed.packages()){install.packages('ggspatial')} #for north arrow
library(ggspatial)

# create NDVI basemap 
# source: https://dieghernan.github.io/tidyterra/reference/geom_spatraster.html
modis_ndvi_map <- ggplot() +
  geom_spatraster(data = ndvi_data, aes(fill = ndvi), show.legend = T) +
  scale_fill_terrain_c(name = 'NDVI', limits = c(0,0.6))

# read step dataset
dat <- read.csv(paste0(input_filepath, '4_a1_cov_resp_dataset_', random_data_method, suffix, '.csv'))
dat$random_id_ <- as.factor(dat$random_id_)
 
# add title and theme to plot
mov_map <- modis_ndvi_map +
  labs(title = title, subtitle = paste(ID, week), x = "Longitude", y = "Latitude") +
  annotation_north_arrow(location = 'tl', which_north = 'true', 
                         pad_x = unit(0.5, "cm"), pad_y = unit(0.6, "cm"),
  style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = 'br', pad_x = unit(1.0, "cm"), pad_y = unit(0.5, "cm")) +
  theme_minimal() + 
  theme(legend.position = c(1.1, 0.3))

mov_map

t <- mov_map + 
  geom_path(data = dat[dat$case_ == F & dat$burst_ == 1 & dat$random_id_ == 1,], 
            aes(x = x1_, y = y1_, color = 'random path'), linewidth = 0.4, linetype = 2) + 
  scale_color_manual(name = "Legend", values = 'grey40')
t

# plot presence and pseudo-absence paths on basemap by looping for each path 
# source: https://stackoverflow.com/questions/9206110/using-png-function-not-working-when-called-within-a-function
for(i in 1:length(unique(dat$burst_))){
  for(j in 1:length(unique(dat$random_id_))){
    # source: https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html
    if(i == 1 & j == 1){legend_setting <- T}else{legend_setting <- F}
    mov_map <- mov_map + geom_path(data = dat[dat$case_ == F & dat$burst_ == i & dat$random_id_ == j,], aes(x = x1_, y = y1_), color = 'grey40', linewidth = 0.4, linetype = 2, show.legend = legend_setting)
  }
  mov_map <- mov_map + geom_path(data = dat[dat$case_ == T & dat$burst_ == i,], aes(x = x1_, y = y1_), color = 'black', linewidth = 0.5, show.legend = legend_setting)
} 

mov_map
