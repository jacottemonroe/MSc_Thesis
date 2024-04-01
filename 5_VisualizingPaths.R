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