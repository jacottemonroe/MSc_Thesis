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

print(downscaling_setting)
print(class(downscaling_setting))

# replace NA from suffix columns of run settings to empty strings
if(is.na(run_settings[[6]])){run_settings[[6]] <- ''}
if(is.na(run_settings[[7]])){run_settings[[7]] <- ''}

# define input and output suffixes
input_suffix <- run_settings[[6]]
output_suffix <- run_settings[[7]]

# define run filepath 
run_filepath <- paste0('data/', ID, '/', week, '/')

print(paste('Now visualizing the movement paths for elephant', ID, 'of week', week, '...'))



###########
## Map elephant paths
###########
# load function
source('functions/5_a_visualizingPaths.R')

# load necessary packages
if(!('terra') %in% installed.packages()){install.packages('terra')}
library(terra)
if(!('tidyr') %in% installed.packages()){install.packages('tidyr')}
library(tidyr)
if(!('ggplot2') %in% installed.packages()){install.packages('ggplot2')}
library(ggplot2)
if(!('tidyterra') %in% installed.packages()){install.packages('tidyterra')} # for mapping spatraster
library(tidyterra)
if(!('ggspatial') %in% installed.packages()){install.packages('ggspatial')}
library(ggspatial)
if(!('cowplot') %in% installed.packages()){install.packages('cowplot')}
library(cowplot)
if(!('patchwork') %in% installed.packages()){install.packages('patchwork')}
library(patchwork)

# run function 
visualizePaths(run_filepath, ID, week, pseudo_abs_method, downscaling= downscaling_setting, 
               downscaling_model = downscaling_model, input_suffix = input_suffix, 
               title = "Elephant Movement on Mean NDVI", output_directory = 'output/', output_suffix = output_suffix)

print(paste('(DONE) Visualizing the movement paths for elephant', ID, 'of week', week))



