## MSc_Thesis
## Jacotte Monroe 
## 19/03/24

## R script that runs function to visualize presence and absence elephant paths for week of interest. 
## A raster of the mean NDVI over that week is used as the basemap. 
## Script outputs: 
##    5_a1: A map of the elephant movement over the mean NDVI, saved as png. (final output)


# l <- list.files('output/', pattern = glob2rx('5_a1*'), recursive = T)
# l1 <- l[1]
# id <- sub('/2.*', '', l)
# week <- sub('.*/', '', sub('/5.*', '', l))
# method <- sub('.*map_', '', sub('.png', '', l))
# 
# d <- data.frame(ID = id, week = week, pseudo_abs_method = method)
# write.csv(d, 'data/run_settings_visualizations.csv')
# r <- read.csv('data/run_settings_visualizations.csv')
# r <- r[,2:ncol(r)]
# write.csv(r, 'data/run_settings_visualizations.csv')

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
source('functions_elephant_ssf/5_a_visualizingPaths.R')

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






# 
# 
# 
# # # define elephant ID
# # ID <- 'LA2'
# # 
# # # define week to test 
# # week <- 2068
# # 
# # # define pseudo-absence path generator method --> Choices are 1) random_path_custom_distr 2) random_path_buffer_point 3) random_step
# # pseudo_abs_method <- 'random_path_custom_distr'
# # 
# # # set downscaling parameters 
# # downscaling_setting <- 'NULL'
# # downscaling_model <- 'NULL'
# # 
# # # define input and output suffixes
# # input_suffix <- '_newPathWithCV'
# # output_suffix <- '_newPathWithCV'
# 
# run_settings <- run_settings[5,]
# 
# # define elephant ID
# ID <- run_settings[[1]]
# 
# # define week to test 
# week <- run_settings[[2]]
# 
# # define pseudo-absence path generator method --> Choices are 1) random_path_custom_distr 2) random_path_buffer_point 3) random_step
# pseudo_abs_method <- run_settings[[3]]
# 
# # set downscaling parameters 
# downscaling_setting <- run_settings[[4]]
# downscaling_model <- run_settings[[5]]
# 
# print(downscaling_setting)
# print(class(downscaling_setting))
# 
# # replace NA from suffix columns of run settings to empty strings
# if(is.na(run_settings[[6]])){run_settings[[6]] <- ''}
# if(is.na(run_settings[[7]])){run_settings[[7]] <- ''}
# 
# # define input and output suffixes
# input_suffix <- run_settings[[6]]
# output_suffix <- run_settings[[7]]
# 
# # define run filepath 
# run_filepath <- paste0('data/', ID, '/', week, '/')
# 
# print(paste('Now visualizing the movement paths for elephant', ID, 'of week', week, '...'))
# 
# ###########
# ## Map elephant paths
# ###########
# # load function
# source('functions_elephant_ssf/5_a_visualizingPaths.R')
# 
# # load necessary packages
# if(!('terra') %in% installed.packages()){install.packages('terra')}
# library(terra)
# if(!('tidyr') %in% installed.packages()){install.packages('tidyr')}
# library(tidyr)
# if(!('ggplot2') %in% installed.packages()){install.packages('ggplot2')}
# library(ggplot2)
# if(!('tidyterra') %in% installed.packages()){install.packages('tidyterra')} # for mapping spatraster
# library(tidyterra)
# if(!('ggspatial') %in% installed.packages()){install.packages('ggspatial')}
# library(ggspatial)
# if(!('cowplot') %in% installed.packages()){install.packages('cowplot')}
# library(cowplot)
# if(!('patchwork') %in% installed.packages()){install.packages('patchwork')}
# library(patchwork)
# 
# # run function 
# # visualizePaths(run_filepath, ID, week, pseudo_abs_method, downscaling= downscaling_setting, 
# #                downscaling_model = downscaling_model, input_suffix = input_suffix, 
# #                title = "Elephant Movement on Mean NDVI", output_directory = 'output/', output_suffix = output_suffix)
# 
# visualizePaths(run_filepath, ID, week, pseudo_abs_method, downscaling= downscaling_setting, 
#                downscaling_model = downscaling_model, input_suffix = input_suffix, 
#                title = "Elephant Movement on Mean NDVI", output_directory = 'output/', output_suffix = output_suffix)
# 
# print(paste('(DONE) Visualizing the movement paths for elephant', ID, 'of week', week))
# 
# 
# 











# 
# 
# # 
# # 
# 
# ID <- 'LA14'
# week <- 2128
# input_filepath <- paste0('data/', ID, '/', week, '/')
# random_data_method <- 'random_path_custom_distr'
# downscaling = 'NULL'
# downscaling_model = 'ranger_full_selection'
# title = 'Elephant movement on mean NDVI'
# output_directory <- 'output/'
# 
# # create output filepath
# output_filepath <- paste0(output_directory, ID, '/', week, '/')
# 
# # create data directory if it does not yet exist
# if(!dir.exists(output_filepath)){dir.create(output_filepath, recursive = T)}
# 
# 
# 
# 
# # define data directory and suffix
# if(downscaling == 'NULL'){
#   modis_directory <- paste0(input_filepath, '3_a1_modis_images_', random_data_method, input_suffix, '/')
#   
#   suffix <- input_suffix
#   
# }else if(downscaling == T){
#   modis_directory <- paste0(input_filepath, '3_g1_downscaled_modis_images_30m_', downscaling_model, input_suffix, '/')
#   
#   suffix <- paste0('_downscaling_modis_30m', input_suffix)
#   
# }else if(downscaling == F){
#   modis_directory <- paste0(input_filepath, '3_b1_modis_images_downscaling_', random_data_method, input_suffix, '/')
#   
#   suffix <- paste0('_downscaling_modis_250m', input_suffix)
#   
# }else{stop('Incorrect term set for downscaling parameter. Should be one of the following: NULL, T, F.')}
# 
# # read NDVI dataset
# ndvi_data <- rast(paste0(modis_directory, 'mean_ndvi.tif'))
# names(ndvi_data) <- 'ndvi'
# 
# # read step dataset
# dat <- read.csv(paste0(input_filepath, '4_a1_cov_resp_dataset_', random_data_method, suffix, '.csv'), row.names = 1)
# dat$random_id_ <- as.factor(dat$random_id_)
# # 
# # # add new step ID column that restarts count at each burst (so doesn't connect the different paths) --> consistency in dataset
# # dat$stepID <- NA
# # 
# # for(b in unique(dat$burst_)){
# #   # select rows of that burst that are true
# #   steps <- dat[dat$burst_ == b & dat$case_ == T,]
# #   dat$stepID[min(as.numeric(steps$step_id_)):max(as.numeric(steps$step_id_))] <- 1:nrow(steps)
# # }
# # 
# # # transfer the step ID of random steps to new column 
# # # NOTE: could move the code to some other script (unless the columns are useful)
# # dat$stepID[dat$case_ == F] <- dat$step_id_[dat$case_ == F]
# 
# # new column for pathID --> every different path (true and false) has a different ID, necessary for plotting paths separately 
# dat$pathID <- NA
# 
# # create new column for row names 
# dat$rowNames <- rownames(dat)
# 
# # find start of new path 
# smin <- as.numeric(dat$rowNames[dat$step_id_ == min(dat$step_id_)])
# 
# # assign new path ID to start of each new path 
# dat$pathID[smin] <- 1:length(smin)
# 
# # fill the column with values from above 
# # package: tidyr 
# # source: https://tidyr.tidyverse.org/reference/fill.html#ref-examples
# dat <- dat %>% fill(pathID)
# dat$pathID <- as.factor(dat$pathID)
# 
# # rename the case column (necessary for plotting order)
# dat$case_[dat$case_ == T] <- 2
# dat$case_[dat$case_ == F] <- 1
# 
# dat$case_ <- as.factor(dat$case_)
# 
# 
# ## PLOTTING
# 
# # make NDVI map with legend
# # source: https://dieghernan.github.io/tidyterra/reference/geom_spatraster.html
# modis_ndvi_map <- ggplot() +
#   geom_spatraster(data = ndvi_data, aes(fill = ndvi), alpha = 0.6, show.legend = T) +
#   scale_fill_terrain_c(name = 'NDVI', limits = c(0,0.6), breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), alpha = 0.6)
# modis_ndvi_map
# 
# # make rough path graph with legend (legend is what's important here)
# path_map <- ggplot(data = dat, aes(x = x1_, y = y1_, colour = case_, group = pathID, linetype = case_)) +
#   # source: https://stackoverflow.com/questions/27003991/how-can-i-define-line-plotting-order-in-ggplot2-for-grouped-lines
#   geom_path(data = subset(dat, case_ == 1), linewidth = 0.4) + 
#   geom_path(data = subset(dat, case_ == 2), linewidth = 0.7) +
#   # source: https://www.geeksforgeeks.org/control-line-color-and-type-in-ggplot2-plot-legend-in-r/
#   scale_linetype_manual(name = "Elephant Path", labels = c('pseudo-absence', 'presence'), values = c(2,1)) +
#   scale_color_manual(name = "Elephant Path", labels = c('pseudo-absence', 'presence'), values = c('grey30', '#e60000')) + 
#   theme_minimal() 
# path_map
# # make elephant movement on NDVI map without legends 
# image_map <- ggplot() +
#   geom_spatraster(data = ndvi_data, aes(fill = ndvi), alpha = 0.6, show.legend = F) +
#   scale_fill_terrain_c(name = 'NDVI', limits = c(0,0.6), breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), alpha = 0.6) +
#   geom_path(data = subset(dat, case_ == 1), aes(x = x1_, y = y1_, group = pathID), colour = 'grey30', linetype = 2, linewidth = 0.4) + 
#   geom_path(data = subset(dat, case_ == 2), aes(x = x1_, y = y1_, group = pathID,), colour = 'red', linetype = 1, linewidth = 0.7) +
#   labs(title = title, subtitle = paste0('Elephant ', ID, ' from ', as.Date(min(dat$t1_), tz = 'Africa/Maputo') , ' to ', 
#                                         as.Date(max(dat$t2_), tz = 'Africa/Maputo'),' (week ', week, ')'), x = "Longitude", y = "Latitude") +
#   annotation_north_arrow(location = 'tl', which_north = 'true', 
#                          pad_x = unit(0.5, "cm"), pad_y = unit(0.6, "cm"),
#                          style = north_arrow_fancy_orienteering()) +
#   annotation_scale(location = 'br', pad_x = unit(1.0, "cm"), pad_y = unit(0.5, "cm")) +
#   theme_minimal() + 
#   theme(legend.position = 'none')
# image_map
# # retrieve legends 
# ndvi_legend <- get_legend(modis_ndvi_map)
# path_legend <- get_legend(path_map)
# 
# # create blank plot
# blank_p <- plot_spacer() + theme_void()
# 
# # combine legends and plot
# legends <- plot_grid(blank_p, path_legend, ndvi_legend, blank_p, nrow = 4)
# final_map <- plot_grid(image_map, legends, nrow = 1, align = 'h', axis = 't', rel_widths = c(1, 0.3))
# final_map
# 
# 
# 
# 




































# 
# # define data directory and suffix
# if(downscaling == 'NULL'){
#   modis_directory <- paste0(input_filepath, '3_a1_modis_images_', random_data_method, '/')
#   
#   suffix <- ''
#   
# }else if(downscaling == T){
#   modis_directory <- paste0(input_filepath, '3_g1_downscaled_modis_images_30m_', downscaling_model, '/')
#   
#   suffix <- '_downscaling_modis_30m'
#   
# }else if(downscaling == F){
#   modis_directory <- paste0(input_filepath, '3_b1_modis_images_downscaling_', random_data_method, '/')
#   
#   suffix <- '_downscaling_modis_250m'
#   
# }else{stop('Incorrect term set for downscaling parameter. Should be one of the following: NULL, T, F.')}
# 
# # read NDVI dataset
# ndvi_data <- rast(paste0(modis_directory, 'mean_ndvi.tif'))
# names(ndvi_data) <- 'ndvi'
# 
# # read step dataset
# dat <- read.csv(paste0(input_filepath, '4_a1_cov_resp_dataset_', random_data_method, suffix, '.csv'))
# dat$random_id_ <- as.factor(dat$random_id_)
# 
# # add new step ID column that restarts count at each burst (so doesn't connect the different paths) --> consistency in dataset
# dat$stepID <- NA
# 
# for(b in unique(dat$burst_)){
#   # select rows of that burst that are true
#   steps <- dat[dat$burst_ == b & dat$case_ == T,]
#   dat$stepID[min(steps$X):max(steps$X)] <- 1:as.numeric(nrow(steps))
# }
# 
# # transfer the step ID of random steps to new column 
# # NOTE: could move the code to some other script (unless the columns are useful)
# dat$stepID[dat$case_ == F] <- dat$step_id_[dat$case_ == F]
# 
# # new column for pathID --> every different path (true and false) has a different ID, necessary for plotting paths separately 
# dat$pathID <- NA
# 
# # find start of new path 
# smin <- dat$X[dat$stepID == min(dat$stepID)]
# 
# # assign new path ID to start of each new path 
# dat$pathID[smin] <- 1:length(smin)
# 
# # fill the column with values from above 
# # source: https://tidyr.tidyverse.org/reference/fill.html#ref-examples
# dat <- dat %>% fill(pathID)
# dat$pathID <- as.factor(dat$pathID)
# 
# # rename the case column (necessary for plotting order)
# dat$case_[dat$case_ == T] <- 2
# dat$case_[dat$case_ == F] <- 1
# 
# dat$case_ <- as.factor(dat$case_)
# 
# 
# ## PLOTTING
# 
# # make NDVI map with legend
# # source: https://dieghernan.github.io/tidyterra/reference/geom_spatraster.html
# modis_ndvi_map <- ggplot() +
#   geom_spatraster(data = ndvi_data, aes(fill = ndvi), show.legend = T) +
#   scale_fill_terrain_c(name = 'NDVI', limits = c(0,0.6))
# 
# # make rough path graph with legend (legend is what's important here)
# path_map <- ggplot(data = dat, aes(x = x1_, y = y1_, colour = case_, group = pathID, linetype = case_)) +
#   # source: https://stackoverflow.com/questions/27003991/how-can-i-define-line-plotting-order-in-ggplot2-for-grouped-lines
#   geom_path(data = subset(dat, case_ == 1), linewidth = 0.4) + 
#   geom_path(data = subset(dat, case_ == 2), linewidth = 0.4) +
#   # source: https://www.geeksforgeeks.org/control-line-color-and-type-in-ggplot2-plot-legend-in-r/
#   scale_linetype_manual(name = "Elephant Path", labels = c('pseudo-absence', 'presence'), values = c(2,1)) +
#   scale_color_manual(name = "Elephant Path", labels = c('pseudo-absence', 'presence'), values = c('grey50', 'darkred')) + 
#   theme_minimal() 
# 
# # make elephant movement on NDVI map without legends 
# image_map <- ggplot() +
#   geom_spatraster(data = ndvi_data, aes(fill = ndvi), show.legend = F) +
#   scale_fill_terrain_c(name = 'NDVI', limits = c(0,0.6)) + 
#   geom_path(data = subset(dat, case_ == 1), aes(x = x1_, y = y1_, group = pathID), colour = 'grey50', linetype = 2, linewidth = 0.4) + 
#   geom_path(data = subset(dat, case_ == 2), aes(x = x1_, y = y1_, group = pathID,), colour = 'darkred', linetype = 1, linewidth = 0.4) +
#   labs(title = title, subtitle = paste0('Elephant ', ID, ' from ', as.Date(min(dat$t1_), tz = 'Africa/Maputo') , ' to ', 
#                                         as.Date(max(dat$t2_), tz = 'Africa/Maputo'),' (week ', week, ')'), x = "Longitude", y = "Latitude") +
#   annotation_north_arrow(location = 'tl', which_north = 'true', 
#                          pad_x = unit(0.5, "cm"), pad_y = unit(0.6, "cm"),
#                          style = north_arrow_fancy_orienteering()) +
#   annotation_scale(location = 'br', pad_x = unit(1.0, "cm"), pad_y = unit(0.5, "cm")) +
#   theme_minimal() + 
#   theme(legend.position = 'none')
# 
# # retrieve legends 
# ndvi_legend <- get_legend(modis_ndvi_map)
# path_legend <- get_legend(path_map)
# 
# # create blank plot
# blank_p <- plot_spacer() + theme_void()
# 
# # combine legends and plot
# legends <- plot_grid(blank_p, path_legend, ndvi_legend, blank_p, nrow = 4)
# final_map <- plot_grid(image_map, legends, nrow = 1, align = 'h', axis = 't', rel_widths = c(1, 0.3))
# 
# # save map as png
# png(paste0(output_filepath, '5_a1_elephant_movement_map_', random_data_method, suffix,'.png'))
# print(final_map)
# dev.off()























#   
# # define data directory and suffix
# if(downscaling == 'NULL'){
#   modis_directory <- paste0(input_filepath, '3_a1_modis_images_', random_data_method, '/')
#   
#   suffix <- ''
#   
# }else if(downscaling == T){
#   modis_directory <- paste0(input_filepath, '3_g1_downscaled_modis_images_30m_', downscaling_model, '/')
#   
#   suffix <- '_downscaling_modis_30m'
#   
# }else if(downscaling == F){
#   modis_directory <- paste0(input_filepath, '3_b1_modis_images_downscaling_', random_data_method, '/')
#   
#   suffix <- '_downscaling_modis_250m'
#   
# }else{stop('Incorrect term set for downscaling parameter. Should be one of the following: NULL, T, F.')}
# 
# # read NDVI dataset
# ndvi_data <- rast(paste0(modis_directory, 'mean_ndvi.tif'))
# names(ndvi_data) <- 'ndvi'
# 
# 
# 
# if(!('ggspatial') %in% installed.packages()){install.packages('ggspatial')} #for north arrow
# library(ggspatial)
# 
# # read step dataset
# dat <- read.csv(paste0(input_filepath, '4_a1_cov_resp_dataset_', random_data_method, suffix, '.csv'))
# dat$random_id_ <- as.factor(dat$random_id_)
# 
# # add new step ID column that restarts count at each burst (so doesn't connect the different paths)
# dat$stepID <- NA
# steps <- dat[dat$burst_ == 1 & dat$case_ == T,]
# dat$stepID[min(steps$X):max(steps$X)] <- 1:as.numeric(nrow(steps))
# for(b in unique(dat$burst_)){
#   # select rows of that burst that are true
#   steps <- dat[dat$burst_ == b & dat$case_ == T,]
#   dat$stepID[min(steps$X):max(steps$X)] <- 1:as.numeric(nrow(steps))
# }
# 
# # transfer the step ID of random steps to new column 
# # NOTE: could move the code to some other script (unless the columns are useful)
# dat$stepID[dat$case_ == F] <- dat$step_id_[dat$case_ == F]
# 
# # new column for pathID
# dat$pathID <- NA
# 
# # find start of new path 
# smin <- dat$X[dat$stepID == min(dat$stepID)]
# 
# # assign new path ID to start of each new path 
# dat$pathID[smin] <- 1:length(smin)
# 
# # fill the column with values from above 
# # source: https://tidyr.tidyverse.org/reference/fill.html#ref-examples
# library(tidyr)
# dat <- dat %>% fill(pathID)
# dat$pathID <- as.factor(dat$pathID)
# 
# # rename the case column (necessary for plotting order)
# dat$case_[dat$case_ == T] <- 2
# dat$case_[dat$case_ == F] <- 1
# 
# dat$case_ <- as.factor(dat$case_)
# 
# 
# 
# # make multiple maps then retrieve legends 
# # tutorial: https://stackoverflow.com/questions/52060601/ggplot-multiple-legends-arrangement
# if(!('cowplot') %in% installed.packages()){install.packages('cowplot')}
# library(cowplot)
# library(patchwork)
# 
# # make NDVI map with legend
# # source: https://dieghernan.github.io/tidyterra/reference/geom_spatraster.html
# modis_ndvi_map <- ggplot() +
#   geom_spatraster(data = ndvi_data, aes(fill = ndvi), show.legend = T) +
#   scale_fill_terrain_c(name = 'NDVI', limits = c(0,0.6))
# 
# # make path graph with legend 
# path_map <- ggplot(data = dat, aes(x = x1_, y = y1_, colour = case_, group = pathID, linetype = case_)) +
#   # source: https://stackoverflow.com/questions/27003991/how-can-i-define-line-plotting-order-in-ggplot2-for-grouped-lines
#   geom_path(data = subset(dat, case_ == 1), linewidth = 0.4) + 
#   geom_path(data = subset(dat, case_ == 2), linewidth = 0.4) +
#   # source: https://www.geeksforgeeks.org/control-line-color-and-type-in-ggplot2-plot-legend-in-r/
#   scale_linetype_manual(name = "Elephant Path", labels = c('pseudo-absence', 'presence'), values = c(2,1)) +
#   scale_color_manual(name = "Elephant Path", labels = c('pseudo-absence', 'presence'), values = c('grey50', 'darkred')) + 
#   theme_minimal() 
# 
# # make visual map without legends 
# image_map <- ggplot() +
#   geom_spatraster(data = ndvi_data, aes(fill = ndvi), show.legend = F) +
#   scale_fill_terrain_c(name = 'NDVI', limits = c(0,0.6)) + 
#   geom_path(data = subset(dat, case_ == 1), aes(x = x1_, y = y1_, group = pathID), colour = 'grey50', linetype = 2, linewidth = 0.4) + 
#   geom_path(data = subset(dat, case_ == 2), aes(x = x1_, y = y1_, group = pathID,), colour = 'darkred', linetype = 1, linewidth = 0.4) +
#   labs(title = title, subtitle = paste0('Elephant ', ID, ' from ', as.Date(min(dat$t1_), tz = 'Africa/Maputo') , ' to ', 
#                                         as.Date(max(dat$t2_), tz = 'Africa/Maputo'),' (week ', week, ')'), x = "Longitude", y = "Latitude") +
#   annotation_north_arrow(location = 'tl', which_north = 'true', 
#                          pad_x = unit(0.5, "cm"), pad_y = unit(0.6, "cm"),
#                          style = north_arrow_fancy_orienteering()) +
#   annotation_scale(location = 'br', pad_x = unit(1.0, "cm"), pad_y = unit(0.5, "cm")) +
#   theme_minimal() + 
#   theme(legend.position = 'none')
# 
# # retrieve legends 
# ndvi_legend <- get_legend(modis_ndvi_map)
# path_legend <- get_legend(path_map)
# 
# # create blank plot
# blank_p <- plot_spacer() + theme_void()
# 
# # combine legends and plot on blank plot
# legends <- plot_grid(blank_p, path_legend, ndvi_legend, blank_p, nrow = 4)
# 
# png('output/test.png')
# final_map <- plot_grid(image_map, legends, nrow = 1, align = 'h', axis = 't', rel_widths = c(1, 0.3))
# final_map
# dev.off()
# 
# 

