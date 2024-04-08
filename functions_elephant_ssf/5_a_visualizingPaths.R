## MSc Thesis 
## Jacotte Monroe 

## Visualization function of paths on mean MODIS NDVI

# Function takes filenames of step dataset and MODIS mean NDVI (mean week value).
# Note: Downscaling term makes the distinction between MODIS dataset that was retrieved without running any downscaling scripts (downscaling = NULL), 
#         MODIS 250m retrieved through downscaling JN script (downscaling = F), MODIS 30m generated through downscaling in R (downscaling = T).
# Note: Specify downscaling model if downscaling = T --> should match last section of name of MODIS 30m folder 
# Packages: tidyr (fill data frame column with values above), ggplot2 (general plotting), ggspatial and tidyterra (plot raster), 
#           cowplot (retrieve plot legend), patchwork (create blank plot and mosaic plots together)
# Input: filepaths for modis and step dataset, elephant ID and week of interest, 
#         the map title, and the directory for output. All inputs are strings. 
# Output: Map of filepaths on top of NDVI raster saved as png. 

visualizePaths <- function(input_filepath, ID, week, random_data_method, downscaling= 'NULL', downscaling_model = 'ranger_full_selection',
                           title = 'Elephant Movement on NDVI', output_directory = 'output/'){
  
  # create output filepath 
  output_filepath <- paste0(output_directory, ID, '/', week, '/')
  
  # create data directory if it does not yet exist
  if(!dir.exists(output_filepath)){dir.create(output_filepath, recursive = T)}
  
  
  
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
  
  # read step dataset
  dat <- read.csv(paste0(input_filepath, '4_a1_cov_resp_dataset_', random_data_method, suffix, '.csv'))
  dat$random_id_ <- as.factor(dat$random_id_)
  
  # add new step ID column that restarts count at each burst (so doesn't connect the different paths) --> consistency in dataset
  dat$stepID <- NA
  
  b <- 3
  steps <- dat[dat$burst_ == b & dat$case_ == T,]
  dat$stepID[min(steps$X):max(steps$X)] <- 1:as.numeric(nrow(steps))
  
  for(b in unique(dat$burst_)){
    # select rows of that burst that are true
    steps <- dat[dat$burst_ == b & dat$case_ == T,]
    dat$stepID[min(steps$X):max(steps$X)] <- 1:as.numeric(nrow(steps))
  }
  
  # transfer the step ID of random steps to new column 
  # NOTE: could move the code to some other script (unless the columns are useful)
  dat$stepID[dat$case_ == F] <- dat$step_id_[dat$case_ == F]
  
  # new column for pathID --> every different path (true and false) has a different ID, necessary for plotting paths separately 
  dat$pathID <- NA
  
  # find start of new path 
  smin <- dat$X[dat$stepID == min(dat$stepID)]
  
  # assign new path ID to start of each new path 
  dat$pathID[smin] <- 1:length(smin)
  
  # fill the column with values from above 
  # source: https://tidyr.tidyverse.org/reference/fill.html#ref-examples
  dat <- dat %>% fill(pathID)
  dat$pathID <- as.factor(dat$pathID)
  
  # rename the case column (necessary for plotting order)
  dat$case_[dat$case_ == T] <- 2
  dat$case_[dat$case_ == F] <- 1
  
  dat$case_ <- as.factor(dat$case_)
  
  
  ## PLOTTING
  
  # make NDVI map with legend
  # source: https://dieghernan.github.io/tidyterra/reference/geom_spatraster.html
  modis_ndvi_map <- ggplot() +
    geom_spatraster(data = ndvi_data, aes(fill = ndvi), show.legend = T) +
    scale_fill_terrain_c(name = 'NDVI', limits = c(0,0.6))
  
  # make rough path graph with legend (legend is what's important here)
  path_map <- ggplot(data = dat, aes(x = x1_, y = y1_, colour = case_, group = pathID, linetype = case_)) +
    # source: https://stackoverflow.com/questions/27003991/how-can-i-define-line-plotting-order-in-ggplot2-for-grouped-lines
    geom_path(data = subset(dat, case_ == 1), linewidth = 0.4) + 
    geom_path(data = subset(dat, case_ == 2), linewidth = 0.4) +
    # source: https://www.geeksforgeeks.org/control-line-color-and-type-in-ggplot2-plot-legend-in-r/
    scale_linetype_manual(name = "Elephant Path", labels = c('pseudo-absence', 'presence'), values = c(2,1)) +
    scale_color_manual(name = "Elephant Path", labels = c('pseudo-absence', 'presence'), values = c('grey50', 'darkred')) + 
    theme_minimal() 
  
  # make elephant movement on NDVI map without legends 
  image_map <- ggplot() +
    geom_spatraster(data = ndvi_data, aes(fill = ndvi), show.legend = F) +
    scale_fill_terrain_c(name = 'NDVI', limits = c(0,0.6)) + 
    geom_path(data = subset(dat, case_ == 1), aes(x = x1_, y = y1_, group = pathID), colour = 'grey50', linetype = 2, linewidth = 0.4) + 
    geom_path(data = subset(dat, case_ == 2), aes(x = x1_, y = y1_, group = pathID,), colour = 'darkred', linetype = 1, linewidth = 0.4) +
    labs(title = title, subtitle = paste0('Elephant ', ID, ' from ', as.Date(min(dat$t1_), tz = 'Africa/Maputo') , ' to ', 
                                          as.Date(max(dat$t2_), tz = 'Africa/Maputo'),' (week ', week, ')'), x = "Longitude", y = "Latitude") +
    annotation_north_arrow(location = 'tl', which_north = 'true', 
                           pad_x = unit(0.5, "cm"), pad_y = unit(0.6, "cm"),
                           style = north_arrow_fancy_orienteering()) +
    annotation_scale(location = 'br', pad_x = unit(1.0, "cm"), pad_y = unit(0.5, "cm")) +
    theme_minimal() + 
    theme(legend.position = 'none')
  
  # retrieve legends 
  ndvi_legend <- get_legend(modis_ndvi_map)
  path_legend <- get_legend(path_map)
  
  # create blank plot
  blank_p <- plot_spacer() + theme_void()
  
  # combine legends and plot
  legends <- plot_grid(blank_p, path_legend, ndvi_legend, blank_p, nrow = 4)
  final_map <- plot_grid(image_map, legends, nrow = 1, align = 'h', axis = 't', rel_widths = c(1, 0.3))

  # save map as png
  png(paste0(output_filepath, '5_a1_elephant_movement_map_', random_data_method, suffix,'.png'))
  print(final_map)
  dev.off()
}