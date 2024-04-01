## MSc Thesis 
## Jacotte Monroe 

## Visualization function of paths on mean MODIS NDVI

# Function takes filenames of step dataset and MODIS mean NDVI (mean week value).
# Note: Downscaling term makes the distinction between MODIS dataset that was retrieved without running any downscaling scripts (downscaling = NULL), 
#         MODIS 250m retrieved through downscaling JN script (downscaling = F), MODIS 30m generated through downscaling in R (downscaling = T).
# Note: Specify downscaling model if downscaling = T --> should match last section of name of MODIS 30m folder 
# Input: filepaths for modis and step dataset, elephant ID and week of interest, 
#         the map title, and the directory for output. All inputs are strings. 
# Output: Map of filepaths on top of NDVI raster saved as png. 

visualizePaths <- function(input_filepath, ID, week, random_data_method, downscaling= 'NULL', downscaling_model = 'ranger_full_selection',
                           title = 'Elephant Movement on NDVI', output_directory = 'output/'){
  
  # create output filepath 
  output_filepath <- paste0(output_directory, ID, '/', week, '/')
  
  # create data directory if it does not yet exist
  if(!dir.exists(output_filepath)){dir.create(output_filepath, recursive = T)}
  
  # define data directory and output suffix
  if(downscaling == 'NULL'){
    modis_directory <- paste0(input_filepath, '3_a1_modis_images_', random_data_method, '/')
    
    output_suffix <- ''
    
  }else if(downscaling == T){
    modis_directory <- paste0(input_filepath, '3_g1_downscaled_modis_images_30m_', downscaling_model, '/')
    
    output_suffix <- '_downscaling_modis_30m'
    
  }else if(downscaling == F){
    modis_directory <- paste0(input_filepath, '3_b1_modis_images_downscaling_', random_data_method, '/')
    
    output_suffix <- '_downscaling_modis_250m'
    
  }else{stop('Incorrect term set for downscaling parameter. Should be one of the following: NULL, T, F.')}
  
  # read NDVI dataset
  ndvi_data <- rast(paste0(modis_directory, 'mean_ndvi.tif'))
  names(ndvi_data) <- 'ndvi'
  
  # create NDVI basemap 
  # source: https://dieghernan.github.io/tidyterra/reference/geom_spatraster.html
  modis_ndvi_map <- ggplot() +
    geom_spatraster(data = ndvi_data, aes(fill = ndvi), show.legend = T) +
    scale_fill_terrain_c(name = 'NDVI')
  
  # read step dataset
  dat <- read.csv(paste0(input_filepath, '4_a1_cov_resp_dataset_', random_data_method, '.csv'))
  dat$random_id_ <- as.factor(dat$random_id_)
  
  # add title and theme to plot
  mov_map <- modis_ndvi_map +
    labs(title = title, subtitle = paste(ID, week), x = "Longitude", y = "Latitude") +
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
  
  # save map as png
  png(paste0(output_filepath, '5_a1_elephant_movement_map_', random_data_method, output_suffix,'.png'))
  print(mov_map)
  dev.off()
  
}