## MSc Thesis 
## Jacotte Monroe 
## 13/05/24
## Downscaling
## Function script

## Function samples regularly spaced points from the dataset of covariates and response. 
## The points get an ID depending on where they are in the image (grid ID), as well as the covariates and response values.
## A set of folds is defined for cross-validation (9 folds corresponding to the grid cells). 

## Input: dataset path, ID, week, the date of interest, output directory.
## Output: the sampled training points (saved as RDS) and the list of point indices in each CV fold (saved as RDS).




sampleTrainingPoints <- function(dataset_filepath, ID, week, modis_date, input_suffix = '', output_directory = 'data/', output_suffix = ''){
  
  # load the covariates/response dataset 
  dataset <- rast(paste0(dataset_filepath, '3_d1_', modis_date, '_dataset', input_suffix, '.tif'))
  
  # create grid object from an empty raster
  # source: https://gis.stackexchange.com/questions/431873/creating-regular-sampling-grid-with-specific-distance-between-points-using-r
  grid <- rast(extent = ext(dataset), crs = crs(dataset), nrow = 3, ncol = 3)
  values(grid) <- 1:9
  names(grid) <- 'grid_ID'
  
  # resample the grid raster to the resolution of the dataset --> we get a raster of the same dimensions as dataset that has 9 cells 
  grid <- resample(grid, dataset, method = 'average')
  
  # stack dataset and grid rasters to add the grid IDs to the dataset 
  dataset <- c(dataset, grid)
  
  # sample from the dataset raster 
  # source: https://www.rdocumentation.org/packages/terra/versions/1.7-71/topics/spatSample
  sample_points <- spatSample(dataset, size = 5000, method = 'regular', as.points = T) 
  
  # transform SpatVector object into sf object 
  sample_points <- st_as_sf(sample_points, crs = st_crs_crs(dataset))
  
  # define output filepath 
  output_filepath <- paste0(output_directory, ID, '/', week, '/')
  
  # create filepath if it does not yet exist
  if(!dir.exists(output_filepath)){dir.create(output_filepath, recursive = T)}
  
  # save sampled points as RDS
  saveRDS(sample_points, paste0(output_filepath, '3_e1_', modis_date, '_trainingPoints', output_suffix, '.RDS'))
  
  # create a list of folds for cross-validation 
  # each list has the indices of points to include or exclude for each CV iteration
  # source: https://www.rdocumentation.org/packages/CAST/versions/0.2.1/topics/CreateSpacetimeFolds
  cv_folds <- CreateSpacetimeFolds(sample_points, spacevar = 'grid_ID', k = 9)
  
  # save list of folds 
  saveRDS(cv_folds, paste0(output_filepath, '3_e2_', modis_date, '_CVFolds', output_suffix, '.RDS'))
}