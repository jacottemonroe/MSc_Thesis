## MSc Thesis 
## Jacotte Monroe 
## Downscaling script 

## Function that takes all the Landsat scenes and stitches them together. 
## Input: Landsat file containing the scenes, the date of images to stitch together 
## Output: A final Landsat scene for the relevant date. 

stitchScenes <- function(input_directory = 'data/', suffix = ''){
  
  # define landsat filepath 
  landsat_filepath <- paste0(input_directory, '3_b2_landsat_images_downscaling_', pseudo_abs_method, suffix, '/')
  
  # list directories of scenes 
  scene_directories <- list.dirs(landsat_filepath, full.names = F)[-1]
  
  # get filenames of landsat images 
  landsat_filenames_list <- list.files(paste0(landsat_filepath, 'BL_BL'))
  
  # loop through the different landsat image names 
  for(landsat_filename in landsat_filenames_list){
    # loop through the directories to load the scenes for the corresponding image date and append them to a list
    scenes <- list()
    for(sc_folder in scene_directories){
      # load scene 
      sc <- rast(paste0(landsat_filepath, sc_folder, '/', landsat_filename))
      scenes <- append(scenes, list(sc))
    }
    
    # stitch the scenes with the same filename together to make one large landsat 8 image
    # source: # source: https://gis.stackexchange.com/questions/224781/merge-rasters-with-different-origins-in-r
    # source: https://rdrr.io/cran/terra/man/merge.html
    l8_mosaic <- do.call(merge, scenes)
    
    # save new image 
    writeRaster(l8_mosaic, paste0(landsat_filepath, landsat_filename), overwrite = T)
  }
  
  # remove scene folders and files since now have composite image
  # source: https://stackoverflow.com/questions/28097035/how-to-remove-a-directory-in-r
  #unlink(paste0(landsat_filepath, scene_directories), recursive = T)
  
  # need to do a second level of stitching to stitch different landsat tiles together (rows 73 and 74)
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
    
    # load the images with matching date
    for(image in list_images){
      sc <- rast(paste0(landsat_filepath, image))
      scenes <- append(scenes, list(sc))
    }
    
    # stitch the scenes together to make one large landsat 8 image
    # source: # source: https://gis.stackexchange.com/questions/224781/merge-rasters-with-different-origins-in-r
    # source: https://rdrr.io/cran/terra/man/merge.html
    l8_mosaic <- do.call(merge, scenes)
    
    # create new file to store stitched images 
    if(!dir.exists(paste0(landsat_filepath, 'stitched/'))){dir.create(paste0(landsat_filepath, 'stitched/'), recursive = T)}
    
    # save new image 
    writeRaster(l8_mosaic, paste0(landsat_filepath, 'stitched/', 'LC08_179073_4_', date, '_stitched.tif'), overwrite = T)
  }
  
  # # remove scene files since now have composite image
  # # list and remove files that do not match pattern
  # list_files <- list.files(landsat_filepath)
  # list_files <- list_files[grep('*3_4_2*', list_files, invert = T)]
  # file.remove(paste0(landsat_filepath, list_files))
}