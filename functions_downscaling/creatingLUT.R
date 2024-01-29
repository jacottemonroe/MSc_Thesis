## MSc Thesis 
## Jacotte Monroe 

## Function that creates look up table to pair Landsat and MODIS images 
## Input: folder names
## Output: LUT

# Packages 
if(!('terra') %in% installed.packages()){install.packages('terra')} # to read rasters
library(terra)

createLUT <- function(m_folder_name, l_folder_name){
  
  ## get list of all dates from all landsat images
  ## for all items in folder, retrieve data from file name and store in list
  
  # get all file names from folder
  # source: https://stat.ethz.ch/R-manual/R-devel/library/base/html/list.files.html
  l_files <- list.files(l_folder_name)
  
  # get all dates from file names 
  # source: https://stackoverflow.com/questions/17215789/extract-a-substring-according-to-a-pattern
  l_dates <- sub('.*3_', '', l_files)
  l_dates <- sub('.tif', '', l_dates)
  
  # convert items of list into date objects
  # source: https://stackoverflow.com/questions/70755258/convert-string-into-date-format-in-r
  l_dates <- as.Date(as.POSIXlt(l_dates, format="%Y%m%d"))
  
  
  ## create LUT of MODIS images 
  
  # get all MODIS file names from MODIS folder
  m_files <- list.files(m_folder_name, pattern = glob2rx('*.tif'))
  
  # get MODIS dates 
  m_dates <- sub('.tif', '', m_files)
  m_dates <- as.Date(as.POSIXlt(m_dates, format = '%Y_%m_%d'))
  
  # convert into dataframe and create empty LUT
  lut <- data.frame(modis_image = m_files, modis_date = m_dates, closest_landsat_image = NA)
  
  # for each image get closest l  ndsat date from list then get closest landsat image file name 
  getClosestDate <- function(x){
    # source: https://www.projectpro.io/recipes/find-difference-between-two-dates-r
    date <- format(l_dates[which.min(abs(as.numeric(difftime(lut$modis_date[x], l_dates))))])
    # source: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/grep
    image <- grep(gsub('-', '', date), l_files, value = T)
    return(image)
  }
  
  lut$closest_landsat_image <- sapply(1:nrow(lut), getClosestDate)
  
  saveRDS(lut, 'data/LUT.RData')
}





