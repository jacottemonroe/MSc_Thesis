## MSc Thesis 
## Jacotte Monroe 

## Elephant Preprocessing 

## Function script


# Function that reprojects the data frame (coordinates) to a new coordinate system 
# Input: elephant data and crs 
# Ouput: elephant data reprojected 


# Necessary package 
if(!('sp') %in% installed.packages()){install.packages('sp')}
library(sp)

reprojectElephantData <- function(elephant_dataset, crs = 'EPSG: 4326', new_crs = 'EPSG:32733'){
  
  # source: https://stackoverflow.com/questions/59774421/how-to-i-convert-coordinates-keeping-all-the-info-from-the-dataframe
  
  # transform elephant GPS coordinates into SpatialPoint object 
  # projection of elephant GPS data according to README from original dataset
  elephant_coord_4326 <- SpatialPoints(coords = elephant_dataset[, c('location.long', 'location.lat')], proj4string = CRS(crs))
  
  # change projection of spatial points
  elephant_coord_32733 <- coordinates(spTransform(elephant_coord_4326, CRS(new_crs)))
  
  # plug in new coordinates into original elephant dataset
  elephant_dataset$location.long <- elephant_coord_32733[,1]
  elephant_dataset$location.lat <- elephant_coord_32733[,2]
  
  # return elephant dataset 
  return(elephant_dataset)
  
}
