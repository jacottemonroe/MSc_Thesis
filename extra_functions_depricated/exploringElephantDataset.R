## MSc Thesis 
## Data exploration 

## Function to explore each elephant dataset

# packages 
if(!('geosphere') %in% installed.packages()){install.packages('geosphere')} # for calc distance
library(geosphere)


exploreElephantData <- function(elephant_dataset, elephant_id, basemap){
  
  # clean elephant dataset
  source('functions/cleaningElephantDataset.R')
  elephant <- cleanElephantDataset(elephant_dataset, elephant_id)
  
  # function to calc different between two timestamps 
  # source: https://www.geeksforgeeks.org/how-to-subtract-time-in-r/
  calcTimeDifference <- function(i){
    difftime(elephant$timestamp[i], elephant$timestamp[i-1], units = 'hours')
  }
  
  # new col for time interval --> apply the function to all rows of dataframe using sapply()
  # first define the sequence of indices that sapply() should go through then apply function
  # first instance has to be turned into NA because it is removed
  # source: https://stackoverflow.com/questions/68000621/distance-between-coordinates-in-dataframe-sequentially
  elephant$time_interval <- c(NA, sapply(seq.int(2,nrow(elephant)), calcTimeDifference))
  
  # function that derives the distance for each pair coord and the pair coord in next instance
  # makes use of geosphere.distHaversine function 
  # source: https://stackoverflow.com/questions/68000621/distance-between-coordinates-in-dataframe-sequentially
  calcDistance <- function(i){
    distm(c(elephant$location.long[i-1],elephant$location.lat[i-1]),
          c(elephant$location.long[i], elephant$location.lat[i]),
          fun = distHaversine)
  }
  
  # new col for distance --> apply function to all rows in dataframe using sapply()
  elephant$distance <- c(NA, sapply(seq.int(2,nrow(elephant)), calcDistance))
  
  # get summary statistics for elephant 
  statistics_table <- summary(elephant)
  
  # to get position of where NA values are --> which(is.na(dataset1), arr.ind=TRUE)
  na_positions <- which(is.na(elephant), arr.ind = T)
  
  # identify outlier values 
  outliers_time <- which(elephant$time_interval > 24, arr.ind = T)
  outliers_distance <- which(elephant$distance > 10000, arr.ind = T)
  
  # get map of movement --> call the input variable of the basemap 
  mov_map <- basemap + 
    labs(title = "Elephant Movement", subtitle = elephant_id, x = "Longitude", y = "Latitude") +
    geom_path(data = elephant, aes(x = location.long, y = location.lat), linewidth = 0.1, show.legend = F) +
    annotation_north_arrow(location = 'tl', which_north = 'true', 
                           pad_x = unit(0.04, "in"), pad_y = unit(0.3, "in"),
                           style = north_arrow_fancy_orienteering()) +
    annotation_scale(location = 'tl') +
    theme_minimal()
  
  # summarize all outputs into one list 
  stats_list <- list(basic_statistics = statistics_table, positions_NA_values = na_positions, 
                     positions_temporal_outliers = outliers_time, positions_spatial_outliers = outliers_distance, 
                     movement_map = mov_map)
  # check if script above works, if doesn't this is alternative otherwise remove this line: names(stats_list) <- c('basic_statistics', 'positions_NA_values', 'positions_temporal_outliers', 'positions_spatial_outliers', 'movement_map')
  
  return(stats_list)
}


# # get histograms of movement and fixes 
# histogram_fixes <- ggplot(data = df_id, aes(x = time_interval)) +
#   geom_histogram(bins = 30, binwidth = 10)
# 
# histogram_fixes
# 
# histogram_dist <- ggplot(data = df_id, aes(x = distance)) +
#   geom_histogram(bins = 30, binwidth = 100)
# 
# histogram_dist








