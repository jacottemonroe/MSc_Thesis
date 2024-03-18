## MSc Thesis 
## Jacotte Monroe 
## Function script 


# Function that reads a dataframe of elehpant data and selects a specific week. 
# Changes datetime format. Turns dataframe into track object for further processing. 
# 
# Input: file name (as string), week of interest (numeric)
# Output: the elephant data of interest as track object 


transformToTrackObject <- function(file_name, ID, week, output_directory = 'data/'){
  
  # create data directory if it does not yet exist
  output_filepath <- paste0(output_directory, ID, '/', week, '/')
  
  if(!dir.exists(output_filepath)){dir.create(output_filepath, recursive = T)}
  
  # get elephant dataset
  full_df <- read.csv(file_name, row.names = 1)
  
  # get data for week of interest 
  subset_df <- full_df[full_df$week == week,]
  
  # change date_time format to remove time 
  full_df$date_time <- as.POSIXct(full_df$date_time, tz = 'Africa/Maputo')
  subset_df$date_time <- as.POSIXct(subset_df$date_time, tz = 'Africa/Maputo')
  
  # turn elephant data frame into track_xyt object for model building
  # 'burst_' is a fixed/mandatory column name if want to generate steps for multiple paths
  full_track <- make_track(full_df, location.long, location.lat, date_time, week = week, burst_ = path)
  subset_track <- make_track(subset_df, location.long, location.lat, date_time, week = week, burst_ = path)
  
  # save new track_xyt object as RDS 
  saveRDS(full_track, paste0(output_filepath, '1_a1_elephant_full_track_xyt.RDS'))
  saveRDS(subset_track, paste0(output_filepath, '1_a2_elephant_track_xyt.RDS'))
  
}