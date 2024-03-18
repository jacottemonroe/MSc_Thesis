## MSc Thesis 
## Jacotte Monroe 
## Function script 


# Function that reads a dataframe of elehpant data and selects a specific week. 
# Changes datetime format. Turns dataframe into track object for further processing. 
# 
# Input: file name (as string), week of interest (numeric)
# Output: the elephant data of interest as track object 


transformToTrackObject <- function(file_name, ID, week = NULL, output_directory = 'data/elephant_etosha/'){
  
  # get elephant dataset
  full_df <- read.csv(file_name, row.names = 1)
  
  # get week of interest if specified, otherwise take whole dataset
  if(!is.null(week)){
    df <- full_df[full_df$week == week,]
    
    # create output filepath 
    output_filepath <- paste0(output_directory, ID, '/', week, '/')
    
    output_filename <- 'elephant_track_xyt.RDS'
    
  }else{
    df <- full_df
    
    # create output filepath 
    output_filepath <- paste0(output_directory, ID, '/')
    
    output_filename <- 'elephant_full_track_xyt.RDS'
  }
  
  # create data directory if it does not yet exist
  if(!dir.exists(output_filepath)){dir.create(output_filepath, recursive = T)}
  
  # change date_time format to remove time 
  df$date_time <- as.POSIXct(df$date_time, tz = 'Africa/Maputo')
  
  # turn elephant data frame into track_xyt object for model building
  # 'burst_' is a fixed/mandatory column name if want to generate steps for multiple paths
  df_track <- make_track(df, location.long, location.lat, date_time, week = week, burst_ = path)
  
  # save new track_xyt object as RDS 
  saveRDS(df_track, paste0(output_filepath, output_filename))
  
  #return(df_track)
}