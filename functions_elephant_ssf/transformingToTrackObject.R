## MSc Thesis 
## Jacotte Monroe 
## Function script 


# Function that reads a dataframe of elehpant data and selects a specific week. 
# Changes datetime format. Turns dataframe into track object for further processing. 
# 
# Input: file name (as string), week of interest (numeric)
# Output: the elephant data of interest as track object 

if(!('lubridate') %in% installed.packages()){install.packages('lubridate')}
library(lubridate)

if(!('amt') %in% installed.packages()){install.packages('amt')}
library(amt)


transformToTrackObject <- function(file_name, week){
  # get elephant dataset
  full_df <- read.csv(file_name, row.names = 1)
  
  # get week of interest 
  df <- full_df[full_df$week == week,]
  
  # change date_time format to remove time 
  df$date_time <- as.POSIXct(df$date_time, tz = 'Africa/Maputo')
  
  # turn elephant data frame into track_xyt object for model building
  # 'burst_' is a fixed/mandatory column name if want to generate steps for multiple paths
  df_track <- make_track(df, location.long, location.lat, date_time, week = week, burst_ = path)
  
  return(df_track)
}