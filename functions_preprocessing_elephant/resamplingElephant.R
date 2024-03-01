## MSc Thesis
## Jacotte Monroe 

## Preprocessing 

## function to resample elephant dataset

if(!('dplyr') %in% installed.packages()){install.packages('dplyr')} #for dataframe slicing & resampling & path spliting 
library(dplyr)

if(!('padr') %in% installed.packages()){install.packages('padr')} # for resampling (thicken function)
library(padr)

if(!('zoo') %in% installed.packages()){install.packages('zoo')} # for linear interpolation
library(zoo)

resampleElephantData <- function(elephant_data, time_interval = 4, acceptable_NA_gap = 1, interpolation_method = 'linear interpolation') {
  
  # specify time interval as string
  interval_str <- paste(as.character(time_interval), 'hour')
  
  # resample data into groups of specific interval (using dplyr and padr)
  # source: https://stackoverflow.com/questions/44155615/how-to-perform-r-time-based-resampling-with-a-given-time-period-equivalently-to
  # source: https://dplyr.tidyverse.org/reference/summarise_all.html
  # source: https://cengel.github.io/R-data-wrangling/dplyr.html#add-new-columns
  # source: https://stackoverflow.com/questions/66232833/summarise-multiple-columns-using-dplyr-r
  elephant_resampled <- elephant_data %>% thicken(interval_str, colname = 'resampled_time') %>%
    group_by(resampled_time) %>% summarize(across(c(location.long, location.lat), mean))
  
  # identify missing data by creating full date range
  # source: https://stackoverflow.com/questions/55982164/how-can-i-complete-missing-values-in-time-series
  elephant_gaps <- data.frame(date_time = seq(min(elephant_resampled$resampled_time), 
                                              max(elephant_resampled$resampled_time), by = interval_str), 
                              location.long = NA, location.lat = NA)
  
  # fill known coordinates into new dataframe
  elephant_gaps[elephant_gaps$date_time %in% elephant_resampled$resampled_time, ]$location.long = elephant_resampled$location.long
  elephant_gaps[elephant_gaps$date_time %in% elephant_resampled$resampled_time, ]$location.lat = elephant_resampled$location.lat
  
  # calculate NA gap that is less than 24h 
  #gap <- (24-time_interval)/time_interval
  gap <- acceptable_NA_gap
  
  # interpolation of missing coordinate values (if gap is less than 24h, otherwise NAs remain)
  if (interpolation_method == 'linear interpolation'){
    elephant_gaps$location.long <- na.approx(elephant_gaps$location.long, maxgap = gap)
    elephant_gaps$location.lat <- na.approx(elephant_gaps$location.lat, maxgap = gap)
    print('linear interpolation done')
  } else{
    elephant_gaps$location.long <- na.spline(elephant_gaps$location.long, maxgap = gap)
    elephant_gaps$location.lat <- na.spline(elephant_gaps$location.lat, maxgap = gap)
    print('cubic spline interpolation done')
  }
  
  return(elephant_gaps)
}