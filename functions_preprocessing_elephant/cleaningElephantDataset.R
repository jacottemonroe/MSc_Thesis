## MSc Thesis 
## Data Exploration 
## Jacotte Monroe 

# function to clean dataset 
# INPUT: elephant dataset, ID
# OUTPUT: clean version of elephant dataset for single elephant 

cleanElephantDataset <- function(elephant_dataset, elephant_ID){
  
  # select relevant columns only
  df <- data.frame(elephant_dataset['individual.local.identifier'], elephant_dataset['location.long'], elephant_dataset['location.lat'], elephant_dataset['timestamp'])
  
  # new dataframe for one elephant 
  df_id <- df[df['individual.local.identifier'] == elephant_ID, ]
  
  # reset indexing 
  # source: https://statisticsglobe.com/change-index-numbers-of-data-frame-rows-in-r
  rownames(df_id) <- 1:nrow(df_id)
  
  # remove indentifier col 
  # source: https://datatofish.com/remove-column-dataframe-r/
  df_id <- subset(df_id, select = -individual.local.identifier)
  
  # change date format 
  # source: https://www.neonscience.org/resources/learning-hub/tutorials/dc-convert-date-time-posix-r
  # NOTE: must specify timezone otherwise some timestamps because NA because dates may not exist in the default time zone (due to daylight savings)
  # source: https://stackoverflow.com/questions/21160094/using-as-posixct-in-r-giving-na-for-identical-character-structures
  # source for Namibia timezone (Central Africa Time): https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
  df_id$timestamp <- as.POSIXct(df_id$timestamp,format="%d/%m/%Y %H:%M", tz = 'Africa/Maputo')
  
  return(df_id)
}


