## MSc Thesis 
## Jacotte Monroe 
## Function SSF script 


# Function that takes the elephant dataset of steps and derives the spatiotemporal extents of the dataset
#   for satellite image retrieval. 
#   
# Input: dataset filename, elephant ID and week (for output name), lag of ndvi rate (calculated later, but need data prior to passage), 
#   output directory name. 
#   
# Output: Look up table of step extents saved as csv. 


createStepExtentLUT <- function(input_filename, elephant_ID, week_of_data, ndvi_rate_lag = 7, output_directory = 'data/step_extents/'){
  
  # read elephant step dataset
  all_steps <- read.csv(input_filename)
  
  # get start date of step
  all_steps$start_date <- as.Date(all_steps$t1_)
  
  # get end date of step 
  all_steps$end_date <- all_steps$start_date + 1
  
  # get start and end dates for the week before 
  all_steps$start_date_prev_week <- all_steps$start_date - ndvi_rate_lag
  all_steps$end_date_prev_week <- all_steps$start_date_prev_week + 1
  
  # get extreme coordinates for each day
  step_extents <- all_steps
  
  step_extents <- step_extents %>% group_by(start_date) %>% mutate(xmin = min(c(x1_, x2_))) %>% mutate(ymin = min(c(y1_, y2_))) %>% mutate(xmax = max(c(x1_, x2_))) %>% mutate(ymax = max(c(y1_, y2_)))
  
  # get table of all extents per date
  step_extents <- unique(step_extents[,c('start_date', 'end_date', 'start_date_prev_week', 'end_date_prev_week', 'xmin', 'ymin', 'xmax', 'ymax')])
  
  # add largest extent as new row in step extent LUT 
  # source: https://www.rdocumentation.org/packages/terra/versions/1.7-71/topics/ext
  step_extents <- rbind(step_extents, data.frame('start_date' = min(step_extents$start_date, na.rm = T), 
                                                 'end_date' = max(step_extents$end_date, na.rm = T), 
                                                 'start_date_prev_week' = min(step_extents$start_date_prev_week, na.rm = T),
                                                 'end_date_prev_week' = max(step_extents$end_date_prev_week, na.rm = T), 
                                                 'xmin' = min(step_extents$xmin), 'xmax' = max(step_extents$xmax), 
                                                 'ymin' = min(step_extents$ymin), 'ymax' = max(step_extents$ymax)))
  
  # save table 
  write.csv(step_ex, paste0(output_directory, elephant_ID, '_step_ex_w', as.character(week_of_data),'.csv'))
  
}