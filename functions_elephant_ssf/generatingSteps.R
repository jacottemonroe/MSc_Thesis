## MSc Thesis 
## Jacotte Monroe 
## Function SSF script 


# Function that takes the elephant fixes dataset (as track object) and transforms it into steps
#   with step length and turning angle attributes. Generates random pseudo-absence steps based on specified distributions. 
#   
# Input: elephant dataset (track object), number of random steps to generate per observed step (numeric), 
#   distribution type for step length and turning angle (string), output filename (string).
#   
# Output: step dataset saved as csv 

if(!('dplyr') %in% installed.packages()){install.packages('dplyr')} #for grouping in table (max/min)
library(dplyr)


generateSteps <- function(track_dataset, n_random_steps = 20, step_length_distribution = 'gamma', turn_angle_distribution = 'vonmises', output_filename){
  
  ##### create observed dataset of steps 
  
  # turn fixes into observed steps 
  # by burst so steps not created for fixes in different paths
  true_steps <- steps_by_burst(track_dataset)
  
  # add information attributes as new columns (to differentiate the true and false steps)
  true_steps$case_ <- T
  true_steps$step_id_ <- row.names(true_steps)
  true_steps$random_id_ <- NA
  
  # retrieve average step length to use as fixed step length for random steps
  set_distance <- mean(true_steps$sl_)
  
  
  ##### create final dataset of presence and absence steps 
  
  # add presence steps to final step dataset
  all_steps <- true_steps
  
  
  ##### get initial starting points from the observed dataset (necessary for multiple paths)
  
  # select starting fixes of each burst
  # PACKAGE: dplyr should be installed
  # source: https://www.r-bloggers.com/2022/07/select-the-first-row-by-group-in-r/
  starting_fixes <- track_dataset %>% group_by(burst_) %>% filter(row_number()==1)
  
  
  ##### generate random pseudo-absence paths 

  source('functions_elephant_ssf/generatingRandomPath.R')
  
  # generate random sets of paths matching the full elephant movement of that week (all true separate paths included)
  for(loop in 1:n_random_steps){
    
    # for each true separate path generate a corresponding false path 
    for(burst in 1:nrow(starting_fixes)){
      
      # generate a random pseudo-absence path for the corresponding true path and add it to the dataset of all steps
      all_steps <- generateRandomPath(track_dataset, true_steps, starting_fixes, all_steps, set_distance, burst, loop) 
    }
  }
  
  
  
  ##### save output dataframe of all steps
  
  write.csv(all_steps, output_filename)
  
  # # generate corresponding random pseudo-absence steps
  # set.seed(1234)
  # all_steps <- random_steps(true_steps, n_control = n_random_steps,
  #                           sl_distr = fit_distr(true_steps$sl_, step_length_distribution),
  #                           ta_distr = fit_distr(true_steps$ta_, turn_angle_distribution))
  
  return(all_steps)
}

# 
# 
# ############################# 5. write a function that will generate a fake path
# 
# generateFakePath <- function(true_fixes_dataset, true_step_dataset, starting_fixes_dataset, 
#                              all_steps_dataset, step_distance, burst_number, loop_number){
#   
#   # create dataframe of starting point for burst of interest 
#   fake_path <- data.frame(t_ = starting_fixes_dataset$t_[starting_fixes_dataset$burst_ == burst_number],  
#                           x_ = starting_fixes_dataset$x_[starting_fixes_dataset$burst_ == burst_number], 
#                           y_ = starting_fixes_dataset$y_[starting_fixes_dataset$burst_ == burst_number])
#   
#   # for all true steps from the burst of interest, generate a random step to get a random path of the same length
#   for(i in 1:nrow(true_step_dataset[true_step_dataset$burst_ == burst_number,])){
#     
#     # transform starting point coordinates into a spatial object 
#     # source: https://www.dpi.inpe.br/gilberto/tutorials/software/R-contrib/sp/html/SpatialPoints.html
#     starting_point <- st_as_sf(fake_path[nrow(fake_path),2:3], coords = c('x_', 'y_'), 
#                                crs = crs('EPSG:32733'))
#     
#     # create buffer around point with fixed distance = average step length from all observed steps
#     # source: https://gis.stackexchange.com/questions/292327/creating-buffers-around-points-and-merging-with-spatialpolygonsdataframe-to-crea
#     buffer_point <- st_buffer(starting_point, dist = set_distance)
#     
#     # convert buffer polygon into polyline and sample random point along line
#     # source: https://stackoverflow.com/questions/68987453/generating-random-locations-along-the-outer-border-of-a-shp-polygon-using-r
#     set.seed(i+100*(loop_number-1))
#     new_point <- st_sample(st_cast(buffer_point, 'MULTILINESTRING'), 1)
#     
#     # add coordinates to fake path data frame
#     # source: https://rdrr.io/cran/sf/man/st_coordinates.html
#     fake_path <- rbind(fake_path, data.frame(t_ = true_fixes_dataset$t_[i+1], 
#                                              x_ = st_coordinates(new_point)[[1]], 
#                                              y_ = st_coordinates(new_point)[[2]]), 
#                        make.row.names = F)
#     
#   }
#   
#   # add new column for information on the burst of interest in the new fake path dataset
#   fake_path$burst_ <- burst_number
#   
#   # turn the list of random points into a track object
#   fake_track <- make_track(fake_path, x_, y_, t_, burst_ = burst_)
#   
#   # transform the fake points dataset into steps
#   fake_steps <- steps_by_burst(fake_track)
#   
#   # add columns to the new fake steps dataset for the case (F = false steps), 
#   #     the corresponding step ID (to match with true steps), and the loop number 
#   #     to differentiate between randomly generated steps
#   fake_steps$case_ <- F
#   fake_steps$step_id_ <- row.names(fake_steps)
#   fake_steps$random_id_ <- loop_number
#   
#   # add the new fake steps to the larger dataset containing all steps 
#   all_steps_dataset <- rbind(all_steps_dataset, fake_steps)
#   
#   return(all_steps_dataset)
# }
# 
# 
# ############### 6. loop the function for all bursts/paths in the weekly movement 
# ########################################### and generate 20 random sets of paths
# 
# # generate 20 random sets of paths matching the full elephant movement of that week (all true separate paths included)
# for(loop in 1:20){
#   
#   # for each true separate path generate a corresponding false path 
#   for(burst in 1:nrow(starting_fixes)){
#     
#     # generate a random pseudo-absence path for the corresponding true path and add it to the dataset of all steps
#     all_steps <- generateFakePath(df_track, true_steps, starting_fixes, all_steps, set_distance, burst, loop) 
#   }
# }
# 
# ########################### 7. save output dataframe of all steps for that model 
# 
# write.csv(all_steps, 'data/elephant_etosha/elephant_steps/all_steps_LA2_w2027.csv')