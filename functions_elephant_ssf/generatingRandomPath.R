## MSc Thesis 
## Jacotte Monroe 

## Function elephant modeling script 


# Function that takes point coordinates (true observations) and generates random steps by 
#   buffering each point and retrieving a random point on the buffer line. The random points
#   are then transformed into steps. 
#   
# Input: Dataset of observed point fixes, dataset of observed steps, dataset of starting 
#         point fixes, dataset of all steps, defined distance for buffer size 
#         (average step length from observed steps), burst or path number 
#         (since will be looping this function), loop number (since will be generating multiple random paths). 
# 
# Output: Updated dataset of all steps containing previously generated steps plus the newly generated steps. 


if(!('sf') %in% installed.packages()){install.packages('sf')} #for grouping in table (max/min)
library(sf)



generateRandomPath <- function(starting_fixes_dataset, true_step_dataset, step_lengths, 
                               turning_angles, density_distr_step_lengths, density_distr_turning_angles, 
                               true_fixes_dataset, all_steps_dataset, burst_number, loop_number){
  
  # call necessary functions 
  source('functions_elephant_ssf/samplingCustomDistribution.R')
  source('functions_elephant_ssf/calculatingNewPoint.R')
  
  # create dataframe of starting point for burst of interest 
  fake_path <- data.frame(t_ = starting_fixes_dataset$t_[starting_fixes_dataset$burst_ == burst_number],  
                          x_ = starting_fixes_dataset$x_[starting_fixes_dataset$burst_ == burst_number], 
                          y_ = starting_fixes_dataset$y_[starting_fixes_dataset$burst_ == burst_number])
  
  # for all true steps from the burst of interest, generate a random step to get a random path of the same length
  for(i in 1:nrow(true_step_dataset[true_step_dataset$burst_ == burst_number,])){
    
    # get starting coordinates 
    starting_x <- fake_path[nrow(fake_path), 2]
    starting_y <- fake_path[nrow(fake_path), 3]
    
    # set seed 
    seed <- i+100*(loop_number-1)
    
    # generate random step length 
    random_sl <- sampleCustomDistribution(step_lengths, density_distr_step_lengths, seed) #full_sl
    
    # generate random turning angle 
    random_ta <- sampleCustomDistribution(turning_angles, density_distr_turning_angles, seed) #full_ta
    
    # calculate coordinates of new point 
    new_coordinates <- calculateNewPoint(starting_x, starting_y, random_sl, random_ta)
    
    # add coordinates to fake path data frame
    # source: https://rdrr.io/cran/sf/man/st_coordinates.html
    fake_path <- rbind(fake_path, data.frame(t_ = true_fixes_dataset$t_[i+1],
                                             x_ = new_coordinates[1],
                                             y_ = new_coordinates[2]),
                       make.row.names = F)
    
  }
  
  # add new column for information on the burst of interest in the new fake path dataset
  fake_path$burst_ <- burst_number
  
  # turn the list of random points into a track object
  fake_track <- make_track(fake_path, x_, y_, t_, burst_ = burst_)
  
  # transform the fake points dataset into steps
  fake_steps <- steps_by_burst(fake_track)
  
  # add columns to the new fake steps dataset for the case (F = false steps), 
  #     the corresponding step ID (to match with true steps), and the loop number 
  #     to differentiate between randomly generated steps
  fake_steps$case_ <- F
  fake_steps$step_id_ <- row.names(fake_steps)
  fake_steps$random_id_ <- loop_number
  
  # add the new fake steps to the larger dataset containing all steps 
  all_steps_dataset <- rbind(all_steps_dataset, fake_steps)
  
  return(all_steps_dataset)
}