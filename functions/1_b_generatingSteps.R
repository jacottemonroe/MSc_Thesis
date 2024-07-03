## MSc Thesis 
## Jacotte Monroe 
## 18/03/24
## Function script 


# Function that takes the elephant fixes dataset (as track object) and transforms it into steps
#   with step length and turning angle attributes. Generates random pseudo-absence steps based on specified distributions. 
#   
# Input: elephant dataset (track object), number of random steps to generate per observed step (numeric), 
#   distribution type for step length and turning angle (string), output filename (string).
#   
# Output: step dataset saved as csv 

# NOTE: This script has kept the option of generating pseudo-absence steps by means of 1) sampling from density distributions, 
#       2) buffering the points 3) randomly generating single step at each step. THE FIRST METHOD SHOULD BE USED (SET AS DEFAULT ALREADY). The other methods are less realistic.

# if(!('dplyr') %in% installed.packages()){install.packages('dplyr')} #for grouping in table (max/min)
# library(dplyr)


generateSteps <- function(track_dataset_directory, 
                          ID, week, n_random_steps = 20, random_data_method = 'random_path_custom_distr', 
                          output_directory = 'data/', input_suffix = '', output_suffix = ''){
  
  # create output filepath 
  output_filepath <- paste0(output_directory, ID, '/', week, '/')
  
  # create data directory if it does not yet exist
  if(!dir.exists(output_filepath)){dir.create(output_filepath, recursive = T)}
  
  # load track datasets 
  track_dataset_full <- readRDS(paste0(track_dataset_directory, '1_a1_elephant_full_track_xyt', input_suffix, '.RDS'))
  track_dataset_subset <- readRDS(paste0(track_dataset_directory, '1_a2_elephant_track_xyt', input_suffix, '.RDS'))
  
  
  ##### create observed dataset of steps 
  
  # turn fixes into observed steps 
  # by burst so steps not created for fixes in different paths
  true_steps <- steps_by_burst(track_dataset_subset)
  
  # add information attributes as new columns (to differentiate the true and false steps)
  true_steps$case_ <- T
  true_steps$step_id_ <- NA
  true_steps$random_id_ <- NA
  
  # number each true step by burst
  for(b in unique(true_steps$burst_)){
    # select rows of that burst that are true
    steps <- true_steps[true_steps$burst_ == b,]
    true_steps$step_id_[true_steps$burst_ == b] <- 1:nrow(steps)
  }
  
  
  ##### create custom distributions from full dataset of all observations for that elephant 
  
  # turn the full dataset into steps 
  full_steps <- steps_by_burst(track_dataset_full)
  
  if(random_data_method == 'random_path_custom_distr'){
    
    # retrieve the step lengths and turning angles of the true steps
    sl <- full_steps$sl_
    ta <- full_steps$ta_
    
    # derive the probability density functions from step lengths and turning angles of the elephant in week of interest
    # source: https://www.tutorialspoint.com/how-to-generate-a-probability-density-distribution-from-a-set-of-observations-in-r
    density_sl <- density(sl, na.rm = T)
    density_ta <- density(ta, na.rm = T) 
    
  }else if(random_data_method == 'random_path_buffer_point'){
    
    # calculate average step length from full elephant dataset 
    average_step_length <- mean(full_steps$sl_)
    
  }
  
  
  ##### create final dataset of presence and absence steps 
  
  # add presence steps to final step dataset
  all_steps <- true_steps
  
  
  ##### get initial starting points from the observed dataset of the week (necessary for multiple paths)
  
  # select starting fixes of each burst
  # PACKAGE: dplyr should be installed
  # source: https://www.r-bloggers.com/2022/07/select-the-first-row-by-group-in-r/
  starting_fixes <- all_steps %>% group_by(burst_) %>% filter(row_number()==1)
  
  
  ##### generate random pseudo-absence paths 

  source('functions/1_b_generatingRandomPath.R')
  
  if(random_data_method == 'random_path_custom_distr'){
    
    # take random step length and turning angle from custom distributions then generate steps to create pseudo-absence paths
    
    # generate random sets of paths matching the full elephant movement of that week (all true separate paths included)
    for(loop in 1:n_random_steps){
      
      # for each true separate path generate a corresponding false path 
      for(burst in starting_fixes$burst_){
        
        # generate a random pseudo-absence path for the corresponding true path and add it to the dataset of all steps
        all_steps <- generateRandomPathFromCustomDistribution(starting_fixes, true_steps, sl, ta, density_sl,
                                                              density_ta, all_steps, burst, loop) 
      }
    }
    
    # define filename 
    output_filename <- paste0('1_b1_all_steps_', random_data_method, output_suffix, '.RDS')
    
  }else if(random_data_method == 'random_path_buffer_point'){
    
    # create a spatial buffer around a point then randomly sample a point on the buffer line to create a new point 
    # then generate steps to create pseudo-absence paths 
    
    # generate random sets of paths matching the full elephant movement of that week (all true separate paths included)
    for(loop in 1:n_random_steps){
      
      # for each true separate path generate a corresponding false path 
      for(burst in 1:nrow(starting_fixes)){
        
        # generate a random pseudo-absence path for the corresponding true path and add it to the dataset of all steps
        all_steps <- generateRandomPathFromBufferPoint(starting_fixes, true_steps, track_dataset_subset, 
                                                       all_steps, average_step_length, burst, loop) 
      }
    }
    
    # define filename 
    output_filename <- paste0('1_b1_all_steps_', random_data_method, output_suffix, '.RDS')
    
  }else if(random_data_method == 'random_step'){
    
    # create random pseudo-absence steps at the starting point of each true step 
    # step lengths drawn from gamma distribution and turning angles from von mises distribution 
    set.seed(1234)
    all_steps <- random_steps(true_steps, n_control = n_random_steps,
                              sl_distr = fit_distr(true_steps$sl_, 'gamma'),
                              ta_distr = fit_distr(true_steps$ta_, 'vonmises'))
    
    # define filename 
    output_filename <- paste0('1_b1_all_steps_', random_data_method, output_suffix, '.RDS')
  }else{print('Wrong pseudo-absence method specified. Choices are 1) random_path_custom_distr 2) random_path_buffer_point 3) random_step')}
  
  ##### save output dataframe of all steps
  # source: https://rstudio-education.github.io/hopr/dataio.html
  saveRDS(all_steps, paste0(output_filepath, output_filename))
  
}

