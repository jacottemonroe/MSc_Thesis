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


generateSteps <- function(track_dataset, n_random_steps = 20, step_length_distribution = 'gamma', turn_angle_distribution = 'vonmises', output_filename){
  
  # turn fixes into observed steps 
  # by burst so steps not created for fixes in different paths
  true_steps <- steps_by_burst(track_dataset)
  
  # generate corresponding random pseudo-absence steps
  set.seed(1234)
  all_steps <- random_steps(true_steps, n_control = n_random_steps,
                            sl_distr = fit_distr(true_steps$sl_, step_length_distribution),
                            ta_distr = fit_distr(true_steps$ta_, turn_angle_distribution))
  
  # save table
  write.csv(all_steps, output_filename)
  
}