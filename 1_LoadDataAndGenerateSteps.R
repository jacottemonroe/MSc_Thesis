## MSc_Thesis 
## Jacotte Monroe 
## 18/03/24

## R script that runs functions to retrieve elephant fixes dataset for week of interest. 
## Data is transformed into a dataset of movement steps that constitute the presence movement paths. 
## Pseudo-absence movement paths are generated following the method specified in run settings. 
## Script outputs: 
##    1_a1: Dataset of all fixes from elephant of interest as track_xyt object, saved as RDS object. (intermediate output)
##    1_a2: Dataset of fixes from week and elephant of interest as track_xyt object, saved as RDS object. (intermediate output)
##    1_b1: Dataset of presence/pseudo-absence steps from elephant and week of interest as steps_xyt object, saved as RDS object. (intermediate output)




###########
## Read run settings 
###########

run_settings <- read.csv('data/single_run_settings.csv', header = F, row.names = 1)

# define elephant ID
ID <- run_settings[[1]]

# define week to test 
week <- run_settings[[2]]

# define pseudo-absence path generator method --> Choices are 1) random_path_custom_distr 2) random_path_buffer_point 3) random_step
pseudo_abs_method <- run_settings[[3]]

# replace NA from suffix columns of run settings to empty strings
if(is.na(run_settings[[7]])){run_settings[[7]] <- ''}

# define input and output suffixes
output_suffix <- run_settings[[7]]

# define run filepath 
run_filepath <- paste0('data/', ID, '/', week, '/')

# define file name of elephant fixes based on elephant ID from run settings 
file_name <- paste0('data/elephant_etosha/preprocessed_elephant_', ID,'.csv')

print(paste('(START) Processing elephant', ID, 'of week', week, '...'))



###########
## Load elephant data and transform data frame into a track_xyt object
###########

# load function
source('functions/1_a_transformingToTrackObject.R')

# necessary packages to load 
if(!('amt') %in% installed.packages()){install.packages('amt')}
library(amt)

# run function 
transformToTrackObject(file_name, ID, week, output_directory = 'data/')

print(paste('(DONE) Transforming elephant', ID, 'of week', week, 'into track object.'))
print(paste('Now generating steps for elephant', ID, 'of week', week, '...'))



###########
## Generate the elephant presence and pseudo-absence steps dataset
###########

# load function 
source('functions/1_b_generatingSteps.R')

# load necessary libraries (commented out = necessary but already loaded from previous command)
# if(!('amt') %in% installed.packages()){install.packages('amt')}
# library(amt)
if(!('dplyr') %in% installed.packages()){install.packages('dplyr')} 
library(dplyr)

if(pseudo_abs_method == 'random_path_buffer_point'){
  if(!('sf') %in% installed.packages()){install.packages('sf')} 
  library(sf)
  if(!('terra') %in% installed.packages()){install.packages('terra')} 
  library(terra)
}

# create custom seed for run by combining elephant ID and week 
# Note: the seed should be the same every time I run a certain run but different between runs
seed <- as.numeric(paste0(sub('LA', '', ID), week))

set.seed(seed)

# run function
generateSteps(run_filepath, ID, week, 20, random_data_method = pseudo_abs_method, output_suffix = output_suffix)

print(paste('(DONE) Generating steps for elephant', ID, 'of week', week, 'using pseudo-absence method:', pseudo_abs_method))

