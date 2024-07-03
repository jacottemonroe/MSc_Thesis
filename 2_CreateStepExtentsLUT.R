## MSc_Thesis 
## Jacotte Monroe 
## 18/03/24

## R script that runs a function to create a look-up table of the spatial extents in the elephant step dataset. 
## Script takes the run settings to retrieve the step_xyt dataset and summarizes the information into a table for MODIS image retrieval.
## The last row contains information on the first/last dates of the dataset as well as the largest spatial extent.
## Script outputs: 
##    2_a1: Look-up table of spatial extents and dates of the movement steps, saved as csv object. (intermediate output)




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
if(is.na(run_settings[[6]])){run_settings[[6]] <- ''}
if(is.na(run_settings[[7]])){run_settings[[7]] <- ''}

# define input and output suffixes
input_suffix <- run_settings[[6]]
output_suffix <- run_settings[[7]]

# define run filepath 
run_filepath <- paste0('data/', ID, '/', week, '/')

print(paste('Now creating step extent look-up table for elephant', ID, 'of week', week, '...'))



###########
## Create step extents look-up table for image retrieval
###########

# load function 
source('functions/2_a_creatingStepExtentsLUT.R')

# necessary packages 
if(!('dplyr') %in% installed.packages()){install.packages('dplyr')} #for grouping in table (max/min)
library(dplyr)

# run function
createStepExtentsLUT(run_filepath, ID, week, pseudo_abs_method, input_suffix = input_suffix, 
                     output_suffix = output_suffix, output_directory = 'data/')

print(paste('(DONE) Creating step extent LUT for elephant', ID, 'of week', week))
