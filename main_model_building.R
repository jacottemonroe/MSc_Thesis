## MSc Thesis 
## Jacotte Monroe 
## Model Building 

###############################################################################
###### START BY RUNNING THIS PART OF THE SCRIPT UNTIL BREAK ###################
###############################################################################

# Dataframe of runs with parameter combination --> get row of interest TEMPORARY 
run_table <- data.frame(ID = 'LA2', week = 2027, pseudo_abs_method = 'random_path_custom_distr')

# need to save as csv otherwise having issues reading the dataframe in python (even though this has useless column)
write.csv(run_table, 'data/run_settings.csv')


################################ load elephant data #########################

# read the run settings --> specify first (redundant row needed for python reading) as df index
run_table <- read.csv('data/run_settings.csv', row.names = 1)

# select elephant ID 
# original test was with elephant LA2
ID <- run_table$ID #'LA26'

# select week to test 
# original test was with week 2027
week <- run_table$week #2048 #2060 #2027 #2300

# select pseudo-absence path generator method --> Choices are 1) random_path_custom_distr 2) random_path_buffer_point 3) random step')
pseudo_abs_method <- run_table$pseudo_abs_method

# define run filepath 
run_filepath <- paste0('data/', ID, '/', week, '/')

file_name <- paste0('data/elephant_etosha/elephant_fixes/preprocessed_elephant_', ID,'.csv')

source('functions_elephant_ssf/transformingToTrackObject.R')

# necessary libraries 
if(!('amt') %in% installed.packages()){install.packages('amt')}
library(amt)

transformToTrackObject(file_name, ID, week, output_directory = 'data/')


########################## generate steps ############################## 

source('functions_elephant_ssf/generatingSteps.R')

# load necessary libraries 
# if(!('amt') %in% installed.packages()){install.packages('amt')}
# library(amt)
if(!('dplyr') %in% installed.packages()){install.packages('dplyr')} #for grouping in table (max/min)
library(dplyr)

if(pseudo_abs_method == 'random_path_buffer_point'){
  if(!('sf') %in% installed.packages()){install.packages('sf')} #for grouping in table (max/min)
  library(sf)
  if(!('terra') %in% installed.packages()){install.packages('terra')} #for grouping in table (max/min)
  library(terra)
}

generateSteps(run_filepath, ID, week, 20, random_data_method = pseudo_abs_method, paste0('data/'))



######################### step extents table ########################## 

source('functions_elephant_ssf/creatingStepExtentLUT.R')

# necessary packages 
if(!('dplyr') %in% installed.packages()){install.packages('dplyr')} #for grouping in table (max/min)
library(dplyr)

createStepExtentLUT(run_filepath, ID, week, pseudo_abs_method, output_directory = 'data/')


###############################################################################
###### GO TO JN TO GENERATE MODIS IMAGES USING STEP EXTENT LUT ################
###### ONCE HAVE GENERATED IMAGES CAN CONTINUE RUNNING SCRIPT #################
###############################################################################


########################## extract covariates ##########################

# create table of all extracted covariates for each step
source('functions_elephant_ssf/extractingCovariates.R')

# necessary packages 
if(!('terra') %in% installed.packages()){install.packages('terra')}
library(terra)
if(!('amt') %in% installed.packages()){install.packages('amt')}
library(amt)

loadAndExtractCovariates(run_filepath, ID, week, lag, random_data_method = pseudo_abs_method, output_directory = 'data/')


###################### visualize paths #####################
source('functions_elephant_ssf/visualizingPaths.R')

# libraries to run next function (needed to generate correlation matrix)
# if(!('terra') %in% installed.packages()){install.packages('terra')}
# library(terra)
if(!('ggplot2') %in% installed.packages()){install.packages('ggplot2')}
library(ggplot2)
if(!('tidyterra') %in% installed.packages()){install.packages('tidyterra')} # for mapping spatraster
library(tidyterra)

visualizePaths(run_filepath, ID, week, pseudo_abs_method, title = "Elephant Movement on Mean NDVI", output_directory = 'output/')



######################### fit model #################################

source('functions_elephant_ssf/fittingSSFModel.R')

# libraries to run next function (needed to generate correlation matrix)
if(!('dplyr') %in% installed.packages()){install.packages('dplyr')}
library(dplyr) 
# if(!('ggplot2') %in% installed.packages()){install.packages('ggplot2')}
# library(ggplot2) # needed for loading GGally package 
if(!('GGally') %in% installed.packages()){install.packages('GGally')}
library(GGally)
# if(!('amt') %in% installed.packages()){install.packages('amt')}
# library(amt)
if(!('car') %in% installed.packages()){install.packages('car')}
library(car) # needed to run vif()

fitSSFModel(run_filepath, ID, week, pseudo_abs_method, multicolinearity_check = T, full = T, 'output/')
fitSSFModel(run_filepath, ID, week, pseudo_abs_method, multicolinearity_check = F, full = F, 'output/')


