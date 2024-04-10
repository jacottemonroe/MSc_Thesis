## MSc_Thesis 
## Jacotte Monroe 





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

# set downscaling parameter
downscaling_setting <- run_settings[[4]]

# define run filepath 
run_filepath <- paste0('data/', ID, '/', week, '/')

print(paste('Now visualizing model for elephant', ID, 'of week', week, '...'))



############
## Make graphs from model
############

# load function 
source('functions_elephant_ssf/7_a_plottingModels.R')

# necessary packages 
if(!('sjPlot') %in% installed.packages()){install.packages('sjPlot')} 
library(sjPlot)
if(!('ggplot2') %in% installed.packages()){install.packages('ggplot2')} 
library(ggplot2)


# run function 
#plotModels(directory = 'output/', ID, week, random_data_method = pseudo_abs_method, downscaling = downscaling_setting, full = T)
plotModels(directory = 'output/', ID, week, random_data_method = pseudo_abs_method, downscaling = downscaling_setting, full = F)
