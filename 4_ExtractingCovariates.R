## MSc_Thesis
## Jacotte Monroe 
## 19/03/24

## R script that runs function to extract covarites from the movement steps and MODIS images. 
## Script takes the run settings to retrieve the step_xyt dataset and the MODIS images. 
## The images are matched to each step. The NDVI is retrieved along each step and the percentiles and standard deviation values are calculated. 
## The change in NDVI between the image at the time of movement and one week prior is also calculated, the percentiles and stdev are derived. 
## The extracted covariates are stored as additional columns in the step dataset. 
## The final dataframe therefore contains the response variable and predictors for running a model.
## Script outputs: 
##    4_a1: The movement step dataset with retrieved covariates, saved as csv. (intermediate result)



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
downscaling_model <- run_settings[[5]]

print(downscaling_setting)
print(class(downscaling_setting))

# replace NA from suffix columns of run settings to empty strings
if(is.na(run_settings[[6]])){run_settings[[6]] <- ''}
if(is.na(run_settings[[7]])){run_settings[[7]] <- ''}

# define input and output suffixes
input_suffix <- run_settings[[6]]
output_suffix <- run_settings[[7]]

# define run filepath 
run_filepath <- paste0('data/', ID, '/', week, '/')

print(paste('Now extracting covariates for elephant', ID, 'of week', week, '...'))

###########
## Extract covariates or each step from the MODIS images 
###########
# load function
source('functions_elephant_ssf/4_a_extractingCovariates.R')

# load necessary packages 
if(!('terra') %in% installed.packages()){install.packages('terra')}
library(terra)
if(!('amt') %in% installed.packages()){install.packages('amt')}
library(amt)

# run function
# Note: Downscaling term makes the distinction between MODIS dataset that was retrieved without running any downscaling scripts (downscaling = NULL), 
#         MODIS 250m retrieved through downscaling JN script (downscaling == F), MODIS 30m generated through downscaling in R (downscaling == T).
# Note: Specify downscaling model if downscaling = T --> should match last section of name of MODIS 30m folder 
# loadAndExtractCovariates(run_filepath, ID, week, random_data_method = pseudo_abs_method, 
#                          downscaling = downscaling_setting, downscaling_model = downscaling_model, 
#                          input_suffix = input_suffix, output_directory = 'data/', output_suffix = output_suffix)

loadAndExtractCovariates(run_filepath, ID, week, random_data_method = pseudo_abs_method, 
                         downscaling = downscaling_setting, downscaling_model = downscaling_model, 
                         input_suffix = input_suffix, output_directory = 'data/', output_suffix = output_suffix)

print(paste('(DONE) Extracting covariates for elephant', ID, 'of week', week))



# 
# 
# #run_settings <- read.csv('data/run_settings_downscaling.csv', row.names = 1)
# run_settings <- read.csv('data/run_settings_LTS_final.csv', row.names = 1)
# row.names(run_settings) <- 1:nrow(run_settings)
# 
# # try generating a few runs that didn't pass that first time 
# run_settings <- run_settings[run_settings$ID == 'LA12' & run_settings$week == 2120, ]
# 
# for(i in 1:nrow(run_settings)){
#   
#   # define elephant ID
#   ID <- run_settings$ID[i]
#   
#   # define week to test 
#   week <- run_settings$week[i]
#   
#   # define pseudo-absence path generator method --> Choices are 1) random_path_custom_distr 2) random_path_buffer_point 3) random_step
#   pseudo_abs_method <- run_settings$pseudo_abs_method[i]
#   
#   # set downscaling parameter
#   downscaling_setting <- run_settings$downscaling[i]
#   
#   # define run filepath 
#   run_filepath <- paste0('data/', ID, '/', week, '/')
#   
#   # define input and output suffixes
#   input_suffix <- '_newPathWithCV'
#   output_suffix <- '_newPathWithCV'
#   
#   ###########
#   ## Extract covariates or each step from the MODIS images 
#   ###########
#   # # load function
#   # source('functions_elephant_ssf/4_a_extractingCovariates.R')
#   # 
#   # # load necessary packages 
#   # if(!('terra') %in% installed.packages()){install.packages('terra')}
#   # library(terra)
#   # if(!('amt') %in% installed.packages()){install.packages('amt')}
#   # library(amt)
#   
#   # run function
#   # Note: Downscaling term makes the distinction between MODIS dataset that was retrieved without running any downscaling scripts (downscaling = NULL), 
#   #         MODIS 250m retrieved through downscaling JN script (downscaling == F), MODIS 30m generated through downscaling in R (downscaling == T).
#   # Note: Specify downscaling model if downscaling = T --> should match last section of name of MODIS 30m folder 
#   # loadAndExtractCovariates(run_filepath, ID, week, random_data_method = pseudo_abs_method, 
#   #                          downscaling = downscaling_setting, downscaling_model = downscaling_model, 
#   #                          input_suffix = input_suffix, output_directory = 'data/', output_suffix = output_suffix)
#   
#   loadAndExtractCovariates(run_filepath, ID, week, random_data_method = pseudo_abs_method, 
#                            downscaling = downscaling_setting, downscaling_model = downscaling_model, 
#                            input_suffix = input_suffix, output_directory = 'data/', output_suffix = output_suffix)
#   
#   print(paste('(DONE) Extracting covariates for elephant', ID, 'of week', week))
#   
# }
#   
#   
#   
#   
#   