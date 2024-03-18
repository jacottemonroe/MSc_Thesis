## MSc Thesis 
## Jacotte Monroe 
## Model Building 

###############################################################################
###### START BY RUNNING THIS PART OF THE SCRIPT UNTIL BREAK ###################
###############################################################################

## libraries 
if(!('terra') %in% installed.packages()){install.packages('terra')}
library(terra)
if(!('tidyterra') %in% installed.packages()){install.packages('tidyterra')} # for mapping spatraster
library(tidyterra)
if(!('ggplot2') %in% installed.packages()){install.packages('ggplot2')}
library(ggplot2)
if(!('lubridate') %in% installed.packages()){install.packages('lubridate')}
library(lubridate)
if(!('dplyr') %in% installed.packages()){install.packages('dplyr')} #for grouping in table (max/min)
library(dplyr)
if(!('amt') %in% installed.packages()){install.packages('amt')}
library(amt)


#print('Wrong pseudo-absence method specified. Choices are 1) random_path_custom_distr 2) random_path_buffer_point 3) random step')

################################ load elephant data #########################

# select elephant ID 
# original test was with elephant LA2
ID <- 'LA2' #'LA2'

# select week to test 
# original test was with week 2027
week <- 2027 #2048 #2060 #2027 #2300

file_name <- paste0('data/elephant_etosha/elephant_fixes/preprocessed_elephant_', ID,'.csv')

source('functions_elephant_ssf/transformingToTrackObject.R')

# necessary libraries 
if(!('lubridate') %in% installed.packages()){install.packages('lubridate')}
library(lubridate)

if(!('amt') %in% installed.packages()){install.packages('amt')}
library(amt)

transformToTrackObject(file_name, ID, week, output_directory = 'data/elephant_etosha/')
transformToTrackObject(file_name, ID)



########################## generate steps ############################## 

source('functions_elephant_ssf/generatingSteps.R')

# load necessary libraries 
if(!('dplyr') %in% installed.packages()){install.packages('dplyr')} #for grouping in table (max/min)
library(dplyr)
if(!('sf') %in% installed.packages()){install.packages('sf')} #for grouping in table (max/min)
library(sf)

true_track_filepath <- paste0('data/elephant_etosha/', ID, '/', week, '/')
full_track_filepath <- paste0('data/elephant_etosha/', ID, '/')

generateSteps(true_track_filepath, full_track_filepath, ID, week, 20, 
              random_data_method = 'random_path_custom_distr', paste0('data/elephant_etosha/'))

generateSteps(true_track_filepath, full_track_filepath, ID, week, 20, 
              random_data_method = 'random_path_buffer_point', paste0('data/elephant_etosha/'))

generateSteps(true_track_filepath, full_track_filepath, ID, week, 20, 
              random_data_method = 'random_step', paste0('data/elephant_etosha/'))




###############################################################################
###### CLOSE AND RESTART PROGRAM WHEN GET HERE TO CLEAR HISTORY ###############
###### START RUNNING THE CODE AFTER THIS BREAK DONT LOAD AMT PACKAGE ##########
###############################################################################


## libraries 
if(!('terra') %in% installed.packages()){install.packages('terra')}
library(terra)
if(!('tidyterra') %in% installed.packages()){install.packages('tidyterra')} # for mapping spatraster
library(tidyterra)
if(!('ggplot2') %in% installed.packages()){install.packages('ggplot2')}
library(ggplot2)
if(!('lubridate') %in% installed.packages()){install.packages('lubridate')}
library(lubridate)
if(!('dplyr') %in% installed.packages()){install.packages('dplyr')} #for grouping in table (max/min)
library(dplyr)

ID <- 'LA2' #LA26
week <- 2027 #2048 #2060 #2027 #2300

# set lag (number of days prior to passage) --> supposed to be 7 but set it to 6 for now because on this exact step there is missing data due to cloudcover
lag <- 7

######################### step extents table ########################## 

source('functions_elephant_ssf/creatingStepExtentLUT.R')

createStepExtentLUT(paste0('data/elephant_etosha/elephant_steps/all_steps_', ID, '_w', week, '_custom_distr.csv'), ID, week, lag, output_directory = 'data/step_extents/random_paths/')
#createStepExtentLUT(paste0('data/elephant_etosha/elephant_steps/all_steps_', ID, '_w', week, '.csv'), ID, week, lag, output_directory = 'data/step_extents/random_paths/')



###############################################################################
###### GO TO JN TO GENERATE MODIS IMAGES USING STEP EXTENT LUT ################
###### ONCE HAVE GENERATED IMAGES CAN RUN SCRIPT FROM TOP UNTIL FIRST BREAK ###
###### TO GENERATE ELEPHANT DATAFRAME AS STEPS_XYT OBJECT #####################
###############################################################################



######################### create covariate dataset #################### 
# 
# # get correct folder
# w_path = paste0('8_day_', as.character(w))
# # stack all generated MODIS images together (images already cloudmasked and gap filled in JN script)
# modis_images <- rast(list.files(paste0('data/modis/cloudmasked/', as.character(week)), pattern = glob2rx('*.tif'), full.names = T))
#landsat_images <- rast(list.files(paste0('data/l8/cloudmasked/', as.character(week)), pattern = glob2rx('*.tif'), full.names = T))
# #modis_images <- rast(list.files(paste0('data/modis_ssf/', w_path, '/'), pattern = glob2rx('*.tif'), full.names = T))
# 
# # add a time (date) attribute to the spatraster --> daily interval 
# # source: https://rdrr.io/github/rspatial/terra/man/time.html
# # source: https://stackoverflow.com/questions/73259623/how-to-index-individual-layers-from-a-spatraster-object-by-time
# time(modis_images, tstep = 'days') <- as.Date(names(modis_images)[1], format = '%Y_%m_%d', 
#                                               tz = 'Africa/Maputo') + 0:(nlyr(modis_images)-1)
# 
# plot(modis_images[[1]])
# modis_images[[8]]
# modis_images
# 
# landsat_images <- rast('data/l8/cloudmasked/2300/LC08_179073_20131230.tif')
# print(landsat_images)
# plot(landsat_images[[1]])
# t <- landsat_images[[1]]
# t
# plot(t == -9999)
# landsat_images[landsat_images == -9999] <- NA
# 
# print(crs(landsat_images))
# 
# 
# #landsat_images <- rast(list.files(paste0('data/l8/', as.character(week)), pattern = glob2rx('*.tif'), full.names = T))
# landsat_images <- rast('data/l8/2300/LC08_179073_20131230.tif')
# plot(landsat_images[[6]])
# print(landsat_images)
# 
# q <- rast('quality_band.tif')
# q
# plot(q)
# c <- rast('cloudmasked_l8.tif')
# c
# plot(c[[1]] == 0)

########################## extract covariates ##########################

if(!('amt') %in% installed.packages()){install.packages('amt')}
library(amt)

modis_image_directory_name <- paste0('data/modis_ssf/', week, '/') #paste0('data/modis_ssf/', w_path, '/')

# create table of all extracted covariates for each step
source('functions_elephant_ssf/extractingCovariates.R')

loadAndExtractCovariates('data/elephant_etosha/', modis_image_directory_name, ID, week, lag, 
                         random_data_method = 'random_path_custom_distr', 'output/elephant_etosha/')

loadAndExtractCovariates('data/elephant_etosha/', modis_image_directory_name, ID, week, lag, 
                         random_data_method = 'random_path_buffer_point', 'output/elephant_etosha/')




###################### visualize paths #####################
source('functions_elephant_ssf/visualizingPaths.R')

# libraries to run next function (needed to generate correlation matrix)
if(!('ggplot2') %in% installed.packages()){install.packages('ggplot2')}
if(!('terra') %in% installed.packages()){install.packages('terra')}
library(ggplot2)
library(terra)

visualizePaths(modis_filepath = paste0('data/modis_ssf/2027/', 'mean_ndvi.tif'), 
               step_dataset_filepath = 'output/elephant_etosha/random_paths/LA2_2027_step_dataset.csv', 
               ID = ID, week = week, title = "Elephant Movement on Mean NDVI",
               output_directory = 'output/elephant_etosha/')




######################### fit model #################################

source('functions_elephant_ssf/fittingSSFModel.R')

# libraries to run next function (needed to generate correlation matrix)
if(!('dplyr') %in% installed.packages()){install.packages('dplyr')}
if(!('GGally') %in% installed.packages()){install.packages('GGally')}
if(!('car') %in% installed.packages()){install.packages('car')}
library(dplyr) 
library(GGally)
library(car)

fitSSFModel('output/elephant_etosha/random_paths/', ID, week, full = T, 'output/ssf_models/random_paths/', multicolinearity_check = T)
fitSSFModel('output/elephant_etosha/random_paths/', ID, week, full = F, 'output/ssf_models/random_paths/', multicolinearity_check = F)



