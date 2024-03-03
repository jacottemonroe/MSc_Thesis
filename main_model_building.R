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




################################ load elephant data #########################

# select elephant ID 
# original test was with elephant LA2
ID <- 'LA2' #'LA26'

# select week to test 
# original test was with week 2027
w <- 2027 #2300

file_name <- paste0('data/elephant_etosha/preprocessed_VSS_elephant_', ID,'.csv')

source('functions_elephant_ssf/transformingToTrackObject.R')

tr <- transformToTrackObject(file_name, w)

# transformToTrackObject <- function(file_name, w){
#   # get elephant dataset
#   full_df <- read.csv(file_name, row.names = 1)
#   
#   # get week of interest 
#   df <- full_df[full_df$week == w,]
#   
#   # change date_time format to remove time 
#   df$date_time <- as.POSIXct(df$date_time, tz = 'Africa/Maputo')
#   
#   # turn elephant data frame into track_xyt object for model building
#   # 'burst_' is a fixed/mandatory column name if want to generate steps for multiple paths
#   df_track <- make_track(df, location.long, location.lat, date_time, week = week, burst_ = path)
#   
#   return(df_track)
# }

# # get elephant dataset
# full_df <- read.csv(file_name, row.names = 1)
# 
# # get week of interest 
# df <- full_df
# df <- df[df$week == w,]


# # change date_time format to remove time 
# df$date_time <- as.POSIXct(df$date_time, tz = 'Africa/Maputo')

# # visualize elephant data 
# mov_map <- modis_ndvi_map + 
#   labs(title = "Elephant Movement", subtitle = ID, x = "Longitude", y = "Latitude") +
#   geom_path(data = full_df, aes(x = location.long, y = location.lat), color = 'grey50', linewidth = 0.1, show.legend = F) +
#   geom_path(data = df, aes(x = location.long, y = location.lat), color = 'red', linewidth = 1, show.legend = F) +
#   annotation_north_arrow(location = 'tl', which_north = 'true', 
#                          pad_x = unit(0.04, "in"), pad_y = unit(0.3, "in"),
#                          style = north_arrow_fancy_orienteering()) +
#   annotation_scale(location = 'tl') +
#   theme_minimal()
# mov_map


# # turn elephant data frame into track_xyt object for model building
# # 'burst_' is a fixed/mandatory column name if want to generate steps for multiple paths
# tr <- make_track(df, location.long, location.lat, date_time, week = week, burst_ = path)
# 


########################## generate steps ############################## 

source('functions_elephant_ssf/generatingSteps.R')

generateSteps(tr, 20, 'gamma', 'vonmises', 'data/elephant_etosha/elephant_steps/temp_eleph_path.csv')

# generateSteps <- function(track_dataset, n_random_steps = 20, step_length_distribution = 'gamma', turn_angle_distribution = 'vonmises', output_filename){
#   
#   # turn fixes into observed steps 
#   # by burst so steps not created for fixes in different paths
#   true_steps <- steps_by_burst(track_dataset)
#   
#   # generate corresponding random pseudo-absence steps
#   set.seed(1234)
#   all_steps <- random_steps(true_steps, n_control = n_random_steps,
#                                              sl_distr = fit_distr(true_steps$sl_, step_length_distribution),
#                                              ta_distr = fit_distr(true_steps$ta_, turn_angle_distribution))
#   
#   # save table
#   write.csv(all_steps, output_filename)
#   
# }
# 
# # turn elephant track dataset into steps
# # by burst so steps not created for fixes in different paths
# elephant_steps_by_path <- steps_by_burst(tr)
# 
# # generate corresponding random steps
# set.seed(1234)
# elephant_all_steps_by_path <- random_steps(elephant_steps_by_path, n_control = 20,
#                                            sl_distr = fit_distr(elephant_steps_by_path$sl_, "gamma"),
#                                            ta_distr = fit_distr(elephant_steps_by_path$ta_, "vonmises"))
# 
# # save table (TEMPORARY FIX TO PROBLEM)
# write.csv(elephant_all_steps_by_path, 'data/temp_eleph_path.csv')




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

ID <- 'LA2'
week <- 2300

# set lag (number of days prior to passage) --> supposed to be 7 but set it to 6 for now because on this exact step there is missing data due to cloudcover
lag <- 7

######################### step extents table ########################## 

source('functions_elephant_ssf/creatingStepExtentLUT.R')

createStepExtentLUT('data/temp_eleph_path.csv', ID, week, lag, output_directory = 'data/step_extents/')


# 
# createStepExtentLUT <- function(input_filename, elephant_ID, week_of_data, ndvi_rate_lag = 7, output_directory = 'data/step_extents/'){
#   
#   # read elephant step dataset
#   all_steps <- read.csv(input_filename)
#   
#   # get start date of step
#   all_steps$start_date <- as.Date(all_steps$t1_)
#   
#   # get end date of step (necessary for GEE part? to check --> NEEDED)
#   all_steps$end_date <- all_steps$start_date + 1
#   
#   # get start and end dates for the week before 
#   all_steps$start_date_prev_week <- all_steps$start_date - ndvi_rate_lag
#   all_steps$end_date_prev_week <- all_steps$start_date_prev_week + 1
#   
#   # get extreme coordinates for each day
#   step_extents <- all_steps
#   
#   step_extents <- step_extents %>% group_by(start_date) %>% mutate(xmin = min(c(x1_, x2_))) %>% mutate(ymin = min(c(y1_, y2_))) %>% mutate(xmax = max(c(x1_, x2_))) %>% mutate(ymax = max(c(y1_, y2_)))
#   
#   # get table of all extents per date
#   step_extents <- unique(step_extents[,c('start_date', 'end_date', 'start_date_prev_week', 'end_date_prev_week', 'xmin', 'ymin', 'xmax', 'ymax')])
#   
#   # add largest extent as new row in step extent LUT 
#   # source: https://www.rdocumentation.org/packages/terra/versions/1.7-71/topics/ext
#   step_extents <- rbind(step_extents, data.frame('start_date' = min(step_extents$start_date, na.rm = T), 
#                                        'end_date' = max(step_extents$end_date, na.rm = T), 
#                                        'start_date_prev_week' = min(step_extents$start_date_prev_week, na.rm = T),
#                                        'end_date_prev_week' = max(step_extents$end_date_prev_week, na.rm = T), 
#                                        'xmin' = min(step_extents$xmin), 'xmax' = max(step_extents$xmax), 
#                                        'ymin' = min(step_extents$ymin), 'ymax' = max(step_extents$ymax)))
#   
#   # save table 
#   write.csv(step_ex, paste0(output_directory, elephant_ID, '_step_ex_w', as.character(week_of_data),'.csv'))
#   
# }
# # 
# # could make this code more efficient and less repetitive but idk how for now
# eleph_mov <- read.csv('data/temp_eleph_path.csv') #elephant_all_steps_by_path
# 
# # get start date of step
# eleph_mov$start_date <- as.Date(eleph_mov$t1_)
# 
# # get end date of step (necessary for GEE part)
# eleph_mov$end_date <- eleph_mov$start_date + 1
# 
# # get start and end dates for the week before 
# eleph_mov$start_date_prev_week <- eleph_mov$start_date - 7
# eleph_mov$end_date_prev_week <- eleph_mov$start_date_prev_week + 1
# 
# # get extreme coordinates for each day
# step_ex <- eleph_mov
# 
# step_ex <- step_ex %>% group_by(start_date) %>% mutate(xmin = min(c(x1_, x2_))) %>% mutate(ymin = min(c(y1_, y2_))) %>% mutate(xmax = max(c(x1_, x2_))) %>% mutate(ymax = max(c(y1_, y2_)))
# 
# # get table of all extents per date
# step_ex <- unique(step_ex[,c('start_date', 'end_date', 'start_date_prev_week', 'end_date_prev_week', 'xmin', 'ymin', 'xmax', 'ymax')])
# 
# # add largest extent as new row in step extent LUT 
# # source: https://www.rdocumentation.org/packages/terra/versions/1.7-71/topics/ext
# step_ex <- rbind(step_ex, data.frame('start_date' = min(step_ex$start_date, na.rm = T), 
#                                      'end_date' = max(step_ex$end_date, na.rm = T), 
#                                      'start_date_prev_week' = min(step_ex$start_date_prev_week, na.rm = T),
#                                      'end_date_prev_week' = max(step_ex$end_date_prev_week, na.rm = T), 
#                                      'xmin' = min(step_ex$xmin), 'xmax' = max(step_ex$xmax), 
#                                      'ymin' = min(step_ex$ymin), 'ymax' = max(step_ex$ymax)))
# 
# # save table 
# write.csv(step_ex, paste0('data/step_extents/LA2_step_ex_w',as.character(week),'.csv'))





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
# modis_images <- rast(list.files(paste0('data/modis_ssf/', as.character(w)), pattern = glob2rx('*.tif'), full.names = T))
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


########################## extract covariates ##########################

# get correct folder
week <- 2300
w_path <- paste0('8_day_', as.character(week))
modis_image_directory_name <- paste0('data/modis_ssf/', as.character(week), '/') #paste0('data/modis_ssf/', w_path, '/')
elephant_covariates_filename <- paste0('output/elephant_etosha/', ID, '_', as.character(w), '_step_dataset.csv') #paste0('output/elephant_etosha/', ID, '_', w_path, '_step_dataset.csv'))

# create table of all extracted covariates for each step
source('functions_elephant_ssf/extractingCovariates.R')

loadAndExtractCovariates(input_filename = 'data/temp_eleph_path.csv', modis_image_directory_name, 
                         lag, elephant_covariates_filename)


# createAndExtractCovariates <- function(input_filename = 'data/temp_eleph_path.csv', modis_image_directory, ndvi_rate_lag, output_filename){
#   
#   # get elephant step dataset --> CHECK THAT IT IS STEP_XYT FORMAT OTHERWISE HAVE TO TRANSFORM AGAIN?!
#   step_dataset <- read.csv(input_filename)
#   
#   # add columns for covariates 
#   # source: https://sparkbyexamples.com/r-programming/add-empty-column-to-dataframe-in-r/#:~:text=Use%20%24%20operator%2C%20square%20bracket%20%5B%5D,()%20function%20from%20tidyverse%20package.
#   step_dataset <- cbind(step_dataset, step_dataset$ndvi_10=NA, step_dataset$ndvi_50=NA,
#                         step_dataset$ndvi_90=NA, step_dataset$ndvi_sd=NA, step_dataset$ndvi_rate_10=NA,
#                         step_dataset$ndvi_rate_50=NA, step_dataset$ndvi_rate_90=NA, step_dataset$ndvi_rate_sd=NA)
#   
#   # retrieve and stack all generated MODIS images together
#   modis_images <- rast(list.files(modis_image_directory, pattern = glob2rx('*.tif'), full.names = T))
#   
#   # add a time (date) attribute to the spatraster --> daily interval 
#   # source: https://rdrr.io/github/rspatial/terra/man/time.html
#   # source: https://stackoverflow.com/questions/73259623/how-to-index-individual-layers-from-a-spatraster-object-by-time
#   time(modis_images, tstep = 'days') <- as.Date(names(modis_images)[1], format = '%Y_%m_%d', 
#                                                 tz = 'Africa/Maputo') + 0:(nlyr(modis_images)-1)
# 
#   # extract covariates from corresponding MODIS image
#   for(i in 1:nrow(step_dataset)){
#     
#     # retrieve MODIS image (SpatRaster layer) by matching date with step
#     # source: https://stackoverflow.com/questions/73259623/how-to-index-individual-layers-from-a-spatraster-object-by-time
#     step_modis <- modis_images[[time(modis_images) == as.Date(step_dataset$t1_[i], tz = 'Africa/Maputo')]]
#     
#     # extract NDVI for all pixels along step on day of passage
#     ndvi_along <- unlist(extract_covariates_along(step_dataset[i,], step_modis, name_covar = 'ndvi_along'))
#     
#     # calculate percentile values of NDVI along step
#     # source: https://www.statology.org/how-to-fix-in-r-error-in-sort-intx-na-last-decreasing-x-must-be-atomic/
#     step_dataset$ndvi_10[i] <- quantile(ndvi_along, probs = 0.1, names = F, na.rm = T)
#     step_dataset$ndvi_50[i] <- quantile(ndvi_along, probs = 0.5, names = F, na.rm = T)
#     step_dataset$ndvi_90[i] <- quantile(ndvi_along, probs = 0.9, names = F, na.rm = T)
#     step_dataset$ndvi_sd[i] <- sd(ndvi_along, na.rm = T)
#     
#     # retrieve MODIS image from prior to passage (*tested on step 43, had to take 6 days instead of 7 days because of cloudcover)
#     step_modis_prior <- modis_images[[time(modis_images) == as.Date(step_dataset$t1_[i], tz = 'Africa/Maputo')-ndvi_rate_lag]]
#     
#     # extract NDVI for all pixels along step for prior passage
#     ndvi_along_prior <- extract_covariates_along(step_dataset[i,], step_modis_prior, name_covar = 'ndvi_along_prior')
#     
#     # calculate rate of NDVI change for all pixels along step for prior to passage
#     # note: have to retrieve vector inside list if want to do arithmatics
#     ndvi_rate_along <- (ndvi_along[[1]] - ndvi_along_prior[[1]])/ndvi_rate_lag
#     
#     # calculate percentile values of NDVI change rate along step
#     # source: https://www.statology.org/how-to-fix-in-r-error-in-sort-intx-na-last-decreasing-x-must-be-atomic/
#     step_dataset$ndvi_rate_10[i] <- quantile(ndvi_rate_along, probs = 0.1, names = F, na.rm = T)
#     step_dataset$ndvi_rate_50[i] <- quantile(ndvi_rate_along, probs = 0.5, names = F, na.rm = T)
#     step_dataset$ndvi_rate_90[i] <- quantile(ndvi_rate_along, probs = 0.9, names = F, na.rm = T)
#     step_dataset$ndvi_rate_sd[i] <- sd(ndvi_rate_along, na.rm = T)
#     
#   }
#   
#   # save this dataframe for now since took so long to generate 
#   #write.csv(step_dataset, paste0('output/elephant_etosha/LA2_', as.character(w), '_step_dataset.csv'))
#   write.csv(step_dataset, output_filename)
#   
# }

# # create new dataset to add covariates to and new column (make sure it's steps_xyt object)
# step_dataset <- elephant_all_steps_by_path 
# 
# step_dataset$ndvi_10 <- NA
# step_dataset$ndvi_50 <- NA
# step_dataset$ndvi_90 <- NA
# step_dataset$ndvi_sd <- NA
# 
# step_dataset$ndvi_rate_10 <- NA
# step_dataset$ndvi_rate_50 <- NA
# step_dataset$ndvi_rate_90 <- NA
# step_dataset$ndvi_rate_sd <- NA
# 
# # extract covariates from correct MODIS image
# 
# # option 1: extract only end pixel of each step 
# #step_dataset <- extract_covariates_var_time(step_dataset, modis_images, where = 'end', max_time = period(1, units = 'days'), name_covar = 'ndvi_var_time')
# 
# # option 2: extract all pixels along step for correct day 
# # need to construct a loop that will retrieve correct modis image for each step
# # this is less efficient than option 1 but try it 
# 
# # Attempt1: loop over all steps 
# 
# for(i in 1:nrow(step_dataset)){
#   
#   # retrieve MODIS image (SpatRaster layer) by matching date with step
#   # source: https://stackoverflow.com/questions/73259623/how-to-index-individual-layers-from-a-spatraster-object-by-time
#   step_modis <- modis_images[[time(modis_images) == as.Date(step_dataset$t1_[i], tz = 'Africa/Maputo')]]
#   
#   # extract NDVI for all pixels along step on day of passage
#   ndvi_along <- unlist(extract_covariates_along(step_dataset[i,], step_modis, name_covar = 'ndvi_along')) # do i need this last parameter?
#   
#   # calculate percentile values of NDVI along step
#   # source: https://www.statology.org/how-to-fix-in-r-error-in-sort-intx-na-last-decreasing-x-must-be-atomic/
#   step_dataset$ndvi_10[i] <- quantile(ndvi_along, probs = 0.1, names = F, na.rm = T)
#   step_dataset$ndvi_50[i] <- quantile(ndvi_along, probs = 0.5, names = F, na.rm = T)
#   step_dataset$ndvi_90[i] <- quantile(ndvi_along, probs = 0.9, names = F, na.rm = T)
#   step_dataset$ndvi_sd[i] <- sd(ndvi_along, na.rm = T)
#   
#   # retrieve MODIS image from previous week (*tested on step 43, had to take 6 days instead of 7 days because of cloudcover)
#   step_modis_prev_week <- modis_images[[time(modis_images) == as.Date(step_dataset$t1_[i], tz = 'Africa/Maputo')-lag]]
#   
#   # extract NDVI for all pixels along step for 1 week* prior passage
#   ndvi_along_prev_week <- extract_covariates_along(step_dataset[i,], step_modis_prev_week, name_covar = 'ndvi_along_prev_week')
#   
#   # calculate rate of NDVI change for all pixels along step for 1 week* prior to passage
#   # note: have to retrieve vector inside list if want to do arithmatics
#   ndvi_rate_along <- (ndvi_along[[1]] - ndvi_along_prev_week[[1]])/lag
#   
#   # calculate percentile values of NDVI change rate along step
#   # source: https://www.statology.org/how-to-fix-in-r-error-in-sort-intx-na-last-decreasing-x-must-be-atomic/
#   step_dataset$ndvi_rate_10[i] <- quantile(ndvi_rate_along, probs = 0.1, names = F, na.rm = T)
#   step_dataset$ndvi_rate_50[i] <- quantile(ndvi_rate_along, probs = 0.5, names = F, na.rm = T)
#   step_dataset$ndvi_rate_90[i] <- quantile(ndvi_rate_along, probs = 0.9, names = F, na.rm = T)
#   step_dataset$ndvi_rate_sd[i] <- sd(ndvi_rate_along, na.rm = T)
#   
# }
# 
# # save this dataframe for now since took so long to generate 
# #write.csv(step_dataset, paste0('output/elephant_etosha/LA2_', as.character(w), '_step_dataset.csv'))
# write.csv(step_dataset, paste0('output/elephant_etosha/LA2_', w_path, '_step_dataset.csv'))

####################### scatter plot fo the data ###################
# create simplified dataframe where y = case and x = ndvi 
step_dataset <- read.csv(paste0('output/elephant_etosha/LA2_', w, '_step_dataset.csv'))
data_to_plot <- data.frame(y = step_dataset$case_, step_dataset[,13:ncol(step_dataset)])

# turn case into binary 1 0 values to plot 
# source: https://stackoverflow.com/questions/33930188/convert-dataframe-column-to-1-or-0-for-true-false-values-and-assign-to-dataf
data_to_plot$y <- as.integer(data_to_plot$y)

# plot for covariates vs response 
ggplot(data_to_plot, aes(x=ndvi_10 + ndvi_50 + ndvi_90 + ndvi_sd + ndvi_rate_10 + 
                           ndvi_rate_50 + ndvi_rate_90 + ndvi_rate_sd, y=y)) + 
  geom_point() + 
  stat_smooth(method="glm", color="green", se=FALSE, 
              method.args = list(family=binomial))

ggplot(data_to_plot, aes(x=ndvi_10 + ndvi_90 + ndvi_sd + 
                           ndvi_rate_10 + ndvi_rate_90 + ndvi_rate_sd, y=y)) + 
  geom_point() + 
  stat_smooth(method="glm", color="green", se=FALSE, 
              method.args = list(family=binomial))

# plot for covariate against itself
m <- ggplot(data_to_plot, aes(x = ndvi_10 + ndvi_50 + ndvi_90 + ndvi_sd + ndvi_rate_10 + 
                           ndvi_rate_50 + ndvi_rate_90 + ndvi_rate_sd, y = ndvi_10 + ndvi_50 + ndvi_90 + ndvi_sd + ndvi_rate_10 + 
                           ndvi_rate_50 + ndvi_rate_90 + ndvi_rate_sd)) + 
  geom_point(aes(color = as.factor(y), size = as.factor(y))) + 
  scale_color_manual(values=c('grey40','cyan'))
m
ggplot(data_to_plot, aes(x = ndvi_10 + ndvi_90 + ndvi_sd + 
                           ndvi_rate_10 + ndvi_rate_90 + ndvi_rate_sd, y = ndvi_10 + ndvi_90 + ndvi_sd + 
                           ndvi_rate_10 + ndvi_rate_90 + ndvi_rate_sd)) + 
  geom_point(aes(color = as.factor(y), size = as.factor(y))) + 
  scale_color_manual(values=c('grey40','cyan'))

# plot all predictors against each other --> check multicolinearity 
# source: https://r-charts.com/correlation/ggpairs/?utm_content=cmp-true
if(!('GGally') %in% installed.packages()){install.packages('GGally')}
library(GGally)

p <- ggpairs(data_to_plot, columns = 3:10, aes(color = as.factor(y), alpha = 0.5))    
p


######################### fit model #################################

source('functions_elephant_ssf/fittingSSFModel.R')

fitSSFModel('output/elephant_etosha/', ID, week, 'output/ssf_models/')


# 
# fitSSFModel <- function(input_repository = 'output/elephant_etosha/', ID, week, output_directory = 'output/ssf_models/'){
#   
#   # read step dataset
#   step_dataset <- read.csv(paste0(input_repository, ID, '_', as.character(week), '_step_dataset.csv'))
#   
#   # select only rows with NA
#   step_dataset_NA <- step_dataset[!complete.cases(step_dataset),]
#   
#   # print information about the dataset 
#   print(paste('Dataset elephant:', ID, '\n', 
#               'Week:', as.character(week), '\n', 
#               'Number of total steps:', as.character(nrow(step_dataset)), '\n', 
#               'Number of observed steps:', as.character(length(unique(step_dataset$step_id_))), '\n', 
#               'Number of steps with NA:', as.character(length(unique(step_dataset_NA$step_id_))), '\n', 
#               'Number of observed steps with NA:', as.character(sum(step_dataset_NA$case_ == T))))
#   
#   # fit SSF model 
#   ssf_model <- fit_clogit(step_dataset, case_ ~ ndvi_10 + ndvi_50 + ndvi_90 + ndvi_sd + 
#                            ndvi_rate_10 + ndvi_rate_50 + ndvi_rate_90 + ndvi_rate_sd + strata(step_id_))
#   
#   # print model summary
#   print(summary(ssf_model))
#   
#   # save model as RDS 
#   saveRDS(ssf_model, file = paste0(output_directory, ID, '_', as.character(week), '_ssf_model.rds'))
#   
# }
# 
# s <- step_dataset #[1:294,]
# print(length(unique(s$step_id_)))
# s_NA <- s[!complete.cases(s),]
# print(length(unique(s_NA$step_id_)))
# print(sum(s_NA$case_ == T))
# 
# ss_model <- fit_clogit(step_dataset, case_ ~ ndvi_10 + ndvi_50 + ndvi_90 + ndvi_sd + 
#                          ndvi_rate_10 + ndvi_rate_50 + ndvi_rate_90 + ndvi_rate_sd + strata(step_id_))
# 
# ss_model <- fit_clogit(step_dataset, case_ ~ ndvi_10 + ndvi_90 + ndvi_sd + 
#                          ndvi_rate_10 + ndvi_rate_90 + ndvi_rate_sd + strata(step_id_))
# 
# summary(ss_model)
# 
# t <- ss_model$model






###################" other test ######################


## PICK UP FROM HERE (below is draft ideas)

b <- unique(elephant_all_steps_by_path$step_id_)
c <- unique(step_dataset$step_id_)
d <- step_dataset[!complete.cases(step_dataset),]
e <- unique(d$step_id_)
print(length(e))

f <- step_dataset
f$NA_count <- rowSums(is.na(f))
g <- f[f$NA_count >= 3,]
print(length(unique(g$step_id_)))

print(length(unique(step_dataset$step_id_)))
s_NA <- step_dataset[!complete.cases(step_dataset),]
print(length(unique(s_NA$step_id_)))
print(sum(s_NA$case_ == T))











# visualize elephant data
m <- modis_images[[8]]
m
names(m) <- 'ndvi'
modis_ndvi_map <- ggplot() +
  geom_spatraster(data = m, aes(fill = ndvi), show.legend = T) +
  scale_fill_terrain_c(name = 'NDVI')

modis_ndvi_map

mov_map <- modis_ndvi_map +
  labs(title = "Elephant Movement", subtitle = ID, x = "Longitude", y = "Latitude") +
  geom_path(data = s1_df, aes(x = location.long, y = location.lat), color = 'red', linewidth = 1, show.legend = F) +
  theme_minimal()
mov_map







t <- modis_images[[8]]
t


