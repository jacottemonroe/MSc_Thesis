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
ID <- 'LA2' #'LA2'

# select week to test 
# original test was with week 2027
week <- 2027 #2048 #2060 #2027 #2300

file_name <- paste0('data/elephant_etosha/elephant_fixes/preprocessed_elephant_', ID,'.csv')

source('functions_elephant_ssf/transformingToTrackObject.R')

true_tr <- transformToTrackObject(file_name, week)
full_tr <- transformToTrackObject(file_name)



########################## generate steps ############################## 

source('functions_elephant_ssf/generatingSteps.R')

all_steps_dataset <- generateSteps(true_tr, full_tr, 20, paste0('data/elephant_etosha/elephant_steps/all_steps_', ID, '_w', week, '_custom_distr.csv'))

# generateRandomPath_old <- function(true_fixes_dataset, true_step_dataset, starting_fixes_dataset, 
#                                all_steps_dataset, buffer_distance, burst_number, loop_number){
#   
#   # create dataframe of starting point for burst of interest 
#   fake_path <- data.frame(t_ = starting_fixes_dataset$t_[starting_fixes_dataset$burst_ == burst_number],  
#                           x_ = starting_fixes_dataset$x_[starting_fixes_dataset$burst_ == burst_number], 
#                           y_ = starting_fixes_dataset$y_[starting_fixes_dataset$burst_ == burst_number])
#   
#   # for all true steps from the burst of interest, generate a random step to get a random path of the same length
#   for(i in 1:nrow(true_step_dataset[true_step_dataset$burst_ == burst_number,])){
#     
#     # transform starting point coordinates into a spatial object 
#     # source: https://www.dpi.inpe.br/gilberto/tutorials/software/R-contrib/sp/html/SpatialPoints.html
#     starting_point <- st_as_sf(fake_path[nrow(fake_path),2:3], coords = c('x_', 'y_'), 
#                                crs = crs('EPSG:32733'))
#     
#     # create buffer around point with fixed distance = average step length from all observed steps
#     # source: https://gis.stackexchange.com/questions/292327/creating-buffers-around-points-and-merging-with-spatialpolygonsdataframe-to-crea
#     buffer_point <- st_buffer(starting_point, dist = buffer_distance)
#     
#     # convert buffer polygon into polyline and sample random point along line
#     # source: https://stackoverflow.com/questions/68987453/generating-random-locations-along-the-outer-border-of-a-shp-polygon-using-r
#     set.seed(i+100*(loop_number-1))
#     new_point <- st_sample(st_cast(buffer_point, 'MULTILINESTRING'), 1)
#     
#     # add coordinates to fake path data frame
#     # source: https://rdrr.io/cran/sf/man/st_coordinates.html
#     fake_path <- rbind(fake_path, data.frame(t_ = true_fixes_dataset$t_[i+1], 
#                                              x_ = st_coordinates(new_point)[[1]], 
#                                              y_ = st_coordinates(new_point)[[2]]), 
#                        make.row.names = F)
#     
#   }
#   
#   # add new column for information on the burst of interest in the new fake path dataset
#   fake_path$burst_ <- burst_number
#   
#   # turn the list of random points into a track object
#   fake_track <- make_track(fake_path, x_, y_, t_, burst_ = burst_)
#   
#   # transform the fake points dataset into steps
#   fake_steps <- steps_by_burst(fake_track)
#   
#   # add columns to the new fake steps dataset for the case (F = false steps), 
#   #     the corresponding step ID (to match with true steps), and the loop number 
#   #     to differentiate between randomly generated steps
#   fake_steps$case_ <- F
#   fake_steps$step_id_ <- row.names(fake_steps)
#   fake_steps$random_id_ <- loop_number
#   
#   # add the new fake steps to the larger dataset containing all steps 
#   all_steps_dataset <- rbind(all_steps_dataset, fake_steps)
#   
#   return(all_steps_dataset)
# }
# 
# generateSteps_old <- function(track_dataset, n_random_steps = 20, step_length_distribution = 'gamma', turn_angle_distribution = 'vonmises', output_filename){
#   
#   ##### create observed dataset of steps 
#   
#   # turn fixes into observed steps 
#   # by burst so steps not created for fixes in different paths
#   true_steps <- steps_by_burst(track_dataset)
#   
#   # add information attributes as new columns (to differentiate the true and false steps)
#   true_steps$case_ <- T
#   true_steps$step_id_ <- row.names(true_steps)
#   true_steps$random_id_ <- NA
#   
#   # retrieve average step length to use as fixed step length for random steps
#   set_distance <- mean(true_steps$sl_)
#   
#   
#   ##### create final dataset of presence and absence steps 
#   
#   # add presence steps to final step dataset
#   all_steps <- true_steps
#   
#   
#   ##### get initial starting points from the observed dataset (necessary for multiple paths)
#   
#   # select starting fixes of each burst
#   # PACKAGE: dplyr should be installed
#   # source: https://www.r-bloggers.com/2022/07/select-the-first-row-by-group-in-r/
#   starting_fixes <- track_dataset %>% group_by(burst_) %>% filter(row_number()==1)
#   
#   
#   ##### generate random pseudo-absence paths 
#   
#   source('functions_elephant_ssf/generatingRandomPath.R')
#   
#   # generate random sets of paths matching the full elephant movement of that week (all true separate paths included)
#   for(loop in 1:n_random_steps){
#     
#     # for each true separate path generate a corresponding false path 
#     for(burst in 1:nrow(starting_fixes)){
#       
#       # generate a random pseudo-absence path for the corresponding true path and add it to the dataset of all steps
#       all_steps <- generateRandomPath_old(track_dataset, true_steps, starting_fixes, all_steps, set_distance, burst, loop) 
#     }
#   }
#   
#   ##### save output dataframe of all steps
#   
#   write.csv(all_steps, output_filename)
#   
#   # # generate corresponding random pseudo-absence steps
#   # set.seed(1234)
#   # all_steps <- random_steps(true_steps, n_control = n_random_steps,
#   #                           sl_distr = fit_distr(true_steps$sl_, step_length_distribution),
#   #                           ta_distr = fit_distr(true_steps$ta_, turn_angle_distribution))
#   
#   return(all_steps)
# }
# 
# all_steps_dataset <- generateSteps_old(tr, 20, 'gamma', 'vonmises', 'data/elephant_etosha/elephant_steps/all_steps_LA2_w2027.csv')


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


#w_path <- paste0('8_day_', as.character(week))

#REMOVE "OLD"" AFTER THIS CHECK (OLD USES BUFFER METHOD FOR GENERATING FAKE PATHS)
modis_image_directory_name <- paste0('data/modis_ssf/', as.character(week), '/') #paste0('data/modis_ssf/', w_path, '/')
elephant_covariates_filename <- paste0('output/elephant_etosha/random_paths/', ID, '_', as.character(week), '_step_dataset.csv') #paste0('output/elephant_etosha/', ID, '_', w_path, '_step_dataset.csv'))

# create table of all extracted covariates for each step
source('functions_elephant_ssf/extractingCovariates.R')

loadAndExtractCovariates(all_steps_dataset, modis_image_directory_name, lag, elephant_covariates_filename)




####################### scatter plot fo the data ###################
# create simplified dataframe where y = case and x = ndvi 
step_dataset <- read.csv(paste0('output/elephant_etosha/random_paths/', ID, '_', as.character(week), '_step_dataset.csv'))
data_to_plot <- data.frame(y = step_dataset$case_, step_dataset[,16:ncol(step_dataset)])

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

p <- ggpairs(data_to_plot, columns = 2:9, aes(color = as.factor(y), alpha = 0.5))    
p


######################### fit model #################################

source('functions_elephant_ssf/fittingSSFModel.R')

# libraries to run next function (needed to generate correlation matrix)
if(!('dplyr') %in% installed.packages()){install.packages('dplyr')}
if(!('GGally') %in% installed.packages()){install.packages('GGally')}
library(dplyr) 
library(GGally)

fitSSFModel('output/elephant_etosha/random_paths/', ID, week, full = T, 'output/ssf_models/random_paths/', multicolinearity_check = T)
fitSSFModel('output/elephant_etosha/random_paths/', ID, week, full = F, 'output/ssf_models/random_paths/', multicolinearity_check = F)

png(filename = paste0('output/ssf_models/random_paths/LA2/2027/', 'correlation_matrix.png'))
ggpairs(covariates, aes(color = as.factor(case_), alpha = 0.5))    
dev.off()

# source: https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/

t <- glm(case_ ~ ndvi_10 + ndvi_50 + ndvi_90 + ndvi_sd + ndvi_rate_10 + ndvi_rate_50 + ndvi_rate_90 + ndvi_rate_sd, 
         family = binomial(link = 'logit'), data = step_dataset)

tsum <- summary(t)

print(tsum$aic)
print(tsum$null.deviance)
print(tsum$df.null)
print(tsum$deviance)
print(tsum$df.residual) #df for residual deviance 
print(tsum$coefficients)


t <- glm(case_ ~ ndvi_50 + ndvi_sd + ndvi_rate_50 + ndvi_rate_sd, 
         family = binomial(link = 'logit'), data = step_dataset)

summary(t)

t <- glm(case_ ~ ndvi_10 + ndvi_90 + ndvi_sd + ndvi_rate_10 + ndvi_rate_90 + ndvi_rate_sd, 
         family = binomial(link = 'logit'), data = step_dataset)

summary(t)

t <- glm(case_ ~ ndvi_90 + ndvi_sd + ndvi_rate_90 + ndvi_rate_sd, 
         family = binomial(link = 'logit'), data = step_dataset)

summary(t)


if(!('car') %in% installed.packages()){install.packages('car')}
library(car)
vif(t) 

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


