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
ID <- 'LA2'

# select week to test 
w <- 2027

file_name <- paste0('data/elephant_etosha/preprocessed_VSS_elephant_', ID,'.csv')

# get elephant dataset
full_df <- read.csv(file_name, row.names = 1)

# get week of interest 
df <- full_df
df <- df[df$week == w,]

## reproject GPS coordinates --> COULD MOVE THIS TO PREPROCESSING 
# source: https://stackoverflow.com/questions/59774421/how-to-i-convert-coordinates-keeping-all-the-info-from-the-dataframe

# load package 
if(!('sp') %in% installed.packages()){install.packages('sp')}
library(sp)

# transform elephant GPS coordinates into SpatialPoint object 
# projection of elephant GPS data according to README from original dataset
elephant_coord_4326 <- SpatialPoints(coords = df[, c('location.long', 'location.lat')], proj4string = CRS('EPSG: 4326'))

elephant_coord_32733 <- coordinates(spTransform(elephant_coord_4326, CRS('EPSG:32733')))
df$location.long <- elephant_coord_32733[,1]
df$location.lat <- elephant_coord_32733[,2]


# change date_time format to remove time 
df$date_time <- as.POSIXct(df$date_time, tz = 'Africa/Maputo')

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


# turn elephant data frame into track_xyt object for model building
# 'burst_' is a fixed/mandatory column name if want to generate steps for multiple paths
tr <- make_track(df, location.long, location.lat, date_time, week = week, burst_ = path)



########################## generate steps ############################## 

# turn elephant track dataset into steps
# by burst so steps not created for fixes in different paths
elephant_steps_by_path <- steps_by_burst(tr)

# generate corresponding random steps
set.seed(1234)
elephant_all_steps_by_path <- random_steps(elephant_steps_by_path, n_control = 20,
                                           sl_distr = fit_distr(elephant_steps_by_path$sl_, "gamma"),
                                           ta_distr = fit_distr(elephant_steps_by_path$ta_, "vonmises"))

# save table (TEMPORARY FIX TO PROBLEM)
write.csv(elephant_all_steps_by_path, 'data/temp_eleph_path.csv')




###############################################################################
###### CLOSE AND RESTART PROGRAM WHEN GET HERE TO CLEAR HISTORY ###############
###### START RUNNING THE CODE AFTER THIS BREAK DONT LOAD AMT PACKAGE ##########
###############################################################################


## libraries 
if(!('terra') %in% installed.packages()){install.packages('terra')}
library(terra)
if(!('tidyterra') %in% installed.packages()){install.packages('tidyterra')} # for mapping spatraster
#library(tidyterra)
if(!('ggplot2') %in% installed.packages()){install.packages('ggplot2')}
#library(ggplot2)
if(!('lubridate') %in% installed.packages()){install.packages('lubridate')}
library(lubridate)
if(!('dplyr') %in% installed.packages()){install.packages('dplyr')} #for grouping in table (max/min)
library(dplyr)


######################### step extents table ########################## 

# could make this code more efficient and less repetitive but idk how for now
eleph_mov <- read.csv('data/temp_eleph_path.csv') #elephant_all_steps_by_path

# get start date of step
eleph_mov$start_date <- as.Date(eleph_mov$t1_)

# get end date of step (necessary for GEE part)
eleph_mov$end_date <- eleph_mov$start_date + 1

# get start and end dates for the week before 
eleph_mov$start_date_prev_week <- eleph_mov$start_date - 7
eleph_mov$end_date_prev_week <- eleph_mov$start_date_prev_week + 1

# convert dates into strings 
eleph_mov$start_date <- as.character(eleph_mov$start_date)
eleph_mov$end_date <- as.character(eleph_mov$end_date)
eleph_mov$end_date_prev_week <- as.character(eleph_mov$end_date_prev_week)
eleph_mov$end_date_prev_week <- as.character(eleph_mov$end_date_prev_week)

# get extreme coordinates for each day
step_ex <- eleph_mov

step_ex <- step_ex %>% group_by(start_date) %>% mutate(xmin = min(c(x1_, x2_))) %>% mutate(ymin = min(c(y1_, y2_))) %>% mutate(xmax = max(c(x1_, x2_))) %>% mutate(ymax = max(c(y1_, y2_)))

# get table of all extents per date
step_ex <- unique(step_ex[,c('start_date', 'end_date', 'start_date_prev_week', 'end_date_prev_week', 'xmin', 'ymin', 'xmax', 'ymax')])

# add largest extent as new row in step extent LUT 
# source: https://www.rdocumentation.org/packages/terra/versions/1.7-71/topics/ext
step_ex <- rbind(step_ex, data.frame('start_date' = NA, 'end_date' = NA, 'start_date_prev_week' = NA,
                                     'end_date_prev_week' = NA, 'xmin' = min(step_ex$xmin), 
                                     'xmax' = max(step_ex$xmax), 'ymin' = min(step_ex$ymin), 'ymax' = max(step_ex$ymax)))

# save table 
write.csv(step_ex, 'data/step_extents/LA2_step_ex_w2027.csv')



###############################################################################
###### GO TO JN TO GENERATE MODIS IMAGES USING STEP EXTENT LUT ################
###### ONCE HAVE GENERATED IMAGES CAN RUN SCRIPT FROM TOP UNTIL FIRST BREAK ###
###### TO GENERATE ELEPHANT DATAFRAME AS STEPS_XYT OBJECT #####################
###############################################################################



######################### create covariate dataset #################### 

# stack all generated MODIS images together 
modis_images <- rast(list.files('data/modis_ssf', pattern = glob2rx('*.tif'), full.names = T))

# add a time (date) attribute to the spatraster --> daily interval 
# source: https://rdrr.io/github/rspatial/terra/man/time.html
# source: https://stackoverflow.com/questions/73259623/how-to-index-individual-layers-from-a-spatraster-object-by-time
time(modis_images, tstep = 'days') <- as.Date(names(modis_images)[1], format = '%Y_%m_%d', 
                                              tz = 'Africa/Maputo') + 0:(nlyr(modis_images)-1)



########################## extract covariates ##########################

# create new dataset to add covariates to and new column (make sure it's steps_xyt object)
step_dataset <- elephant_all_steps_by_path #[1:30,]

step_dataset$ndvi_10 <- NA
step_dataset$ndvi_50 <- NA
step_dataset$ndvi_90 <- NA
step_dataset$ndvi_sd <- NA

step_dataset$ndvi_rate_10 <- NA
step_dataset$ndvi_rate_50 <- NA
step_dataset$ndvi_rate_90 <- NA
step_dataset$ndvi_rate_sd <- NA

# extract covariates from correct MODIS image

# option 1: extract only end pixel of each step 
#step_dataset <- extract_covariates_var_time(step_dataset, modis_images, where = 'end', max_time = period(1, units = 'days'), name_covar = 'ndvi_var_time')

# option 2: extract all pixels along step for correct day 
# need to construct a loop that will retrieve correct modis image for each step
# this is less efficient than option 1 but try it 

# set lag (number of days prior to passage) --> supposed to be 7 but set it to 6 for now because on this exact step there is missing data due to cloudcover
lag <- 6

# loop over all steps 

for(i in 1:nrow(step_dataset)){
  
  # retrieve MODIS image (SpatRaster layer) by matching date with step
  # source: https://stackoverflow.com/questions/73259623/how-to-index-individual-layers-from-a-spatraster-object-by-time
  step_modis <- modis_images[[time(modis_images) == as.Date(step_dataset$t1_[i])]]
  names(step_modis) <- 'ndvi' # necessary for plotting but otherwise not needed
  
  # extract NDVI for all pixels along step on day of passage
  ndvi_along <- extract_covariates_along(step_dataset, step_modis, name_covar = 'ndvi_along') # do i need this last parameter?
  
  # calculate percentile values of NDVI along step
  # source: https://www.statology.org/how-to-fix-in-r-error-in-sort-intx-na-last-decreasing-x-must-be-atomic/
  step_dataset$ndvi_10[i] <- quantile(unlist(ndvi_along), probs = 0.1, names = F, na.rm = T)
  step_dataset$ndvi_50[i] <- quantile(unlist(ndvi_along), probs = 0.5, names = F, na.rm = T)
  step_dataset$ndvi_90[i] <- quantile(unlist(ndvi_along), probs = 0.9, names = F, na.rm = T)
  step_dataset$ndvi_sd[i] <- sd(unlist(ndvi_along), na.rm = T)
  
  # retrieve MODIS image from previous week (*tested on step 43, had to take 6 days instead of 7 days because of cloudcover)
  step_modis_prev_week <- modis_images[[time(modis_images) == as.Date(step_dataset$t1_[i])-lag]]
  names(step_modis_prev_week) <- 'ndvi' # necessary for plotting but otherwise not needed
  
  # extract NDVI for all pixels along step for 1 week* prior passage
  ndvi_along_prev_week <- extract_covariates_along(step_dataset, step_modis_prev_week, name_covar = 'ndvi_along_prev_week')
  
  # calculate rate of NDVI change for all pixels along step for 1 week* prior to passage
  # note: have to retrieve vector inside list if want to do arithmatics
  ndvi_rate_along <- (ndvi_along[[1]] - ndvi_along_prev_week[[1]])/lag
  
  # calculate percentile values of NDVI change rate along step
  # source: https://www.statology.org/how-to-fix-in-r-error-in-sort-intx-na-last-decreasing-x-must-be-atomic/
  step_dataset$ndvi_rate_10[i] <- quantile(ndvi_rate_along, probs = 0.1, names = F, na.rm = T)
  step_dataset$ndvi_rate_50[i] <- quantile(ndvi_rate_along, probs = 0.5, names = F, na.rm = T)
  step_dataset$ndvi_rate_90[i] <- quantile(ndvi_rate_along, probs = 0.9, names = F, na.rm = T)
  step_dataset$ndvi_rate_sd[i] <- sd(ndvi_rate_along, na.rm = T)
  
}

# save this dataframe for now since took so long to generate --> full of NAs?!
write.csv(step_dataset, 'output/elephant_etosha/LA2_2027_step_dataset.csv')


## PICK UP FROM HERE (below is draft ideas)







# select step 
s1 <- step_dataset[43:45,]

s1$step_id_[2] <- 900

print(s1)

print(step_dataset$t1_[1])

# retrieve MODIS image (SpatRaster layer) by matching date with step
# source: https://stackoverflow.com/questions/73259623/how-to-index-individual-layers-from-a-spatraster-object-by-time
s1_modis <- modis_images[[time(modis_images) == as.Date(s1$t1_)]]
names(s1_modis) <- 'ndvi' # necessary for plotting but otherwise not needed

# extract NDVI for all pixels along step on day of passage
ndvi_along <- extract_covariates_along(s1, s1_modis, name_covar = 'ndvi_along')

# retrieve MODIS image from previous week (*tested on step 43, had to take 6 days instead of 7 days because of cloudcover)
s1_modis_prev_week <- modis_images[[time(modis_images) == as.Date(s1$t1_)-lag]]
names(s1_modis_prev_week) <- 'ndvi' # necessary for plotting but otherwise not needed

# extract NDVI for all pixels along step for 1 week* prior passage
ndvi_along_prev_week <- extract_covariates_along(s1, s1_modis_prev_week, name_covar = 'ndvi_along_prev_week')

# calculate rate of NDVI change for all pixels along step for 1 week* prior to passage
# note: have to retrieve vector inside list if want to do arithmatics
ndvi_rate_along <- (ndvi_along[[1]] - ndvi_along_prev_week[[1]])/lag

# calculate percentile values of covariates along step
# Note: make sure column for covariate exists before filling it in otherwise error
# source: https://www.statology.org/how-to-fix-in-r-error-in-sort-intx-na-last-decreasing-x-must-be-atomic/
s1$ndvi_10 <- quantile(unlist(ndvi_along), probs = 0.1, names = F)
s1$ndvi_50 <- quantile(unlist(ndvi_along), probs = 0.5, names = F)
s1$ndvi_90 <- quantile(unlist(ndvi_along), probs = 0.9, names = F)
s1$ndvi_sd <- sd(unlist(ndvi_along))

s1$ndvi_rate_10 <- quantile(ndvi_rate_along, probs = 0.1, names = F)
s1$ndvi_rate_50 <- quantile(ndvi_rate_along, probs = 0.5, names = F)
s1$ndvi_rate_90 <- quantile(ndvi_rate_along, probs = 0.9, names = F)
s1$ndvi_rate_sd <- sd(ndvi_rate_along)
 


print(s1$ndvi_along)



# visualize elephant data

modis_ndvi_map <- ggplot() +
  geom_spatraster(data = s1_modis_prev_week, aes(fill = ndvi), show.legend = T) +
  scale_fill_terrain_c(name = 'NDVI')

modis_ndvi_map

s1_df <- data.frame(location.long = c(s1$x1_, s1$x2_), location.lat = c(s1$y1_, s1$y2_))
s1_df

mov_map <- modis_ndvi_map +
  labs(title = "Elephant Movement", subtitle = ID, x = "Longitude", y = "Latitude") +
  geom_path(data = s1_df, aes(x = location.long, y = location.lat), color = 'red', linewidth = 1, show.legend = F) +
  theme_minimal()
mov_map


# step_dataset$ndvi_10[1] <- unname(quantile(step_dataset_along[[1]], probs = 0.1))
# 
# print(step_dataset[1])
# 
# 
# i <- rast('data/modis_ssf/2008_11_06.tif')
# i
# plot(i)
# 
# a <- period(1)
# class(a)
# 
# 
# # function that calculates mean value of covariates along a step 
# # Note: make sure column for covariate exists before filling it in otherwise error
# getAlongValue_10 <- function(i){
#   step_dataset$ndvi_10[i] <- quantile(step_dataset_along[[i]], probs = 0.1, names = F)
# }
# 
# getAlongValue_50 <- function(i){
#   step_dataset$ndvi_50[i] <- quantile(step_dataset_along[[i]], probs = 0.5)
# }
# 
# getAlongValue_90 <- function(i){
#   step_dataset$ndvi_90[i] <- mean(step_dataset_along[[i]])
# }
# 
# # apply function to each step 
# step_dataset$ndvi_10 <- sapply(1:756, getAlongValue_10)
# step_dataset$ndvi_50 <- sapply(seq.int(1, nrow(step_dataset)), getAlongValue_50)
# step_dataset$ndvi_90 <- sapply(seq.int(1, nrow(step_dataset)), getAlongValue_90)




######################### fit model #################################

ss_model <- fit_clogit(step_dataset, case_ ~ ndvi + strata(step_id_))

summary(ss_model)






