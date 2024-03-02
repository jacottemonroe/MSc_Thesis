## test fake path 


############################## 1. get fix dataset for specific elephant and week 

# select elephant ID 
# original test was with elephant LA2
ID <- 'LA2' #'LA26'

# select week to test 
# original test was with week 2027
week <- 2027 #2300

file_name <- paste0('data/elephant_etosha/preprocessed_elephant_', ID,'.csv')

# get elephant dataset
full_df <- read.csv(file_name, row.names = 1)

# get week of interest 
df <- full_df[full_df$week == week,]

# change date_time format to remove time 
df$date_time <- as.POSIXct(df$date_time, tz = 'Africa/Maputo')

# turn elephant data frame into track_xyt object for model building
# 'burst_' is a fixed/mandatory column name if want to generate steps for multiple paths
df_track <- make_track(df, location.long, location.lat, date_time, week = week, burst_ = path)


############################################ 2. create observed dataset of steps 
# turn track fixes into observed steps 
# by burst so steps not created for fixes in different paths
true_steps <- steps_by_burst(df_track)

true_steps$case_ <- F
true_steps$step_id_ <- row.names(true_steps)

# retrieve average step length to use as fixed step length for random steps
set_distance <- mean(true_steps$sl_)


############################ 3. get initial starting point from observed dataset

# select starting fixes of each burst
# PACKAGE: dplyr should be installed
# source: https://www.r-bloggers.com/2022/07/select-the-first-row-by-group-in-r/
starting_fixes <- df_track %>% group_by(burst_) %>% filter(row_number()==1)


#### try buffer approach 

library(sf)

fake_path <- data.frame(t_ = starting_fixes$t_,  x_ = starting_fixes$x_, y_ = starting_fixes$y_)

row_place <- 1
seed <- 1

for(i in 1:nrow(true_steps)){
  
  # transform starting point coordinates into a spatial object 
  # source: https://www.dpi.inpe.br/gilberto/tutorials/software/R-contrib/sp/html/SpatialPoints.html
  starting_point <- st_as_sf(fake_path[nrow(fake_path),2:3], coords = c('location.long', 'location.lat'), crs = crs('EPSG:32733'))
  
  # create buffer around point 
  # source: https://gis.stackexchange.com/questions/292327/creating-buffers-around-points-and-merging-with-spatialpolygonsdataframe-to-crea
  buffer_point <- st_buffer(starting_point, dist = set_distance)
  
  # convert buffer polygon into polyline and sample random point along line
  # source: https://stackoverflow.com/questions/68987453/generating-random-locations-along-the-outer-border-of-a-shp-polygon-using-r
  set.seed(seed)
  new_point <- st_sample(st_cast(buffer_point, 'MULTILINESTRING'), 1)
  
  # add coordinates to fake path data frame
  # source: https://rdrr.io/cran/sf/man/st_coordinates.html
  fake_path <- rbind(fake_path, data.frame(date_time = df$date_time[row_place+1], location.long = st_coordinates(new_point)[[1]], 
                                     location.lat = st_coordinates(new_point)[[2]]), make.row.names = F)
  
  seed <- seed + 1
  row_place <- row_place + 1
  
}

fake_path$burst_ <- 1

fake_track <- make_track(fake_path, location.long, location.lat, date_time, burst_ = burst_)

fake_steps <- steps_by_burst(fake_track)

fake_steps$case_ <- F
fake_steps$step_id_ <- row.names(fake_steps)














# plot observed and random paths 


#r <- random_points(df_track, n = nrow(df_track))
fake_track <- make_track(fake_path, location.long, location.lat, date_time)

library(ggplot2)
mov_map <- ggplot() +
  labs(title = "Elephant Movement", subtitle = ID, x = "Longitude", y = "Latitude") +
  geom_path(data = df_track, aes(x = x_, y = y_), color = 'lightgreen', linewidth = 1, show.legend = F) +
  geom_path(data = fake_track, aes(x = x_, y = y_), color = 'red', linewidth = 1, show.legend = F) +
  # geom_point(data = r[r$case_ == T,], aes(x = x_, y = y_), color = 'darkgreen') +
  # geom_point(data = r[r$case_ == F,], aes(x = x_, y = y_), color = 'darkred') +
  theme_minimal()
mov_map






















######################################################################################
############################## other attempts ########################################
######################################################################################

# # select starting point for the first burst (loop it for all bursts)
# # PACKAGE: dplyr should be installed 
# # source: https://www.r-bloggers.com/2022/07/select-the-first-row-by-group-in-r/
# starting_steps <- true_steps %>% filter(row_number()==1|row_number()==2)
# 
# 
# ############################################ 4. create initial fake path dataset 
# 
# false_path <- df[1:2,1:3]
# 
# false_path$case_ <- T
# 
# false_path$step_id_ <- 0
# 
# row_place <- 3
# 
# average_sl <- mean(true_steps$sl_)
# 
# ################################## 5. generate 1 random step from starting point 
# 
# if(!('amt') %in% installed.packages()){install.packages('amt')}
# library(amt)
# 
# set.seed(1234)
# # source: https://www.geeksforgeeks.org/extract-last-n-rows-of-dataframe-in-r/
# fake_step <- random_steps(tail(starting_steps, 2), n_control = 1, 
#                           #sl_distr = fit_distr(true_steps$sl_, 'gamma'),
#                           rand_sl = average_sl,
#                           #ta_distr = fit_distr(true_steps$ta_, 'vonmises'),
#                           rand_ta = random_numbers(fit_distr(true_steps$ta_, 'vonmises')))[2,]
# 
# 
# ######################### 6. add newly generated fake step to false path dataset
# # source: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/cbind
# false_path <- rbind(false_path, data.frame(date_time = df$date_time[row_place], location.long = fake_step$x2_, 
#                                            location.lat = fake_step$y2_, case_ = fake_step$case_, 
#                                            step_id_ = fake_step$step_id_), make.row.names = F)
# 
# 
# 
# 
# 
# 
# 
# #################### 7. make new starting steps 
# starting_steps <- steps(make_track(false_path, location.long, location.lat, date_time))
# 
# # switch the coordinates of the last row (step going the other way)
# starting_steps[nrow(starting_steps), c('x1_', 'x2_', 'y1_', 'y2_')] <- starting_steps[nrow(starting_steps), c('x2_', 'x1_', 'y2_', 'y1_')]
# 
# # inverse direction?
# starting_steps$direction_p[nrow(starting_steps)] <- starting_steps$direction_p[nrow(starting_steps)] * -1
# 
# #################### 8. generate new step 
# set.seed(123)
# fake_step <- random_steps(tail(starting_steps, 2), n_control = 1, 
#                           #sl_distr = fit_distr(true_steps$sl_, 'gamma'),
#                           rand_sl = average_sl,
#                           #ta_distr = fit_distr(true_steps$ta_, 'vonmises'), 
#                           rand_ta = random_numbers(fit_distr(true_steps$ta_, 'vonmises')))[2,]
# 
# row_place <- row_place + 1
# step_number <- max(false_path$step_id_) + 1
# 
# seed <- seed + 1
# 
# false_path <- rbind(false_path, data.frame(date_time = df$date_time[row_place], location.long = fake_step$x2_, 
#                                            location.lat = fake_step$y2_, case_ = fake_step$case_, 
#                                            step_id_ = step_number), make.row.names = F)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ###### set up loop 
# 
# seed <- 1
# 
# for(i in 1:nrow(true_steps)){
#   
#   #################### 7. make new starting steps 
#   starting_steps <- steps(make_track(false_path, location.long, location.lat, date_time))
#   
#   #################### 8. generate new step 
#   set.seed(1234)
#   fake_step <- random_steps(tail(starting_steps, 2), n_control = 1, 
#                             #sl_distr = fit_distr(true_steps$sl_, 'gamma'),
#                             rand_sl = average_sl,
#                             #ta_distr = fit_distr(true_steps$ta_, 'vonmises'), 
#                             rand_ta = random_numbers(fit_distr(true_steps$ta_, 'vonmises')))[2,]
#   
#   row_place <- row_place + 1
#   step_number <- max(false_path$step_id_) + 1
#   
#   seed <- seed + 1
#   
#   false_path <- rbind(false_path, data.frame(date_time = df$date_time[row_place], location.long = fake_step$x2_, 
#                                              location.lat = fake_step$y2_, case_ = fake_step$case_, 
#                                              step_id_ = step_number), make.row.names = F)
# }
# 
# 
# f <- fit_distr(true_steps$ta_, 'vonmises')
# class(f)
# 
# 
# ##################################################### 7. make new starting steps 
# 
# starting_steps <- steps(make_track(false_path, location.long, location.lat, date_time))
# 
# #################### 8. generate new step 
# set.seed(123)
# fake_step <- random_steps(tail(starting_steps, 2), n_control = 1, 
#                           sl_distr = fit_distr(true_steps$sl_, 'gamma'),
#                           ta_distr = fit_distr(true_steps$ta_, 'vonmises'))[2,]
# 
# row_place <- row_place + 1
# step_number <- max(false_path$step_id_) + 1
# 
# false_path <- rbind(false_path, data.frame(date_time = df$date_time[row_place], location.long = fake_step$x2_, 
#                                            location.lat = fake_step$y2_, case_ = fake_step$case_, 
#                                            step_id_ = step_number), make.row.names = F)
# 
# 
# 
# step_id <- max(true_steps$burst_) + 1
# 
# starting_step$case_ <- T
# 
# 
# 
# 
# # ○ Create new df with x, y, t of last original step + new generated step 
# # ○ Make track from both steps 
# # ○ Turn into actual steps? 
# #   ○ Generate new random step 
# # ○ Create new df with x, y, t of previous step and new step …. 
# # Repeat until have same number of steps as in original data 
# 
# 
# 
# 
# 
# 
# 
# # read data with coords and timestamp
# 
# # select elephant ID 
# # original test was with elephant LA2
# ID <- 'LA2' #'LA26'
# 
# # select week to test 
# # original test was with week 2027
# week <- 2027 #2300
# 
# file_name <- paste0('data/elephant_etosha/preprocessed_elephant_', ID,'.csv')
# 
# # transform fix data to track object 
# source('functions_elephant_ssf/transformingToTrackObject.R')
# 
# #tr <- transformToTrackObject(file_name, w)
# 
# # get elephant dataset
# full_df <- read.csv(file_name, row.names = 1)
# 
# # get week of interest 
# df <- full_df[full_df$week == week,]
# 
# # change date_time format to remove time 
# df$date_time <- as.POSIXct(df$date_time, tz = 'Africa/Maputo')
# 
# # turn elephant data frame into track_xyt object for model building
# # 'burst_' is a fixed/mandatory column name if want to generate steps for multiple paths
# df_track <- make_track(df, location.long, location.lat, date_time, week = week, burst_ = path)
# 
# # turn fixes into observed steps 
# # by burst so steps not created for fixes in different paths
# true_steps <- steps_by_burst(df_track)
# 
# 
# 
# 
# df_fake <- df
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # retrieve all first steps from the dataset --> dataset may contain multiple paths = multiple starting steps
# # PACKAGE: dplyr should be installed 
# # source: https://www.r-bloggers.com/2022/07/select-the-first-row-by-group-in-r/
# starting_steps <- true_steps %>% group_by(burst_) %>% filter(row_number()==1|row_number()==2)
# 
# # add column for number of steps to generate 
# countStepsInPath <- function(i){
#   nrow(true_steps[true_steps$burst_ == starting_steps$burst_[i],])
# }
# 
# starting_steps$n_steps <- sapply(seq(1:nrow(starting_steps)), countStepsInPath)
# 
# # remove starting step where path = 1 step (need 2 observed steps to generate random step?)
# starting_steps <- starting_steps[starting_steps$n_steps > 1,]
# 
# 
# ## HERE NEED TO LOOP FOR EACH DIFFERENT BURST IN THE STARTING STEPS DATASET 
# 
# 
# # select the rows from true step dataset that match those from starting steps 
# # NOTE: need to do that because starting steps dataset was grouped = becomes grouped_df object which is not recognized by random_step function
# starting_step <- true_steps[true_steps$x1_ == starting_steps$x1_[1:2] & true_steps$y2_ == starting_steps$y2_[1:2],]
# 
# # add step id and case for initial true step 
# starting_step$step_id_ <- max(true_steps$burst_) + 1
# 
# starting_step$case_ <- T
# 
# # random step for starting step (only 1) > save random step > random step again but with new step > save > loop = 1 random path 
# 
# if(!('amt') %in% installed.packages()){install.packages('amt')}
# library(amt)
# 
# set.seed(1234)
# # source: https://www.geeksforgeeks.org/extract-last-n-rows-of-dataframe-in-r/
# fake_path <- random_steps(tail(starting_step, 2), n_control = 1, 
#                           sl_distr = fit_distr(true_steps$sl_, 'gamma'),
#                           ta_distr = fit_distr(true_steps$ta_, 'vonmises'))[2,]
# 
# fake_path$direction_p <- direction_abs(fake_path)
# starting_step <- cbind(starting_step, fake_path)
# 
# class(fake_path)
# 
# # loop 20 times to create 20 random paths 
# 
