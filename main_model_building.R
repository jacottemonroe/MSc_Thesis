## MSc Thesis 
## Jacotte Monroe 
## Model Building 


## libraries 
if(!('terra') %in% installed.packages()){install.packages('terra')}
library(terra)
if(!('tidyterra') %in% installed.packages()){install.packages('tidyterra')} # for mapping spatraster
library(tidyterra)
if(!('ggplot2') %in% installed.packages()){install.packages('ggplot2')}
library(ggplot2)
if(!('lubridate') %in% installed.packages()){install.packages('lubridate')}
library(lubridate)
if(!('amt') %in% installed.packages()){install.packages('amt')}
library(amt)

if(!('dplyr') %in% installed.packages()){install.packages('dplyr')} #for grouping in table (max/min)
library(dplyr)


 
########################### load MODIS data ################################

# ## load MODIS NDVI image --> random one for now 
# modis <- rast('data/MODIS_250m_feb17_2014_sampleLA2.tif')
# 
# # remove pixels with outside of study area (assign NA)
# modis[modis == 0] <- NA
# 
# # calculate NDVI
# modis$ndvi <- (modis$sur_refl_b02 - modis$sur_refl_b01)/(modis$sur_refl_b02 + modis$sur_refl_b01)
# 
# # NDVI values above 1 were originally masked pixels (assign NA)
# modis[modis$ndvi > 1] <- NA
# modis$ndvi[modis$ndvi < 0] <- 0
# 
# # map NDVI 
# # source: https://dieghernan.github.io/tidyterra/reference/geom_spatraster.html
# # make sure to have loaded tidyterra package and ggplot2 
# # source: https://stackoverflow.com/questions/75074597/plotting-rasterlayer-objects-using-ggplot
# modis_ndvi_map <- ggplot() + 
#   geom_spatraster(data = modis, aes(fill = ndvi), show.legend = T) + 
#   scale_fill_terrain_c(name = 'NDVI')
# 
# modis_ndvi_map



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

# summary to check step interval --> it is correctly 4h nice
#summarize_sampling_rate(tr)



########################## generate steps ##############################

# turn elephant track dataset into steps
# by burst so steps not created for fixes in different paths
elephant_steps_by_path <- steps_by_burst(tr)

# generate corresponding random steps
elephant_all_steps_by_path <- random_steps(elephant_steps_by_path, n_control = 20,
                                           sl_distr = fit_distr(elephant_steps_by_path$sl_, "gamma"),
                                           ta_distr = fit_distr(elephant_steps_by_path$ta_, "vonmises"))

# save table 
write.csv(elephant_all_steps_by_path, 'data/temp_eleph_path.csv')



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

# save table 
write.csv(step_ex, 'data/step_extents/LA2_step_ex_w2027.csv')


######################### create covariate dataset #################### WHERE I LEFT OFF

## stack all MODIS NDVI images 
cov <- sprc(list.files('data/modis_ssf', pattern = glob2rx('*.tif'), full.names = T))

rast_list <- list()

for(i in 1:length(cov)){
  layer <- cov[i]
  time(layer, tstep = 'days') <- as.POSIXlt(names(layer), format = '%Y_%m_%d', tz = 'Africa/Maputo')
  print(time(layer))
  rast_list <- c(rast_list, layer)
}

cov_col <- sprc(rast_list)
cov_col


# https://stackoverflow.com/questions/20733555/how-to-create-a-raster-brick-with-rasters-of-different-extents


########################## extract covariates ##########################

# create new dataset to add covariates to and new column
step_dataset <- eleph_mov[1:30,]
step_dataset$ndvi_10 <- NA
step_dataset$ndvi_50 <- NA
step_dataset$ndvi_90 <- NA

# # change date string format (could be more efficient code)
# step_dataset$start_date <- gsub('-', '_', step_dataset$start_date)    
# step_dataset$end_date <- gsub('-', '_', step_dataset$end_date)   
# step_dataset$start_date_prev_week <- gsub('-', '_', step_dataset$start_date_prev_week)   
# step_dataset$end_date_prev_week <- gsub('-', '_', step_dataset$end_date_prev_week)   

# extract covariates along step 
step_dataset_along <- extract_covariates_along(step_dataset, cov[which(names(cov) == step_dataset$start_date)], name_covar = 'ndvi_along')

step_dataset$ndvi_10[1] <- unname(quantile(step_dataset_along[[1]], probs = 0.1))


i <- rast('data/modis_ssf/2008_11_06.tif')
i
plot(i)



# function that calculates mean value of covariates along a step 
# Note: make sure column for covariate exists before filling it in otherwise error
getAlongValue_10 <- function(i){
  step_dataset$ndvi_10[i] <- quantile(step_dataset_along[[i]], probs = 0.1, names = F)
}

getAlongValue_50 <- function(i){
  step_dataset$ndvi_50[i] <- quantile(step_dataset_along[[i]], probs = 0.5)
}

getAlongValue_90 <- function(i){
  step_dataset$ndvi_90[i] <- mean(step_dataset_along[[i]])
}

# apply function to each step 
step_dataset$ndvi_10 <- sapply(1:756, getAlongValue_10)
step_dataset$ndvi_50 <- sapply(seq.int(1, nrow(step_dataset)), getAlongValue_50)
step_dataset$ndvi_90 <- sapply(seq.int(1, nrow(step_dataset)), getAlongValue_90)




######################### fit model #################################

ss_model <- fit_clogit(step_dataset, case_ ~ ndvi + strata(step_id_))

summary(ss_model)






