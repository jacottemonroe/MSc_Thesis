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
if(!('sf') %in% installed.packages()){install.packages('sf')} #creating geometries of bbox
library(sf)

 
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



###### amt package currently not working on VM so temp solution is to create the steps dataset on local
######  computer and then manually load in table into VM

## NOTE: currently step coordinates are in EPSG: 4326 when the raster images are in 32733 --> need to change coordinate of images (one line change in JN code)

LA2_steps <- read.csv('data/elephant_etosha/LA2_elephant_all_steps_temporary.csv')

# could make this code more efficient and less repetitive but idk how for now
LA2_step_ex <- LA2_steps

# get date of step
LA2_step_ex$date <- as.Date(LA2_step_ex$t1_)

# get extreme coordinates for each day
LA2_step_ex <- LA2_step_ex %>% group_by(date) %>% mutate(xmin = min(c(x1_, x2_))) %>% mutate(ymin = min(c(y1_, y2_))) %>% mutate(xmax = max(c(x1_, x2_))) %>% mutate(ymax = max(c(y1_, y2_)))

# get table of all extents per date
LA2_step_ex <- unique(LA2_step_ex[,c('date', 'xmin', 'ymin', 'xmax', 'ymax')])

# create empty col for geometry of bbox
LA2_step_ex$geom <- NA

# generate bbox geometry --> for some reason have to turn it into geometry twice for it to work (could do this better)
createBboxGeometry <- function(x){
  bbox <- st_as_sfc(st_bbox(c(xmin = LA2_step_ex$xmin[x], xmax = LA2_step_ex$xmax[x], 
                                             ymax = LA2_step_ex$ymax[x], ymin = LA2_step_ex$ymin[x]), crs = st_crs(4326)))
}

LA2_step_ex$geom <- sapply(1:nrow(LA2_step_ex), createBboxGeometry)

LA2_step_ex$geom <- st_as_sfc(LA2_step_ex$geom, crs = st_crs(4326))

# visualize bboxes for each day
plot(st_geometry(LA2_step_ex$geom))

# save table 
write.csv(LA2_step_ex, 'data/step_extents/LA2_step_ex_w2027.csv')


# 
# # turn elephant data frame into track_xyt object for model building
# # 'burst_' is a fixed/mandatory column name if want to generate steps for multiple paths
# tr <- make_track(df, location.long, location.lat, date_time, week = week, burst_ = path)
# 
# # summary to check step interval --> it is correctly 4h nice 
# summarize_sampling_rate(tr)
# 
# 
# 
# ########################## generate steps ##############################
# 
# # turn elephant track dataset into steps
# # by burst so steps not created for fixes in different paths 
# elephant_steps_by_path <- steps_by_burst(tr)
# 
# # generate corresponding random steps 
# elephant_all_steps_by_path <- random_steps(elephant_steps_by_path, n_control = 20, 
#                                            sl_distr = fit_distr(elephant_steps_by_path$sl_, "gamma"), 
#                                            ta_distr = fit_distr(elephant_steps_by_path$ta_, "vonmises"))



######################### step extents table ##########################

t <- data.frame(step_number = elephant_all_steps_by_path$, step_date = elephant_all_steps_by_path$, 
                xmin = min(elephant_all_steps_by_path$), ymin = min(elephant_all_steps_by_path$), 
                xmax = max(elephant_all_steps_by_path$), ymax = max(elephant_all_steps_by_path$))


########################## extract covariates ##########################

# create new dataset to add covariates to and new column
step_dataset <- elephant_all_steps_by_path
step_dataset$ndvi_10 <- NA
step_dataset$ndvi_50 <- NA
step_dataset$ndvi_90 <- NA

# extract covariates along step 
step_dataset_along <- extract_covariates_along(step_dataset, modis$ndvi, name_covar = 'ndvi_along')

step_dataset$ndvi_10[1] <- unname(quantile(step_dataset_along[[1]], probs = 0.1))




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






