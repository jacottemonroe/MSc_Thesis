## Thesis - Data Exploration 

## Map out some elephant tracking data to see what it looks like 


# install packages 
if(!('terra') %in% installed.packages()){install.packages('terra')}
if(!('sf') %in% installed.packages()){install.packages('sf')}

if(!('dplyr') %in% installed.packages()){install.packages('dplyr')} #for dataframe slicing & resampling & path spliting 
library(dplyr)

if(!('rlang') %in% installed.packages()){install.packages('rlang')} # for turning string into dataframe column frame in dplyr piping 
library(rlang)

if(!('geosphere') %in% installed.packages()){install.packages('geosphere')} # for calc distance
library(geosphere)

if(!('ggplot2') %in% installed.packages()){install.packages('ggplot2')} # for histograms and maps
library(ggplot2)

if(!('ggspatial') %in% installed.packages()){install.packages('ggspatial')} #for north arrow
library(ggspatial)

if(!('padr') %in% installed.packages()){install.packages('padr')} # for resampling (thicken function)
library(padr)

if(!('zoo') %in% installed.packages()){install.packages('zoo')} # for linear interpolation
library(zoo)

if(!('lubridate') %in% installed.packages()){install.packages('lubridate')} # for getting weeks 
library(lubridate)

# 
# if(!('tidyterra') %in% installed.packages()){install.packages('tidyterra')}
# if(!('gganimate') %in% installed.packages()){install.packages('gganimate')}
# if(!('gifski') %in% installed.packages()){install.packages('gifski')}

library(terra)
library(sf)
# library(ggplot2)
# library(ggspatial)
# 
# library(tidyterra)
# library(gganimate)
# library(gifski)

# create directories 
if(!dir.exists('data')){dir.create('data')}
if(!dir.exists('output')){dir.create('output')}



######################### user input ####################################

study_area_filename <- 'data/etosha_geometry.geojson'
landcover_filename <- 'data/etosha_landcover_dataframe_cop.csv'
elephant_dataset_filename <- 'data/elephant_etosha/africanElephantEtoshaNP_analysis.csv'

# list all faulty elephant datasets to remove (NA if none to remove)
faulty_elephant_ID <- c(NA) #c('LA5') 

data_exploration <- 'OFF' #or 'ON'

# specify time interval between fixes 
interval <- 4

# specify time period of a path (in number of weeks)
time_period <- 1





######################### set-up - no user input ############################

# # load study area --> need SF package for this
# sa <- st_read(study_area_filename)
# 
# # retrieve the landcover dataframe of etosha for plotting 
# lc_etoshaNP <- read.csv(landcover_filename, sep = ',')
# 
# # create basemap of Etosha (for later mapping)
# source("functions/creatingEtoshaBasemap.R")
# basemap_Etosha <- createBasemapEtosha(sa, lc_etoshaNP)
# basemap_Etosha

# read in the tracking data csv file 
elephant_original_dataset <- read.table(file = elephant_dataset_filename, sep = ',', header = T)

# remove elephant from dataset
source('functions_preprocessing_elephant/removeFaultyElephant.R')
elephant_data <- removeElephantByID(elephant_original_dataset, faulty_elephant_ID)

# get list of elephant IDs
# source: https://www.digitalocean.com/community/tutorials/strsplit-function-in-r
elephant_IDs <- strsplit(unique(elephant_data$individual.local.identifier), ' ')
#print(elephant_IDs)



########################### data exploration ################################

if (data_exploration == 'ON') {
  # explore each elephant dataset --> could loop this? 
  source("functions_preprocessing_elephant/exploringElephantDataset.R")
  elephant_results_list <- list()
  
  for (ID in elephant_IDs) {
    results <- list(exploreElephantData(elephant_data, ID, basemap_Etosha))
    elephant_results_list <- append(elephant_results_list, results)
  }
  
  names(elephant_results_list) <- unlist(elephant_IDs)
}




##################### elephant data preprocessing ########################

source('functions_preprocessing_elephant/cleaningElephantDataset.R')
source('functions_preprocessing_elephant/resamplingElephant.R')
source('functions_preprocessing_elephant/groupingElephant.R')
source('functions_preprocessing_elephant/reprojectingElephantData.R')

# create dataframe for pseudo-absence path generating method 
method <- data.frame(pseudo_abs_method = c('random_path_custom_distr', 'random_path_buffer_point', 'random_step'))

# create empty dataframe for storing run parameter settings 
run_settings_table <- data.frame()

# loop for each elephant dataset
for (ID in elephant_IDs) {
  
  # clean data 
  elephant_clean <- cleanElephantDataset(elephant_data, ID)
  
  # reproject data 
  elephant_reprojected <- reprojectElephantData(elephant_clean)
  
  # resample data to time interval 
  elephant_resampled <- resampleElephantData(elephant_reprojected, interval, acceptable_NA_gap = 1, 'linear interpolation')
  
  # group data into periods (weeks)
  elephant_preprocessed <- groupElephantByPeriod(elephant_resampled, time_period, constant_step_size = F)
  
  # save preprocessed elephant dataset as csv
  write.csv(elephant_preprocessed, file = paste0('data/elephant_etosha/preprocessed_elephant_', ID, '.csv'))
  
  # create new dataframe with elephant ID and week numbers of data 
  elephant_information <- data.frame(ID = ID, week = unique(elephant_preprocessed$week))
  
  # add the method dataframe as a new column to the information dataframe
  run_settings_elephant <- merge(elephant_information, method)
  
  # arrange rows in order of oldest to most recent (by weeks)
  run_settings_elephant <- run_settings_elephant[order(run_settings_elephant$week),]
  
  # bind the elephant dataset to the large run settings table 
  run_settings_table <- rbind(run_settings_table, run_settings_elephant)
  
}

# code to determine which weeks to run --> which weeks are most frequent in data? 
t <- data.frame()
for(i in 1:length(unique(run_settings_table$week))){
  w <- unique(run_settings_table$week)[i]
  e <- length(unique(run_settings_table$ID[run_settings_table$week == w]))
  #print(paste('Week', w, 'appears in', length(e), 'datasets.'))
  t <- rbind(t, data.frame(week = w, frq = e))
}

t <- t[order(-t$frq),]

high_freq_weeks <- t[t$frq >= 10, ]
high_freq_weeks <- high_freq_weeks[order(high_freq_weeks$week),]

t2 <- t[order(t$week),]
t3 <- as.vector(rep(t2$week, t2$frq))
hist(t3)

# create run settings table with all elephants for week 2075
run_table <- data.frame()
week_of_interest <- 2075
for(ID in elephant_IDs){
  d <- read.csv(paste0('data/elephant_etosha/preprocessed_elephant_', ID, '.csv'))
  if(week_of_interest %in% d$week){
    r <- data.frame(ID = ID, week = week_of_interest)
    r <- merge(r, method)
    run_table <- rbind(run_table, r)
  }
}

# save run settings combination table 
write.csv(run_table, 'data/run_settings.csv')

c <- read.csv('data/run_settings.csv')




























#### leftover code ########

# test 
ID <- 'LA2'

# clean data 
elephant_clean <- cleanElephantDataset(elephant_data, ID)

# reproject data 
elephant_reprojected <- reprojectElephantData(elephant_clean)

# resample data to time interval 
elephant_resampled <- resampleElephantData(elephant_reprojected, interval, acceptable_NA_gap = 1, 'linear interpolation')

# group data into periods (weeks)
elephant_preprocessed <- groupElephantByPeriod(elephant_resampled, time_period, constant_step_size = F)

# save preprocessed elephant dataset as csv
write.csv(elephant_preprocessed, file = paste0('data/elephant_etosha/preprocessed_elephant_', ID, '.csv'))

# retrieve all week numbers from the dataset of the elephant 
elephant_run_settings <- data.frame(ID = 'LA2', week = unique(elephant_preprocessed$week))

# create dataframe for pseudo-absence path generating method 
method <- data.frame(pseudo_abs_method = c('random_path_custom_distr', 'random_path_buffer_point', 'random_step'))

# join the method to the run settings table
run_settings <- merge(elephant_run_settings, method)

# arrange rows in order of oldest to most recent (by weeks)
run_settings_ordered <- run_settings[order(run_settings$week),]

second_eleph <- run_settings_ordered

second_eleph$ID <- 'LA5'

# bind datasets of elephants 

run_settings <- rbind(run_settings_ordered, second_eleph)


e <- data.frame()
e <- rbind(e, run_settings_ordered)







ID <- 'LA15'
elephant_clean <- cleanElephantDataset(elephant_data, ID)

elephant_resampled <- resampleElephantData(elephant_clean, interval, acceptable_NA_gap = 1, 'linear interpolation')


elephant_resampled['week'] <- as.integer(as.numeric(elephant_resampled$date_time)/(604800*1)) # 604800 seconds in a week

elephant_resampled['NA_count'] <- 0

elephant_resampled$NA_count[is.na(elephant_resampled$location.long)] <- 1

period <- syms('week')
elephant_resampled <- elephant_resampled %>% group_by(!!!period) %>% mutate(csum = cumsum(NA_count)) 

elephant_resampled <- elephant_resampled[!is.na(elephant_resampled$location.long),]

elephant_resampled <- elephant_resampled %>% group_by(!!!period) %>% mutate(path = match(csum, unique(csum)))

elephant_resampled <- subset(elephant_resampled, select = -c(NA_count, csum))

e <- elephant_resampled

# source: https://stackoverflow.com/questions/27829558/count-occurrences-in-unique-group-combination
e_count <- count(e, week, path) %>% ungroup()

e_solo_steps <- e_count[e_count$n == 1, 1:2]

number_e_solo_steps <- nrow(e_solo_steps)
print(number_e_solo_steps)
print(nrow(e) - number_e_solo_steps)

for(i in 1:nrow(e_solo_steps)){
  e <- e[-(e$week == e_solo_steps$week[i] & e$path == e_solo_steps$path[i]),]
}




elephant_preprocessed_constantSS <- groupElephantByPeriod(elephant_resampled, time_period, constant_step_size = T)
elephant_preprocessed_variableSS <- groupElephantByPeriod(elephant_resampled, time_period, constant_step_size = F)





########################## see elephant time ranges ####################

# empty dataset of elephant presence dates 
presence <- data.frame(ID = NA, start_date = NA, end_date = NA, week = NA, path = NA)

# each elephant gets 1 column in dataframe where fill in matching dates 
for (ID in elephant_IDs) {
  
  file_name <- paste0('data/elephant_etosha/preprocessed_elephant_', ID,'.csv')
  
  # get elephant dataset
  df <- read.csv(file_name)
  
  # change date_time format to remove time 
  df$date_time <- as.Date(df$date_time, tz = 'Africa/Maputo')
  
  # get list of periods 
  # source: https://www.tutorialspoint.com/how-to-extract-unique-combinations-of-two-or-more-variables-in-an-r-data-frame
  period_list <- period_list <- unique(df[c('week', 'path')]) 
  
  for(p in 1:nrow(period_list)){
    
    # subset period
    period_date_range <- df[df$week == period_list$week[p] & df$path == period_list$path[p],]
    
    # add row to dataframe (need tidyverse package)
    # source: https://sparkbyexamples.com/r-programming/add-row-to-dataframe-in-r/
    presence <- presence %>% add_row(ID = ID, start_date = min(period_date_range$date_time),
                                     end_date = max(period_date_range$date_time), 
                                     week = period_list$week[p], path = period_list$path[p])
  }
}




####################### visualize ###############################

# remove first row that had NA (it was the default start row)
presence <- na.omit(presence)

# define start and end of green up
GU_start <- c("2008-11-01", "2009-11-01", "2010-11-01", "2011-11-01", "2012-11-01", "2013-11-01")
GU_end <- c("2009-04-30", "2010-04-30", "2011-04-30", "2012-04-30", "2013-04-30", "2014-04-30")
obs_start <- c("2008-11-01", "2009-10-01", "2010-10-01", "2011-10-01", "2012-10-01", "2013-10-01")
obs_end <- c("2009-04-15", "2010-04-15", "2011-04-15", "2012-04-15", "2013-04-15", "2014-03-27")

green_up <- data.frame(start_greenup = as.Date(GU_start), end_greenup = as.Date(GU_end), 
                       start_observ = as.Date(obs_start), end_observ = as.Date(obs_end))

# plot elephant temporal presence 
# source: https://stackoverflow.com/questions/72165869/plotting-date-ranges-for-each-id-and-marking-specific-dates-using-ggplot # for line ranges 
elephant_presence_plot <- ggplot(presence) + 
  # geom_rect(data = green_up, aes(xmin = start_observ, xmax = end_observ, ymin = -Inf, 
  #                                ymax = Inf), fill = 'grey70', alpha = 0.2) +
  geom_rect(data = green_up, aes(xmin = start_greenup, xmax = end_greenup, ymin = -Inf, 
                                 ymax = Inf, fill = 'Wet Season'), alpha = 0.1) +
  geom_linerange(aes(y = ID, xmin = start_date, xmax = end_date, color = 'Elephant Path Data'), linewidth = 4) + 
  scale_fill_manual(name = '', values = c('Wet Season' = 'green4')) +
  scale_color_manual(name = '', values = c('Elephant Path Data' = 'orange')) +
  guide_legend(override.aes = list(alpha = 0.1)) +
  xlab('Time') + ylab('Elephant ID') + 
  theme_minimal()

elephant_presence_plot







### EXTRA: perform checks 
# check that there is no missing data 
# missing_data_check <- any(is.na(elephant_weekly))

# check that all weeks have same number of fixes 
# week_length_check <- elephant_weekly %>% count(week)
# should be the same value as: 
# number_weekly_fixes <- 7*(24/interval)













LA5_daily <- elephant_data[elephant_data$individual.local.identifier == 'LA11',]




#### plot data

mov_map <- ggplot() +
  geom_raster(data = lc_etoshaNP, aes(x = x, y = y, fill = landcover), show.legend = F) + 
  geom_sf(data = sa$geometry, fill = NA, color = 'black', lwd = 1) +
  #scale_fill_grey(start = 1, end = 0.7) +
  scale_fill_manual(values = c('grey94', 'grey16', 'grey32', 'grey65', 'grey80', 'grey90', 'grey25', 'grey20')) +
  #scale_fill_manual(name = 'Land cover', values = lc_lut$lc.color, labels = lc_lut$lc.class, na.translate = F) +
  #labs(title = "Elephant Movement", subtitle = "2008 - 2014", x = "Longitude", y = "Latitude") +
  geom_path(data = LA5_daily, aes(x = location.long, y = location.lat, color = 'red3', group=1), linewidth = 0.1, show.legend = F) +
  #annotation_north_arrow(location = 'tl', which_north = 'true', 
  # pad_x = unit(0.04, "in"), pad_y = unit(0.3, "in"),
  # style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = 'tl') +
  theme_minimal()

mov_map



lc_map <- ggplot() +
  geom_raster(data = lc_etosha_df, aes(x = x, y = y, fill = lc_etosha_df$landcover), show.legend = F) + 
  geom_sf(data = sa$geometry, fill = NA, color = 'black', lwd = 1) +
  scale_fill_manual(name = 'Land cover', values = lc_lut$lc.color, labels = lc_lut$lc.class, na.translate = F) +
  labs(title = "Etosha National Park",
       subtitle = "Namibia",
       x = "Longitude",
       y = "Latitude") +
  annotation_north_arrow(location = 'tl', which_north = 'true', 
                         pad_x = unit(0.04, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = 'tl') +
  #annotation_custom(grob = ggplotGrob(mini_map), xmin = 17, xmax = 17.5, ymin = 18.6, ymax = 18.8) +
  theme_minimal() + 
  theme(plot.margin=unit(c(1,1,4,0.5),"cm")) + 
  theme(legend.position = c(1.2, .1))
lc_map


# tutorial: https://conservancy.umn.edu/bitstream/handle/11299/220339/time-maps-tutorial-v2.html?sequence=3&isAllowed=y
# tutorial: https://hansenjohnson.org/post/animate-movement-in-r/

# plot basemap 
base_map <- ggplot() +
  # source: https://bookdown.org/mcwimberly/gdswr-book/raster-geospatial-data---discrete.html --> can't get it to work because of the different land cover data structure
  #geom_raster(data = lc_masked, show.legend = F, aes(fill = discrete_classification)) +
  #scale_fill_manual(name = 'Land cover', values = lc_colors, labels = lc_classes, na.translate = F)
  geom_sf(data = sa$geometry, color = "black") #, fill = "white") 

#base_map

# plot data on basemap 
map_with_data <- base_map +
  #geom_point(data = LA5_daily, aes(x = location.long, y = location.lat, group=date), color = 'royalblue1', size = 0.8) +
  geom_path(data = LA5_daily, aes(x = location.long, y = location.lat, color = tag, group=1), linewidth = 0.1)

#map_with_data

# zoom in 
min_long <- min(LA5_daily$location.long)
max_long <- max(LA5_daily$location.long)
min_lat <- min(LA5_daily$location.lat)
max_lat <- max(LA5_daily$location.lat)

map_with_data <- map_with_data +
  coord_sf(xlim = c(min_long, max_long),  ylim = c(min_lat, max_lat))

map_with_data

# animate 
map_with_animation <- mov_map #map_with_data +
transition_reveal(along = as.Date(timestamp)) +
  ggtitle('Date: {frame_along}',
          subtitle = 'Frame {frame} of {nframes}')

map_with_shadow <- map_with_animation + 
  shadow_mark()

days <- unique(LA5_daily$date)
ndays <- max(days) - min(days) + 1

animate(map_with_shadow, nframes = ndays, fps = 100)
anim_save('output/elephant_LA14_timelapse.gif', animation = last_animation())










l <- list.files('data/elephant_etosha/elephant_fixes', full.names = T)
wtab <- data.frame()

for(i in 1:length(l)){
  f <- read.csv(l[i])
  id <- sub('.*preprocessed_elephant_', '', sub('.csv', '', l[i]))
  entry <- data.frame(ID = id, week = unname(data.frame(unique(f$week))))
  wtab <- rbind(wtab, entry)
}

t <- data.frame(table(wtab$week))


p <- read.csv('data/elephant_etosha/preprocessed_elephant_LA11.csv')
# select 2 years of data to run for RQ2
p <- p[p$week %in% seq(2065,2194),]

r <- read.csv('data/run_settings.csv', row.names = 1)
r <- r[r$pseudo_abs_method == 'random_path_custom_distr',]
list_id <- unique(r$ID)

# create new run table
rt <- data.frame()
for(id in list_id){
  entry <- data.frame(ID = id, week = c(2065, 2067, 2068, 2069, 2073, 2074, 2076, 2077, seq(2085,2089)), pseudo_abs_method = r$pseudo_abs_method[1], downscaling = 'NULL', downscaling_model = 'NULL')
  rt <- rbind(rt, entry)
}

#rt <- data.frame(ID = 'LA11', week = c(2065, 2067, 2068, 2069, 2073, 2074, 2076, 2077, seq(2085,2089)), pseudo_abs_method = r$pseudo_abs_method[1], downscaling = 'NULL', downscaling_model = 'NULL')

# save table for RQ2
write.csv(rt, 'data/run_settings_RQ2_STS.csv')




d <- read.csv('data/elephant_etosha/preprocessed_elephant_LA14.csv')
w <- unique(d$week)
w <- seq(2095, 2281)
rt <- data.frame(ID = 'LA14', week = w, pseudo_abs_method = 'random_path_custom_distr', downscaling = 'NULL', downscaling_model = 'NULL')
write.csv(rt, 'data/run_settings_LA14_LTS_extended.csv')

# create new run table for downscaling runs that have not been rescaled
d <- read.csv('data/run_settings_downscaling.csv', row.names = 1)
dm <- d[d$ID == 'LA14' & d$downscaling == F,]
d <- d[rownames(d) != as.numeric(rownames(dm)),]
d <- d[rownames(d) != c(11),]
write.csv(d, 'data/run_settings_downscaling_rescaling.csv')



# calculate average number of paths per week of elephant dataset 
p <- list.files('data/elephant_etosha', pattern = glob2rx('preprocessed*.csv'))
p

d <- data.frame()
for(item in p){
  df <- read.csv(paste0('data/elephant_etosha/', item))
  id <- as.character(sub('.*_', '', sub('.csv', '', item)))
  # source: https://stackoverflow.com/questions/17421776/add-count-of-unique-distinct-values-by-group-to-the-original-data
  df <- df %>% group_by(week) %>% mutate(unique_types = n_distinct(path))
  n <- unique(df[,c('week', 'unique_types')])
  entry <- data.frame(ID = id, n)
  d <- rbind(d, entry)
}

summary(d$unique_types)

id_list <- unique(d$ID)
id_list <- id_list[-c(6, 12)]

# calculate average step length for all datasets 
l <- list.files('data', pattern = glob2rx('1_b1*'), recursive = T)
d <- data.frame()
for(item in l){
  id <- sub('/.*', '', item)
  w <- sub('.*/', '', sub('/1_.*', '', item))
  df <- readRDS(paste0('data/', item))
  l <- df$sl_[df$case_ == T]
  ta <- df$ta_[df$case_ == T]
  entry <- data.frame(ID = id, week = w, sl = l, ta = ta)
  d <- rbind(d, entry)
}

summary(d$sl)
sd(d$sl)
summary(d$ta)
sd(d$ta)



