## MSc Thesis
## Elephant Preprocessing 
## Jacotte Monroe 
## 29/01/24


## Script takes the elephant fix dataset. 
## The sequence of fixes is reprojected, resampled into consistent interval, split by elephant, week, and path. 
## Missing data is either interpolated or removed. 
## The script also generates a run table of all possible dataset entries. One entry per elephant and week (and method for generate false elephant data).
## Input: Raw elephant dataset as csv. 
## Output: Preprocessed elephant fix datasets per elephant (as csv) and full run table (as csv).

## Note: The generated run table contains all possible weeks to run. However, some weeks were faulty and the actual study split the data into new run tables. 
##        It is therefore recommended to use the pre-existing run tables to fully replicate the study.

## This script also has some leftover (optional code) to illustrate the elephant data availability over time + some other lines of code to get statistics on the data.




### install necessary packages 
if(!('terra') %in% installed.packages()){install.packages('terra')}
library(terra)

if(!('sf') %in% installed.packages()){install.packages('sf')}
library(sf)

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

# other packages that may or may not need 
# if(!('tidyterra') %in% installed.packages()){install.packages('tidyterra')}
# if(!('gganimate') %in% installed.packages()){install.packages('gganimate')}
# if(!('gifski') %in% installed.packages()){install.packages('gifski')}



### create directories 
if(!dir.exists('data')){dir.create(file.path('data', 'elephant_etosha'), recursive = T)}
if(!dir.exists('output')){dir.create('output')}



### load elephant dataset and set up parameters
# define filename for elephant dataset *change accordingly*
elephant_dataset_filename <- 'data/africanElephantEtoshaNP.csv'

# read in the tracking data csv file 
elephant_original_dataset <- read.table(file = elephant_dataset_filename, sep = ',', header = T)

# specify and remove any elephants that do not wish to use (in this case LA5)
faulty_elephant_ID <- 'LA5' # or set as c(NA)
elephant_data <- elephant_original_dataset[!(elephant_original_dataset$individual.local.identifier %in% faulty_elephant_ID),]

# get list of elephant IDs
# source: https://www.digitalocean.com/community/tutorials/strsplit-function-in-r
elephant_IDs <- strsplit(unique(elephant_data$individual.local.identifier), ' ')

# specify time interval between fixes 
interval <- 4

# specify time period of a path (in number of weeks)
time_period <- 1





##################### elephant data preprocessing ########################

source('functions/0_a_cleaningElephantDataset.R')
source('functions/0_b_reprojectingElephantData.R')
source('functions/0_c_resamplingElephant.R')
source('functions/0_d_groupingElephant.R')


# create dataframe for pseudo-absence path generating method (this will go into the run table settings)
method <- data.frame(pseudo_abs_method = c('random_path_custom_distr'))
# if want to test and compare methods for generating pseudo-absence data, specify in this variable
#method <- data.frame(pseudo_abs_method = c('random_path_custom_distr', 'random_path_buffer_point', 'random_step'))

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
  elephant_preprocessed <- groupElephantByPeriod(elephant_resampled, time_period)
  
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

write.csv(run_settings_table, 'data/run_settings_default.csv')











############################### ADDITIONAL CODE ################################

## Note: This code is much less well documented and not an integral part of study


###### Plot elephant data availability

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

# remove first row that had NA (it was the default start row)
presence <- na.omit(presence)

presence$combo <- paste0(presence$ID, '_', presence$week)

S <- read.csv(paste0('data/run_settings', '_STS_final', '.csv'), row.names = 1)
S$combo <- paste0(S$ID, '_', S$week)

L <- read.csv(paste0('data/run_settings', '_LTS_final', '.csv'), row.names = 1)
L$combo <- paste0(L$ID, '_', L$week)

D <- read.csv(paste0('data/run_settings', '_downscaling_final', '.csv'), row.names = 1)
D$combo <- paste0(D$ID, '_', D$week)

presence$phase[presence$combo %in% S$combo] <- 'STS'
presence$phase[presence$combo %in% L$combo] <- 'LTS'
presence$phase[presence$combo %in% D$combo] <- 'DS'
presence$phase[presence$combo %in% S$combo & presence$combo %in% L$combo] <- 'STS&LTS'
presence$phase[presence$combo %in% L$combo & presence$combo %in% D$combo] <- 'LTS&DS'
presence$phase[is.na(presence$phase)] <- 'Omitted'


# define start and end of green up
GU_start <- c("2008-11-01", "2009-11-01", "2010-11-01", "2011-11-01", "2012-11-01", "2013-11-01")
GU_end <- c("2009-04-30", "2010-04-30", "2011-04-30", "2012-04-30", "2013-04-30", "2014-04-30")
obs_start <- c("2008-11-01", "2009-10-01", "2010-10-01", "2011-10-01", "2012-10-01", "2013-10-01")
obs_end <- c("2009-04-15", "2010-04-15", "2011-04-15", "2012-04-15", "2013-04-15", "2014-03-27")

green_up <- data.frame(start_greenup = as.Date(GU_start), end_greenup = as.Date(GU_end), 
                       start_observ = as.Date(obs_start), end_observ = as.Date(obs_end))

# plot elephant data temporal presence and use per stage in research
elephant_presence_plot <- ggplot(presence) + 
  # source: https://stackoverflow.com/questions/72165869/plotting-date-ranges-for-each-id-and-marking-specific-dates-using-ggplot # for line ranges 
  geom_rect(data = green_up, aes(xmin = start_greenup, xmax = end_greenup, ymin = -Inf, 
                                 ymax = Inf, fill = 'Wet Season', alpha = 'Wet Season')) +
  geom_linerange(aes(y = ID, xmin = start_date, xmax = end_date, color = factor(phase, 
                                                                                level = c('STS', 'STS&LTS', 'LTS', 'LTS&DS', 'Omitted'))), linewidth = 4) + 
  scale_fill_manual(name = 'Season', values = c('Wet Season' = '#bce784')) +
  scale_color_manual(name = 'Data Used per\nResearch Question', values = c('STS' = '#004D40', 'STS&LTS' = '#FFC107', 'LTS' = '#1E88E5', 
                                                                           'LTS&DS' = '#D81B60', 'Omitted' = 'grey70'), 
                     labels = c('RQ1', 'RQ1 & RQ2', 'RQ2', 'RQ2 & RQ3', 'Omitted')) +
  scale_alpha_manual(name = 'Season', values = c('Wet Season' = 0.2)) +
  xlab('Time') + ylab('Elephant ID') + 
  theme_minimal()

elephant_presence_plot



# 
# 
# ###### Code to determine which weeks to run to cover max number of elephant --> which weeks are most frequent in data? 
# 
# t <- data.frame()
# for(i in 1:length(unique(run_settings_table$week))){
#   w <- unique(run_settings_table$week)[i]
#   e <- length(unique(run_settings_table$ID[run_settings_table$week == w]))
#   #print(paste('Week', w, 'appears in', length(e), 'datasets.'))
#   t <- rbind(t, data.frame(week = w, frq = e))
# }
# 
# t <- t[order(-t$frq),]
# 
# high_freq_weeks <- t[t$frq >= 10, ]
# high_freq_weeks <- high_freq_weeks[order(high_freq_weeks$week),]
# 
# t2 <- t[order(t$week),]
# t3 <- as.vector(rep(t2$week, t2$frq))
# hist(t3)
# 
# 
# 
# 
# ##### SOME STATS ON THE DATA
# 
# # calculate average number of paths per week of elephant dataset 
# p <- list.files('data/elephant_etosha', pattern = glob2rx('preprocessed*.csv'))
# p
# 
# d <- data.frame()
# for(item in p){
#   df <- read.csv(paste0('data/elephant_etosha/', item))
#   id <- as.character(sub('.*_', '', sub('.csv', '', item)))
#   # source: https://stackoverflow.com/questions/17421776/add-count-of-unique-distinct-values-by-group-to-the-original-data
#   df <- df %>% group_by(week) %>% mutate(unique_types = n_distinct(path))
#   n <- unique(df[,c('week', 'unique_types')])
#   entry <- data.frame(ID = id, n)
#   d <- rbind(d, entry)
# }
# 
# summary(d$unique_types)
# 
# id_list <- unique(d$ID)
# id_list <- id_list[-c(6, 12)]
# 
# # calculate average step length for all datasets 
# l <- list.files('data', pattern = glob2rx('1_b1*'), recursive = T)
# d <- data.frame()
# for(item in l){
#   id <- sub('/.*', '', item)
#   w <- sub('.*/', '', sub('/1_.*', '', item))
#   df <- readRDS(paste0('data/', item))
#   l <- df$sl_[df$case_ == T]
#   ta <- df$ta_[df$case_ == T]
#   entry <- data.frame(ID = id, week = w, sl = l, ta = ta)
#   d <- rbind(d, entry)
# }
# 
# summary(d$sl)
# sd(d$sl)
# summary(d$ta)
# sd(d$ta)
# 
# 
# 
