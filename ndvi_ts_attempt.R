

run_table <- read.csv('data/run_settings_LTS_final.csv', row.names = 1)

ndvi_df <- data.frame()

for(i in 1:nrow(run_table)){
  
  ID <- run_table$ID[i]
  week <- run_table$week[i]
  
  df <- read.csv(paste0('data/',ID, '/', week, '/4_a1_cov_resp_dataset_random_path_custom_distr_newPathWithCV.csv'), row.names = 1)
  
  df <- df[df$case_ == T,]
  
  entry <- data.frame(ID = ID, week = week, datetime = df$t1_, ndvi = df$ndvi_mean)
  
  ndvi_df <- rbind(ndvi_df, entry)
}

ndvi_df_ag <- ndvi_df

ndvi_df_ag$date <- as.Date(ndvi_df_ag$datetime, tz = 'Africa/Maputo')

ndvi_df_ag <- ndvi_df_ag[,c('date', 'ndvi')]

library(dplyr)
ndvi_df_ag <- ndvi_df_ag %>% group_by(date) %>% summarise_all('mean')

library(ggplot2)
ggplot(data = ndvi_df_ag, aes(x = date, y = ndvi, group = 1)) + 
  geom_line()

write.csv(ndvi_df_ag, 'output/LTS_elephant_step_ndvi_ts.csv')

ndvi_etosha <- read.csv('data/etoshaNP/lts_etosha_ndvi_8d_ts.csv')
ndvi_etosha$date <- as.Date(ndvi_etosha$system.time_start, '%b %d, %Y')

precip_etosha <- read.csv('data/etoshaNP/sts_etosha_precip_daily_ts.csv')
precip_etosha$date <- as.Date(precip_etosha$system.time_start, '%b %d, %Y')

sts_model <- read.csv('output/STS_df_results_aggregated_newPath10fCV_final.csv')
sts_model <- sts_model[sts_model$predictor == 'Avg. NDVI', c('date', 'glm_value')]
sts_model <- sts_model %>% group_by(date) %>% summarise_all('mean')
sts_model$date <- as.Date(sts_model$date)

ggplot() + 
  geom_area(data = lts_ndvi_df_ag, aes(x = date, y = ndvi), fill = '#bce784', alpha = 0.8) + 
  geom_line(data = precip_etosha, aes(x = date, y = total_precipitation_sum*10), color = '#43A5C5') + 
  #geom_area(data = ndvi_etosha, aes(x = date, y = NDVI), fill = 'grey', color = 'grey') + 
  #geom_line(data = ndvi_df_ag, aes(x = date, y = ndvi, group = 1), color = 'green', alpha = 0.5) + 
  #geom_line(data = ndvi_etosha, aes(x = date, y = NDVI, group = 1), color = 'red', alpha = 0.5) + 
  scale_y_continuous(name = 'NDVI', sec.axis = sec_axis(~.*0.01*10000, name = 'Accumulated Daily Precipitation [mm]')) + 
  theme_minimal() + 
  theme(axis.text.y.right = element_text(color = '#3897B6'), 
        axis.text.y.left = element_text(color = '#80C626')) + 
  xlab('Time')


# FOR LTS
ggplot() + 
  geom_line(data = precip_etosha, aes(x = date, y = total_precipitation_sum*100), color = '#43A5C5', alpha = 0.5) + 
  geom_line(data = sts_model, aes(x = date, y = glm_value*0.5), color = 'red') + 
  scale_y_continuous(name = 'Accumulated Precipitation [cm]', sec.axis = sec_axis(~.*2, name = 'Coef')) + 
  theme_minimal() + 
  theme(axis.text.y.right = element_text(color = 'red3'), 
        axis.text.y.left = element_text(color = '#3897B6')) + 
  xlab('Time')

ggplot() + 
  geom_line(data = lts_ndvi_df_ag, aes(x = date, y = ndvi), color = '#bce784') +
  geom_line(data = sts_model, aes(x = date, y = glm_value*0.1), color = 'red') + 
  scale_y_continuous(name = 'NDVI', sec.axis = sec_axis(~.*10, name = 'Coef')) + 
  theme_minimal() + 
  theme(axis.text.y.right = element_text(color = 'red3'), 
        axis.text.y.left = element_text(color = '#80C626')) + 
  xlab('Time')

# FOR STS
ggplot() + 
  geom_line(data = precip_etosha, aes(x = date, y = total_precipitation_sum*100), color = '#43A5C5', alpha = 0.5) + 
  geom_line(data = sts_model, aes(x = date, y = glm_value*2), color = 'red') + 
  scale_y_continuous(name = 'Accumulated Precipitation [cm]', sec.axis = sec_axis(~.*0.5, name = 'Coef')) + 
  theme_minimal() + 
  theme(axis.text.y.right = element_text(color = 'red3'), 
        axis.text.y.left = element_text(color = '#3897B6')) + 
  xlab('Time')

ggplot() + 
  geom_line(data = sts_ndvi_df_ag, aes(x = date, y = ndvi), color = '#bce784') +
  geom_line(data = sts_model, aes(x = date, y = glm_value*0.5), color = 'red') + 
  scale_y_continuous(name = 'NDVI', sec.axis = sec_axis(~.*2, name = 'Coef')) + 
  theme_minimal() + 
  theme(axis.text.y.right = element_text(color = 'red3'), 
        axis.text.y.left = element_text(color = '#80C626')) + 
  xlab('Time')
