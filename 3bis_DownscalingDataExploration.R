## MSc_Thesis 
## Jacotte Monroe 
## Exploring downscaling results 


# Packages 
if(!('terra') %in% installed.packages()){install.packages('terra')} # to read rasters
library(terra)

if(!('ggplot2') %in% installed.packages()){install.packages('ggplot2')} # to plot timeseries
library(ggplot2)

# enter settings
ID <- 'LA14'
week <- 2260
pseudo_abs_method <- 'random_path_custom_distr'

# define run filepath 
run_filepath <- paste0('data/', ID, '/', week, '/')

# define landsat filepath 
landsat_filepath <- paste0(run_filepath, '3_b2_landsat_images_downscaling_', pseudo_abs_method, '/')

# define modis filepath 
modis_filepath <- paste0(run_filepath, '3_b1_modis_images_downscaling_', pseudo_abs_method, '/')

# load LUT
LUT <- readRDS(paste0(run_filepath, '3_c1_MODISLandsatLUT.RData'))




###########
## result analysis and interpretation 
###########

# see script for that --> not running it atm because its more exploration of results 
# change this later dep on what want to show 

# plot MAE, RMSE, R² over time for each model type 
# each row is a date, each is a model type 
# one plot per performance metric 

# create empty dataframe to store metrics 
results_df <- data.frame()

for(i in 1:nrow(LUT)){
  
  # get run date
  modis_date <- LUT$modis_date[i]
  
  # select model result files for date
  #model_results_files <- list.files(run_filepath, pattern = glob2rx(paste0('3_f7_', modis_date, '_*')))
  model_results_files <- list.files(run_filepath, pattern = glob2rx(paste0('3_f2_', modis_date, '_*')))
  
  # create empty dataframe to store metrics for date
  entry <- data.frame(date = modis_date)
  
  for(file in model_results_files){
    
    # read results file 
    item <- read.csv(paste0(run_filepath, file), row.names = 1)
    
    # for when running the trained model metrics (not for prediction metrics)
    item <- item[item$RMSE == min(item$RMSE),c('RMSE', 'Rsquared', 'MAE')]
    
    # retrieve model name and rename columns 
    # source: https://www.digitalocean.com/community/tutorials/sub-and-gsub-function-r
    #model <- sub(paste0('3_f7_', modis_date, '_'), '', sub('_predNDVI_250m_results.csv', '', file))
    model <- sub(paste0('3_f2_', modis_date, '_'), '', sub('_results.csv', '', file))
    names(item) <- c(paste0(model, '.', 'RMSE'), paste0(model, '.', 'R2'), paste0(model, '.', 'MAE'))
    
    # attach results to dataframe 
    entry <- cbind(entry, item)
  }
  results_df <- rbind(results_df, entry)
}

# save summary table 
# for cv metrics: '3_g1_summary_metrics_CV.csv'
# for prediction metrics: '3_g2_summary_metrics_prediction.csv'
write.csv(results_df, paste0(run_filepath, '3_g1_summary_metrics_CV.csv'))

d <- results_df

# # normalize RMSE
# rmse_df <- d[,c('lm_full.RMSE', 'lm_ffs.RMSE', 'cubist_full.RMSE', 'ranger_full.RMSE')]
# norm_rmse <- data.frame()
# for(r in 1:nrow(rmse_df)){
#   #max_value <- max(rmse_df[r,])
#   #min_value <- min(rmse_df[r,])
#   mean_value <- unname(rowMeans(rmse_df[1,]))
#   #n <- data.frame(day = d$date[r], rmse_df[r,]/(max_value - min_value))
#   n <- data.frame(day = d$date[r], rmse_df[r,]/mean_value)
#   norm_rmse <- rbind(norm_rmse, n)
# }


# source: https://www.datanovia.com/en/blog/how-to-create-a-ggplot-with-multiple-lines/
# source: http://www.sthda.com/english/wiki/ggplot2-title-main-axis-and-legend-titles
ggplot(data = d, aes(x = date)) + 
  geom_line(aes(y = lm_full.RMSE, color = 'lm_full')) + 
  geom_line(aes(y = lm_ffs.RMSE, color = 'lm_ffs')) + 
  geom_line(aes(y = cubist_full.RMSE, color = 'cubist_full')) + 
  geom_line(aes(y = ranger_full.RMSE, color = 'rf_full')) + 
  scale_color_manual(values = c("red", "orange", 'green', 'blue')) + 
  labs(title="Comparing Model RMSE of LA14 over week 2260",
       x ="Date", y = "RMSE") + 
  theme_minimal()
ggplot(data = d, aes(x = date)) + 
  geom_line(aes(y = lm_full.MAE, color = 'lm_full')) + 
  geom_line(aes(y = lm_ffs.MAE, color = 'lm_ffs')) + 
  geom_line(aes(y = cubist_full.MAE, color = 'cubist_full')) + 
  geom_line(aes(y = ranger_full.MAE, color = 'rf_full')) + 
  scale_color_manual(values = c("red", "orange", 'green', 'blue')) + 
  labs(title="Comparing Model MAE of LA14 over week 2260",
       x ="Date", y = "MAE") + 
  theme_minimal()
ggplot(data = d, aes(x = date)) + 
  geom_line(aes(y = lm_full.R2, color = 'lm_full')) + 
  geom_line(aes(y = lm_ffs.R2, color = 'lm_ffs')) + 
  geom_line(aes(y = cubist_full.R2, color = 'cubist_full')) + 
  geom_line(aes(y = ranger_full.R2, color = 'rf_full')) + 
  scale_color_manual(values = c("red", "orange", 'green', 'blue')) + 
  labs(title="Comparing Model R2 of LA14 over week 2260",
       x ="Date", y = "R2") + 
  theme_minimal()















# plot variable importance frequency 
# read the variable importances and attribute a rank to each variable from most to least important 
# then look at who has the highest number of who is most frequently in top ranks 

dfp <- data.frame()
f4_files <- list.files(run_filepath, pattern = glob2rx('3_f4_*_full_importances.csv'), full.names = T)
for(f in f4_files){
  a <- read.csv(f, header = F)
  a <- a[,2:ncol(a)]
  b <- data.frame(predictor = t(a[1,]), importances = t(a[2,]))
  names(b) <- c('predictors', 'importances')
  b <- b[order(as.numeric(b[,2]), decreasing = T),]
  dfp <- rbind(dfp, b$predictors)
}

t0 <- data.frame()
for(i in 1:ncol(dfp)){
  t <- data.frame(table(dfp[,i])*i)
  t0 <- rbind(t0, t)
}

# source: https://www.geeksforgeeks.org/how-to-use-aggregate-function-in-r/
t0a <- aggregate(t0$Freq, list(t0$Var1), FUN = sum)
names(t0a) <- c('variable', 'redundancy')
t0a <- t0a[order(t0a$redundancy, decreasing = F),]

# plot
# source: https://stackoverflow.com/questions/49105358/plot-feature-importance-computed-by-ranger-function
ggplot(t0a, aes(x = reorder(variable, redundancy), y = redundancy, fill = redundancy)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  ylab("Redundancy mark") +
  xlab("") +
  ggtitle("Variable redundancy for all models") +
  guides(fill = "none") +
  scale_fill_gradient(low = "blue", high = "red")













# look at VIF  
# library(car)
# a <- readRDS(paste0(run_filepath, '3_f1_2013-04-18_lm_full_model.RDS'))
# va <- vif(a$finalModel)
# b <- readRDS(paste0(run_filepath, '3_f1_2013-04-19_lm_full_model.RDS'))
# vb <- vif(b$finalModel)
# a$finalModel$param
# b$finalModel
# a$control


vif_files_list <- list.files(run_filepath, pattern = glob2rx('3_*_ffs_vif.csv'), full.names = F)
v_file <- vif_files_list[14] # full = bands 1, 9, 10
v <- read.csv(paste0(run_filepath, v_file), header = F, row.names = 1)
rownames(v) <- c('1', '2')
v <- data.frame(Preds = t(v[1,]), VIF = t(v[2,]))
colnames(v) <- c('Preds', 'VIF')
v$VIF <- as.numeric(v$VIF)
d <- sub('3_f5_', '', sub('_lm_ffs_vif.csv', '', v_file))

#library("RColorBrewer")
ggplot(data = v, aes(x = Preds)) +
  geom_bar(aes(y = VIF, fill = VIF), stat = 'identity') +
  # source: https://stackoverflow.com/questions/57177608/how-to-add-dashed-horizontal-line-with-label-in-ggplot
  geom_hline(yintercept = 10, linetype = 'dashed') +
  # source: https://stackoverflow.com/questions/57119146/how-to-fix-continuous-value-supplied-to-discrete-scale-in-with-scale-color-bre
  # source: https://stackoverflow.com/questions/39625749/set-levels-in-scale-fill-distiller
  scale_fill_distiller(palette = "RdYlGn", trans = 'log10') +  #limits = c(0, 10)) +
  # source: https://stackoverflow.com/questions/25685185/limit-ggplot2-axes-without-removing-data-outside-limits-zoom
  # source: https://www.geeksforgeeks.org/zoom-into-ggplot2-plot-without-removing-data-in-r/
  #coord_flip(ylim = c(0, 5000)) + # for full model
  coord_flip(ylim = c(0, 150)) +  # for ffs model
  ggtitle('VIF of Predictors from FFS Linear Model', subtitle = d)


# 
# vif_ts <- data.frame()
# for(f in vif_files_list){
#   date <- sub('3_f5_', '', sub('_lm_full_vif.csv', '', f))
#   vif <- read.csv(paste0(run_filepath, f), row.names = 1)
#   entry <- data.frame(date = date, vif)
#   vif_ts <- rbind(vif_ts, entry)
# }
# 
# row.names(vif_ts) <- 1:nrow(vif_ts)
# 
# # plot VIF 
# # source: https://www.geeksforgeeks.org/draw-multiple-time-series-in-same-plot-in-r/
# if(!('reshape2') %in% installed.packages()){install.packages('reshape2')} # to plot timeseries
# library(reshape2)
# # Convert sample_data from wide form to long form
# vif_ts <- melt(vif_ts, id.vars = "date")
# ggplot(vif_ts, aes(x = date, y = value, group = variable)) + geom_line()
# # source: https://www.statology.org/r-geom_path-each-group-consists-of-only-one-observation/
# ggplot(vif_ts, aes(x = date, group = 1)) + geom_line(aes(y = B2.B1))
# 
# 
# vif_files_list_ffs <- list.files(run_filepath, pattern = glob2rx('3_*_ffs_vif.csv'), full.names = F)
# vif_ts_ffs <- data.frame()
# for(f in vif_files_list_ffs){
#   date <- sub('3_f5_', '', sub('_lm_ffs_vif.csv', '', f))
#   vif <- read.csv(paste0(run_filepath, f), row.names = 1)
#   entry <- data.frame(date = date, vif)
#   vif_ts_ffs <- rbind.fill(vif_ts_ffs, entry)
# }