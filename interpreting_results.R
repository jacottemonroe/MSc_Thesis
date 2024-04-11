## interpreting results 



# define name of run (downscaling, RQ2, specific week or elephant idk)
run_label <- '_RQ2_LA13'


################ CHECK RUN PROGRESS AND COMPLETION ####################


run_settings <- read.csv(paste0('data/run_settings', run_label, '.csv'), row.names = 1)

df_progress <- data.frame()

for(i in 1:nrow(run_settings)){
  
  # get run settings
  ID <- run_settings$ID[i]
  week <- run_settings$week[i]
  method <- run_settings$pseudo_abs_method[i]
  
  # define filepaths
  data_path <- paste0('data/', ID, '/', week, '/')
  output_path <- paste0('output/', ID, '/', week, '/')
  
  # get files for data and output paths
  data_files <- list.files(data_path)
  output_files <- list.files(output_path)
  
  # define files that should have for each step
  step1_files <- c('1_a1_elephant_full_track_xyt.RDS', '1_a2_elephant_track_xyt.RDS', '1_b1_all_steps_random_path_custom_distr.RDS')
  step2_files <- c('2_a1_step_extents_LUT_random_path_custom_distr.csv')
  step3_files <- c('3_a1_modis_images_random_path_custom_distr')
  step4_files <- c('4_a1_cov_resp_dataset_random_path_custom_distr.csv')
  step5_files <- c('5_a1_elephant_movement_map_random_path_custom_distr.png')
  step6_files <- c('6_b0_clr_50p_sd_model_random_path_custom_distr.RDS', '6_b0_glm_50p_sd_model_random_path_custom_distr.RDS', 
                   '6_b1_clr_50p_sd_coefs_random_path_custom_distr.csv', '6_b2_clr_50p_sd_tests_random_path_custom_distr.csv', 
                   '6_a1_correlation_matrix_random_path_custom_distr.png', '6_b3_glm_50p_sd_coefs_random_path_custom_distr.csv', 
                   '6_b4_glm_50p_sd_deviances_random_path_custom_distr.csv', '6_b5_glm_50p_sd_vif_random_path_custom_distr.csv')
  step7_files <- c('7_b1_50p_sd_plot_log_odds_random_path_custom_distr.png', '7_b2_50p_sd_plot_odd_ratios_random_path_custom_distr.png', 
                   '7_b3_50p_sd_plot_curve_random_path_custom_distr.png')
  
  # check if each folder has the correct files and mark the answer in table 
  if(all(step1_files %in% data_files)){step1 = T}else{step1 = F}
  if(all(step2_files %in% data_files)){step2 = T}else{step2 = F}
  if(all(step3_files %in% data_files) & length(list.files(paste0(data_path, step3_files))) == 15){step3 = T}else{step3 = F}
  if(all(step4_files %in% data_files)){step4 = T}else{step4 = F}
  if(all(step5_files %in% output_files)){step5 = T}else{step5 = F}
  if(all(step6_files %in% output_files)){step6 = T}else{step6 = F}
  if(all(step7_files %in% output_files)){step7 = T}else{step7 = F}
  
  # check if all steps complete 
  if(all(c(step1, step2, step3, step4, step5, step6, step7) == T)){complete = T}else{complete = F}
  
  # fill entry 
  entry <- data.frame(ID = ID, week = week, method = method, step1 = step1, step2 = step2, step3 = step3, step4 = step4, step5 = step5, step6 = step6, step7 = step7, complete = complete)
  
  df_progress <- rbind(df_progress, entry)
  
}

# dfr <- df$week[df$step3 == F]
# print(dfr)

# rr <- r[r$week %in% dfr,]
# 
# write.csv(rr, 'data/run_settings_RQ2_rerun.csv')

# 
# s <- readRDS('data/LA14/2112/1_b1_all_steps_random_path_custom_distr.RDS')
# ss <- readRDS('data/LA14/2112/1_a2_elephant_track_xyt.RDS')
# 
# 
# 
# r <- r[2:nrow(r),]
# rownames(r) <- 1:nrow(r)
# write.csv(r, 'data/run_settings_RQ2.csv')
# r <- read.csv('data/run_settings_RQ2.csv', row.names = 1)
# r$X <- NULL








############## CREATE SUMMARY TABLE ##################################


# create empty summary results table 
summary_results <- data.frame()

suffix <- '' 

# loop over each run 
for(i in 1:nrow(run_settings)){
  # retrieve the run
  run <- run_settings[i,]
  
  # define elephant ID
  ID <- run[[1]]
  
  # define week to test 
  week <- run[[2]]
  
  # define pseudo-absence path generator method --> Choices are 1) random_path_custom_distr 2) random_path_buffer_point 3) random_step
  pseudo_abs_method <- run[[3]]
  
  # define run filepath 
  run_filepath <- paste0('output/', ID, '/', week, '/')
  
  # create results data entry 
  entry <- data.frame(ID = ID, week = week, pseudo_abs_method = pseudo_abs_method, 
                      VIF_full = NA, full_glm_sig_coef = NA, full_glm_sig_coef_which = NA, full_glm_deviance = NA, full_glm_sig = NA, full_clr_sig_coef = NA, full_clr_sig_coef_which = NA, full_clr_concord = NA, full_clr_concord_se = NA, 
                      VIF_sub = NA, sub_glm_sig_coef = NA, sub_glm_sig_coef_which = NA, sub_glm_deviance = NA, sub_glm_sig = NA, sub_clr_sig_coef = NA, sub_clr_sig_coef_which = NA, sub_clr_concord = NA, sub_clr_concord_se = NA)
  
  
  ## are the VIF values of the model acceptable? 
  # retrieve the dataset
  #full_df <- read.csv(paste0(run_filepath, '6_a5_glm_full_vif_', pseudo_abs_method, '.csv'))
  sub_df <- read.csv(paste0(run_filepath, '6_b5_glm_50p_sd_vif_', pseudo_abs_method, suffix, '.csv'))
  
  # check if vif values below 5
  #if(any(full_df$vif_results <5)){entry$VIF_full <- T}else{entry$VIF_full <- F}
  if(any(sub_df$vif_results <5)){entry$VIF_sub <- T}else{entry$VIF_sub <- F}
  
  
  ## does the glm model have a significant coefficient? 
  # retrieve dataset
  #full_df <- read.csv(paste0(run_filepath, '6_a3_glm_full_coefs_', pseudo_abs_method, '.csv'))
  sub_df <- read.csv(paste0(run_filepath, '6_b3_glm_50p_sd_coefs_', pseudo_abs_method, suffix, '.csv'))
  
  # exclude intercept 
  #full_df <- full_df[full_df$X != '(Intercept)', ]
  sub_df <- sub_df[sub_df$X != '(Intercept)', ]
  
  # check if pvalues below 0.05
  #if(any(full_df$Pr...z.. <=0.05)){entry$full_glm_sig_coef <- T}else{entry$full_glm_sig_coef <- F}
  if(any(sub_df$Pr...z.. <=0.05)){entry$sub_glm_sig_coef <- T}else{entry$sub_glm_sig_coef <- F}
  
  # select predictors that are significant with 95% confidence 
  # source: https://sparkbyexamples.com/r-programming/r-merge-vector-to-string/
  #entry$full_glm_sig_coef_which <- paste(full_df$X[full_df$Pr...z.. < 0.05], collapse = '; ')
  entry$sub_glm_sig_coef_which <- paste(sub_df$X[sub_df$Pr...z.. < 0.05], collapse = '; ')

  ## how high is the deviance of the fitted model? is the model significant? 
  # retrieve dataset
  #full_df <- read.csv(paste0(run_filepath, '6_a4_glm_full_deviances_', pseudo_abs_method, '.csv'))
  sub_df <- read.csv(paste0(run_filepath, '6_b4_glm_50p_sd_deviances_', pseudo_abs_method, suffix, '.csv'))
  
  # select residual deviance 
  #entry$full_glm_deviance <- full_df$residual_deviance
  entry$sub_glm_deviance <- sub_df$residual_deviance
  
  # derive p-value of model from chi-square stat
  # source: https://stats.stackexchange.com/questions/340489/interpretation-of-deviance-in-logistic-model
  # source: https://www.r-bloggers.com/2022/05/calculate-the-p-value-from-chi-square-statistic-in-r/
  #full_pval <- pchisq(full_df$null_deviance - full_df$residual_deviance, full_df$null_df - full_df$residual_df, lower.tail = F)
  sub_pval <- pchisq(sub_df$null_deviance - sub_df$residual_deviance, sub_df$null_df - sub_df$residual_df, lower.tail = F)
  
  # check if pvalues below 0.05
  #if(full_pval <=0.05){entry$full_glm_sig <- T}else{entry$full_glm_sig <- F}
  if(sub_pval <=0.05){entry$sub_glm_sig <- T}else{entry$sub_glm_sig <- F}
  
  
  ## does the clr model have a significant coefficient? 
  # retrieve dataset
  #full_df <- read.csv(paste0(run_filepath, '6_a1_clr_full_coefs_', pseudo_abs_method, '.csv'))
  sub_df <- read.csv(paste0(run_filepath, '6_b1_clr_50p_sd_coefs_', pseudo_abs_method, suffix, '.csv'))
  
  # check if pvalues below 0.05
  #if(any(full_df$Pr...z.. <=0.05)){entry$full_clr_sig_coef <- T}else{entry$full_clr_sig_coef <- F}
  if(any(sub_df$Pr...z.. <=0.05)){entry$sub_clr_sig_coef <- T}else{entry$sub_clr_sig_coef <- F}
  
  # select predictors that are significant with 95% confidence 
  # source: https://sparkbyexamples.com/r-programming/r-merge-vector-to-string/
  #entry$full_clr_sig_coef_which <- paste(full_df$X[full_df$Pr...z.. < 0.05], collapse = '; ')
  entry$sub_clr_sig_coef_which <- paste(sub_df$X[sub_df$Pr...z.. < 0.05], collapse = '; ')

  ## what is the concordance? how much does it vary by? 
  # retrieve dataset
  #full_df <- read.csv(paste0(run_filepath, '6_a2_clr_full_tests_', pseudo_abs_method, '.csv'))
  sub_df <- read.csv(paste0(run_filepath, '6_b2_clr_50p_sd_tests_', pseudo_abs_method, suffix, '.csv'))
  
  # select concordance and SE
  # entry$full_clr_concord <- full_df$concordance[1]
  # entry$full_clr_concord_se <- full_df$concordance[4]
  entry$sub_clr_concord <- sub_df$concordance[1]
  entry$sub_clr_concord_se <- sub_df$concordance[4]
  
  # rbind the entry to the summary table 
  summary_results <- rbind(summary_results, entry)
}

# results for running 13 elephants on week 2075 with three methods 
write.csv(summary_results, paste0('output/summary_results', run_label, '.csv'))









############### CREATE SUMMARY TABLE OF GLM SIGNIFICANCE AND DEVIANCE AND VIF

df_deviance_vif <- data.frame()
for(r in 1:nrow(run_settings)){
  # get settings
  ID <- run_settings$ID[r]
  week <- run_settings$week[r]
  pseudo_abs_method <- run_settings$pseudo_abs_method[r]
  downscaling_setting <- run_settings$downscaling[r]
  
  # get model specifications
  if(downscaling_setting == 'NULL'){
    suffix <- ''
    modis_label <- 'MODIS 250m'
    
  }else if(downscaling_setting == T){
    suffix <- '_downscaling_modis_30m'
    modis_label <- 'MODIS 30m (D)'
    
  }else if(downscaling_setting == F){
    suffix <- '_downscaling_modis_250m'
    modis_label <- 'MODIS 250m (D)'
    
  }else{stop('Incorrect term set for downscaling parameter. Should be one of the following: NULL, T, F.')}
  
  # load dataset 
  dev_df <- read.csv(paste0('output/', ID, '/', week, '/6_b4_glm_50p_sd_deviances_', pseudo_abs_method, suffix, '.csv'))
  vif_df <- read.csv(paste0('output/', ID, '/', week, '/6_b5_glm_50p_sd_vif_', pseudo_abs_method, suffix, '.csv'), row.names = 1, header = T)
  
  # retrieve the difference in deviance and pvalue for the model 
  diff_div <- dev_df$null_deviance - dev_df$residual_deviance
  pval <- pchisq(dev_df$null_deviance - dev_df$residual_deviance, dev_df$null_df - dev_df$residual_df, lower.tail = F)
  
  if(pval < 0.001){sig = '99.9%'}else if(pval >= 0.001 & pval <0.01){sig = '99%'}else if(pval >= 0.01 & pval < 0.05){sig = '95%'}else if(pval >= 0.05 & pval < 0.1){sig = '90%'}else if(pval >= 0.1 & pval < 1){sig = 'not sig'}else{sig = '?'}
  
  # retrieve date of week 
  dfile <- read.csv(paste0('data/', ID, '/', week, '/2_a1_step_extents_LUT_', method, '.csv'), row.names = 1)
  date <- dfile$start_date[1]
  
  # compile information into data frame
  entry <- data.frame(ID = ID, week = week, date = date, method = pseudo_abs_method, modis = modis_label, 
                      vif_ndvi_50 = vif_df$vif_results[1], vif_ndvi_sd = vif_df$vif_results[2], 
                      vif_ndvi_rate_50 = vif_df$vif_results[3], vif_ndvi_srate_d = vif_df$vif_results[4], 
                      deviance = dev_df$residual_deviance, diff_deviance_to_null = diff_div, p_value = pval, significance = sig)
  
  df_deviance_vif <- rbind(df_deviance_vif, entry)
}

write.csv(df_deviance_vif, file = paste0('output/summary_deviances_vif', run_label, '.csv'))


















##### FOR RQ2 TIMESERIES

# retrieves summary results for only sig models GLM 
srsig <- summary_results[summary_results$sub_glm_sig == T,] # these are the models i want to retrieve coefs for 

# get weeks of sig models 
sigw <- srsig$week

# get run table for only weeks that are sig 
rsig <- run_settings[run_settings$week %in% sigw,]

df_glm_coef <- data.frame()

for(i in 1:nrow(rsig)){
  
  # get run settings
  ID <- rsig$ID[i]
  week <- rsig$week[i]
  method <- rsig$pseudo_abs_method[i]
  
  # define filepaths
  data_path <- paste0('data/', ID, '/', week, '/')
  output_path <- paste0('output/', ID, '/', week, '/')
  
  # retrieve date of week 
  dfile <- read.csv(paste0(data_path, '2_a1_step_extents_LUT_', method, '.csv'), row.names = 1)
  date <- dfile$start_date[1]
  
  # retrieve coefs glm 
  c <- read.csv(paste0(output_path, '6_b3_glm_50p_sd_coefs_random_path_custom_distr', suffix, '.csv'))
  c <- c[2:nrow(c),]
  
  # specify if sig or not with 90% confidence (so pvalue < 0.1)
  c$Pr...z..[as.numeric(c$Pr...z..) < 0.1] <- 'sig'
  c$Pr...z..[as.numeric(c$Pr...z..) >= 0.1] <- 'not sig'
  
  # create new entry with coefs and pvalues 
  entry <- data.frame(ID = ID, week = week, method = method, date = date, predictor = c[,1], value = c$Estimate, significance = c$Pr...z..)
  
  df_glm_coef <- rbind(df_glm_coef, entry)
}


#library(ggplot2)
png(paste0('output/timeseries_glm_coef', run_label, '.png'))
ggplot(data = df_glm_coef, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = value, group = predictor, color = predictor)) + 
  geom_line() + geom_point(data = df_glm_coef[df_glm_coef$significance == 'sig',], aes(y = value), shape = 8) + 
  geom_hline(yintercept = 0) + 
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') + 
  xlab('Time') + ylab('Coefficient Value') + ggtitle('Timeseries of GLM predictors for LA14 from August 2009 to July 2011')
dev.off()  
 




## just testing for CLR instead of GLM
df_clr_coef <- data.frame()

for(i in 1:nrow(run_settings)){
  
  # get run settings
  ID <- run_settings$ID[i]
  week <- run_settings$week[i]
  method <- run_settings$pseudo_abs_method[i]
  
  # define filepaths
  data_path <- paste0('data/', ID, '/', week, '/')
  output_path <- paste0('output/', ID, '/', week, '/')
  
  # retrieve date of week 
  dfile <- read.csv(paste0(data_path, '2_a1_step_extents_LUT_', method, '.csv'), row.names = 1)
  date <- dfile$start_date[1]
  
  # retrieve coefs glm 
  c <- read.csv(paste0(output_path, '6_b1_clr_50p_sd_coefs_random_path_custom_distr', suffix, '.csv'))
  
  # specify if sig or not with 90% confidence (so pvalue < 0.1)
  c$Pr...z..[as.numeric(c$Pr...z..) < 0.1] <- 'sig'
  c$Pr...z..[as.numeric(c$Pr...z..) >= 0.1] <- 'not sig'
  
  # create new entry with coefs and pvalues 
  # entry <- data.frame(ID = ID, week = week, method = method, date = date, ndvi_50 = c$Estimate[1], 
  #                     ndvi_sd = c$Estimate[2], ndvi_rate_50 = c$Estimate[3], ndvi_rate_sd = c$Estimate[4], 
  #                     pval.ndvi_50 = c$Pr...z..[1], pval.ndvi_sd = c$Pr...z..[2], pval.ndvi_rate_50 = c$Pr...z..[3], pval.ndvi_rate_sd = c$Pr...z..[4])
  entry <- data.frame(ID = ID, week = week, method = method, date = date, predictor = c[,1], value = c$coef, significance = c$Pr...z..)
  
  df_clr_coef <- rbind(df_clr_coef, entry)
}

png(paste0('output/timeseries_clr_coef', run_label, '.png'))
ggplot(data = df_clr_coef, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = value, group = predictor, color = predictor)) + 
  geom_line() + geom_point(data = df_clr_coef[df_clr_coef$significance == 'sig',], aes(y = value), shape = 8) + 
  geom_hline(yintercept = 0) + 
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') + 
  xlab('Time') + ylab('Coefficient Value') + ggtitle('Timeseries of CLR predictors for LA14 from August 2009 to July 2011')
dev.off()






## timeseries for deviance
df_div <- df_deviance_vif
df_div$significance[grepl('%', df_div$significance)] <- 'sig'
df_div <- df_div[df_div$significance == 'sig', ]
# ggplot(data = df_div, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = deviance)) + 
#   geom_line() + geom_point(data = df_div[df_div$significance == 'sig',], aes(y = deviance), shape = 8) + 
#   geom_hline(yintercept = 0) + 
#   xlab('Time') + ylab('Deviance Value') + ggtitle('Timeseries of CLR Deviance for LA14 from August 2009 to July 2011')

png(paste0('output/timeseries_glm_deviance_improvement', run_label, '.png'), width = 600)
ggplot(data = df_div, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = diff_deviance_to_null)) + 
  geom_line() + geom_point(data = df_div, aes(y = diff_deviance_to_null), shape = 8) + 
  geom_hline(yintercept = 0) + 
  xlab('Time') + ylab('Deviance Value') + ggtitle('Timeseries of CLR Deviance Improvement for LA14 from August 2009 to July 2011')
dev.off()


# timseries for vif 
df_vif <- data.frame()

for(r in 1:nrow(run_settings)){
  # get settings
  ID <- run_settings$ID[r]
  week <- run_settings$week[r]
  pseudo_abs_method <- run_settings$pseudo_abs_method[r]
  downscaling_setting <- run_settings$downscaling[r]
  
  # get model specifications
  if(downscaling_setting == 'NULL'){
    suffix <- ''
    modis_label <- 'MODIS 250m'
    
  }else if(downscaling_setting == T){
    suffix <- '_downscaling_modis_30m'
    modis_label <- 'MODIS 30m (D)'
    
  }else if(downscaling_setting == F){
    suffix <- '_downscaling_modis_250m'
    modis_label <- 'MODIS 250m (D)'
    
  }else{stop('Incorrect term set for downscaling parameter. Should be one of the following: NULL, T, F.')}
  
  # load dataset 
  dev_df <- read.csv(paste0('output/', ID, '/', week, '/6_b4_glm_50p_sd_deviances_', pseudo_abs_method, suffix, '.csv'))
  vif_df <- read.csv(paste0('output/', ID, '/', week, '/6_b5_glm_50p_sd_vif_', pseudo_abs_method, suffix, '.csv'), header = T)
  
  # retrieve the difference in deviance and pvalue for the model 
  pval <- pchisq(dev_df$null_deviance - dev_df$residual_deviance, dev_df$null_df - dev_df$residual_df, lower.tail = F)
  
  if(pval < 0.1){sig = 'sig'}else if(pval >= 0.1 & pval < 1){sig = 'not sig'}else{sig = '?'}
  
  # retrieve date of week 
  dfile <- read.csv(paste0('data/', ID, '/', week, '/2_a1_step_extents_LUT_', method, '.csv'), row.names = 1)
  date <- dfile$start_date[1]
  
  # compile information into data frame
  entry <- data.frame(ID = ID, week = week, date = date, method = pseudo_abs_method, 
                      modis = modis_label, predictor = vif_df[,1], vif = vif_df$vif_results, significance = sig)
  
  df_vif <- rbind(df_vif, entry)
}

png(paste0('output/timeseries_vif', run_label, '.png'))
ggplot(data = df_vif, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = vif, group = predictor, color = predictor)) + 
  geom_line() + geom_point(data = df_vif[df_vif$significance == 'sig',], aes(y = vif), shape = 8) + 
  #geom_hline(yintercept = 0) + 
  geom_hline(yintercept = 5, linetype = 'dashed') +
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'fixed') + 
  xlab('Time') + ylab('Coefficient Value') + ggtitle('Timeseries of VIF for LA14 from August 2009 to July 2011')
dev.off()

######################################### end RQ2 code 






