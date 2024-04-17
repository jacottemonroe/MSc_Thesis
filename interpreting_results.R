## interpreting results 



# define name of run (downscaling, RQ2, specific week or elephant idk)
run_label <- '_RQ2_STS_phase2'


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
  step6_files <- c('6_b0_clr_50p_sd_model_random_path_custom_distr_scaled.RDS', '6_b0_glm_50p_sd_model_random_path_custom_distr_scaled.RDS', 
                   '6_b1_clr_50p_sd_coefs_random_path_custom_distr_scaled.csv', '6_b2_clr_50p_sd_tests_random_path_custom_distr_scaled.csv', 
                   '6_a1_correlation_matrix_random_path_custom_distr.png', '6_b3_glm_50p_sd_coefs_random_path_custom_distr_scaled.csv', 
                   '6_b4_glm_50p_sd_deviances_random_path_custom_distr_scaled.csv', '6_b5_glm_50p_sd_vif_random_path_custom_distr_scaled.csv')
  step7_files <- c('7_b1_50p_sd_plot_log_odds_random_path_custom_distr_scaled.png', '7_b2_50p_sd_plot_odd_ratios_random_path_custom_distr_scaled.png', 
                   '7_b3_50p_sd_plot_curve_random_path_custom_distr_scaled.png')
  
  # check if each folder has the correct files and mark the answer in table 
  if(all(step1_files %in% data_files)){step1 = T}else{step1 = F}
  if(all(step2_files %in% data_files)){step2 = T}else{step2 = F}
  if(all(step3_files %in% data_files) & length(list.files(paste0(data_path, step3_files))) > 1){step3 = T}else{step3 = F}
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

dfr <- df_progress$week[df_progress$step6 == F]
print(dfr)

rr <- run_settings[run_settings$week %in% dfr,]

write.csv(rr, 'data/run_settings_RQ2_rerun.csv')

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



# retrieve weeks that are true
# dfr <- df_progress$week[df_progress$step6 == T]
# print(dfr)
# 
# run_settings <- run_settings[run_settings$week %in% dfr,]

#modify run table to only include datasets that passed phase 1 successfully
a <- df_progress[df_progress$step3 == F, 1:2]
a$combo <- paste(a$ID, a$week, sep = '_')

run_settings$combo <- paste(run_settings$ID, run_settings$week, sep = '_')

run_settings <- run_settings[run_settings$combo %in% a$combo,]
run_settings <- run_settings[,1:5]

n <- data.frame(ID = unique(rr$ID), week = 2066, pseudo_abs_method = 'random_path_custom_distr', downscaling = 'NULL', downscaling_model = 'NULL')
run_settings <- rbind(run_settings, n)

write.csv(run_settings, 'data/run_settings_RQ2_STS_phase2.csv')
write.csv(run_settings, 'data/run_settings_RQ2_STS_phase1.csv')


############## CREATE SUMMARY TABLE ##################################


# create empty summary results table 
summary_results <- data.frame()

suffix <- '_scaled' 

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
  if(suffix == ''){
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
  }else{
    modis_label <- "MODIS 250m"
  }

  
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

elephant <- 'LA11'

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
png(paste0('output/timeseries_glm_coef', run_label, '_extended.png'))
ggplot(data = df_glm_coef[df_glm_coef$significance == 'sig',], aes(x = as.Date(date, tz = 'Africa/Maputo'), y = value, group = predictor, color = predictor)) + 
  geom_line() + geom_point(data = df_glm_coef[df_glm_coef$significance == 'sig',], aes(y = value), shape = 8) + 
  geom_hline(yintercept = 0) + 
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') + 
  xlab('Time') + ylab('Coefficient Value') + ggtitle(paste0('Timeseries of GLM predictors for ', elephant, ' from August 2009 to January 2012'))
dev.off()  

ggplot(data = df_glm_coef, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = value, group = predictor, color = predictor)) + 
  geom_line() + geom_point(data = df_glm_coef[df_glm_coef$significance == 'sig',], aes(y = value), shape = 8) + 
  geom_hline(yintercept = 0) + 
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') + 
  xlab('Time') + ylab('Coefficient Value') + ggtitle(paste0('Timeseries of GLM predictors for ', elephant, ' from August 2009 to January 2012'))

 




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

png(paste0('output/timeseries_clr_coef', run_label, '_extended.png'))
ggplot(data = df_clr_coef[df_clr_coef$significance == 'sig',], aes(x = as.Date(date, tz = 'Africa/Maputo'), y = value, group = predictor, color = predictor)) + 
  geom_line() + geom_point(data = df_clr_coef[df_clr_coef$significance == 'sig',], aes(y = value), shape = 8) + 
  geom_hline(yintercept = 0) + 
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') + 
  xlab('Time') + ylab('Coefficient Value') + ggtitle(paste('Timeseries of CLR predictors for', elephant, 'from August 2009 to January 2012'))
dev.off()

ggplot(data = df_clr_coef, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = value, group = predictor, color = predictor)) + 
  geom_line() + geom_point(data = df_clr_coef[df_clr_coef$significance == 'sig',], aes(y = value), shape = 8) + 
  geom_hline(yintercept = 0) + 
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') + 
  xlab('Time') + ylab('Coefficient Value') + ggtitle(paste('Timeseries of CLR predictors for', elephant, 'from August 2009 to January 2012'))






## timeseries for deviance
df_div <- df_deviance_vif
df_div$significance[grepl('%', df_div$significance)] <- 'sig'
df_div <- df_div[df_div$significance == 'sig', ]
# ggplot(data = df_div, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = deviance)) + 
#   geom_line() + geom_point(data = df_div[df_div$significance == 'sig',], aes(y = deviance), shape = 8) + 
#   geom_hline(yintercept = 0) + 
#   xlab('Time') + ylab('Deviance Value') + ggtitle('Timeseries of CLR Deviance for LA14 from August 2009 to July 2011')

png(paste0('output/timeseries_glm_deviance_improvement', run_label, '_extended.png'), width = 600)
ggplot(data = df_div, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = diff_deviance_to_null)) + 
  geom_line() + geom_point(data = df_div, aes(y = diff_deviance_to_null), shape = 8) + 
  geom_hline(yintercept = 0) + 
  xlab('Time') + ylab('Deviance Value') + ggtitle(paste('Timeseries of GLM Deviance Improvement for', elephant, 'from August 2009 to January 2012'))
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

png(paste0('output/timeseries_vif', run_label, '_extended.png'))
ggplot(data = df_vif, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = vif, group = predictor, color = predictor)) + 
  geom_line() + geom_point(data = df_vif[df_vif$significance == 'sig',], aes(y = vif), shape = 8) + 
  #geom_hline(yintercept = 0) + 
  geom_hline(yintercept = 5, linetype = 'dashed') +
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'fixed') + 
  xlab('Time') + ylab('Coefficient Value') + ggtitle(paste('Timeseries of VIF for', elephant, 'from August 2009 to January 2012'))
dev.off()

######################################### end RQ2 code 






###################### RQ1 long timeseries 

# create big run dataset that merges RQ2 long timeseries datasets 
# l <- list.files('data', pattern = glob2rx('run_settings_RQ2_LA*.csv'), full.names = T)
# run_table_RQ1 <- data.frame()
# for(item in l){
#   entry <- read.csv(item, row.names = 1)
#   run_table_RQ1 <- rbind(run_table_RQ1, entry)
# }

# retrieve all coef values for all 4 datasets and place in large table 

run_table_RQ1 <- data.frame()

for(ID in c('LA11', 'LA12', 'LA13', 'LA14')){
  
  # read run table 
  run_settings <- read.csv(paste0('data/run_settings_RQ2_', ID, '.csv'), row.names = 1)
  
  # read summary table 
  summary_results <- read.csv(paste0('output/summary_results_RQ2_', ID, '.csv'), row.names = 1)
  
  # retrieves summary results for only sig models GLM 
  srsig <- summary_results[summary_results$sub_glm_sig == T,] # these are the models i want to retrieve coefs for 
  
  # get weeks of sig models 
  sigw <- srsig$week
  print(length(sigw))
  
  # get run table for only weeks that are sig 
  rsig <- run_settings[run_settings$week %in% sigw,]
  
  run_table_RQ1 <- rbind(run_table_RQ1, rsig)
  
}

row.names(run_table_RQ1) <- 1:nrow(run_table_RQ1)

RQ1_glm_coef <- data.frame()

s <- ''
for(i in 1:nrow(run_table_RQ1)){
  
  # get run settings
  ID <- run_table_RQ1$ID[i]
  week <- run_table_RQ1$week[i]
  method <- run_table_RQ1$pseudo_abs_method[i]
  
  # define filepaths
  data_path <- paste0('data/', ID, '/', week, '/')
  output_path <- paste0('output/', ID, '/', week, '/')
  
  # retrieve date of week 
  dfile <- read.csv(paste0(data_path, '2_a1_step_extents_LUT_', method, '.csv'), row.names = 1)
  date <- dfile$start_date[1]
  
  # retrieve coefs glm 
  c <- read.csv(paste0(output_path, '6_b3_glm_50p_sd_coefs_random_path_custom_distr', s, '.csv'))
  c <- c[2:nrow(c),]
  
  # specify if sig or not with 90% confidence (so pvalue < 0.1)
  c$Pr...z..[as.numeric(c$Pr...z..) < 0.1] <- 'sig'
  c$Pr...z..[as.numeric(c$Pr...z..) >= 0.1] <- 'not sig'
  
  # create new entry with coefs and pvalues 
  entry <- data.frame(ID = ID, week = week, method = method, date = date, predictor = c[,1], value = c$Estimate, significance = c$Pr...z..)
  
  RQ1_glm_coef <- rbind(RQ1_glm_coef, entry)
}

RQ1_glm_coef_sig <- RQ1_glm_coef[RQ1_glm_coef$significance == 'sig',]
row.names(RQ1_glm_coef_sig) <- 1:nrow(RQ1_glm_coef_sig)


# manual modifications TEMPORARY
# RQ1_glm_coef_sig$value[352] <- -100
# RQ1_glm_coef_sig$value[343] <- -1000
# RQ1_glm_coef_sig$value[347] <- -1000
# RQ1_glm_coef_sig$value[353] <- 1000
# RQ1_glm_coef_sig$value[341] <- -3500

#RQ1_glm_coef_sig <- RQ1_glm_coef_sig[RQ1_glm_coef_sig$week < 2175,]
RQ1_glm_coef_sig$combo <- paste0(RQ1_glm_coef_sig$week, RQ1_glm_coef_sig$predictor, sep = '_')

ag <- aggregate(RQ1_glm_coef_sig$value, list(RQ1_glm_coef_sig$week, RQ1_glm_coef_sig$predictor), FUN = summary)
agx <- ag$x
agx_mean <- agx[,4]
agx_q1 <- agx[,2]
agx_q3 <- agx[,5]
agc <- paste0(ag$Group.1, ag$Group.2, sep = '_')

agdf <- data.frame(combo = agc, Q1 = agx_q1, Mean = agx_mean, Q3 = agx_q3)

# source: https://www.guru99.com/r-merge-data-frames.html
RQ1_glm_coef_sig <- merge(RQ1_glm_coef_sig, agdf, by.x = 'combo')

start_date <- min(RQ1_glm_coef_sig$date)
end_date <- max(RQ1_glm_coef_sig$date)
elephant <- '4 elephants'

#library(ggplot2)
# png(paste0('output/timeseries_glm_coef', '_RQ1_LTS', '_extended.png'))
# ggplot(data = RQ1_glm_coef_sig, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = value, group = predictor, color = ID)) + 
#   geom_line() + geom_point(data = RQ1_glm_coef_sig, aes(y = value), shape = 8) + 
#   geom_hline(yintercept = 0) + 
#   # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
#   facet_grid(vars(predictor), scale = 'free') + 
#   xlab('Time') + ylab('Coefficient Value') + ggtitle(paste0('Timeseries of GLM predictors for ', elephant, ' from August 2009 to January 2012'))
# dev.off()  

# ggplot(data = RQ1_glm_coef, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = value, group = predictor, color = ID)) + 
#   geom_line() + geom_point(data = RQ1_glm_coef[RQ1_glm_coef$significance == 'sig',], aes(y = value), shape = 8) + 
#   geom_hline(yintercept = 0) + 
#   # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
#   facet_grid(vars(predictor), scale = 'free') + 
#   xlab('Time') + ylab('Coefficient Value') + ggtitle(paste0('Timeseries of GLM predictors for ', elephant, ' from August 2009 to January 2012'))

ggplot(data = RQ1_glm_coef_sig, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = value, group = predictor)) + 
  #geom_point(data = RQ1_glm_coef_sig, aes(y = value), shape = 8, colour = 'black') + 
  # source: https://stackoverflow.com/questions/14704909/plotting-depth-range-in-time-series-using-ggplot
  geom_ribbon(aes(ymin = Q1, ymax = Q3), fill = 'grey') + 
  geom_line(aes(y = Mean), color = 'blue') +
  geom_hline(yintercept = 0, linetype = 'dashed') + 
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') + 
  xlab('Time') + ylab('Coefficient Value') + ggtitle(paste0('Averaged time-series of estimated model coefficients for ', elephant, ' from ', start_date, ' to ', end_date)) + 
  theme_minimal()








###################### RQ2 short TS

# create run table with only sig models 
RQ2_run_table <- data.frame()

# fill new run table with only runs that give sig models 
srsig <- summary_results[summary_results$sub_glm_sig == T,] # these are the models i want to retrieve coefs for 

# get ID and weeks of sig models 
srsig$combo <- paste(srsig$ID, srsig$week, sep = '_')

run_settings$combo <- paste(run_settings$ID, run_settings$week, sep = '_')

RQ2_run_table <- run_settings[run_settings$combo %in% srsig$combo,]
RQ2_run_table <- RQ2_run_table[,1:5]

row.names(RQ2_run_table) <- 1:nrow(RQ2_run_table)

RQ2_glm_coef <- data.frame()

for(i in 1:nrow(RQ2_run_table)){
  
  # get run settings
  ID <- RQ2_run_table$ID[i]
  week <- RQ2_run_table$week[i]
  method <- RQ2_run_table$pseudo_abs_method[i]
  
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
  
  RQ2_glm_coef <- rbind(RQ2_glm_coef, entry)
}

RQ2_glm_coef_sig <- RQ2_glm_coef[RQ2_glm_coef$significance == 'sig',]
start_date <- max(RQ2_glm_coef_sig$date)
end_date <- min(RQ2_glm_coef_sig$date)
elephant <- 'many elephants'

ggplot(data = RQ2_glm_coef_sig, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = value, group = predictor, color = ID)) + 
  geom_point(data = RQ2_glm_coef[RQ2_glm_coef$significance == 'sig',], aes(y = value), shape = 8) + 
  geom_hline(yintercept = 0) + 
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') + 
  xlab('Time') + ylab('Coefficient Value') + ggtitle(paste0('Timeseries of GLM predictors for ', elephant, ' from ', start_date, ' to ', end_date))


start_date <- max(RQ2_glm_coef$date)
end_date <- min(RQ2_glm_coef$date)
elephant <- 'many elephants'

RQ2_glm_coef$period[RQ2_glm_coef$week %in% seq(2065, 2069)] <- 'August'
RQ2_glm_coef$period[RQ2_glm_coef$week %in% seq(2073, 2077)] <- 'October'
RQ2_glm_coef$period[RQ2_glm_coef$week %in% seq(2085, 2089)] <- 'December'

ggplot(data = RQ2_glm_coef, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = value, group = predictor, color = ID)) + 
  geom_line() + geom_point(data = RQ2_glm_coef[RQ2_glm_coef$significance == 'sig',], aes(y = value), shape = 8) + 
  geom_hline(yintercept = 0) + 
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  # source: https://www.statology.org/ggplot-facet-order/
  facet_grid(c(predictor) ~ c(factor(period, levels=c('August', 'October', 'December'))), scale = 'free') + 
  xlab('Time') + ylab('Coefficient Value') + ggtitle(paste0('Timeseries of GLM predictors for ', elephant, ' from ', start_date, ' to ', end_date))

