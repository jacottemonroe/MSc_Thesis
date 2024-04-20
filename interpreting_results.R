## interpreting results 



# define name of run (downscaling, RQ2, specific week or elephant idk)
run_label <- '_LA14_LTS' #'_LA14_LTS_rerun'  #'_LA14_LTS_full'


################ CHECK RUN PROGRESS AND COMPLETION ####################


run_settings <- read.csv(paste0('data/run_settings', run_label, '.csv'), row.names = 1)
# 
# id <- unique(run_settings$ID)
# t <- data.frame()
# for(i in id){
#   entry <- data.frame(ID = i, week = seq(2065, 2089), pseudo_abs_method = run_settings$pseudo_abs_method[1], downscaling = 'NULL', downscaling_model = 'NULL')
#   t <- rbind(t, entry)
# }
# run_settings <- t

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
  if(all(step3_files %in% data_files) & length(list.files(paste0(data_path, step3_files))) > 6){step3 = T}else{step3 = F}
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

# dfr <- df_progress$week[df_progress$step3 == F]
# print(dfr)
# 
# rr <- run_settings[run_settings$week %in% dfr,]
# 
# write.csv(rr, 'data/run_settings_LA14_LTS_rerun2.csv')


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



# retrieve weeks that are true for single elephant dataset
dfr <- df_progress$week[df_progress$step6 == T]
print(dfr)

run_settings <- run_settings[run_settings$week %in% dfr,]


#modify run table to only include datasets that passed phase 1 successfully
#a <- df_progress[df_progress$step2 == T & df_progress$step4 == F, 1:2]

# to select when have multiple elephants 
# a <- df_progress[df_progress$step6 == T, 1:2]
# a$combo <- paste(a$ID, a$week, sep = '_')
# 
# run_settings$combo <- paste(run_settings$ID, run_settings$week, sep = '_')
# 
# run_settings <- run_settings[run_settings$combo %in% a$combo,]
# run_settings <- run_settings[,1:5]




# 
# n <- data.frame(ID = unique(rr$ID), week = 2066, pseudo_abs_method = 'random_path_custom_distr', downscaling = 'NULL', downscaling_model = 'NULL')
# run_settings <- rbind(run_settings, n)

# write.csv(run_settings, 'data/run_settings_RQ2_STS_phase2.csv')
# write.csv(run_settings, 'data/run_settings_RQ2_STS_phase1.csv')
# write.csv(run_settings, 'data/run_settings_RQ2_STS_phaseJN.csv')
# 
# write.csv(run_settings, 'data/run_settings_RQ2_rerun.csv')
# 
# unique(run_settings$ID)
# unique(run_settings$week)
# 
# sts <- read.csv('data/run_settings_RQ2_STS.csv', row.names = 1)
# run_settings <- rbind(sts, run_settings)
# run_settings$combo <- paste(run_settings$ID, run_settings$week, sep = '_')
# run_settings <- run_settings[!duplicated(run_settings),]

#write.csv(run_settings, 'data/run_settings_STS.csv')




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
  
  # get date 
  dfile <- read.csv(paste0('data/', ID, '/', week, '/2_a1_step_extents_LUT_', pseudo_abs_method, '.csv'), row.names = 1)
  date <- dfile$start_date[1]
  
  # create results data entry 
  entry <- data.frame(ID = ID, week = week, date = date, pseudo_abs_method = pseudo_abs_method, 
                      VIF_full = NA, full_glm_sig_coef = NA, full_glm_sig_coef_which = NA, full_glm_deviance = NA, full_glm_sig = NA, full_clr_sig_coef = NA, full_clr_sig_coef_which = NA, full_clr_concord = NA, full_clr_concord_se = NA, 
                      VIF_sub = NA, sub_glm_sig_coef = NA, sub_glm_sig_coef_which = NA, sub_glm_deviance = NA, sub_glm_sig = NA, sub_clr_sig_coef = NA, sub_clr_sig_coef_which = NA, sub_clr_concord = NA, sub_clr_concord_se = NA)
  
  
  ## are the VIF values of the model acceptable? 
  # retrieve the dataset
  #full_df <- read.csv(paste0(run_filepath, '6_a5_glm_full_vif_', pseudo_abs_method, '.csv'))
  sub_df <- read.csv(paste0(run_filepath, '6_b5_glm_50p_sd_vif_', pseudo_abs_method, suffix, '.csv'))
  
  # check if vif values below 5
  #if(any(full_df$vif_results <5)){entry$VIF_full <- T}else{entry$VIF_full <- F}
  if(any(sub_df$vif_results >5)){entry$VIF_sub <- F}else{entry$VIF_sub <- T}
  
  
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

summary_results <- summary_results[,grep('full', names(summary_results), invert = T)]

# results for running 13 elephants on week 2075 with three methods 
write.csv(summary_results, paste0('output/summary_results', run_label, '.csv'))


# summary table of summary table per elephant
sst_e <- data.frame()
for(i in unique(summary_results$ID)){
  t <- summary_results[summary_results$ID == i,]
  v <- t[t$VIF_sub == F,]
  vsig <- v[v$sub_glm_sig == T,]
  tsig <- t[t$sub_glm_sig == T,]
  entry <- data.frame(ID_week = i, total_models = nrow(t), large_VIF_models = nrow(v), large_VIF_sig_models = nrow(vsig), sig_models = nrow(tsig), 
                      percent_large_VIF_models = round((nrow(v)/nrow(t))*100, 2), percent_large_VIF_sig_models = round((nrow(vsig)/nrow(v))*100, 2), 
                      percent_sig_models = round((nrow(tsig)/nrow(t))*100, 2))
  sst_e <- rbind(sst_e, entry)
}

sst_w <- data.frame()
for(i in unique(summary_results$week)){
  t <- summary_results[summary_results$week == i,]
  v <- t[t$VIF_sub == F,]
  vsig <- v[v$sub_glm_sig == T,]
  tsig <- t[t$sub_glm_sig == T,]
  entry <- data.frame(ID_week = i, total_models = nrow(t), large_VIF_models = nrow(v), large_VIF_sig_models = nrow(vsig), sig_models = nrow(tsig), 
                      percent_large_VIF_models = round((nrow(v)/nrow(t))*100, 2), percent_large_VIF_sig_models = round((nrow(vsig)/nrow(v))*100, 2), 
                      percent_sig_models = round((nrow(tsig)/nrow(t))*100, 2))
  sst_w <- rbind(sst_w, entry)
}

summary(sst_e$percent_large_VIF_models)
summary(sst_e$percent_large_VIF_sig_models)
summary(sst_e$percent_sig_models)

summary(sst_w$percent_large_VIF_models)
summary(sst_w$percent_large_VIF_sig_models)
summary(sst_w$percent_sig_models)


# set fixed starting date in summary table (for plotting purposes) manual fix!! not replicable on other datasets
# summary_results$date[summary_results$week == 2065] <- min(summary_results$date)
# summary_results$date[summary_results$week == 2074] <- summary_results$date[129]
# summary_results$date[summary_results$week == 2080] <- summary_results$date[16]
# summary_results$date[summary_results$week == 2082] <- summary_results$date[18]
# summary_results$date[summary_results$week == 2084] <- summary_results$date[20]
# summary_results$date[summary_results$week == 2085] <- summary_results$date[21]


d <- data.frame(date = unique(summary_results$date))
# source: https://stackoverflow.com/questions/22603847/how-to-extract-month-from-date-in-r
d$season[strftime(as.Date(d$date, tz = 'Africa/Maputo'), '%m') %in% c('11', '12', '01', '02', '03')] <- 'wet'
d$season[strftime(as.Date(d$date, tz = 'Africa/Maputo'), '%m') %in% c('05', '06', '07', '08', '09')] <- 'dry'
d$season[strftime(as.Date(d$date, tz = 'Africa/Maputo'), '%m') %in% c('10', '04')] <- 'transition'

sst_w$date <- d$date
sst_w$season <- d$season

plot(as.Date(d, tz = 'Africa/Maputo'), sst_w$percent_sig_models)

ggplot(data = sst_w, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
  # source: https://stackoverflow.com/questions/33322061/change-background-color-panel-based-on-year-in-ggplot-r
  geom_rect(aes(xmin = as.Date(min(sst_w$date[sst_w$season == 'dry']), tz = 'Africa/Maputo'), 
                xmax = as.Date(max(sst_w$date[sst_w$season == 'dry']), tz = 'Africa/Maputo'), 
                ymin = -Inf, ymax = Inf, fill = 'dry'), alpha = .02) + 
  geom_rect(aes(xmin = as.Date(max(sst_w$date[sst_w$season == 'dry']), tz = 'Africa/Maputo'), 
                xmax = as.Date(min(sst_w$date[sst_w$season == 'wet']), tz = 'Africa/Maputo'), 
                ymin = -Inf, ymax = Inf, fill = 'transition'), alpha = .02) + 
  geom_rect(aes(xmin = as.Date(min(sst_w$date[sst_w$season == 'wet']), tz = 'Africa/Maputo'), 
                xmax = as.Date(max(sst_w$date[sst_w$season == 'wet']), tz = 'Africa/Maputo'), 
                ymin = -Inf, ymax = Inf, fill = 'wet'), alpha = .02) + 
  # source: https://www.statology.org/r-geom_path-each-group-consists-of-only-one-observation/
  geom_line(aes(y = percent_sig_models, group = 1, linetype = 'Significant Models')) + 
  geom_line(aes(y = percent_large_VIF_models, group = 1, linetype = 'Models with VIF > 5')) + 
  scale_fill_manual(name = 'Season', values = c('red', 'orange', 'green'), 
                    # source: https://stackoverflow.com/questions/34772911/alpha-does-not-change-transparency-but-adds-to-ggplot2-legend-with-geom-rect
                    guide = guide_legend(override.aes = list(alpha = .15))) + 
  # source: https://stackoverflow.com/questions/40791082/ggplot2-control-linetypes-when-more-than-one-line
  scale_linetype_manual(values = c(1,2), name = 'Model Type', breaks = c('Significant Models', 'Models with VIF > 5')) + 
  theme_minimal() + 
  xlab('Time') + ylab('Proportion of Models') + ggtitle('Proportion of GLM Models per Week over Time')
  










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

elephant <- 'LA14'

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

start_date <- min(df_glm_coef$date)
end_date <- max(df_glm_coef$date)

#library(ggplot2)
png(paste0('output/timeseries_glm_coef', run_label, '_rescaled.png'))
ggplot(data = df_glm_coef, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = value, group = predictor, color = predictor)) + 
  geom_line() + geom_point(data = df_glm_coef[df_glm_coef$significance == 'sig',], aes(y = value), shape = 8) + 
  geom_hline(yintercept = 0) + 
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') + 
  xlab('Time') + ylab('Coefficient Value') + ggtitle(paste0('Timeseries of GLM predictors for ', elephant, ' from ', start_date, ' to ', end_date))
dev.off()  

ggplot(data = df_glm_coef[df_glm_coef$significance == 'sig',], aes(x = as.Date(date, tz = 'Africa/Maputo'), y = value, group = predictor, color = predictor)) + 
  geom_line() + geom_point(data = df_glm_coef[df_glm_coef$significance == 'sig',], aes(y = value), shape = 8) + 
  geom_hline(yintercept = 0) + 
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') + 
  xlab('Time') + ylab('Coefficient Value') + ggtitle(paste0('Timeseries of GLM predictors for ', elephant, ' from ', start_date, ' to ', end_date))





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

start_date <- min(df_clr_coef$date)
end_date <- max(df_clr_coef$date)

png(paste0('output/timeseries_clr_coef', run_label, '_rescaled.png'))
ggplot(data = df_clr_coef, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = value, group = predictor, color = predictor)) + 
  geom_line() + geom_point(data = df_clr_coef[df_clr_coef$significance == 'sig',], aes(y = value), shape = 8) + 
  geom_hline(yintercept = 0) + 
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') + 
  xlab('Time') + ylab('Coefficient Value') + ggtitle(paste('Timeseries of CLR predictors for', elephant, 'from', start_date, 'to', end_date))
dev.off()

ggplot(data = df_clr_coef[df_clr_coef$significance == 'sig',], aes(x = as.Date(date, tz = 'Africa/Maputo'), y = value, group = predictor, color = predictor)) + 
  geom_line() + geom_point(data = df_clr_coef[df_clr_coef$significance == 'sig',], aes(y = value), shape = 8) + 
  geom_hline(yintercept = 0) + 
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') + 
  xlab('Time') + ylab('Coefficient Value') + ggtitle(paste('Timeseries of CLR predictors for', elephant, 'from', start_date, 'to', end_date))






## timeseries for deviance
df_div <- df_deviance_vif
df_div$significance[grepl('%', df_div$significance)] <- 'sig'
df_div <- df_div[df_div$significance == 'sig', ]

start_date <- min(df_clr_coef$date)
end_date <- max(df_clr_coef$date)

# ggplot(data = df_div, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = deviance)) + 
#   geom_line() + geom_point(data = df_div[df_div$significance == 'sig',], aes(y = deviance), shape = 8) + 
#   geom_hline(yintercept = 0) + 
#   xlab('Time') + ylab('Deviance Value') + ggtitle('Timeseries of CLR Deviance for LA14 from August 2009 to July 2011')

png(paste0('output/timeseries_glm_deviance_improvement', run_label, '_rescaled.png'), width = 600)
ggplot(data = df_div, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = diff_deviance_to_null)) + 
  geom_line() + geom_point(data = df_div, aes(y = diff_deviance_to_null), shape = 8) + 
  geom_hline(yintercept = 0) + 
  xlab('Time') + ylab('Deviance Value') + ggtitle(paste('Timeseries of GLM Deviance Improvement for', elephant, 'from', start_date, 'to', end_date))
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

start_date <- min(df_vif$date)
end_date <- max(df_vif$date)

png(paste0('output/timeseries_vif', run_label, '_rescaled.png'))
ggplot(data = df_vif, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = vif, group = predictor, color = predictor)) + 
  geom_line() + geom_point(data = df_vif[df_vif$significance == 'sig',], aes(y = vif), shape = 8) + 
  #geom_hline(yintercept = 0) + 
  geom_hline(yintercept = 5, linetype = 'dashed') +
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'fixed') + 
  xlab('Time') + ylab('Coefficient Value') + ggtitle(paste('Timeseries of VIF for', elephant, 'from', start_date, 'to', end_date))
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

#RQ2_run_table <- run_settings[run_settings$combo %in% srsig$combo,]
STS_run_table <- run_settings
STS_run_table <- STS_run_table[,1:5]

row.names(STS_run_table) <- 1:nrow(STS_run_table)

STS_coef <- data.frame()

suffix <- '_scaled'

for(i in 1:nrow(STS_run_table)){
  
  # get run settings
  ID <- STS_run_table$ID[i]
  week <- STS_run_table$week[i]
  method <- STS_run_table$pseudo_abs_method[i]
  
  # define filepaths
  data_path <- paste0('data/', ID, '/', week, '/')
  output_path <- paste0('output/', ID, '/', week, '/')
  

  # load and retrieve difference in deviance 
  dev_df <- read.csv(paste0(output_path, '6_b4_glm_50p_sd_deviances_', method, suffix, '.csv'))
  
  # calculate deviance improvement 
  dev_diff <- dev_df$null_deviance - dev_df$residual_deviance
  
  # calculate chi-square from deviance to get model significance 
  # source: https://stats.stackexchange.com/questions/340489/interpretation-of-deviance-in-logistic-model
  # source: https://www.r-bloggers.com/2022/05/calculate-the-p-value-from-chi-square-statistic-in-r/
  dev_sig <- pchisq(dev_diff, dev_df$null_df - dev_df$residual_df, lower.tail = F)
  
  # check if pvalues below 0.05
  if(dev_sig <=0.05){dev_sig <- 'sig'}else{dev_sig <- 'not sig'}
  
  # load and retrieve VIF 
  vif_df <- read.csv(paste0('output/', ID, '/', week, '/6_b5_glm_50p_sd_vif_', method, suffix, '.csv'), row.names = 1, header = T)
  
  # retrieve date of week 
  dfile <- read.csv(paste0(data_path, '2_a1_step_extents_LUT_', method, '.csv'), row.names = 1)
  date <- dfile$start_date[1]
  
  # retrieve coefs glm 
  c <- read.csv(paste0(output_path, '6_b3_glm_50p_sd_coefs_random_path_custom_distr', suffix, '.csv'))
  c <- c[2:nrow(c),]
  c_clr <- read.csv(paste0(output_path, '6_b1_clr_50p_sd_coefs_random_path_custom_distr', suffix, '.csv'))
  
  # specify if sig or not with 90% confidence (so pvalue < 0.1)
  c$Pr...z..[as.numeric(c$Pr...z..) < 0.1] <- 'sig'
  c$Pr...z..[as.numeric(c$Pr...z..) >= 0.1] <- 'not sig'
  c_clr$Pr...z..[as.numeric(c_clr$Pr...z..) < 0.1] <- 'sig'
  c_clr$Pr...z..[as.numeric(c_clr$Pr...z..) >= 0.1] <- 'not sig'
  
  # create new entry with coefs and pvalues 
  entry <- data.frame(ID = ID, week = week, method = method, date = date, predictor = c[,1], 
                      VIF = vif_df$vif_results, deviance_improvement = dev_diff, model_sig = dev_sig, 
                      glm_value = c$Estimate, glm_significance = c$Pr...z.., 
                      clr_value = c_clr$coef, clr_significance = c_clr$Pr...z..)
  
  STS_coef <- rbind(STS_coef, entry)
}




# STS_coef_sig <- STS_coef[STS_coef$significance == 'sig',]
# start_date <- max(STS_coef_sig$date)
# end_date <- min(STS_coef_sig$date)
# elephant <- 'many elephants'

# ggplot(data = STS_coef_sig, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = value, group = predictor, color = ID)) + 
#   geom_point(data = STS_coef[STS_coef$significance == 'sig',], aes(y = value), shape = 8) + 
#   geom_hline(yintercept = 0) + 
#   # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
#   facet_grid(vars(predictor), scale = 'free') + 
#   xlab('Time') + ylab('Coefficient Value') + ggtitle(paste0('Timeseries of GLM predictors for ', elephant, ' from ', start_date, ' to ', end_date))


# start_date <- max(STS_coef$date)
# end_date <- min(STS_coef$date)
# elephant <- 'many elephants'
# 
# STS_coef$period[STS_coef$week %in% seq(2065, 2069)] <- 'August'
# STS_coef$period[STS_coef$week %in% seq(2073, 2077)] <- 'October'
# ST_coef$period[STS_coef$week %in% seq(2085, 2089)] <- 'December'
# 
# ggplot(data = STS_coef, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = value, group = predictor, color = ID)) + 
#   geom_line() + geom_point(data = STS_coef[STS_coef$significance == 'sig',], aes(y = value), shape = 8) + 
#   geom_hline(yintercept = 0) + 
#   # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
#   # source: https://www.statology.org/ggplot-facet-order/
#   facet_grid(c(predictor) ~ c(factor(period, levels=c('August', 'October', 'December'))), scale = 'free') + 
#   xlab('Time') + ylab('Coefficient Value') + ggtitle(paste0('Timeseries of GLM predictors for ', elephant, ' from ', start_date, ' to ', end_date))



STS_coef_m <- STS_coef
STS_coef_m[STS_coef_m$ID == 'LA11' & STS_coef_m$week == 2085, c('glm_value', 'clr_value')] <- NA


start_date <- min(STS_coef_m$date)
end_date <- max(STS_coef_m$date)
elephant <- '13 elephants'

# plot all at once - GLM
png('output/STS_timeseries_glm_coef.png')
ggplot(data = STS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = glm_value, group = interaction(predictor, ID), color = ID)) + 
  geom_line() + 
  geom_point(data = STS_coef_m[STS_coef_m$glm_significance == 'sig',], aes(y = glm_value), shape = 8) + 
  geom_hline(yintercept = 0) + 
  facet_grid(vars(predictor), scale = 'free') + 
  xlab('Time') + ylab('Coefficient Value') + ggtitle(paste0('Timeseries of GLM predictors for ', elephant, ' from ', start_date, ' to ', end_date))
dev.off()

# plot all at once - CLR
png('output/STS_timeseries_clr_coef.png')
ggplot(data = STS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = clr_value, group = interaction(predictor, ID), color = ID)) + 
  geom_line() + 
  geom_point(data = STS_coef_m[STS_coef_m$clr_significance == 'sig',], aes(y = clr_value), shape = 8) + 
  geom_hline(yintercept = 0) + 
  facet_grid(vars(predictor), scale = 'free') + 
  xlab('Time') + ylab('Coefficient Value') + ggtitle(paste0('Timeseries of CLR predictors for ', elephant, ' from ', start_date, ' to ', end_date))
dev.off()


# aggregate results of all elephants --> retrieve summary stats
STS_coef_m$combo <- paste0(STS_coef_m$week, STS_coef_m$predictor, sep = '_')

ag_glm <- aggregate(STS_coef_m$glm_value, list(STS_coef_m$week, STS_coef_m$predictor), FUN = mean, na.rm = T)
ag_glm_m <- ag_glm$x
ag_glm_q1 <- aggregate(STS_coef_m$glm_value, list(STS_coef_m$week, STS_coef_m$predictor), FUN = quantile, probs = 0.25, na.rm = T)$x
ag_glm_q2 <- aggregate(STS_coef_m$glm_value, list(STS_coef_m$week, STS_coef_m$predictor), FUN = quantile, probs = 0.50, na.rm = T)$x
ag_glm_q3 <- aggregate(STS_coef_m$glm_value, list(STS_coef_m$week, STS_coef_m$predictor), FUN = quantile, probs = 0.75, na.rm = T)$x

ag_clr_m <- aggregate(STS_coef_m$clr_value, list(STS_coef_m$week, STS_coef_m$predictor), FUN = mean, na.rm = T)$x
ag_clr_q1 <- aggregate(STS_coef_m$clr_value, list(STS_coef_m$week, STS_coef_m$predictor), FUN = quantile, probs = 0.25, na.rm = T)$x
ag_clr_q2 <- aggregate(STS_coef_m$clr_value, list(STS_coef_m$week, STS_coef_m$predictor), FUN = quantile, probs = 0.50, na.rm = T)$x
ag_clr_q3 <- aggregate(STS_coef_m$clr_value, list(STS_coef_m$week, STS_coef_m$predictor), FUN = quantile, probs = 0.75, na.rm = T)$x

ag_vif_m <- aggregate(STS_coef_m$VIF, list(STS_coef_m$week, STS_coef_m$predictor), FUN = mean, na.rm = T)$x
ag_vif_q1 <- aggregate(STS_coef_m$VIF, list(STS_coef_m$week, STS_coef_m$predictor), FUN = quantile, probs = 0.25, na.rm = T)$x
ag_vif_q2 <- aggregate(STS_coef_m$VIF, list(STS_coef_m$week, STS_coef_m$predictor), FUN = quantile, probs = 0.50, na.rm = T)$x
ag_vif_q3 <- aggregate(STS_coef_m$VIF, list(STS_coef_m$week, STS_coef_m$predictor), FUN = quantile, probs = 0.75, na.rm = T)$x

ag_di_m <- aggregate(STS_coef_m$deviance_improvement, list(STS_coef_m$week, STS_coef_m$predictor), FUN = mean, na.rm = T)$x
ag_di_q1 <- aggregate(STS_coef_m$deviance_improvement, list(STS_coef_m$week, STS_coef_m$predictor), FUN = quantile, probs = 0.25, na.rm = T)$x
ag_di_q2 <- aggregate(STS_coef_m$deviance_improvement, list(STS_coef_m$week, STS_coef_m$predictor), FUN = quantile, probs = 0.50, na.rm = T)$x
ag_di_q3 <- aggregate(STS_coef_m$deviance_improvement, list(STS_coef_m$week, STS_coef_m$predictor), FUN = quantile, probs = 0.75, na.rm = T)$x

# function to calculate relative number of sig models 
# source: https://www.geeksforgeeks.org/aggregate-data-using-custom-functions-using-r/
relative_sum = function(x) {
  return((sum(x == 'sig')/length(x))*100)
}

ag_model <- aggregate(STS_coef_m$model_sig, list(STS_coef_m$week, STS_coef_m$predictor), FUN = length)$x

ag_model_sig <- aggregate(STS_coef_m$model_sig, list(STS_coef_m$week, STS_coef_m$predictor), FUN = relative_sum)$x

agc <- paste0(ag_glm$Group.1, ag_glm$Group.2, sep = '_')

agdf <- data.frame(combo = agc, total_models = ag_model, VIF_Q1 = ag_vif_q1, VIF_Mean = ag_vif_m, VIF_Q2 = ag_vif_q2, VIF_Q3 = ag_vif_q3,
                   DEV_IMP_Q1 = ag_di_q1, DEV_IMP_Mean = ag_di_m, DEV_IMP_Q2 = ag_di_q2, DEV_IMP_Q3 = ag_di_q3,
                   GLM_Q1 = ag_glm_q1, GLM_Mean = ag_glm_m, GLM_Q2 = ag_glm_q2, GLM_Q3 = ag_glm_q3, 
                   CLR_Q1 = ag_clr_q1, CLR_Mean = ag_clr_m, CLR_Q2 = ag_clr_q2, CLR_Q3 = ag_clr_q3, Sig_Model_Proportion = ag_model_sig)

# source: https://www.guru99.com/r-merge-data-frames.html
STS_coef_m <- merge(STS_coef_m, agdf, by.x = 'combo')
# 
# ggplot(data = STS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = clr_value, group = predictor)) + 
#   # source: https://stackoverflow.com/questions/14704909/plotting-depth-range-in-time-series-using-ggplot
#   geom_ribbon(aes(ymin = CLR_Q1, ymax = CLR_Q3), fill = 'grey') + 
#   geom_line(aes(y = CLR_Mean, color = 'Mean')) +
#   geom_line(aes(y = CLR_Q2, color = 'Median')) +
#   geom_hline(yintercept = 0, linetype = 'dashed') + 
#   scale_color_manual(name = 'Function', values = c('blue', 'red')) +
#   # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
#   facet_grid(vars(predictor), scale = 'free') + 
#   xlab('Time') + ylab('Coefficient Value') + ggtitle('Aggregated time-series of estimated CLR model coefficients', subtitle = paste0(elephant, ' from ', start_date, ' to ', end_date)) + 
#   theme_minimal()


library("cowplot") # to add multiple plots as one

STS_coef_m$date[STS_coef_m$week == 2065] <- min(summary_results$date)
STS_coef_m$date[STS_coef_m$week == 2074] <- summary_results$date[129]
STS_coef_m$date[STS_coef_m$week == 2080] <- summary_results$date[16]
STS_coef_m$date[STS_coef_m$week == 2082] <- summary_results$date[18]
STS_coef_m$date[STS_coef_m$week == 2084] <- summary_results$date[20]
STS_coef_m$date[STS_coef_m$week == 2085] <- summary_results$date[21]

# addign a column for proportion of sig models --> have to aggregate because unique would omit some values that repeat for diff weeks
ag_sig <- aggregate(STS_coef_m$Sig_Model_Proportion, list(STS_coef_m$week), FUN = quantile, probs = 0.5, na.rm = T)$x
ag_di <- aggregate(STS_coef_m$deviance_improvement, list(STS_coef_m$week), FUN = mean)$x

# retrieve the number of models from sst_w df 
df_bar <- data.frame(week = unique(STS_coef_m$date), total_models = sst_w$total_models, deviance_improvement = ag_di, sig_models = ag_sig)

# change label names of predictors 
STS_coef_m$predictor[STS_coef_m$predictor == 'ndvi_50_scaled'] <- 'Avg. NDVI'
STS_coef_m$predictor[STS_coef_m$predictor == 'ndvi_sd_scaled'] <- 'Dev. NDVI'
STS_coef_m$predictor[STS_coef_m$predictor == 'ndvi_rate_50_scaled'] <- 'Avg. NDVI Growth'
STS_coef_m$predictor[STS_coef_m$predictor == 'ndvi_rate_sd_scaled'] <- 'Dev. NDVI Growth' 

# save STS coef table 
write.csv(STS_coef_m, 'output/STS_df_results_aggregated.csv')


png('output/STS_timeseries_glm_coef_aggregated.png')

bp <- ggplot(data = df_bar, aes(x = as.Date(week, tz = 'Africa/Maputo'))) + 
  geom_bar(aes(y = total_models, fill = sig_models), stat = 'identity', position = 'dodge', show.legend = F) +
  #geom_histogram(aes(y = total_models, color = Sig_Model_Proportion), stat = 'identity') +
  #scale_color_grey(name = '% Significant Models') + 
  scale_fill_continuous(name = '% Significant Models', type = 'gradient') +
  ylab('No. of Models') + xlab('Time') +
  theme(legend.position = 'none') + 
  theme_minimal()

lp <- ggplot(data = STS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = glm_value, group = predictor)) + 
  #geom_bar(aes(y = total_models, color = Sig_Model_Proportion)) +
  # source: https://stackoverflow.com/questions/14704909/plotting-depth-range-in-time-series-using-ggplot
  # source: https://stackoverflow.com/questions/28648698/alpha-transparency-not-working-in-ggplot2
  geom_ribbon(data = STS_coef_m, aes(ymin = GLM_Q1, ymax = GLM_Q3, fill = 'Qu. Range'), alpha = 0.5, show.legend = F) + 
  geom_line(aes(y = GLM_Mean, color = Sig_Model_Proportion, linetype = 'Mean'), linewidth = 1, show.legend = F) +
  #geom_line(aes(y = GLM_Q2, color = 'Median')) +
  geom_hline(yintercept = 0, linetype = 'dashed') + 
  # source: https://www.geeksforgeeks.org/combine-and-modify-ggplot2-legends-with-ribbons-and-lines/
  scale_linetype_manual(name = 'Function', values = c('Mean' = 'solid')) +
  # source: https://www.geeksforgeeks.org/how-to-remove-legend-title-in-r-with-ggplot2/
  scale_fill_manual(name = 'Margin', values = c('Qu. Range' = 'grey')) + 
  scale_color_continuous(name = '% Significant Models', type = 'gradient') + 
  guides(colour = guide_colourbar(order = 3),
         linetype = guide_legend(order = 1),
         fill = guide_legend(order = 2)) +
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') + 
  xlab('Time') + ylab('Coefficient Value') + ggtitle('Aggregated time-series of estimated GLM model coefficients', subtitle = paste0(elephant, ' from ', start_date, ' to ', end_date)) + 
  theme_minimal() 
  # source: https://stackoverflow.com/questions/68719513/ggplot-wont-remove-axis-ticks
  #theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
  

lp_leg <- ggplot(data = STS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = glm_value, group = predictor)) + 
  # source: https://stackoverflow.com/questions/14704909/plotting-depth-range-in-time-series-using-ggplot
  # source: https://stackoverflow.com/questions/28648698/alpha-transparency-not-working-in-ggplot2
  geom_ribbon(data = STS_coef_m, aes(ymin = GLM_Q1, ymax = GLM_Q3, fill = 'Qu. Range'), alpha = 0.5) + 
  geom_line(aes(y = GLM_Mean, color = Sig_Model_Proportion, linetype = 'Mean'), linewidth = 1) +
  # source: https://www.geeksforgeeks.org/combine-and-modify-ggplot2-legends-with-ribbons-and-lines/
  scale_linetype_manual(name = 'Function', values = c('Mean' = 'solid')) +
  # source: https://www.geeksforgeeks.org/how-to-remove-legend-title-in-r-with-ggplot2/
  scale_fill_manual(name = 'Margin', values = c('Qu. Range' = 'grey')) + 
  scale_color_continuous(name = '% Significant Models', type = 'gradient') + 
  guides(colour = guide_colourbar(order = 3),
         linetype = guide_legend(order = 1),
         fill = guide_legend(order = 2)) +
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') 


# source: https://wilkelab.org/cowplot/reference/get_legend.html
lp_leg <- get_legend(lp_leg)

# source: https://wilkelab.org/cowplot/articles/plot_grid.html
plot_grid(lp, lp_leg, bp, ncol = 2, nrow = 2, rel_heights = c(3,1), rel_widths = c(3,1))

dev.off()



png('output/STS_timeseries_clr_coef_aggregated.png')
bp <- ggplot(data = df_bar, aes(x = as.Date(week, tz = 'Africa/Maputo'))) + 
  geom_bar(aes(y = total_models, fill = sig_models), stat = 'identity', position = 'dodge', show.legend = F) +
  #geom_histogram(aes(y = total_models, color = Sig_Model_Proportion), stat = 'identity') +
  #scale_color_grey(name = '% Significant Models') + 
  scale_fill_continuous(name = '% Significant Models', type = 'gradient') +
  ylab('No. of Models') + xlab('Time') +
  theme(legend.position = 'none') + 
  theme_minimal()

lp <- ggplot(data = STS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = clr_value, group = predictor)) + 
  #geom_bar(aes(y = total_models, color = Sig_Model_Proportion)) +
  # source: https://stackoverflow.com/questions/14704909/plotting-depth-range-in-time-series-using-ggplot
  # source: https://stackoverflow.com/questions/28648698/alpha-transparency-not-working-in-ggplot2
  geom_ribbon(data = STS_coef_m, aes(ymin = CLR_Q1, ymax = CLR_Q3, fill = 'Qu. Range'), alpha = 0.5, show.legend = F) + 
  geom_line(aes(y = CLR_Mean, color = Sig_Model_Proportion, linetype = 'Mean'), linewidth = 1, show.legend = F) +
  #geom_line(aes(y = GLM_Q2, color = 'Median')) +
  geom_hline(yintercept = 0, linetype = 'dashed') + 
  # source: https://www.geeksforgeeks.org/combine-and-modify-ggplot2-legends-with-ribbons-and-lines/
  scale_linetype_manual(name = 'Function', values = c('Mean' = 'solid')) +
  # source: https://www.geeksforgeeks.org/how-to-remove-legend-title-in-r-with-ggplot2/
  scale_fill_manual(name = 'Margin', values = c('Qu. Range' = 'grey')) + 
  scale_color_continuous(name = '% Significant Models', type = 'gradient') + 
  guides(colour = guide_colourbar(order = 3),
         linetype = guide_legend(order = 1),
         fill = guide_legend(order = 2)) +
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') + 
  xlab('Time') + ylab('Coefficient Value') + ggtitle('Aggregated time-series of estimated CLR model coefficients', subtitle = paste0(elephant, ' from ', start_date, ' to ', end_date)) + 
  theme_minimal() 
# source: https://stackoverflow.com/questions/68719513/ggplot-wont-remove-axis-ticks
#theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())


lp_leg <- ggplot(data = STS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = clr_value, group = predictor)) + 
  # source: https://stackoverflow.com/questions/14704909/plotting-depth-range-in-time-series-using-ggplot
  # source: https://stackoverflow.com/questions/28648698/alpha-transparency-not-working-in-ggplot2
  geom_ribbon(data = STS_coef_m, aes(ymin = CLR_Q1, ymax = CLR_Q3, fill = 'Qu. Range'), alpha = 0.5) + 
  geom_line(aes(y = CLR_Mean, color = Sig_Model_Proportion, linetype = 'Mean'), linewidth = 1) +
  # source: https://www.geeksforgeeks.org/combine-and-modify-ggplot2-legends-with-ribbons-and-lines/
  scale_linetype_manual(name = 'Function', values = c('Mean' = 'solid')) +
  # source: https://www.geeksforgeeks.org/how-to-remove-legend-title-in-r-with-ggplot2/
  scale_fill_manual(name = 'Margin', values = c('Qu. Range' = 'grey')) + 
  scale_color_continuous(name = '% Significant Models', type = 'gradient') + 
  guides(colour = guide_colourbar(order = 3),
         linetype = guide_legend(order = 1),
         fill = guide_legend(order = 2)) +
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') 


# source: https://wilkelab.org/cowplot/reference/get_legend.html
lp_leg <- get_legend(lp_leg)

# source: https://wilkelab.org/cowplot/articles/plot_grid.html
plot_grid(lp, lp_leg, bp, ncol = 2, nrow = 2, rel_heights = c(3,1), rel_widths = c(3,1))

dev.off()

# plot deviance improvement and VIF 
png('output/STS_timeseries_dev_VIF_aggregated.png')
bp <- ggplot(data = df_bar, aes(x = as.Date(week, tz = 'Africa/Maputo'))) + 
  geom_bar(aes(y = deviance_improvement, fill = sig_models), stat = 'identity', position = 'dodge') +
  scale_fill_continuous(name = '% Significant Models', type = 'gradient') +
  ylab('Deviance Improvement') + xlab('Time') +
  theme_minimal() + 
  theme(legend.position = 'none') 

lp <- ggplot(data = STS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = VIF, group = predictor)) + 
  # source: https://stackoverflow.com/questions/14704909/plotting-depth-range-in-time-series-using-ggplot
  # source: https://stackoverflow.com/questions/28648698/alpha-transparency-not-working-in-ggplot2
  geom_ribbon(data = STS_coef_m, aes(ymin = VIF_Q1, ymax = VIF_Q3, fill = 'Qu. Range'), alpha = 0.5) + 
  geom_line(aes(y = VIF_Mean, color = Sig_Model_Proportion, linetype = 'Mean'), linewidth = 1) +
  geom_hline(yintercept = 0, linetype = 'dashed') + 
  # source: https://www.geeksforgeeks.org/combine-and-modify-ggplot2-legends-with-ribbons-and-lines/
  scale_linetype_manual(name = 'Function', values = c('Mean' = 'solid')) +
  # source: https://www.geeksforgeeks.org/how-to-remove-legend-title-in-r-with-ggplot2/
  scale_fill_manual(name = 'Margin', values = c('Qu. Range' = 'grey')) + 
  scale_color_continuous(name = '% Significant Models', type = 'gradient') + 
  guides(colour = guide_colourbar(order = 3),
         linetype = guide_legend(order = 1),
         fill = guide_legend(order = 2)) +
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') + 
  xlab('Time') + ylab('Coefficient Value') + ggtitle('Aggregated time-series of VIF and Deviance Improvement', subtitle = paste0(elephant, ' from ', start_date, ' to ', end_date)) + 
  theme_minimal() + 
  theme(legend.position = 'none') 

lp_leg <- ggplot(data = STS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = VIF, group = predictor)) + 
  # source: https://stackoverflow.com/questions/14704909/plotting-depth-range-in-time-series-using-ggplot
  # source: https://stackoverflow.com/questions/28648698/alpha-transparency-not-working-in-ggplot2
  geom_ribbon(data = STS_coef_m, aes(ymin = VIF_Q1, ymax = VIF_Q3, fill = 'Qu. Range'), alpha = 0.5) + 
  geom_line(aes(y = VIF_Mean, color = Sig_Model_Proportion, linetype = 'Mean'), linewidth = 1) +
  # source: https://www.geeksforgeeks.org/combine-and-modify-ggplot2-legends-with-ribbons-and-lines/
  scale_linetype_manual(name = 'Function', values = c('Mean' = 'solid')) +
  # source: https://www.geeksforgeeks.org/how-to-remove-legend-title-in-r-with-ggplot2/
  scale_fill_manual(name = 'Margin', values = c('Qu. Range' = 'grey')) + 
  scale_color_continuous(name = '% Significant Models', type = 'gradient') + 
  guides(colour = guide_colourbar(order = 3),
         linetype = guide_legend(order = 1),
         fill = guide_legend(order = 2)) +
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') 


# source: https://wilkelab.org/cowplot/reference/get_legend.html
lp_leg <- get_legend(lp_leg)

# source: https://wilkelab.org/cowplot/articles/plot_grid.html
plot_grid(lp, lp_leg, bp, ncol = 2, nrow = 2, rel_heights = c(3,1), rel_widths = c(3,1))
dev.off()








# ggplot(data = STS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = clr_value, group = predictor)) + 
#   # source: https://stackoverflow.com/questions/14704909/plotting-depth-range-in-time-series-using-ggplot
#   # source: https://stackoverflow.com/questions/28648698/alpha-transparency-not-working-in-ggplot2
#   geom_ribbon(data = STS_coef_m, aes(ymin = CLR_Q1, ymax = CLR_Q3, fill = 'Qu. Range'), alpha = 0.5) + 
#   geom_line(aes(y = CLR_Mean, color = Sig_Model_Proportion, linetype = 'Mean'), linewidth = 1) +
#   geom_hline(yintercept = 0, linetype = 'dashed') + 
#   # source: https://www.geeksforgeeks.org/combine-and-modify-ggplot2-legends-with-ribbons-and-lines/
#   scale_linetype_manual(name = 'Function', values = c('Mean' = 'solid')) +
#   # source: https://www.geeksforgeeks.org/how-to-remove-legend-title-in-r-with-ggplot2/
#   scale_fill_manual(name = 'Margin', values = c('Qu. Range' = 'grey')) + 
#   scale_color_continuous(name = '% Significant Models', type = 'gradient') + 
#   guides(colour = guide_colourbar(order = 1),
#          linetype = guide_legend(order = 2),
#          fill = guide_legend(order = 3)) +
#   # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
#   facet_grid(vars(predictor), scale = 'free') + 
#   xlab('Time') + ylab('Coefficient Value') + ggtitle('Aggregated time-series of estimated CLR model coefficients', subtitle = paste0(elephant, ' from ', start_date, ' to ', end_date)) + 
#   theme_minimal()




















### LTS ANALYSIS 
# create run table with only sig models 
#RQ2_run_table <- data.frame()

# fill new run table with only runs that give sig models 
srsig <- summary_results[summary_results$sub_glm_sig == T,] # these are the models i want to retrieve coefs for 

# get ID and weeks of sig models 
srsig$combo <- paste(srsig$ID, srsig$week, sep = '_')

run_settings$combo <- paste(run_settings$ID, run_settings$week, sep = '_')

#RQ2_run_table <- run_settings[run_settings$combo %in% srsig$combo,]
LTS_run_table <- run_settings
LTS_run_table <- LTS_run_table[,1:5]

row.names(LTS_run_table) <- 1:nrow(LTS_run_table)

LTS_coef <- data.frame()

suffix <- '_scaled'

for(i in 1:nrow(LTS_run_table)){
  
  # get run settings
  ID <- LTS_run_table$ID[i]
  week <- LTS_run_table$week[i]
  method <- LTS_run_table$pseudo_abs_method[i]
  
  # define filepaths
  data_path <- paste0('data/', ID, '/', week, '/')
  output_path <- paste0('output/', ID, '/', week, '/')
  
  
  # load and retrieve difference in deviance 
  dev_df <- read.csv(paste0(output_path, '6_b4_glm_50p_sd_deviances_', method, suffix, '.csv'))
  
  # calculate deviance improvement 
  dev_diff <- dev_df$null_deviance - dev_df$residual_deviance
  
  # calculate chi-square from deviance to get model significance 
  # source: https://stats.stackexchange.com/questions/340489/interpretation-of-deviance-in-logistic-model
  # source: https://www.r-bloggers.com/2022/05/calculate-the-p-value-from-chi-square-statistic-in-r/
  dev_sig <- pchisq(dev_diff, dev_df$null_df - dev_df$residual_df, lower.tail = F)
  
  # check if pvalues below 0.05
  if(dev_sig <=0.05){dev_sig <- 'sig'}else{dev_sig <- 'not sig'}
  
  # load and retrieve VIF 
  vif_df <- read.csv(paste0('output/', ID, '/', week, '/6_b5_glm_50p_sd_vif_', method, suffix, '.csv'), row.names = 1, header = T)
  
  # retrieve date of week 
  dfile <- read.csv(paste0(data_path, '2_a1_step_extents_LUT_', method, '.csv'), row.names = 1)
  date <- dfile$start_date[1]
  
  # retrieve coefs glm 
  c <- read.csv(paste0(output_path, '6_b3_glm_50p_sd_coefs_random_path_custom_distr', suffix, '.csv'))
  c <- c[2:nrow(c),]
  c_clr <- read.csv(paste0(output_path, '6_b1_clr_50p_sd_coefs_random_path_custom_distr', suffix, '.csv'))
  
  # specify if sig or not with 90% confidence (so pvalue < 0.1)
  c$Pr...z..[as.numeric(c$Pr...z..) < 0.1] <- 'sig'
  c$Pr...z..[as.numeric(c$Pr...z..) >= 0.1] <- 'not sig'
  c_clr$Pr...z..[as.numeric(c_clr$Pr...z..) < 0.1] <- 'sig'
  c_clr$Pr...z..[as.numeric(c_clr$Pr...z..) >= 0.1] <- 'not sig'
  
  # create new entry with coefs and pvalues 
  entry <- data.frame(ID = ID, week = week, method = method, date = date, predictor = c[,1], 
                      VIF = vif_df$vif_results, deviance_improvement = dev_diff, model_sig = dev_sig, 
                      glm_value = c$Estimate, glm_significance = c$Pr...z.., 
                      clr_value = c_clr$coef, clr_significance = c_clr$Pr...z..)
  
  LTS_coef <- rbind(LTS_coef, entry)
}


LTS_coef_m <- LTS_coef
LTS_coef_m[LTS_coef_m$ID == 'LA11' & LTS_coef_m$week == 2085, c('glm_value', 'clr_value')] <- NA


start_date <- min(LTS_coef_m$date)
end_date <- max(LTS_coef_m$date)
elephant <- 'LA11 and LA14'

# plot all at once - GLM
png('output/LTS_timeseries_glm_coef.png')
ggplot(data = LTS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = glm_value, group = interaction(predictor, ID), color = ID)) + 
  geom_line() + 
  geom_point(data = LTS_coef_m[LTS_coef_m$glm_significance == 'sig',], aes(y = glm_value), shape = 8) + 
  geom_hline(yintercept = 0) + 
  facet_grid(vars(predictor), scale = 'free') + 
  xlab('Time') + ylab('Coefficient Value') + ggtitle(paste0('Timeseries of GLM predictors for ', elephant, ' from ', start_date, ' to ', end_date))
dev.off()

# plot all at once - CLR
png('output/LTS_timeseries_clr_coef.png')
ggplot(data = LTS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = clr_value, group = interaction(predictor, ID), color = ID)) + 
  geom_line() + 
  geom_point(data = LTS_coef_m[LTS_coef_m$clr_significance == 'sig',], aes(y = clr_value), shape = 8) + 
  geom_hline(yintercept = 0) + 
  facet_grid(vars(predictor), scale = 'free') + 
  xlab('Time') + ylab('Coefficient Value') + ggtitle(paste0('Timeseries of CLR predictors for ', elephant, ' from ', start_date, ' to ', end_date))
dev.off()


# aggregate results of all elephants --> retrieve summary stats
LTS_coef_m$combo <- paste0(LTS_coef_m$week, LTS_coef_m$predictor, sep = '_')

ag_glm <- aggregate(LTS_coef_m$glm_value, list(LTS_coef_m$week, LTS_coef_m$predictor), FUN = mean, na.rm = T)
ag_glm_m <- ag_glm$x
ag_glm_q1 <- aggregate(LTS_coef_m$glm_value, list(LTS_coef_m$week, LTS_coef_m$predictor), FUN = quantile, probs = 0.25, na.rm = T)$x
ag_glm_q2 <- aggregate(LTS_coef_m$glm_value, list(LTS_coef_m$week, LTS_coef_m$predictor), FUN = quantile, probs = 0.50, na.rm = T)$x
ag_glm_q3 <- aggregate(LTS_coef_m$glm_value, list(LTS_coef_m$week, LTS_coef_m$predictor), FUN = quantile, probs = 0.75, na.rm = T)$x

ag_clr_m <- aggregate(LTS_coef_m$clr_value, list(LTS_coef_m$week, LTS_coef_m$predictor), FUN = mean, na.rm = T)$x
ag_clr_q1 <- aggregate(LTS_coef_m$clr_value, list(LTS_coef_m$week, LTS_coef_m$predictor), FUN = quantile, probs = 0.25, na.rm = T)$x
ag_clr_q2 <- aggregate(LTS_coef_m$clr_value, list(LTS_coef_m$week, LTS_coef_m$predictor), FUN = quantile, probs = 0.50, na.rm = T)$x
ag_clr_q3 <- aggregate(LTS_coef_m$clr_value, list(LTS_coef_m$week, LTS_coef_m$predictor), FUN = quantile, probs = 0.75, na.rm = T)$x

ag_vif_m <- aggregate(LTS_coef_m$VIF, list(LTS_coef_m$week, LTS_coef_m$predictor), FUN = mean, na.rm = T)$x
ag_vif_q1 <- aggregate(LTS_coef_m$VIF, list(LTS_coef_m$week, LTS_coef_m$predictor), FUN = quantile, probs = 0.25, na.rm = T)$x
ag_vif_q2 <- aggregate(LTS_coef_m$VIF, list(LTS_coef_m$week, LTS_coef_m$predictor), FUN = quantile, probs = 0.50, na.rm = T)$x
ag_vif_q3 <- aggregate(LTS_coef_m$VIF, list(LTS_coef_m$week, LTS_coef_m$predictor), FUN = quantile, probs = 0.75, na.rm = T)$x

ag_di_m <- aggregate(LTS_coef_m$deviance_improvement, list(LTS_coef_m$week, LTS_coef_m$predictor), FUN = mean, na.rm = T)$x
ag_di_q1 <- aggregate(LTS_coef_m$deviance_improvement, list(LTS_coef_m$week, LTS_coef_m$predictor), FUN = quantile, probs = 0.25, na.rm = T)$x
ag_di_q2 <- aggregate(LTS_coef_m$deviance_improvement, list(LTS_coef_m$week, LTS_coef_m$predictor), FUN = quantile, probs = 0.50, na.rm = T)$x
ag_di_q3 <- aggregate(LTS_coef_m$deviance_improvement, list(LTS_coef_m$week, LTS_coef_m$predictor), FUN = quantile, probs = 0.75, na.rm = T)$x

# function to calculate relative number of sig models 
# source: https://www.geeksforgeeks.org/aggregate-data-using-custom-functions-using-r/
relative_sum = function(x) {
  return((sum(x == 'sig')/length(x))*100)
}

ag_model <- aggregate(LTS_coef_m$model_sig, list(LTS_coef_m$week, LTS_coef_m$predictor), FUN = length)$x

ag_model_sig <- aggregate(LTS_coef_m$model_sig, list(LTS_coef_m$week, LTS_coef_m$predictor), FUN = relative_sum)$x

agc <- paste0(ag_glm$Group.1, ag_glm$Group.2, sep = '_')

agdf <- data.frame(combo = agc, total_models = ag_model, VIF_Q1 = ag_vif_q1, VIF_Mean = ag_vif_m, VIF_Q2 = ag_vif_q2, VIF_Q3 = ag_vif_q3,
                   DEV_IMP_Q1 = ag_di_q1, DEV_IMP_Mean = ag_di_m, DEV_IMP_Q2 = ag_di_q2, DEV_IMP_Q3 = ag_di_q3,
                   GLM_Q1 = ag_glm_q1, GLM_Mean = ag_glm_m, GLM_Q2 = ag_glm_q2, GLM_Q3 = ag_glm_q3, 
                   CLR_Q1 = ag_clr_q1, CLR_Mean = ag_clr_m, CLR_Q2 = ag_clr_q2, CLR_Q3 = ag_clr_q3, Sig_Model_Proportion = ag_model_sig)

# source: https://www.guru99.com/r-merge-data-frames.html
LTS_coef_m <- merge(LTS_coef_m, agdf, by.x = 'combo')



library("cowplot") # to add multiple plots as one

LTS_coef_m$date[LTS_coef_m$week == 2065] <- min(summary_results$date) # for both STS and LTS
LTS_coef_m$date[LTS_coef_m$week == 2085] <- LTS_coef_m$date[159]
LTS_coef_m$date[LTS_coef_m$week == 2104] <- LTS_coef_m$date[310]
LTS_coef_m$date[LTS_coef_m$week == 2178] <- LTS_coef_m$date[821]

#print(length(unique(LTS_coef_m$week)) == length(unique(LTS_coef_m$date)))



# addign a column for proportion of sig models --> have to aggregate because unique would omit some values that repeat for diff weeks
ag_sig <- aggregate(LTS_coef_m$Sig_Model_Proportion, list(LTS_coef_m$week), FUN = quantile, probs = 0.5, na.rm = T)$x
ag_di <- aggregate(LTS_coef_m$deviance_improvement, list(LTS_coef_m$week), FUN = mean)$x

# retrieve the number of models from sst_w df 
df_bar <- data.frame(week = unique(LTS_coef_m$date), total_models = sst_w$total_models, deviance_improvement = ag_di, sig_models = ag_sig)

# change label names of predictors 
LTS_coef_m$predictor[LTS_coef_m$predictor == 'ndvi_50_scaled'] <- 'Avg. NDVI'
LTS_coef_m$predictor[LTS_coef_m$predictor == 'ndvi_sd_scaled'] <- 'Dev. NDVI'
LTS_coef_m$predictor[LTS_coef_m$predictor == 'ndvi_rate_50_scaled'] <- 'Avg. NDVI Growth'
LTS_coef_m$predictor[LTS_coef_m$predictor == 'ndvi_rate_sd_scaled'] <- 'Dev. NDVI Growth' 

# save STS coef table 
write.csv(LTS_coef_m, 'output/LTS_df_results_aggregated.csv')



# retrieve dates that want to highlight 
date_hl <- unique(LTS_coef_m$date[grep('-09-', LTS_coef_m$date)])

# get a list of dates by year
# source: https://r-coder.com/split-r/
date_hl <- split(date_hl, f = sub('-.*', '', date_hl))

# get start and end dates for each year
# source: https://stackoverflow.com/questions/43425540/get-max-min-values-from-a-list-of-dataframes-without-loop
date_hl <- sapply(date_hl, function(x) range(x))

# convert to data frame (doesn't work if matrix) of xmin and xmax 
date_hl <- data.frame(t(date_hl))
colnames(date_hl) <- c('xmin', 'xmax')

# convert dates to date object
date_hl$xmin <- as.Date(date_hl$xmin, tz = 'Africa/Maputo')
date_hl$xmax <- as.Date(date_hl$xmax, tz = 'Africa/Maputo')


png('output/LTS_timeseries_glm_coef_aggregated.png')

bp <- ggplot(data = df_bar, aes(x = as.Date(week, tz = 'Africa/Maputo'))) + 
  geom_bar(aes(y = total_models, fill = sig_models), stat = 'identity', position = 'dodge', show.legend = F) +
  #geom_histogram(aes(y = total_models, color = Sig_Model_Proportion), stat = 'identity') +
  #scale_color_grey(name = '% Significant Models') + 
  # scale_fill_continuous(name = '% Significant Models', type = 'gradient') +
  scale_fill_gradientn(name = '% Significant Models', colors = c('#525174', '#348aa7', '#5dd39e', '#bce784')) +
  ylab('No. of Models') + xlab('Time') +
  theme(legend.position = 'none') + 
  theme_minimal()

lp <- ggplot(data = LTS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
  # source: https://stackoverflow.com/questions/29648907/using-geom-rect-for-time-series-shading-in-r
  geom_rect(data = date_hl, mapping=aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf, fill = 'Transition Period'), alpha = 0.1, show.legend = F) +
  # source: https://stackoverflow.com/questions/14704909/plotting-depth-range-in-time-series-using-ggplot
  # source: https://stackoverflow.com/questions/28648698/alpha-transparency-not-working-in-ggplot2
  geom_ribbon(data = LTS_coef_m, aes(ymin = GLM_Q1, ymax = GLM_Q3, fill = 'Qu. Range', group = predictor), alpha = 0.5, show.legend = F) + 
  geom_line(aes(y = GLM_Mean, group = predictor, color = Sig_Model_Proportion, linetype = 'Mean'), linewidth = 1, show.legend = F) +
  #geom_line(aes(y = GLM_Q2, color = 'Median')) +
  geom_hline(yintercept = 0, linetype = 'dashed') + 
  # source: https://www.geeksforgeeks.org/combine-and-modify-ggplot2-legends-with-ribbons-and-lines/
  scale_linetype_manual(name = 'Function', values = c('Mean' = 'solid')) +
  # source: https://www.geeksforgeeks.org/how-to-remove-legend-title-in-r-with-ggplot2/
  scale_fill_manual(name = 'Margins', values = c('Qu. Range' = 'grey', 'Transition Period' = 'orange')) + 
  #scale_color_continuous(name = '% Significant Models', type = 'gradient') + 
  scale_color_gradientn(name = '% Significant Models', colors = c('#525174', '#348aa7', '#5dd39e', '#bce784')) +
  guides(colour = guide_colourbar(order = 3),
         linetype = guide_legend(order = 1),
         fill = guide_legend(order = 2)) +
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') + 
  xlab('Time') + ylab('Coefficient Value') + ggtitle('Aggregated time-series of estimated GLM model coefficients', subtitle = paste0(elephant, ' from ', start_date, ' to ', end_date)) + 
  theme_minimal() 
# source: https://stackoverflow.com/questions/68719513/ggplot-wont-remove-axis-ticks
#theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

lp_leg <- ggplot(data = LTS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
  geom_rect(data = date_hl, mapping=aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf, fill = 'Transition Period'), alpha = 0.1) +
  # source: https://stackoverflow.com/questions/14704909/plotting-depth-range-in-time-series-using-ggplot
  # source: https://stackoverflow.com/questions/28648698/alpha-transparency-not-working-in-ggplot2
  geom_ribbon(data = LTS_coef_m, aes(ymin = GLM_Q1, ymax = GLM_Q3, fill = 'Qu. Range', group = predictor), alpha = 0.5) + 
  geom_line(aes(y = GLM_Mean, group = predictor, color = Sig_Model_Proportion, linetype = 'Mean'), linewidth = 1) +
  # source: https://www.geeksforgeeks.org/combine-and-modify-ggplot2-legends-with-ribbons-and-lines/
  scale_linetype_manual(name = 'Function', values = c('Mean' = 'solid')) +
  # source: https://www.geeksforgeeks.org/how-to-remove-legend-title-in-r-with-ggplot2/
  scale_fill_manual(name = 'Margins', values = c('Qu. Range' = 'grey', 'Transition Period' = 'orange')) + 
  #scale_color_continuous(name = '% Significant Models', type = 'gradient') + 
  scale_color_gradientn(name = '% Significant Models', colors = c('#525174', '#348aa7', '#5dd39e', '#bce784')) +
  guides(colour = guide_colourbar(order = 3),
         linetype = guide_legend(order = 1),
         fill = guide_legend(order = 2)) +
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') 


# source: https://wilkelab.org/cowplot/reference/get_legend.html
lp_leg <- get_legend(lp_leg)

# source: https://wilkelab.org/cowplot/articles/plot_grid.html
plot_grid(lp, lp_leg, bp, ncol = 2, nrow = 2, rel_heights = c(3,1), rel_widths = c(3,1))

dev.off()



png('output/LTS_timeseries_clr_coef_aggregated.png')

bp <- ggplot(data = df_bar, aes(x = as.Date(week, tz = 'Africa/Maputo'))) + 
  geom_bar(aes(y = total_models, fill = sig_models), stat = 'identity', position = 'dodge', show.legend = F) +
  #geom_histogram(aes(y = total_models, color = Sig_Model_Proportion), stat = 'identity') +
  #scale_color_grey(name = '% Significant Models') + 
  # scale_fill_continuous(name = '% Significant Models', type = 'gradient') +
  scale_fill_gradientn(name = '% Significant Models', colors = c('#525174', '#348aa7', '#5dd39e', '#bce784')) +
  ylab('No. of Models') + xlab('Time') +
  theme(legend.position = 'none') + 
  theme_minimal()

lp <- ggplot(data = LTS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
  # source: https://stackoverflow.com/questions/29648907/using-geom-rect-for-time-series-shading-in-r
  geom_rect(data = date_hl, mapping=aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf, fill = 'Transition Period'), alpha = 0.1, show.legend = F) +
  # source: https://stackoverflow.com/questions/14704909/plotting-depth-range-in-time-series-using-ggplot
  # source: https://stackoverflow.com/questions/28648698/alpha-transparency-not-working-in-ggplot2
  geom_ribbon(data = LTS_coef_m, aes(ymin = CLR_Q1, ymax = CLR_Q3, fill = 'Qu. Range', group = predictor), alpha = 0.5, show.legend = F) + 
  geom_line(aes(y = CLR_Mean, group = predictor, color = Sig_Model_Proportion, linetype = 'Mean'), linewidth = 1, show.legend = F) +
  #geom_line(aes(y = CLR_Q2, color = 'Median')) +
  geom_hline(yintercept = 0, linetype = 'dashed') + 
  # source: https://www.geeksforgeeks.org/combine-and-modify-ggplot2-legends-with-ribbons-and-lines/
  scale_linetype_manual(name = 'Function', values = c('Mean' = 'solid')) +
  # source: https://www.geeksforgeeks.org/how-to-remove-legend-title-in-r-with-ggplot2/
  scale_fill_manual(name = 'Margins', values = c('Qu. Range' = 'grey', 'Transition Period' = 'orange')) + 
  #scale_color_continuous(name = '% Significant Models', type = 'gradient') + 
  scale_color_gradientn(name = '% Significant Models', colors = c('#525174', '#348aa7', '#5dd39e', '#bce784')) +
  guides(colour = guide_colourbar(order = 3),
         linetype = guide_legend(order = 1),
         fill = guide_legend(order = 2)) +
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') + 
  xlab('Time') + ylab('Coefficient Value') + ggtitle('Aggregated time-series of estimated CLR model coefficients', subtitle = paste0(elephant, ' from ', start_date, ' to ', end_date)) + 
  theme_minimal() 
# source: https://stackoverflow.com/questions/68719513/ggplot-wont-remove-axis-ticks
#theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

lp_leg <- ggplot(data = LTS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
  geom_rect(data = date_hl, mapping=aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf, fill = 'Transition Period'), alpha = 0.1) +
  # source: https://stackoverflow.com/questions/14704909/plotting-depth-range-in-time-series-using-ggplot
  # source: https://stackoverflow.com/questions/28648698/alpha-transparency-not-working-in-ggplot2
  geom_ribbon(data = LTS_coef_m, aes(ymin = CLR_Q1, ymax = CLR_Q3, fill = 'Qu. Range', group = predictor), alpha = 0.5) + 
  geom_line(aes(y = CLR_Mean, group = predictor, color = Sig_Model_Proportion, linetype = 'Mean'), linewidth = 1) +
  # source: https://www.geeksforgeeks.org/combine-and-modify-ggplot2-legends-with-ribbons-and-lines/
  scale_linetype_manual(name = 'Function', values = c('Mean' = 'solid')) +
  # source: https://www.geeksforgeeks.org/how-to-remove-legend-title-in-r-with-ggplot2/
  scale_fill_manual(name = 'Margins', values = c('Qu. Range' = 'grey', 'Transition Period' = 'orange')) + 
  #scale_color_continuous(name = '% Significant Models', type = 'gradient') + 
  scale_color_gradientn(name = '% Significant Models', colors = c('#525174', '#348aa7', '#5dd39e', '#bce784')) +
  guides(colour = guide_colourbar(order = 3),
         linetype = guide_legend(order = 1),
         fill = guide_legend(order = 2)) +
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') 


# source: https://wilkelab.org/cowplot/reference/get_legend.html
lp_leg <- get_legend(lp_leg)

# source: https://wilkelab.org/cowplot/articles/plot_grid.html
plot_grid(lp, lp_leg, bp, ncol = 2, nrow = 2, rel_heights = c(3,1), rel_widths = c(3,1))

dev.off()



# plot deviance improvement and VIF 
png('output/LTS_timeseries_dev_VIF_aggregated.png')
bp <- ggplot(data = df_bar, aes(x = as.Date(week, tz = 'Africa/Maputo'))) + 
  geom_bar(aes(y = deviance_improvement, fill = sig_models), stat = 'identity', position = 'dodge') +
  # scale_fill_continuous(name = '% Significant Models', type = 'gradient') +
  scale_fill_gradientn(name = '% Significant Models', colors = c('#525174', '#348aa7', '#5dd39e', '#bce784')) +
  ylab('Deviance Improvement') + xlab('Time') +
  theme_minimal() + 
  theme(legend.position = 'none') 

lp <- ggplot(data = LTS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
  # source: https://stackoverflow.com/questions/29648907/using-geom-rect-for-time-series-shading-in-r
  geom_rect(data = date_hl, mapping=aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf, fill = 'Transition Period'), alpha = 0.1) +
  # source: https://stackoverflow.com/questions/14704909/plotting-depth-range-in-time-series-using-ggplot
  # source: https://stackoverflow.com/questions/28648698/alpha-transparency-not-working-in-ggplot2
  geom_ribbon(data = LTS_coef_m, aes(ymin = VIF_Q1, ymax = VIF_Q3, fill = 'Qu. Range', group = predictor), alpha = 0.5) + 
  geom_line(aes(y = VIF_Mean, group = predictor, color = Sig_Model_Proportion, linetype = 'Mean'), linewidth = 1) +
  geom_hline(yintercept = 0, linetype = 'dashed') + 
  # source: https://www.geeksforgeeks.org/combine-and-modify-ggplot2-legends-with-ribbons-and-lines/
  scale_linetype_manual(name = 'Function', values = c('Mean' = 'solid')) +
  # source: https://www.geeksforgeeks.org/how-to-remove-legend-title-in-r-with-ggplot2/
  scale_fill_manual(name = 'Margins', values = c('Qu. Range' = 'grey', 'Transition Period' = 'orange')) + 
  # scale_color_continuous(name = '% Significant Models', type = 'gradient') + 
  scale_color_gradientn(name = '% Significant Models', colors = c('#525174', '#348aa7', '#5dd39e', '#bce784')) +
  guides(colour = guide_colourbar(order = 3),
         linetype = guide_legend(order = 1),
         fill = guide_legend(order = 2)) +
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') + 
  xlab('Time') + ylab('Coefficient Value') + ggtitle('Aggregated time-series of VIF and Deviance Improvement', subtitle = paste0(elephant, ' from ', start_date, ' to ', end_date)) + 
  theme_minimal() + 
  theme(legend.position = 'none') 

lp_leg <- ggplot(data = LTS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
  # source: https://stackoverflow.com/questions/29648907/using-geom-rect-for-time-series-shading-in-r
  geom_rect(data = date_hl, mapping=aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf, fill = 'Transition Period'), alpha = 0.1) +
  # source: https://stackoverflow.com/questions/14704909/plotting-depth-range-in-time-series-using-ggplot
  # source: https://stackoverflow.com/questions/28648698/alpha-transparency-not-working-in-ggplot2
  geom_ribbon(data = LTS_coef_m, aes(ymin = VIF_Q1, ymax = VIF_Q3, fill = 'Qu. Range', group = predictor), alpha = 0.5) + 
  geom_line(aes(y = VIF_Mean, group = predictor, color = Sig_Model_Proportion, linetype = 'Mean'), linewidth = 1) +
  # source: https://www.geeksforgeeks.org/combine-and-modify-ggplot2-legends-with-ribbons-and-lines/
  scale_linetype_manual(name = 'Function', values = c('Mean' = 'solid')) +
  # source: https://www.geeksforgeeks.org/how-to-remove-legend-title-in-r-with-ggplot2/
  scale_fill_manual(name = 'Margin', values = c('Qu. Range' = 'grey', 'Transition Period' = 'orange')) + 
  # scale_color_continuous(name = '% Significant Models', type = 'gradient') + 
  scale_color_gradientn(name = '% Significant Models', colors = c('#525174', '#348aa7', '#5dd39e', '#bce784')) +
  guides(colour = guide_colourbar(order = 3),
         linetype = guide_legend(order = 1),
         fill = guide_legend(order = 2)) +
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') 


# source: https://wilkelab.org/cowplot/reference/get_legend.html
lp_leg <- get_legend(lp_leg)

# source: https://wilkelab.org/cowplot/articles/plot_grid.html
plot_grid(lp, lp_leg, bp, ncol = 2, nrow = 2, rel_heights = c(3,1), rel_widths = c(3,1))
dev.off()

