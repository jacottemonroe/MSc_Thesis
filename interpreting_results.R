## interpreting results 



# define name of run (downscaling, RQ2, specific week or elephant idk)
run_label <- '_downscaling_final' #'_STS_final' #'_downscaling_finql' #'_STS' #"_all_runs_new_path_with_CV"  #"_new_path_with_CV_second_batch" #"_new_path_with_CV_rerun" #'_all_runs_new_path_with_CV' #'_LTS_LA11_LA12_LA13_LA14' #'_STS' #'_downscaling' #'_LA14_LTS_full' #'_LTS_LA11_LA12_LA14' #'_LA14_LTS' #'_LA14_LTS_rerun'  #'_LA14_LTS_full'

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
  downscaling <- run_settings$downscaling[i]
  
  # define filepaths
  data_path <- paste0('data/', ID, '/', week, '/')
  output_path <- paste0('output/', ID, '/', week, '/')
  
  # get files for data and output paths
  data_files <- list.files(data_path)
  output_files <- list.files(output_path)
  
  # define files that should have for each step
  step1_files <- c('1_a1_elephant_full_track_xyt.RDS', '1_a2_elephant_track_xyt.RDS', 
                   '1_b1_all_steps_random_path_custom_distr_newPathWithCV.RDS')
  step2_files <- c('2_a1_step_extents_LUT_random_path_custom_distr_newPathWithCV.csv')
  step3_files <- c('3_a1_modis_images_random_path_custom_distr_newPathWithCV')
  # step3_files <- c('3_b1_modis_images_downscaling_random_path_custom_distr_newPathWithCV',
  #                  '3_b2_landsat_images_downscaling_random_path_custom_distr_newPathWithCV')
  stepD_files <- c('3_g1_downscaled_modis_images_30m_ranger_full_newPathWithCV')
  step4_files <- c('4_a1_cov_resp_dataset_random_path_custom_distr_newPathWithCV.csv')
  step5_files <- c('5_a1_elephant_movement_map_random_path_custom_distr_newPathWithCV.pdf')
  step6_files <- c('6_e6_glm_mean_sd_test_results_random_path_custom_distr_newPathWithCV.csv')
  # step4_files <- c('4_a1_cov_resp_dataset_random_path_custom_distr_downscaling_modis_250m_newPathWithCV.csv',
  #                  '4_a1_cov_resp_dataset_random_path_custom_distr_downscaling_modis_30m_newPathWithCV.csv')
  # step5_files <- c('5_a1_elephant_movement_map_random_path_custom_distr_downscaling_modis_250m_newPathWithCV.pdf',
  #                  '5_a1_elephant_movement_map_random_path_custom_distr_downscaling_modis_30m_newPathWithCV.pdf')
  # step6_files <- c('6_c8_glm_custom_mean_sd_confusion_matrix_random_path_custom_distr_downscaling_modis_250m_newPathWithCV.RDS',
  #                  '6_c8_glm_custom_mean_sd_confusion_matrix_random_path_custom_distr_downscaling_modis_30m_newPathWithCV.RDS')
  # step6_files <- c('6_b0_clr_50p_sd_model_random_path_custom_distr_scaled.RDS', '6_b0_glm_50p_sd_model_random_path_custom_distr_scaled.RDS',
  #                  '6_b1_clr_50p_sd_coefs_random_path_custom_distr_scaled.csv', '6_b2_clr_50p_sd_tests_random_path_custom_distr_scaled.csv',
  #                  '6_a1_correlation_matrix_random_path_custom_distr.png', '6_b3_glm_50p_sd_coefs_random_path_custom_distr_scaled.csv',
  #                  '6_b4_glm_50p_sd_deviances_random_path_custom_distr_scaled.csv', '6_b5_glm_50p_sd_vif_random_path_custom_distr_scaled.csv')
  # step7_files <- c('7_b1_50p_sd_plot_log_odds_random_path_custom_distr_scaled.png', '7_b2_50p_sd_plot_odd_ratios_random_path_custom_distr_scaled.png',
  #                 '7_b3_50p_sd_plot_curve_random_path_custom_distr_scaled.png')
  
  # check if each folder has the correct files and mark the answer in table 
  if(all(step1_files %in% data_files)){step1 = T}else{step1 = F}
  if(all(step2_files %in% data_files)){step2 = T}else{step2 = F}
  if(all(step3_files %in% data_files) & length(list.files(paste0(data_path, step3_files))) > 1){step3 = T}else{step3 = F}
  if(all(stepD_files %in% data_files) & length(list.files(paste0(data_path, stepD_files))) > 1){stepD = T}else{stepD = F}
  if(all(step4_files %in% data_files)){step4 = T}else{step4 = F}
  if(all(step5_files %in% output_files)){step5 = T}else{step5 = F}
  if(all(step6_files %in% output_files)){step6 = T}else{step6 = F}
  #if(all(step7_files %in% output_files)){step7 = T}else{step7 = F}
  
  # check if all steps complete 
  if(all(c(step1, step2, step3, stepD, step4, step5, step6) == T)){complete = T}else{complete = F}
  
  # fill entry 
  entry <- data.frame(ID = ID, week = week, method = method, downscaling = downscaling, step1 = step1, step2 = step2, step3 = step3, stepD = stepD, step4 = step4, step5 = step5, step6 = step6, complete = complete)
  
  df_progress <- rbind(df_progress, entry)
  
}


# s <- read.csv('data/run_settings_LTS_LA11_LA12_LA13_LA14.csv', row.names = 1)
# s26 <- data.frame(ID = 'LA11', week = seq(2195, 2271), pseudo_abs_method = 'random_path_custom_distr', downscaling = 'NULL', downscaling_model = 'NULL')
# 
# s <- rbind(s, s26)
# s <- s[!duplicated(s),]
# 
# write.csv(s, 'data/run_settings_LTS_full.csv')

s <- read.csv('data/run_settings_LTS_full.csv', row.names = 1)

# retrieve elephants from STS
s_names <- unique(s$ID)

a <- df_progress[df_progress$step6 == T, c(1:2, 4)]
a$combo <- paste(a$ID, a$week, a$downscaling, sep = '_')

s$combo <- paste(s$ID, s$week, s$downscaling, sep = '_')

s <- s[s$combo %in% a$combo,]
s <- s[,1:5]

# remove faulty LA12 and LA13 data
s <- s[!(s$ID %in% c('LA12', 'LA13') & s$week %in% seq(2175, 2193)),]

write.csv(s, 'data/run_settings_LTS_newPathWithCV_temp.csv')


# 
# d <- read.csv('output/LTS_df_results_aggregated.csv')[,c('ID', 'week', 'date', 'predictor', 'glm_value')]
# d <- d[d$predictor == 'Avg. NDVI',]
# 
# d <- d %>% group_by(date) %>% mutate(coef = mean(glm_value))
# 
# dc <- d[,c('date', 'coef')]
# 
# dc <- dc[!duplicated(dc),]
# 
# ggplot(data = dc, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = coef)) + geom_line()

# dfr <- df_progress$week[df_progress$step3 == F]
# print(dfr)
# 
# rr <- run_settings[run_settings$week %in% dfr,]
# 
# write.csv(rr, 'data/run_settings_LA14_LTS_rerun2.csv')

# # remove rows that are duplicated 
# run_settings <- run_settings[!duplicated(run_settings), ]
# 
# write.csv(run_settings, 'data/run_settings_LA14_LTS_full.csv')

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

# make consistent the downscaling setting 
# r <- run_settings[run_settings$ID == 'LA14' & run_settings$downscaling == F,]
# write.csv(r, 'data/run_settings_downscaling_LA14_250_rescaling.csv')

# retrieve weeks that are true for single elephant dataset
dfr <- df_progress$week[df_progress$step6 == T]
print(dfr)

run_settings <- run_settings[run_settings$week %in% dfr,]


# r <- run_settings
# run_settings <- rbind(run_settings, r)
# range(run_settings$week)
# rownames(run_settings) <- 1:nrow(run_settings)
# write.csv(run_settings, 'data/run_settings_LTS_LA11_LA12_LA13_LA14.csv')

# # remove the faulty run of LA13 w2181 that has weird steps without error but causes error in modeling 
# run_settings <- run_settings[rownames(run_settings) != 486,]
# write.csv(run_settings, 'data/run_settings_LTS_LA11_LA12_LA13_LA14.csv')

#modify run table to only include datasets that passed phase 1 successfully
#a <- df_progress[df_progress$step2 == T & df_progress$step4 == F, 1:2]

# to select when have multiple elephants 
a <- df_progress[df_progress$step6 == F, c(1:2, 4)]
a$combo <- paste(a$ID, a$week, a$downscaling, sep = '_')

run_settings$combo <- paste(run_settings$ID, run_settings$week, run_settings$downscaling, sep = '_')

run_settings <- run_settings[run_settings$combo %in% a$combo,]
run_settings <- run_settings[,1:7]
row.names(run_settings) <- 1:nrow(run_settings)
# 
# # change downscaling label because it causes issues that i don't get 
# run_settings$downscaling[run_settings$downscaling == 'NULL'] <- 'nope'
# run_settings$downscaling_model[run_settings$downscaling_model == 'NULL'] <- 'nope'
# row.names(run_settings) <- 1:nrow(run_settings)

write.csv(run_settings, 'data/run_settings_LTS_rerun.csv')
#write.csv(run_settings, 'data/run_settings_new_path_with_CV_second_batch.csv')
#write.csv(run_settings, 'data/run_settings_new_path_with_CV_until_220.csv')

# get indices of runs to remove from dataset
rownames(run_settings) <- 1:nrow(run_settings)
remove <- rownames(run_settings[(run_settings$ID == 'LA11' & run_settings$week %in% seq(2272, 2281)) | 
                       (run_settings$ID == 'LA26' & run_settings$week %in% seq(2065, 2079)),])
run_settings <- run_settings[!(rownames(run_settings) %in% remove), ]

# can also remove LA11 2243 --> there is no data that single week 
run_settings <- run_settings[-851,]

write.csv(run_settings, 'data/run_settings_all_runs_new_path_with_CV.csv')

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

# f <- run_settings
# ff <- rbind(f, run_settings)
# write.csv(ff, 'data/run_settings_downscaling_full.csv')

# remove weeks that don't have enough data = LA11 2085, 2103, 2243
# r <- run_settings[!(run_settings$ID == 'LA11' & run_settings$week %in% c(2085, 2103, 2243)),]
# r <- r[!(r$ID %in% c('LA12', 'LA13') & r$week >= 2175),]
# r <- r[!(r$ID == 'LA14' & r$week == 2242),]
# r$input_suffix <- '_newPathWithCV'
# r$output_suffix <- '_newPathWithCV'
# r <- data.frame(ID = 'LA11', week = seq(2140, 2147), pseudo_abs_method = 'random_path_custom_distr',
#                 downscaling = 'NULL', downscaling_model = 'NULL', input_suffix = '_newPathWithCV', output_suffix = '_newPathWithCV')

# r <- data.frame(ID = 'LA11', week = c(2152, seq(2154, 2164)), pseudo_abs_method = 'random_path_custom_distr', 
#                 downscaling = 'NULL', downscaling_model = 'NULL', input_suffix = '_newPathWithCV', output_suffix = '_newPathWithCV')
# run_settings <- rbind(run_settings, r)
# run_settings <- run_settings[order(run_settings$week),]
# 
# write.csv(run_settings, 'data/run_settings_LTS_final.csv')
# 
# run_settings$input_suffix <- '_newPathWithCV'
# run_settings$output_suffix <- '_newPathWithCV'
# r <- data.frame(ID = 'LA4', week = 2084, pseudo_abs_method = 'random_path_custom_distr',
#                 downscaling = 'NULL', downscaling_model = 'NULL', input_suffix = '_newPathWithCV', output_suffix = '_newPathWithCV')
# run_settings <- rbind(run_settings, r)
# run_settings <- run_settings[order(run_settings$week),]
# r <- run_settings[!(run_settings$ID == 'LA11' & run_settings$week %in% c(2085)),]
# row.names(run_settings) <- 1:nrow(run_settings)
# write.csv(run_settings, 'data/run_settings_downscaling_final.csv')
# write.csv(r, 'data/run_settings_STS_final.csv')

run_settings$downscaling_model[run_settings$downscaling_model == 'ranger_full_selection'] <- 'ranger_full'
write.csv(run_settings, 'data/run_settings_downscaling_final.csv')
r <- run_settings[run_settings$downscaling == T,]
write.csv(r, 'data/run_settings_downscaling_rerun.csv')

# trying to understand why some runs didn't pass phases 
# LA11 2242 --> not even phase 1 
e <- read.csv('data/elephant_etosha/africanElephantEtoshaNP_analysis.csv')
d <- readRDS('data/LA14/2069/1_a1_elephant_full_track_xyt.RDS')
a <- readRDS('data/LA11/2142/1_a2_elephant_track_xyt.RDS')



############## CREATE SUMMARY TABLE ##################################


# create empty summary results table 
summary_results <- data.frame()

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
  
  # define if downscaled or not 
  downscaling_setting <- run[[4]]
  
  # replace NA from suffix columns of run settings to empty strings
  if(is.na(run[[6]])){run[[6]] <- ''}
  if(is.na(run[[7]])){run[[7]] <- ''}
  
  # define input and output suffixes
  input_suffix <- run[[6]]
  output_suffix <- run[[7]]
  
  # define run filepath 
  run_filepath <- paste0('output/', ID, '/', week, '/')
  
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
  
  if(input_suffix == '_newPathWithCV_withMean'){
    pred <- 'mean'
  } else{ pred <- '50p'}
  
  suffix <- paste0(suffix, input_suffix)
  
  # get date 
  dfile <- read.csv(paste0('data/', ID, '/', week, '/2_a1_step_extents_LUT_', pseudo_abs_method, '_newPathWithCV', '.csv'), row.names = 1)
  date <- dfile$start_date[1]
  
  # create results data entry 
  # entry <- data.frame(ID = ID, week = week, date = date, pseudo_abs_method = pseudo_abs_method, downscaling = downscaling_setting, 
  #                     VIF_full = NA, full_glm_sig_coef = NA, full_glm_sig_coef_which = NA, full_glm_deviance = NA, full_glm_sig = NA, full_clr_sig_coef = NA, full_clr_sig_coef_which = NA, full_clr_concord = NA, full_clr_concord_se = NA, 
  #                     VIF_sub = NA, sub_glm_sig_coef = NA, sub_glm_sig_coef_which = NA, sub_glm_deviance = NA, sub_glm_sig = NA, sub_clr_sig_coef = NA, sub_clr_sig_coef_which = NA, sub_clr_concord = NA, sub_clr_concord_se = NA)
  # 
  
  entry <- data.frame(ID = ID, week = week, date = date, pseudo_abs_method = pseudo_abs_method, downscaling = downscaling_setting, suffix = input_suffix,
                      VIF = NA, glm_sig_coef = NA, glm_sig_coef_which = NA, glm_deviance = NA, glm_sig = NA, glm_AIC = NA,
                      glm_CV_sig_coef = NA, glm_CV_sig_coef_which = NA, glm_CV_deviance = NA, glm_CV_sig = NA, glm_CV_AIC = NA,
                      glm_CV_threshold = NA, glm_CV_ROC = NA, glm_CV_sens = NA, glm_CV_spec = NA, 
                      glm_CV_test_sens = NA, glm_CV_test_spec = NA, glm_CV_test_precision = NA, glm_CV_test_F1 = NA, glm_CV_test_accuracy = NA, 
                      clr_sig_coef = NA, clr_sig_coef_which = NA, clr_concord = NA, clr_concord_se = NA)
  
  ## are the VIF values of the model acceptable? 
  # retrieve the dataset
  #full_df <- read.csv(paste0(run_filepath, '6_a5_glm_full_vif_', pseudo_abs_method, '.csv'))
  sub_df <- read.csv(paste0(run_filepath, '6_c5_glm_', pred, '_sd_vif_', pseudo_abs_method, suffix, '.csv'))
  
  # check if vif values below 5
  #if(any(full_df$vif_results <5)){entry$VIF_full <- T}else{entry$VIF_full <- F}
  if(any(sub_df$vif_results >5)){entry$VIF <- F}else{entry$VIF <- T}
  
  ## does the glm model have a significant coefficient? 
  # retrieve dataset
  #full_df <- read.csv(paste0(run_filepath, '6_a3_glm_full_coefs_', pseudo_abs_method, '.csv'))
  sub_df <- read.csv(paste0(run_filepath, '6_c3_glm_', pred, '_sd_coefs_', pseudo_abs_method, suffix, '.csv'))
  cv_df <- read.csv(paste0(run_filepath, '6_c3_glm_custom_', pred, '_sd_coefs_', pseudo_abs_method, suffix, '.csv'))
  
  # exclude intercept 
  #full_df <- full_df[full_df$X != '(Intercept)', ]
  sub_df <- sub_df[sub_df$X != '(Intercept)', ]
  cv_df <- cv_df[cv_df$X != '(Intercept)', ]
  
  # check if pvalues below 0.05
  #if(any(full_df$Pr...z.. <=0.05)){entry$full_glm_sig_coef <- T}else{entry$full_glm_sig_coef <- F}
  if(any(sub_df$Pr...z.. <=0.05)){entry$glm_sig_coef <- T}else{entry$glm_sig_coef <- F}
  if(any(cv_df$Pr...z.. <=0.05)){entry$glm_CV_sig_coef <- T}else{entry$glm_CV_sig_coef <- F}

  # select predictors that are significant with 95% confidence 
  # source: https://sparkbyexamples.com/r-programming/r-merge-vector-to-string/
  #entry$full_glm_sig_coef_which <- paste(full_df$X[full_df$Pr...z.. < 0.05], collapse = '; ')
  entry$glm_sig_coef_which <- paste(sub_df$X[sub_df$Pr...z.. <= 0.05], collapse = '; ')
  entry$glm_CV_sig_coef_which <- paste(cv_df$X[cv_df$Pr...z.. <= 0.05], collapse = '; ')
  
  ## how high is the deviance of the fitted model? is the model significant? 
  # retrieve dataset
  #full_df <- read.csv(paste0(run_filepath, '6_a4_glm_full_deviances_', pseudo_abs_method, '.csv'))
  sub_df <- read.csv(paste0(run_filepath, '6_c4_glm_', pred, '_sd_deviances_', pseudo_abs_method, suffix, '.csv'))
  cv_df <- read.csv(paste0(run_filepath, '6_c4_glm_custom_', pred, '_sd_deviances_', pseudo_abs_method, suffix, '.csv'))
  
  # select residual deviance 
  #entry$full_glm_deviance <- full_df$residual_deviance
  entry$glm_deviance <- sub_df$residual_deviance
  entry$glm_CV_deviance <- cv_df$residual_deviance
  
  # derive p-value of model from chi-square stat
  # source: https://stats.stackexchange.com/questions/340489/interpretation-of-deviance-in-logistic-model
  # source: https://www.r-bloggers.com/2022/05/calculate-the-p-value-from-chi-square-statistic-in-r/
  #full_pval <- pchisq(full_df$null_deviance - full_df$residual_deviance, full_df$null_df - full_df$residual_df, lower.tail = F)
  sub_pval <- pchisq(sub_df$null_deviance - sub_df$residual_deviance, sub_df$null_df - sub_df$residual_df, lower.tail = F)
  cv_pval <- pchisq(cv_df$null_deviance - cv_df$residual_deviance, cv_df$null_df - cv_df$residual_df, lower.tail = F)
  
  # check if pvalues below 0.05
  #if(full_pval <=0.05){entry$full_glm_sig <- T}else{entry$full_glm_sig <- F}
  # if(sub_pval <=0.05){entry$glm_sig <- T}else{entry$glm_sig <- F}
  # if(cv_pval <=0.05){entry$glm_CV_sig <- T}else{entry$glm_CV_sig <- F}
  # 
  entry$glm_sig <- sub_pval
  entry$glm_CV_sig <- cv_pval
  
  
  # select aic value
  entry$glm_AIC <- sub_df$AIC
  entry$glm_CV_AIC <- cv_df$AIC
  
  # what are the cross validation results? 
  cv_df <- read.csv(paste0(run_filepath, '6_c6_glm_custom_', pred, '_sd_CV_results_', pseudo_abs_method, suffix, '.csv'))
  
  # retrieve performance metrics from k-fold cross validation 
  entry$glm_CV_threshold <- cv_df$threshold
  entry$glm_CV_ROC <- cv_df$ROC
  entry$glm_CV_sens <- cv_df$Sens
  entry$glm_CV_spec <- cv_df$Spec
  
  # what are the testing results after cross validation? 
  cv_df <- read.csv(paste0(run_filepath, '6_c7_glm_custom_', pred, '_sd_test_results_', pseudo_abs_method, suffix, '.csv'), header = T)

  # retrieve performance metrics 
  entry$glm_CV_test_sens <- cv_df[1,2]
  entry$glm_CV_test_spec <- cv_df[2,2]
  entry$glm_CV_test_precision <- cv_df[5,2]
  entry$glm_CV_test_F1 <- cv_df[9,2]
  entry$glm_CV_test_accuracy <- cv_df[11,2]
  
  ## does the clr model have a significant coefficient? 
  # retrieve dataset
  #full_df <- read.csv(paste0(run_filepath, '6_a1_clr_full_coefs_', pseudo_abs_method, '.csv'))
  sub_df <- read.csv(paste0(run_filepath, '6_c1_clr_', pred, '_sd_coefs_', pseudo_abs_method, suffix, '.csv'))
  
  # check if pvalues below 0.05
  #if(any(full_df$Pr...z.. <=0.05)){entry$full_clr_sig_coef <- T}else{entry$full_clr_sig_coef <- F}
  if(any(sub_df$Pr...z.. <=0.05)){entry$clr_sig_coef <- T}else{entry$clr_sig_coef <- F}
  
  # select predictors that are significant with 95% confidence 
  # source: https://sparkbyexamples.com/r-programming/r-merge-vector-to-string/
  #entry$full_clr_sig_coef_which <- paste(full_df$X[full_df$Pr...z.. < 0.05], collapse = '; ')
  entry$clr_sig_coef_which <- paste(sub_df$X[sub_df$Pr...z.. < 0.05], collapse = '; ')

  ## what is the concordance? how much does it vary by? 
  # retrieve dataset
  #full_df <- read.csv(paste0(run_filepath, '6_a2_clr_full_tests_', pseudo_abs_method, '.csv'))
  sub_df <- read.csv(paste0(run_filepath, '6_c2_clr_', pred, '_sd_tests_', pseudo_abs_method, suffix, '.csv'))
  
  # select concordance and SE
  # entry$full_clr_concord <- full_df$concordance[1]
  # entry$full_clr_concord_se <- full_df$concordance[4]
  entry$clr_concord <- sub_df$concordance[1]
  entry$clr_concord_se <- sub_df$concordance[4]
  
  # rbind the entry to the summary table 
  summary_results <- rbind(summary_results, entry)
}

summary_results <- summary_results[,grep('full', names(summary_results), invert = T)]

# results for running 13 elephants on week 2075 with three methods 
write.csv(summary_results, paste0('output/summary_results', run_label, '.csv'))
#write.csv(summary_results, 'output/summary_results_comparingMeanMedian_withCV.csv')

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

# 
# d <- data.frame(date = unique(summary_results$date))
# # source: https://stackoverflow.com/questions/22603847/how-to-extract-month-from-date-in-r
# d$season[strftime(as.Date(d$date, tz = 'Africa/Maputo'), '%m') %in% c('11', '12', '01', '02', '03')] <- 'wet'
# d$season[strftime(as.Date(d$date, tz = 'Africa/Maputo'), '%m') %in% c('05', '06', '07', '08', '09')] <- 'dry'
# d$season[strftime(as.Date(d$date, tz = 'Africa/Maputo'), '%m') %in% c('10', '04')] <- 'transition'
# 
# sst_w$date <- d$date
# sst_w$season <- d$season
# 
# plot(as.Date(d, tz = 'Africa/Maputo'), sst_w$percent_sig_models)
# 
# ggplot(data = sst_w, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
#   # source: https://stackoverflow.com/questions/33322061/change-background-color-panel-based-on-year-in-ggplot-r
#   geom_rect(aes(xmin = as.Date(min(sst_w$date[sst_w$season == 'dry']), tz = 'Africa/Maputo'), 
#                 xmax = as.Date(max(sst_w$date[sst_w$season == 'dry']), tz = 'Africa/Maputo'), 
#                 ymin = -Inf, ymax = Inf, fill = 'dry'), alpha = .02) + 
#   geom_rect(aes(xmin = as.Date(max(sst_w$date[sst_w$season == 'dry']), tz = 'Africa/Maputo'), 
#                 xmax = as.Date(min(sst_w$date[sst_w$season == 'wet']), tz = 'Africa/Maputo'), 
#                 ymin = -Inf, ymax = Inf, fill = 'transition'), alpha = .02) + 
#   geom_rect(aes(xmin = as.Date(min(sst_w$date[sst_w$season == 'wet']), tz = 'Africa/Maputo'), 
#                 xmax = as.Date(max(sst_w$date[sst_w$season == 'wet']), tz = 'Africa/Maputo'), 
#                 ymin = -Inf, ymax = Inf, fill = 'wet'), alpha = .02) + 
#   # source: https://www.statology.org/r-geom_path-each-group-consists-of-only-one-observation/
#   geom_line(aes(y = percent_sig_models, group = 1, linetype = 'Significant Models')) + 
#   geom_line(aes(y = percent_large_VIF_models, group = 1, linetype = 'Models with VIF > 5')) + 
#   scale_fill_manual(name = 'Season', values = c('red', 'orange', 'green'), 
#                     # source: https://stackoverflow.com/questions/34772911/alpha-does-not-change-transparency-but-adds-to-ggplot2-legend-with-geom-rect
#                     guide = guide_legend(override.aes = list(alpha = .15))) + 
#   # source: https://stackoverflow.com/questions/40791082/ggplot2-control-linetypes-when-more-than-one-line
#   scale_linetype_manual(values = c(1,2), name = 'Model Type', breaks = c('Significant Models', 'Models with VIF > 5')) + 
#   theme_minimal() + 
#   xlab('Time') + ylab('Proportion of Models') + ggtitle('Proportion of GLM Models per Week over Time')
  








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
  
  suffix <- paste0(suffix, '_scaled')

  
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





# 
# ###################### RQ1 long timeseries 
# 
# # create big run dataset that merges RQ2 long timeseries datasets 
# # l <- list.files('data', pattern = glob2rx('run_settings_RQ2_LA*.csv'), full.names = T)
# # run_table_RQ1 <- data.frame()
# # for(item in l){
# #   entry <- read.csv(item, row.names = 1)
# #   run_table_RQ1 <- rbind(run_table_RQ1, entry)
# # }
# 
# # retrieve all coef values for all 4 datasets and place in large table 
# 
# run_table_RQ1 <- data.frame()
# 
# for(ID in c('LA11', 'LA12', 'LA13', 'LA14')){
#   
#   # read run table 
#   run_settings <- read.csv(paste0('data/run_settings_RQ2_', ID, '.csv'), row.names = 1)
#   
#   # read summary table 
#   summary_results <- read.csv(paste0('output/summary_results_RQ2_', ID, '.csv'), row.names = 1)
#   
#   # retrieves summary results for only sig models GLM 
#   srsig <- summary_results[summary_results$sub_glm_sig == T,] # these are the models i want to retrieve coefs for 
#   
#   # get weeks of sig models 
#   sigw <- srsig$week
#   print(length(sigw))
#   
#   # get run table for only weeks that are sig 
#   rsig <- run_settings[run_settings$week %in% sigw,]
#   
#   run_table_RQ1 <- rbind(run_table_RQ1, rsig)
#   
# }
# 
# row.names(run_table_RQ1) <- 1:nrow(run_table_RQ1)
# 
# RQ1_glm_coef <- data.frame()
# 
# s <- ''
# for(i in 1:nrow(run_table_RQ1)){
#   
#   # get run settings
#   ID <- run_table_RQ1$ID[i]
#   week <- run_table_RQ1$week[i]
#   method <- run_table_RQ1$pseudo_abs_method[i]
#   
#   # define filepaths
#   data_path <- paste0('data/', ID, '/', week, '/')
#   output_path <- paste0('output/', ID, '/', week, '/')
#   
#   # retrieve date of week 
#   dfile <- read.csv(paste0(data_path, '2_a1_step_extents_LUT_', method, '.csv'), row.names = 1)
#   date <- dfile$start_date[1]
#   
#   # retrieve coefs glm 
#   c <- read.csv(paste0(output_path, '6_b3_glm_50p_sd_coefs_random_path_custom_distr', s, '.csv'))
#   c <- c[2:nrow(c),]
#   
#   # specify if sig or not with 90% confidence (so pvalue < 0.1)
#   c$Pr...z..[as.numeric(c$Pr...z..) < 0.1] <- 'sig'
#   c$Pr...z..[as.numeric(c$Pr...z..) >= 0.1] <- 'not sig'
#   
#   # create new entry with coefs and pvalues 
#   entry <- data.frame(ID = ID, week = week, method = method, date = date, predictor = c[,1], value = c$Estimate, significance = c$Pr...z..)
#   
#   RQ1_glm_coef <- rbind(RQ1_glm_coef, entry)
# }
# 
# RQ1_glm_coef_sig <- RQ1_glm_coef[RQ1_glm_coef$significance == 'sig',]
# row.names(RQ1_glm_coef_sig) <- 1:nrow(RQ1_glm_coef_sig)
# 
# 
# # manual modifications TEMPORARY
# # RQ1_glm_coef_sig$value[352] <- -100
# # RQ1_glm_coef_sig$value[343] <- -1000
# # RQ1_glm_coef_sig$value[347] <- -1000
# # RQ1_glm_coef_sig$value[353] <- 1000
# # RQ1_glm_coef_sig$value[341] <- -3500
# 
# #RQ1_glm_coef_sig <- RQ1_glm_coef_sig[RQ1_glm_coef_sig$week < 2175,]
# RQ1_glm_coef_sig$combo <- paste0(RQ1_glm_coef_sig$week, RQ1_glm_coef_sig$predictor, sep = '_')
# 
# ag <- aggregate(RQ1_glm_coef_sig$value, list(RQ1_glm_coef_sig$week, RQ1_glm_coef_sig$predictor), FUN = summary)
# agx <- ag$x
# agx_mean <- agx[,4]
# agx_q1 <- agx[,2]
# agx_q3 <- agx[,5]
# agc <- paste0(ag$Group.1, ag$Group.2, sep = '_')
# 
# agdf <- data.frame(combo = agc, Q1 = agx_q1, Mean = agx_mean, Q3 = agx_q3)
# 
# # source: https://www.guru99.com/r-merge-data-frames.html
# RQ1_glm_coef_sig <- merge(RQ1_glm_coef_sig, agdf, by.x = 'combo')
# 
# start_date <- min(RQ1_glm_coef_sig$date)
# end_date <- max(RQ1_glm_coef_sig$date)
# elephant <- '4 elephants'
# 
# #library(ggplot2)
# # png(paste0('output/timeseries_glm_coef', '_RQ1_LTS', '_extended.png'))
# # ggplot(data = RQ1_glm_coef_sig, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = value, group = predictor, color = ID)) + 
# #   geom_line() + geom_point(data = RQ1_glm_coef_sig, aes(y = value), shape = 8) + 
# #   geom_hline(yintercept = 0) + 
# #   # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
# #   facet_grid(vars(predictor), scale = 'free') + 
# #   xlab('Time') + ylab('Coefficient Value') + ggtitle(paste0('Timeseries of GLM predictors for ', elephant, ' from August 2009 to January 2012'))
# # dev.off()  
# 
# # ggplot(data = RQ1_glm_coef, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = value, group = predictor, color = ID)) + 
# #   geom_line() + geom_point(data = RQ1_glm_coef[RQ1_glm_coef$significance == 'sig',], aes(y = value), shape = 8) + 
# #   geom_hline(yintercept = 0) + 
# #   # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
# #   facet_grid(vars(predictor), scale = 'free') + 
# #   xlab('Time') + ylab('Coefficient Value') + ggtitle(paste0('Timeseries of GLM predictors for ', elephant, ' from August 2009 to January 2012'))
# 
# ggplot(data = RQ1_glm_coef_sig, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = value, group = predictor)) + 
#   #geom_point(data = RQ1_glm_coef_sig, aes(y = value), shape = 8, colour = 'black') + 
#   # source: https://stackoverflow.com/questions/14704909/plotting-depth-range-in-time-series-using-ggplot
#   geom_ribbon(aes(ymin = Q1, ymax = Q3), fill = 'grey') + 
#   geom_line(aes(y = Mean), color = 'blue') +
#   geom_hline(yintercept = 0, linetype = 'dashed') + 
#   # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
#   facet_grid(vars(predictor), scale = 'free') + 
#   xlab('Time') + ylab('Coefficient Value') + ggtitle(paste0('Averaged time-series of estimated model coefficients for ', elephant, ' from ', start_date, ' to ', end_date)) + 
#   theme_minimal()
# 
# 
# 





###################### RQ2 short TS
# 
# # create run table with only sig models 
# RQ2_run_table <- data.frame()

# fill new run table with only runs that give sig models 
# srsig <- summary_results[summary_results$sub_glm_sig == T,] # these are the models i want to retrieve coefs for 
# 
# # get ID and weeks of sig models 
# srsig$combo <- paste(srsig$ID, srsig$week, sep = '_')
# 
# run_settings$combo <- paste(run_settings$ID, run_settings$week, sep = '_')

#RQ2_run_table <- run_settings[run_settings$combo %in% srsig$combo,]

STS_run_table <- read.csv('data/run_settings_STS_final.csv', row.names = 1)
#STS_run_table <- run_settings
STS_run_table <- STS_run_table[,1:5]

row.names(STS_run_table) <- 1:nrow(STS_run_table)

STS_coef <- data.frame()

suffix <- '_newPathWithCV'

for(i in 1:nrow(STS_run_table)){
  
  # get run settings
  ID <- STS_run_table$ID[i]
  week <- STS_run_table$week[i]
  method <- STS_run_table$pseudo_abs_method[i]
  
  # define filepaths
  data_path <- paste0('data/', ID, '/', week, '/')
  output_path <- paste0('output/', ID, '/', week, '/')
  

  # load and retrieve difference in deviance 
  dev_df <- read.csv(paste0(output_path, '6_e4_glm_mean_sd_deviances_', method, suffix, '.csv'))
  
  # retrieve AIC
  aic <- dev_df$AIC
  
  # calculate deviance improvement 
  dev_diff <- dev_df$null_deviance - dev_df$residual_deviance
  
  # calculate chi-square from deviance to get model significance 
  # source: https://stats.stackexchange.com/questions/340489/interpretation-of-deviance-in-logistic-model
  # source: https://www.r-bloggers.com/2022/05/calculate-the-p-value-from-chi-square-statistic-in-r/
  dev_sig <- pchisq(dev_diff, dev_df$null_df - dev_df$residual_df, lower.tail = F)
  
  # check if pvalues below 0.05
  if(dev_sig <=0.05){dev_sig <- 'sig'}else{dev_sig <- 'not sig'}
  
  # load and retrieve VIF 
  vif_df <- read.csv(paste0('output/', ID, '/', week, '/6_e5_glm_mean_sd_vif_', method, suffix, '.csv'), row.names = 1, header = T)[,2]
  names(vif_df) <- 'vif_results'
  
  # retrieve date of week 
  dfile <- read.csv(paste0(data_path, '2_a1_step_extents_LUT_', method, '.csv'), row.names = 1)
  date <- dfile$start_date[1]
  
  # retrieve coefs glm 
  c <- read.csv(paste0(output_path, '6_e3_glm_mean_sd_coefs_random_path_custom_distr', suffix, '.csv'))
  c <- c[2:nrow(c),]
  c_clr <- read.csv(paste0(output_path, '6_c1_clr_mean_sd_coefs_random_path_custom_distr', suffix, '.csv'), header = T)
  names(c_clr) <- c('predictor', 'coef', 'exp.coef.', 'se.coef.', 'z', 'Pr...z..')
  # reorder rows to match order of GLM predictors 
  # source: https://www.statology.org/dplyr-arrange-custom-order/
  c_clr <- c_clr %>% arrange(match(predictor, c('ndvi_mean_scaled', 'ndvi_rate_mean_scaled', 'ndvi_rate_sd_scaled', 'ndvi_sd_scaled')))
  
  # specify if sig or not with 90% confidence (so pvalue < 0.1)
  c$Pr...z..[as.numeric(c$Pr...z..) < 0.05] <- 'sig'
  c$Pr...z..[as.numeric(c$Pr...z..) >= 0.05] <- 'not sig'
  c_clr$Pr...z..[as.numeric(c_clr$Pr...z..) < 0.05] <- 'sig'
  c_clr$Pr...z..[as.numeric(c_clr$Pr...z..) >= 0.05] <- 'not sig'
  
  # load validation results 
  test_results <- read.csv(paste0(output_path, '6_e6_glm_mean_sd_test_results_random_path_custom_distr', suffix, '.csv'), header = T)[,2:10]
  
  # create new entry with coefs and pvalues and performance metrics 
  entry <- data.frame(ID = ID, week = week, method = method, date = date, predictor = c[,'predictor'],
                      VIF = vif_df, AIC = aic, deviance_improvement = dev_diff, model_sig = dev_sig,
                      glm_value = c$Estimate, glm_significance = c$Pr...z.., test_results,
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
#STS_coef_m[STS_coef_m$ID == 'LA11' & STS_coef_m$week == 2085, c('glm_value', 'clr_value')] <- NA


start_date <- min(STS_coef_m$date)
end_date <- max(STS_coef_m$date)
elephant <- '12 elephants'

# # plot all at once - GLM
# png('output/STS_timeseries_glm_coef.png')
# ggplot(data = STS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = glm_value, group = interaction(predictor, ID), color = ID)) + 
#   geom_line() + 
#   geom_point(data = STS_coef_m[STS_coef_m$glm_significance == 'sig',], aes(y = glm_value), shape = 8) + 
#   geom_hline(yintercept = 0) + 
#   facet_grid(vars(predictor), scale = 'free') + 
#   xlab('Time') + ylab('Coefficient Value') + ggtitle(paste0('Timeseries of GLM predictors for ', elephant, ' from ', start_date, ' to ', end_date))
# dev.off()
# 
# # plot all at once - CLR
# png('output/STS_timeseries_clr_coef.png')
# ggplot(data = STS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = clr_value, group = interaction(predictor, ID), color = ID)) + 
#   geom_line() + 
#   geom_point(data = STS_coef_m[STS_coef_m$clr_significance == 'sig',], aes(y = clr_value), shape = 8) + 
#   geom_hline(yintercept = 0) + 
#   facet_grid(vars(predictor), scale = 'free') + 
#   xlab('Time') + ylab('Coefficient Value') + ggtitle(paste0('Timeseries of CLR predictors for ', elephant, ' from ', start_date, ' to ', end_date))
# dev.off()


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
ag_glm_sig <- aggregate(STS_coef_m$glm_significance, list(STS_coef_m$week, STS_coef_m$predictor), FUN = relative_sum)$x
ag_clr_sig <- aggregate(STS_coef_m$clr_significance, list(STS_coef_m$week, STS_coef_m$predictor), FUN = relative_sum)$x


agc <- paste0(ag_glm$Group.1, ag_glm$Group.2, sep = '_')

agdf <- data.frame(combo = agc, total_models = ag_model, VIF_Q1 = ag_vif_q1, VIF_Mean = ag_vif_m, VIF_Q2 = ag_vif_q2, VIF_Q3 = ag_vif_q3,
                   DEV_IMP_Q1 = ag_di_q1, DEV_IMP_Mean = ag_di_m, DEV_IMP_Q2 = ag_di_q2, DEV_IMP_Q3 = ag_di_q3,
                   GLM_Q1 = ag_glm_q1, GLM_Mean = ag_glm_m, GLM_Q2 = ag_glm_q2, GLM_Q3 = ag_glm_q3, 
                   CLR_Q1 = ag_clr_q1, CLR_Mean = ag_clr_m, CLR_Q2 = ag_clr_q2, CLR_Q3 = ag_clr_q3, 
                   Sig_Model_Proportion = ag_model_sig, Sig_GLM_Proportion = ag_glm_sig, Sig_CLR_Proportion = ag_clr_sig)

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

# 
# print(length(unique(STS_coef_m$week)) == length(unique(STS_coef_m$date)))
# 
# STS_coef_m$date[STS_coef_m$week == 2065] <- min(STS_coef_m$date)
# #STS_coef_m$date[STS_coef_m$week == 2074] <- summary_results$date[129]
# STS_coef_m$date[STS_coef_m$week == 2080] <- min(STS_coef_m$date[STS_coef_m$week == 2080])
# STS_coef_m$date[STS_coef_m$week == 2082] <- min(STS_coef_m$date[STS_coef_m$week == 2082])
# STS_coef_m$date[STS_coef_m$week == 2084] <- min(STS_coef_m$date[STS_coef_m$week == 2084])
# #STS_coef_m$date[STS_coef_m$week == 2085] <- summary_results$date[21]


d <- data.frame()

for(i in unique(STS_coef$week)){
  df <- STS_coef_m$date[STS_coef_m$week == i]
  entry <- data.frame(week = i, n = length(unique(df)))
  d <- rbind(d, entry)
}

for(i in d$week[d$n > 1]){
  STS_coef_m$date[STS_coef_m$week == i] <- min(STS_coef_m$date[STS_coef_m$week == i])
}

print(length(unique(STS_coef_m$week)) == length(unique(STS_coef_m$date)))


# addign a column for proportion of sig models --> have to aggregate because unique would omit some values that repeat for diff weeks
# ag_sig <- aggregate(STS_coef_m$Sig_Model_Proportion, list(STS_coef_m$week), FUN = mean)$x
# ag_di <- aggregate(STS_coef_m$deviance_improvement, list(STS_coef_m$week), FUN = mean)$x
# #total_m <- aggregate(STS_coef_m$Sig_Model_Proportion, list(STS_coef_m$week, STS_coef_m$predictor), FUN = length)$x

# retrieve the number of models from sst_w df 
df_bar <- data.frame(STS_coef_m[!duplicated(STS_coef_m$date),c('date', 'total_models', 'Sig_Model_Proportion', 'DEV_IMP_Mean')])
# df_bar <- data.frame(week = unique(STS_coef_m$date), total_models = STS_coef_m$total_models, deviance_improvement = ag_di, sig_models = ag_sig)

# change label names of predictors 
STS_coef_m$predictor[STS_coef_m$predictor == 'ndvi_mean_scaled'] <- 'Avg. NDVI'
STS_coef_m$predictor[STS_coef_m$predictor == 'ndvi_sd_scaled'] <- 'Dev. NDVI'
STS_coef_m$predictor[STS_coef_m$predictor == 'ndvi_rate_mean_scaled'] <- 'Avg. NDVI Growth Rate'
STS_coef_m$predictor[STS_coef_m$predictor == 'ndvi_rate_sd_scaled'] <- 'Dev. NDVI Growth Rate' 

# save STS coef table 
write.csv(STS_coef_m, 'output/STS_df_results_aggregated_newPath10fCV_final.csv')
#write.csv(STS_coef_m, 'output/STS_df_results_aggregated_newPathWithCV.csv')


# # retrieve dates that want to highlight 
# date_hl <- unique(STS_coef_m$date[grep('-09-', STS_coef_m$date)])
# 
# # get a list of dates by year
# # source: https://r-coder.com/split-r/
# date_hl <- split(date_hl, f = sub('-.*', '', date_hl))
# 
# # get start and end dates for each year
# # source: https://stackoverflow.com/questions/43425540/get-max-min-values-from-a-list-of-dataframes-without-loop
# date_hl <- sapply(date_hl, function(x) range(x))
# 
# # convert to data frame (doesn't work if matrix) of xmin and xmax 
# date_hl <- data.frame(t(date_hl))
# colnames(date_hl) <- c('xmin', 'xmax')
# 
# # convert dates to date object
# date_hl$xmin <- as.Date(date_hl$xmin, tz = 'Africa/Maputo')
# date_hl$xmax <- as.Date(date_hl$xmax, tz = 'Africa/Maputo')
STS_coef_m <- read.csv('output/STS_df_results_aggregated_newPath10fCV_final.csv')



png('output/STS_timeseries_glm_coef_aggregated_newPath10fCV_final.png')

bp <- ggplot(data = df_bar, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
  geom_bar(aes(y = total_models, fill = Sig_Model_Proportion), stat = 'identity', position = 'dodge', show.legend = F) +
  #geom_histogram(aes(y = total_models, color = Sig_Model_Proportion), stat = 'identity') +
  #scale_color_grey(name = '% Significant Models') + 
  # scale_fill_continuous(name = '% Significant Models', type = 'gradient') +
  scale_fill_gradientn(name = '% Model with Significance', colors = c('#525174', '#348aa7', '#5dd39e', '#bce784')) +
  ylab('No. of Elephants') + xlab('Time') +
  theme(legend.position = 'none') + 
  theme_minimal()

lp <- ggplot(data = STS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
  #geom_bar(aes(y = total_models, color = Sig_Model_Proportion)) +
  # source: https://stackoverflow.com/questions/29648907/using-geom-rect-for-time-series-shading-in-r
  #geom_rect(data = date_hl, mapping=aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf, fill = 'Transition Period'), alpha = 0.1, show.legend = F) +
  # source: https://stackoverflow.com/questions/14704909/plotting-depth-range-in-time-series-using-ggplot
  # source: https://stackoverflow.com/questions/28648698/alpha-transparency-not-working-in-ggplot2
  geom_ribbon(data = STS_coef_m, aes(ymin = GLM_Q1, ymax = GLM_Q3, fill = 'Qu. Range', group = predictor), alpha = 0.5, show.legend = F) + 
  geom_line(aes(y = GLM_Mean, group = predictor, color = Sig_GLM_Proportion, linetype = 'Mean'), linewidth = 1, show.legend = F) +
  #geom_line(aes(y = GLM_Q2, color = 'Median')) +
  geom_hline(yintercept = 0, linetype = 'dashed') + 
  # source: https://www.geeksforgeeks.org/combine-and-modify-ggplot2-legends-with-ribbons-and-lines/
  scale_linetype_manual(name = 'Function', values = c('Mean' = 'solid')) +
  # source: https://www.geeksforgeeks.org/how-to-remove-legend-title-in-r-with-ggplot2/
  scale_fill_manual(name = 'Margins', values = c('Qu. Range' = 'grey')) + 
  #scale_fill_manual(name = 'Margins', values = c('Qu. Range' = 'grey', 'Transition Period' = 'orange')) + 
  #scale_color_continuous(name = '% Significant Models', type = 'gradient') + 
  scale_color_gradientn(name = '% Model with Significance', colors = c('#525174', '#348aa7', '#5dd39e', '#bce784')) +
  guides(colour = guide_colourbar(order = 3),
         linetype = guide_legend(order = 1),
         fill = guide_legend(order = 2)) +
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') + 
  xlab('Time') + ylab('Coefficient Value') + ggtitle('Aggregated time-series of estimated GLM model coefficients', subtitle = paste0(elephant, ' from ', start_date, ' to ', end_date)) + 
  theme_minimal() 
  # source: https://stackoverflow.com/questions/68719513/ggplot-wont-remove-axis-ticks
  #theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
  
lp_leg <- ggplot(data = STS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
  #geom_rect(data = date_hl, mapping=aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf, fill = 'Transition Period'), alpha = 0.1) +
  # source: https://stackoverflow.com/questions/14704909/plotting-depth-range-in-time-series-using-ggplot
  # source: https://stackoverflow.com/questions/28648698/alpha-transparency-not-working-in-ggplot2
  geom_ribbon(data = STS_coef_m, aes(ymin = GLM_Q1, ymax = GLM_Q3, fill = 'Qu. Range', group = predictor), alpha = 0.5) + 
  geom_line(aes(y = GLM_Mean, group = predictor, color = Sig_GLM_Proportion, linetype = 'Mean'), linewidth = 1) +
  # source: https://www.geeksforgeeks.org/combine-and-modify-ggplot2-legends-with-ribbons-and-lines/
  scale_linetype_manual(name = 'Function', values = c('Mean' = 'solid')) +
  # source: https://www.geeksforgeeks.org/how-to-remove-legend-title-in-r-with-ggplot2/
  scale_fill_manual(name = 'Margins', values = c('Qu. Range' = 'grey')) + 
  #scale_fill_manual(name = 'Margins', values = c('Qu. Range' = 'grey', 'Transition Period' = 'orange')) + 
  #scale_color_continuous(name = '% Significant Models', type = 'gradient') + 
  scale_color_gradientn(name = '% Model with Significance', colors = c('#525174', '#348aa7', '#5dd39e', '#bce784')) +
  guides(colour = guide_colourbar(order = 3),
         linetype = guide_legend(order = 1),
         fill = guide_legend(order = 2)) +
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') 


# source: https://wilkelab.org/cowplot/reference/get_legend.html
lp_leg <- get_legend(lp_leg)

# source: https://wilkelab.org/cowplot/articles/plot_grid.html
plot_grid(lp, lp_leg, bp, ncol = 2, nrow = 2, rel_heights = c(4,1), rel_widths = c(3,1))

dev.off()



png('output/STS_timeseries_clr_coef_aggregated_newPath10fCV_final.png')

bp <- ggplot(data = df_bar, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
  geom_bar(aes(y = total_models, fill = Sig_Model_Proportion), stat = 'identity', position = 'dodge', show.legend = F) +
  #geom_histogram(aes(y = total_models, color = Sig_Model_Proportion), stat = 'identity') +
  #scale_color_grey(name = '% Significant Models') + 
  # scale_fill_continuous(name = '% Significant Models', type = 'gradient') +
  scale_fill_gradientn(name = '% Model with Significance', colors = c('#525174', '#348aa7', '#5dd39e', '#bce784')) +
  ylab('No. of Elephants') + xlab('Time') +
  theme(legend.position = 'none') + 
  theme_minimal()

lp <- ggplot(data = STS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
  #geom_bar(aes(y = total_models, color = Sig_Model_Proportion)) +
  # source: https://stackoverflow.com/questions/29648907/using-geom-rect-for-time-series-shading-in-r
  #geom_rect(data = date_hl, mapping=aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf, fill = 'Transition Period'), alpha = 0.1, show.legend = F) +
  # source: https://stackoverflow.com/questions/14704909/plotting-depth-range-in-time-series-using-ggplot
  # source: https://stackoverflow.com/questions/28648698/alpha-transparency-not-working-in-ggplot2
  geom_ribbon(data = STS_coef_m, aes(ymin = CLR_Q1, ymax = CLR_Q3, fill = 'Qu. Range', group = predictor), alpha = 0.5, show.legend = F) + 
  geom_line(aes(y = CLR_Mean, group = predictor, color = Sig_CLR_Proportion, linetype = 'Mean'), linewidth = 1, show.legend = F) +
  #geom_line(aes(y = CLR_Q2, color = 'Median')) +
  geom_hline(yintercept = 0, linetype = 'dashed') + 
  # source: https://www.geeksforgeeks.org/combine-and-modify-ggplot2-legends-with-ribbons-and-lines/
  scale_linetype_manual(name = 'Function', values = c('Mean' = 'solid')) +
  # source: https://www.geeksforgeeks.org/how-to-remove-legend-title-in-r-with-ggplot2/
  scale_fill_manual(name = 'Margins', values = c('Qu. Range' = 'grey')) + 
  #scale_fill_manual(name = 'Margins', values = c('Qu. Range' = 'grey', 'Transition Period' = 'orange')) + 
  #scale_color_continuous(name = '% Significant Models', type = 'gradient') + 
  scale_color_gradientn(name = '% Model with Significance', colors = c('#525174', '#348aa7', '#5dd39e', '#bce784')) +
  guides(colour = guide_colourbar(order = 3),
         linetype = guide_legend(order = 1),
         fill = guide_legend(order = 2)) +
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') + 
  xlab('Time') + ylab('Coefficient Value') + ggtitle('Aggregated time-series of estimated CLR model coefficients', subtitle = paste0(elephant, ' from ', start_date, ' to ', end_date)) + 
  theme_minimal() 
# source: https://stackoverflow.com/questions/68719513/ggplot-wont-remove-axis-ticks
#theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

lp_leg <- ggplot(data = STS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
  #geom_rect(data = date_hl, mapping=aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf, fill = 'Transition Period'), alpha = 0.1) +
  # source: https://stackoverflow.com/questions/14704909/plotting-depth-range-in-time-series-using-ggplot
  # source: https://stackoverflow.com/questions/28648698/alpha-transparency-not-working-in-ggplot2
  geom_ribbon(data = STS_coef_m, aes(ymin = CLR_Q1, ymax = CLR_Q3, fill = 'Qu. Range', group = predictor), alpha = 0.5) + 
  geom_line(aes(y = CLR_Mean, group = predictor, color = Sig_CLR_Proportion, linetype = 'Mean'), linewidth = 1) +
  # source: https://www.geeksforgeeks.org/combine-and-modify-ggplot2-legends-with-ribbons-and-lines/
  scale_linetype_manual(name = 'Function', values = c('Mean' = 'solid')) +
  # source: https://www.geeksforgeeks.org/how-to-remove-legend-title-in-r-with-ggplot2/
  scale_fill_manual(name = 'Margins', values = c('Qu. Range' = 'grey')) + 
  #scale_fill_manual(name = 'Margins', values = c('Qu. Range' = 'grey', 'Transition Period' = 'orange')) + 
  #scale_color_continuous(name = '% Significant Models', type = 'gradient') + 
  scale_color_gradientn(name = '% Model with Significance', colors = c('#525174', '#348aa7', '#5dd39e', '#bce784')) +
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
png('output/STS_timeseries_dev_VIF_aggregated_newPath10fCV_final.png')
bp <- ggplot(data = df_bar, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
  geom_bar(aes(y = total_models, fill = Sig_Model_Proportion), stat = 'identity', position = 'dodge', show.legend = F) +
  #geom_histogram(aes(y = total_models, color = Sig_Model_Proportion), stat = 'identity') +
  #scale_color_grey(name = '% Significant Models') + 
  # scale_fill_continuous(name = '% Significant Models', type = 'gradient') +
  scale_fill_gradientn(name = '% Model with Significance', colors = c('#525174', '#348aa7', '#5dd39e', '#bce784')) +
  ylab('No. of Elephants') + xlab('Time') +
  theme(legend.position = 'none') + 
  theme_minimal()

lp <- ggplot(data = STS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
  # source: https://stackoverflow.com/questions/29648907/using-geom-rect-for-time-series-shading-in-r
  #geom_rect(data = date_hl, mapping=aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf, fill = 'Transition Period'), alpha = 0.1) +
  # source: https://stackoverflow.com/questions/14704909/plotting-depth-range-in-time-series-using-ggplot
  # source: https://stackoverflow.com/questions/28648698/alpha-transparency-not-working-in-ggplot2
  geom_ribbon(data = STS_coef_m, aes(ymin = VIF_Q1, ymax = VIF_Q3, fill = 'Qu. Range', group = predictor), alpha = 0.5) + 
  geom_line(aes(y = VIF_Mean, group = predictor, color = Sig_Model_Proportion, linetype = 'Mean'), linewidth = 1) +
  geom_hline(yintercept = 1, linetype = 'dashed') + 
  geom_hline(yintercept = 5, linetype = 'dashed') + 
  # source: https://www.geeksforgeeks.org/combine-and-modify-ggplot2-legends-with-ribbons-and-lines/
  scale_linetype_manual(name = 'Function', values = c('Mean' = 'solid')) +
  # source: https://www.geeksforgeeks.org/how-to-remove-legend-title-in-r-with-ggplot2/
  scale_fill_manual(name = 'Margins', values = c('Qu. Range' = 'grey')) + 
  #scale_fill_manual(name = 'Margins', values = c('Qu. Range' = 'grey', 'Transition Period' = 'orange')) + 
  # scale_color_continuous(name = '% Significant Models', type = 'gradient') + 
  scale_color_gradientn(name = '% Significant Models', colors = c('#525174', '#348aa7', '#5dd39e', '#bce784')) +
  guides(colour = guide_colourbar(order = 3),
         linetype = guide_legend(order = 1),
         fill = guide_legend(order = 2)) +
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') + 
  xlab('Time') + ylab('VIF') + ggtitle('Aggregated time-series of VIF and Deviance Improvement', subtitle = paste0(elephant, ' from ', start_date, ' to ', end_date)) + 
  theme_minimal() + 
  theme(legend.position = 'none') 

lp_leg <- ggplot(data = STS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
  # source: https://stackoverflow.com/questions/14704909/plotting-depth-range-in-time-series-using-ggplot
  # source: https://stackoverflow.com/questions/28648698/alpha-transparency-not-working-in-ggplot2
  geom_ribbon(data = STS_coef_m, aes(ymin = VIF_Q1, ymax = VIF_Q3, fill = 'Qu. Range', group = predictor), alpha = 0.5) + 
  geom_line(aes(y = VIF_Mean, group = predictor, color = Sig_Model_Proportion, linetype = 'Mean'), linewidth = 1) +
  # source: https://www.geeksforgeeks.org/combine-and-modify-ggplot2-legends-with-ribbons-and-lines/
  scale_linetype_manual(name = 'Function', values = c('Mean' = 'solid')) +
  # source: https://www.geeksforgeeks.org/how-to-remove-legend-title-in-r-with-ggplot2/
  scale_fill_manual(name = 'Margins', values = c('Qu. Range' = 'grey')) + 
  #scale_fill_manual(name = 'Margins', values = c('Qu. Range' = 'grey', 'Transition Period' = 'orange')) + 
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

# # fill new run table with only runs that give sig models 
# srsig <- summary_results[summary_results$sub_glm_sig == T,] # these are the models i want to retrieve coefs for 
# 
# # get ID and weeks of sig models 
# srsig$combo <- paste(srsig$ID, srsig$week, sep = '_')
# 
# run_settings$combo <- paste(run_settings$ID, run_settings$week, sep = '_')

#RQ2_run_table <- run_settings[run_settings$combo %in% srsig$combo,]
# LTS_run_table <- run_settings
LTS_run_table <- read.csv('data/run_settings_LTS_final.csv', row.names = 1)
LTS_run_table <- LTS_run_table[,1:5]

row.names(LTS_run_table) <- 1:nrow(LTS_run_table)

LTS_coef <- data.frame()

suffix <- '_newPathWithCV'
glm_type <- '' # or '_custom'

for(i in 1:nrow(LTS_run_table)){
  
  # get run settings
  ID <- LTS_run_table$ID[i]
  week <- LTS_run_table$week[i]
  method <- LTS_run_table$pseudo_abs_method[i]
  
  # define filepaths
  data_path <- paste0('data/', ID, '/', week, '/')
  output_path <- paste0('output/', ID, '/', week, '/')
  
  
  # load and retrieve difference in deviance 
  dev_df <- read.csv(paste0(output_path, '6_e4_glm', glm_type, '_mean_sd_deviances_', method, suffix, '.csv'))
  
  # retrieve AIC
  aic <- dev_df$AIC
  
  # calculate deviance improvement 
  dev_diff <- dev_df$null_deviance - dev_df$residual_deviance
  
  # calculate chi-square from deviance to get model significance 
  # source: https://stats.stackexchange.com/questions/340489/interpretation-of-deviance-in-logistic-model
  # source: https://www.r-bloggers.com/2022/05/calculate-the-p-value-from-chi-square-statistic-in-r/
  dev_sig <- pchisq(dev_diff, dev_df$null_df - dev_df$residual_df, lower.tail = F)
  
  # check if pvalues below 0.05
  if(dev_sig <=0.05){dev_sig <- 'sig'}else{dev_sig <- 'not sig'}
  
  # load and retrieve VIF 
  vif_df <- read.csv(paste0('output/', ID, '/', week, '/6_e5_glm_mean_sd_vif_', method, suffix, '.csv'), row.names = 1, header = T)[,2]
  names(vif_df) <- 'vif_results'
  
  # retrieve date of week 
  dfile <- read.csv(paste0(data_path, '2_a1_step_extents_LUT_', method, suffix, '.csv'), row.names = 1)
  date <- dfile$start_date[1]
  
  # retrieve coefs glm 
  c <- read.csv(paste0(output_path, '6_e3_glm', glm_type, '_mean_sd_coefs_random_path_custom_distr', suffix, '.csv'))
  c <- c[2:nrow(c),]
  c_clr <- read.csv(paste0(output_path, '6_c1_clr_mean_sd_coefs_random_path_custom_distr', suffix, '.csv'), header = T)
  names(c_clr) <- c('predictor', 'coef', 'exp.coef.', 'se.coef.', 'z', 'Pr...z..')
  # reorder rows to match order of GLM predictors 
  # source: https://www.statology.org/dplyr-arrange-custom-order/
  c_clr <- c_clr %>% arrange(match(predictor, c('ndvi_mean_scaled', 'ndvi_rate_mean_scaled', 'ndvi_rate_sd_scaled', 'ndvi_sd_scaled')))
  
  
  # specify if sig or not with 90% confidence (so pvalue < 0.1)
  c$Pr...z..[as.numeric(c$Pr...z..) < 0.05] <- 'sig'
  c$Pr...z..[as.numeric(c$Pr...z..) >= 0.05] <- 'not sig'
  c_clr$Pr...z..[as.numeric(c_clr$Pr...z..) < 0.05] <- 'sig'
  c_clr$Pr...z..[as.numeric(c_clr$Pr...z..) >= 0.05] <- 'not sig'
  
  # if(glm_type == '_custom'){
  #   
  #   # load CV results 
  #   cv_results <- read.csv(paste0(output_path, '6_c6_glm', glm_type, '_mean_sd_CV_results_random_path_custom_distr', suffix, '.csv'), header = T, row.names = 1)
  #   
  #   # retrieve sensitivity and specificity from CV
  #   recallCV <-  cv_results$Sens 
  #   specCV <- cv_results$Spec
  #   ROC_CV <- cv_results$ROC
  #   thresholdCV <- cv_results$threshold
  #   
  #   # load test results 
  #   test_results <- read.csv(paste0(output_path, '6_c7_glm', glm_type, '_mean_sd_test_results_random_path_custom_distr', suffix, '.csv'), header = T, row.names = 1)
  #   
  #   # retrieve precision and recall from predicted test 
  #   recallTest <- test_results[6,1]
  #   precisionTest <- test_results[5,1]
  #   F1Test <- test_results[7,1]
  #   balanced_accuracyTest <- test_results[11,1]
  #   
  # }else{
  #   
  #   recallCV <- NA
  #   specCV <- NA
  #   ROC_CV <- NA
  #   thresholdCV <- NA
  #   
  #   recallTest <- NA
  #   precisionTest <- NA
  #   F1Test <- NA
  #   balanced_accuracyTest <- NA
    
  # }
    
  # load validation results 
  test_results <- read.csv(paste0(output_path, '6_e6_glm_mean_sd_test_results_random_path_custom_distr', suffix, '.csv'), header = T)[,2:10]
    
  # create new entry with coefs and pvalues
  # entry <- data.frame(ID = ID, week = week, method = method, date = date, predictor = c[,1],
  #                     VIF = vif_df$vif_results, AIC = aic, deviance_improvement = dev_diff, model_sig = dev_sig,
  #                     glm_value = c$Estimate, glm_significance = c$Pr...z..,
  #                     CV_recall = recallCV, CV_specificity = specCV, CV_ROC = ROC_CV, CV_threshold = thresholdCV,
  #                     test_recall = recallTest, test_precision = precisionTest, 
  #                     test_F1 = F1Test, test_balanced_accuracy = balanced_accuracyTest,
  #                     clr_value = c_clr$coef, clr_significance = c_clr$Pr...z..)
  
  entry <- data.frame(ID = ID, week = week, method = method, date = date, predictor = c[,'predictor'],
                      VIF = vif_df, AIC = aic, deviance_improvement = dev_diff, model_sig = dev_sig,
                      glm_value = c$Estimate, glm_significance = c$Pr...z.., test_results,
                      clr_value = c_clr$coef, clr_significance = c_clr$Pr...z..)

  LTS_coef <- rbind(LTS_coef, entry)
}


LTS_coef_m <- LTS_coef

# # this code no longer needed because just removed those datasets (faulty datasets from LA12 and LA13)
# #LTS_coef_m[LTS_coef_m$ID == 'LA11' & LTS_coef_m$week == 2085, c('glm_value', 'clr_value')] <- NA
# LTS_coef_m$glm_value[LTS_coef_m$glm_value > 7 | LTS_coef_m$glm_value < -7] <- NA
# LTS_coef_m$clr_value[LTS_coef_m$clr_value > 7 | LTS_coef_m$clr_value < -7] <- NA

start_date <- min(LTS_coef_m$date)
end_date <- max(LTS_coef_m$date)
elephant <- 'LA11, LA12, LA13, and LA14'
# 
# library(ggplot2)
# # plot all at once - GLM
# png('output/LTS_timeseries_glm_coef.png')
# ggplot(data = LTS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = glm_value, group = interaction(predictor, ID), color = ID)) + 
#   geom_line() + 
#   geom_point(data = LTS_coef_m[LTS_coef_m$glm_significance == 'sig',], aes(y = glm_value), shape = 8) + 
#   geom_hline(yintercept = 0) + 
#   facet_grid(vars(predictor), scale = 'free') + 
#   xlab('Time') + ylab('Coefficient Value') + ggtitle(paste0('Timeseries of GLM predictors for ', elephant, ' from ', start_date, ' to ', end_date))
# dev.off()
# 
# # plot all at once - CLR
# png('output/LTS_timeseries_clr_coef.png')
# ggplot(data = LTS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = clr_value, group = interaction(predictor, ID), color = ID)) + 
#   geom_line() + 
#   geom_point(data = LTS_coef_m[LTS_coef_m$clr_significance == 'sig',], aes(y = clr_value), shape = 8) + 
#   geom_hline(yintercept = 0) + 
#   facet_grid(vars(predictor), scale = 'free') + 
#   xlab('Time') + ylab('Coefficient Value') + ggtitle(paste0('Timeseries of CLR predictors for ', elephant, ' from ', start_date, ' to ', end_date))
# dev.off()


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
ag_glm_sig <- aggregate(LTS_coef_m$glm_significance, list(LTS_coef_m$week, LTS_coef_m$predictor), FUN = relative_sum)$x
ag_clr_sig <- aggregate(LTS_coef_m$clr_significance, list(LTS_coef_m$week, LTS_coef_m$predictor), FUN = relative_sum)$x

agc <- paste0(ag_glm$Group.1, ag_glm$Group.2, sep = '_')

agdf <- data.frame(combo = agc, total_models = ag_model, VIF_Q1 = ag_vif_q1, VIF_Mean = ag_vif_m, VIF_Q2 = ag_vif_q2, VIF_Q3 = ag_vif_q3,
                   DEV_IMP_Q1 = ag_di_q1, DEV_IMP_Mean = ag_di_m, DEV_IMP_Q2 = ag_di_q2, DEV_IMP_Q3 = ag_di_q3,
                   GLM_Q1 = ag_glm_q1, GLM_Mean = ag_glm_m, GLM_Q2 = ag_glm_q2, GLM_Q3 = ag_glm_q3, 
                   CLR_Q1 = ag_clr_q1, CLR_Mean = ag_clr_m, CLR_Q2 = ag_clr_q2, CLR_Q3 = ag_clr_q3, 
                   Sig_Model_Proportion = ag_model_sig, Sig_GLM_Proportion = ag_glm_sig, Sig_CLR_Proportion = ag_clr_sig)

# source: https://www.guru99.com/r-merge-data-frames.html
LTS_coef_m <- merge(LTS_coef_m, agdf, by.x = 'combo')



library("cowplot") # to add multiple plots as one

# make sure all weeks have same date 
# note: some dates differ because of missing data for certain elephants in that week 
# this is hardcoded for now --> could write code to automate this 
# indices correspond to dataset with all 4 elephants 
# LTS_coef_m$date[LTS_coef_m$week == 2065] <- min(LTS_coef_m$date)
# LTS_coef_m$date[LTS_coef_m$week == 2104] <- min(LTS_coef_m$date[LTS_coef_m$week == 2104])
# LTS_coef_m$date[LTS_coef_m$week == 2178] <- min(LTS_coef_m$date[LTS_coef_m$week == 2178])
# LTS_coef_m$date[LTS_coef_m$week == 2227] <- min(LTS_coef_m$date[LTS_coef_m$week == 2227])

d <- data.frame()

for(i in unique(LTS_coef$week)){
  df <- LTS_coef_m$date[LTS_coef_m$week == i]
  entry <- data.frame(week = i, n = length(unique(df)))
  d <- rbind(d, entry)
}

for(i in d$week[d$n > 1]){
  LTS_coef_m$date[LTS_coef_m$week == i] <- min(LTS_coef_m$date[LTS_coef_m$week == i])
}

print(length(unique(LTS_coef_m$week)) == length(unique(LTS_coef_m$date)))



# # addign a column for proportion of sig models --> have to aggregate because unique would omit some values that repeat for diff weeks
# ag_sig <- aggregate(LTS_coef_m$Sig_Model_Proportion, list(LTS_coef_m$week), FUN = quantile, probs = 0.5, na.rm = T)$x
# ag_di <- aggregate(LTS_coef_m$deviance_improvement, list(LTS_coef_m$week), FUN = mean)$x
# 
# # retrieve the number of models from sst_w df 
# df_bar <- data.frame(week = unique(LTS_coef_m$date), total_models = sst_w$total_models, deviance_improvement = ag_di, sig_models = ag_sig)

df_bar <- data.frame(LTS_coef_m[!duplicated(LTS_coef_m$date),c('date', 'total_models', 'Sig_Model_Proportion', 'DEV_IMP_Mean')])


# change label names of predictors 
LTS_coef_m$predictor[LTS_coef_m$predictor == 'ndvi_mean_scaled'] <- 'Avg. NDVI'
LTS_coef_m$predictor[LTS_coef_m$predictor == 'ndvi_sd_scaled'] <- 'Dev. NDVI'
LTS_coef_m$predictor[LTS_coef_m$predictor == 'ndvi_rate_mean_scaled'] <- 'Avg. NDVI Growth Rate'
LTS_coef_m$predictor[LTS_coef_m$predictor == 'ndvi_rate_sd_scaled'] <- 'Dev. NDVI Growth Rate' 

# save STS coef table 
write.csv(LTS_coef_m, 'output/LTS_df_results_aggregated_newPathWith10fCV_final.csv')

# 
# 
# # retrieve dates that want to highlight 
# date_hl <- unique(LTS_coef_m$date[grep('-09-', LTS_coef_m$date)])
# 
# # get a list of dates by year
# # source: https://r-coder.com/split-r/
# date_hl <- split(date_hl, f = sub('-.*', '', date_hl))
# 
# # get start and end dates for each year
# # source: https://stackoverflow.com/questions/43425540/get-max-min-values-from-a-list-of-dataframes-without-loop
# date_hl <- sapply(date_hl, function(x) range(x))
# 
# # convert to data frame (doesn't work if matrix) of xmin and xmax 
# date_hl <- data.frame(t(date_hl))
# colnames(date_hl) <- c('xmin', 'xmax')
# 
# # convert dates to date object
# date_hl$xmin <- as.Date(date_hl$xmin, tz = 'Africa/Maputo')
# date_hl$xmax <- as.Date(date_hl$xmax, tz = 'Africa/Maputo')


png('output/LTS_timeseries_glm_coef_aggregated_newPathWith10fCV_final.png')

bp <- ggplot(data = df_bar, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
  geom_bar(aes(y = total_models, fill = Sig_Model_Proportion), stat = 'identity', position = 'dodge', show.legend = F) +
  #geom_histogram(aes(y = total_models, color = Sig_Model_Proportion), stat = 'identity') +
  #scale_color_grey(name = '% Significant Models') + 
  # scale_fill_continuous(name = '% Significant Models', type = 'gradient') +
  scale_fill_gradientn(name = '% Model with Significance', colors = c('#525174', '#348aa7', '#5dd39e', '#bce784')) +
  ylab('No. of Elephants') + xlab('Time') +
  theme(legend.position = 'none') + 
  theme_minimal()

lp <- ggplot(data = LTS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
  # source: https://stackoverflow.com/questions/29648907/using-geom-rect-for-time-series-shading-in-r
  #geom_rect(data = date_hl, mapping=aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf, fill = 'Transition Period'), alpha = 0.1, show.legend = F) +
  # source: https://stackoverflow.com/questions/14704909/plotting-depth-range-in-time-series-using-ggplot
  # source: https://stackoverflow.com/questions/28648698/alpha-transparency-not-working-in-ggplot2
  geom_ribbon(data = LTS_coef_m, aes(ymin = GLM_Q1, ymax = GLM_Q3, fill = 'Qu. Range', group = predictor), alpha = 0.5, show.legend = F) + 
  geom_line(aes(y = GLM_Mean, group = predictor, color = Sig_GLM_Proportion, linetype = 'Mean'), linewidth = 1, show.legend = F) +
  #geom_line(aes(y = GLM_Q2, color = 'Median')) +
  geom_hline(yintercept = 0, linetype = 'dashed') + 
  # source: https://www.geeksforgeeks.org/combine-and-modify-ggplot2-legends-with-ribbons-and-lines/
  scale_linetype_manual(name = 'Function', values = c('Mean' = 'solid')) +
  # source: https://www.geeksforgeeks.org/how-to-remove-legend-title-in-r-with-ggplot2/
  scale_fill_manual(name = 'Margins', values = c('Qu. Range' = 'grey')) + #, 'Transition Period' = 'orange')) + 
  #scale_color_continuous(name = '% Significant Models', type = 'gradient') + 
  scale_color_gradientn(name = '% Model with Significance', colors = c('#525174', '#348aa7', '#5dd39e', '#bce784')) +
  guides(colour = guide_colourbar(order = 3),
         linetype = guide_legend(order = 1),
         fill = guide_legend(order = 2)) +
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') + 
  xlab('Time') + ylab('Coefficient Value') + #ggtitle('Aggregated time-series of estimated GLM model coefficients', subtitle = paste0(elephant, ' from ', start_date, ' to ', end_date)) + 
  theme_minimal() 
# source: https://stackoverflow.com/questions/68719513/ggplot-wont-remove-axis-ticks
#theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

lp_leg <- ggplot(data = LTS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
  #geom_rect(data = date_hl, mapping=aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf, fill = 'Transition Period'), alpha = 0.1) +
  # source: https://stackoverflow.com/questions/14704909/plotting-depth-range-in-time-series-using-ggplot
  # source: https://stackoverflow.com/questions/28648698/alpha-transparency-not-working-in-ggplot2
  geom_ribbon(data = LTS_coef_m, aes(ymin = GLM_Q1, ymax = GLM_Q3, fill = 'Qu. Range', group = predictor), alpha = 0.5) + 
  geom_line(aes(y = GLM_Mean, group = predictor, color = Sig_GLM_Proportion, linetype = 'Mean'), linewidth = 1) +
  # source: https://www.geeksforgeeks.org/combine-and-modify-ggplot2-legends-with-ribbons-and-lines/
  scale_linetype_manual(name = 'Function', values = c('Mean' = 'solid')) +
  # source: https://www.geeksforgeeks.org/how-to-remove-legend-title-in-r-with-ggplot2/
  scale_fill_manual(name = 'Margins', values = c('Qu. Range' = 'grey')) + #, 'Transition Period' = 'orange')) + 
  #scale_color_continuous(name = '% Significant Models', type = 'gradient') + 
  scale_color_gradientn(name = '% Model with Significance', colors = c('#525174', '#348aa7', '#5dd39e', '#bce784')) +
  guides(colour = guide_colourbar(order = 3),
         linetype = guide_legend(order = 1),
         fill = guide_legend(order = 2)) +
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') 


# source: https://wilkelab.org/cowplot/reference/get_legend.html
lp_leg <- get_legend(lp_leg)

# source: https://wilkelab.org/cowplot/articles/plot_grid.html
plot_grid(lp, lp_leg, bp, ncol = 2, nrow = 2, rel_heights = c(5,1), rel_widths = c(3,1))

dev.off()



png('output/LTS_timeseries_clr_coef_aggregated_newPathWith10fCV_final.png')

bp <- ggplot(data = df_bar, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
  geom_bar(aes(y = total_models, fill = Sig_Model_Proportion), stat = 'identity', position = 'dodge', show.legend = F) +
  #geom_histogram(aes(y = total_models, color = Sig_Model_Proportion), stat = 'identity') +
  #scale_color_grey(name = '% Significant Models') + 
  # scale_fill_continuous(name = '% Significant Models', type = 'gradient') +
  scale_fill_gradientn(name = '% Model with Significance', colors = c('#525174', '#348aa7', '#5dd39e', '#bce784')) +
  ylab('No. of Elephants') + xlab('Time') +
  theme(legend.position = 'none') + 
  theme_minimal()

lp <- ggplot(data = LTS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
  # source: https://stackoverflow.com/questions/29648907/using-geom-rect-for-time-series-shading-in-r
  #geom_rect(data = date_hl, mapping=aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf, fill = 'Transition Period'), alpha = 0.1, show.legend = F) +
  # source: https://stackoverflow.com/questions/14704909/plotting-depth-range-in-time-series-using-ggplot
  # source: https://stackoverflow.com/questions/28648698/alpha-transparency-not-working-in-ggplot2
  geom_ribbon(data = LTS_coef_m, aes(ymin = CLR_Q1, ymax = CLR_Q3, fill = 'Qu. Range', group = predictor), alpha = 0.5, show.legend = F) + 
  geom_line(aes(y = CLR_Mean, group = predictor, color = Sig_CLR_Proportion, linetype = 'Mean'), linewidth = 1, show.legend = F) +
  #geom_line(aes(y = CLR_Q2, color = 'Median')) +
  geom_hline(yintercept = 0, linetype = 'dashed') + 
  # source: https://www.geeksforgeeks.org/combine-and-modify-ggplot2-legends-with-ribbons-and-lines/
  scale_linetype_manual(name = 'Function', values = c('Mean' = 'solid')) +
  # source: https://www.geeksforgeeks.org/how-to-remove-legend-title-in-r-with-ggplot2/
  scale_fill_manual(name = 'Margins', values = c('Qu. Range' = 'grey')) + #, 'Transition Period' = 'orange')) + 
  #scale_color_continuous(name = '% Significant Models', type = 'gradient') + 
  scale_color_gradientn(name = '% Model with Significance', colors = c('#525174', '#348aa7', '#5dd39e', '#bce784')) +
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
  #geom_rect(data = date_hl, mapping=aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf, fill = 'Transition Period'), alpha = 0.1) +
  # source: https://stackoverflow.com/questions/14704909/plotting-depth-range-in-time-series-using-ggplot
  # source: https://stackoverflow.com/questions/28648698/alpha-transparency-not-working-in-ggplot2
  geom_ribbon(data = LTS_coef_m, aes(ymin = CLR_Q1, ymax = CLR_Q3, fill = 'Qu. Range', group = predictor), alpha = 0.5) + 
  geom_line(aes(y = CLR_Mean, group = predictor, color = Sig_CLR_Proportion, linetype = 'Mean'), linewidth = 1) +
  # source: https://www.geeksforgeeks.org/combine-and-modify-ggplot2-legends-with-ribbons-and-lines/
  scale_linetype_manual(name = 'Function', values = c('Mean' = 'solid')) +
  # source: https://www.geeksforgeeks.org/how-to-remove-legend-title-in-r-with-ggplot2/
  scale_fill_manual(name = 'Margins', values = c('Qu. Range' = 'grey')) + #, 'Transition Period' = 'orange')) + 
  #scale_color_continuous(name = '% Significant Models', type = 'gradient') + 
  scale_color_gradientn(name = '% Model with Significance', colors = c('#525174', '#348aa7', '#5dd39e', '#bce784')) +
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
png('output/LTS_timeseries_dev_VIF_aggregated_newPathWith10fCV_final.png')
bp <- ggplot(data = df_bar, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
  geom_bar(aes(y = DEV_IMP_Mean, fill = Sig_Model_Proportion), stat = 'identity', position = 'dodge') +
  # scale_fill_continuous(name = '% Significant Models', type = 'gradient') +
  scale_fill_gradientn(name = '% Significant Models', colors = c('#525174', '#348aa7', '#5dd39e', '#bce784')) +
  ylab('Deviance Improvement') + xlab('Time') +
  theme_minimal() + 
  theme(legend.position = 'none') 

lp <- ggplot(data = LTS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
  # source: https://stackoverflow.com/questions/29648907/using-geom-rect-for-time-series-shading-in-r
  #geom_rect(data = date_hl, mapping=aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf, fill = 'Transition Period'), alpha = 0.1) +
  geom_hline(yintercept = 1, linetype = 'dashed') + 
  geom_hline(yintercept = 5, linetype = 'dashed') +
  # source: https://stackoverflow.com/questions/14704909/plotting-depth-range-in-time-series-using-ggplot
  # source: https://stackoverflow.com/questions/28648698/alpha-transparency-not-working-in-ggplot2
  geom_ribbon(data = LTS_coef_m, aes(ymin = VIF_Q1, ymax = VIF_Q3, fill = 'Qu. Range', group = predictor), alpha = 0.5) + 
  geom_line(aes(y = VIF_Mean, group = predictor, color = Sig_Model_Proportion, linetype = 'Mean'), linewidth = 1) +
  # source: https://www.geeksforgeeks.org/combine-and-modify-ggplot2-legends-with-ribbons-and-lines/
  scale_linetype_manual(name = 'Function', values = c('Mean' = 'solid')) +
  # source: https://www.geeksforgeeks.org/how-to-remove-legend-title-in-r-with-ggplot2/
  scale_fill_manual(name = 'Margins', values = c('Qu. Range' = 'grey')) + #, 'Transition Period' = 'orange')) + 
  # scale_color_continuous(name = '% Significant Models', type = 'gradient') + 
  scale_color_gradientn(name = '% Significant Models', colors = c('#525174', '#348aa7', '#5dd39e', '#bce784')) +
  guides(colour = guide_colourbar(order = 3),
         linetype = guide_legend(order = 1),
         fill = guide_legend(order = 2)) +
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') + 
  xlab('Time') + ylab('VIF') + ggtitle('Aggregated time-series of VIF and Deviance Improvement', subtitle = paste0(elephant, ' from ', start_date, ' to ', end_date)) + 
  theme_minimal() + 
  theme(legend.position = 'none') 

lp_leg <- ggplot(data = LTS_coef_m, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
  # source: https://stackoverflow.com/questions/29648907/using-geom-rect-for-time-series-shading-in-r
  #geom_rect(data = date_hl, mapping=aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf, fill = 'Transition Period'), alpha = 0.1) +
  # source: https://stackoverflow.com/questions/14704909/plotting-depth-range-in-time-series-using-ggplot
  # source: https://stackoverflow.com/questions/28648698/alpha-transparency-not-working-in-ggplot2
  geom_ribbon(data = LTS_coef_m, aes(ymin = VIF_Q1, ymax = VIF_Q3, fill = 'Qu. Range', group = predictor), alpha = 0.5) + 
  geom_line(aes(y = VIF_Mean, group = predictor, color = Sig_Model_Proportion, linetype = 'Mean'), linewidth = 1) +
  # source: https://www.geeksforgeeks.org/combine-and-modify-ggplot2-legends-with-ribbons-and-lines/
  scale_linetype_manual(name = 'Function', values = c('Mean' = 'solid')) +
  # source: https://www.geeksforgeeks.org/how-to-remove-legend-title-in-r-with-ggplot2/
  scale_fill_manual(name = 'Margin', values = c('Qu. Range' = 'grey')) + #, 'Transition Period' = 'orange')) + 
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
#plot_grid(lp, lp_leg, bp, ncol = 2, nrow = 2, rel_heights = c(3,1), rel_widths = c(3,1))
plot_grid(lp, lp_leg, ncol = 2, nrow = 1, rel_widths = c(3,1))

dev.off()




LTS_coef_m <- read.csv('output/LTS_df_results_aggregated_newPathWithoutCV.csv', row.names = 1)
names(LTS_coef_m)

lts <- LTS_coef_m[, -c(6, 7, 11, 12, 20:41)]

lts$combo <- paste0(lts$ID, '_', lts$week)

lts <- lts[!duplicated(lts$combo), ]

# lts <- lts[lts$model_sig == "sig",]

lts$month <- matrix(unlist(strsplit(lts$date, '-')), ncol = 3, byrow = T)[,2]

lts <- lts %>% group_by(month) %>% mutate(total_models = n())

lts <- lts %>% group_by(month) %>% mutate(sig_models = sum(model_sig == 'sig'))

lts$proportion_sig <- lts$sig_models/lts$total_models

g <- lts %>% group_by(month) %>% summarize_all('mean')


# ggplot(data = lts, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
#   geom_line(aes(y = test_specificity))

ggplot(data = lts, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
   geom_line(aes(y = AIC))
ggplot(data = lts, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
   geom_line(aes(y = test_AUC))
ggplot(data = lts, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
   geom_line(aes(y = test_F1_score))
ggplot(data = lts, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
   geom_line(aes(y = test_accuracy))
ggplot(data = lts, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
   geom_line(aes(y = test_cutoff))
ggplot(data = lts, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
   geom_line(aes(y = test_recall))
ggplot(data = lts, aes(x = as.Date(date, tz = 'Africa/Maputo'))) + 
   geom_line(aes(y = test_specificity))


ggplot(data = g, aes(x = month)) + 
  geom_line(aes(y = AIC, group = 1, color = proportion_sig)) + 
  scale_color_gradientn(name = '% Significant Models', colors = rainbow(3))
ggplot(data = g, aes(x = month)) + 
  geom_line(aes(y = test_AUC, group = 1, color = proportion_sig)) + 
  scale_color_gradientn(name = '% Significant Models', colors = rainbow(3))
ggplot(data = g, aes(x = month)) + 
  geom_line(aes(y = test_F1_score, group = 1, color = proportion_sig)) + 
  scale_color_gradientn(name = '% Significant Models', colors = rainbow(3))
# ggplot(data = g, aes(x = month)) + 
#   geom_line(aes(y = test_accuracy, group = 1, color = proportion_sig)) + 
#   scale_color_gradientn(name = '% Significant Models', colors = rainbow(3))
ggplot(data = g, aes(x = month)) + 
  geom_line(aes(y = test_cutoff, group = 1, color = proportion_sig)) + 
  scale_color_gradientn(name = '% Significant Models', colors = rainbow(3))
ggplot(data = g, aes(x = month)) + 
  geom_line(aes(y = test_recall, group = 1, color = proportion_sig)) + 
  scale_color_gradientn(name = '% Significant Models', colors = rainbow(3))
ggplot(data = g, aes(x = month)) + 
  geom_line(aes(y = test_specificity, group = 1, color = proportion_sig)) + 
  scale_color_gradientn(name = '% Significant Models', colors = rainbow(3))

LTS_coef_m <- read.csv('output/LTS_df_results_aggregated_newPathWithCV.csv', row.names = 1)
names(LTS_coef_m)



names(LTS_coef_m)




######## RQ3 spatial detail visualizations 

run_settings <- read.csv('data/run_settings_downscaling_final.csv', row.names = 1)

# create a dataset that summarizes all predictor coefficients for all models and whether they are significant 
downscaling_coef <- data.frame()

for(i in 1:nrow(run_settings)){
  
  # get run settings
  ID <- run_settings$ID[i]
  week <- run_settings$week[i]
  method <- run_settings$pseudo_abs_method[i]
  downscaling_setting <- run_settings$downscaling[i]
  
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
  
  suffix <- paste0(suffix, '_newPathWithCV')
  
  # define filepaths
  data_path <- paste0('data/', ID, '/', week, '/')
  output_path <- paste0('output/', ID, '/', week, '/')
  
  
  # load and retrieve difference in deviance 
  dev_df <- read.csv(paste0(output_path, '6_e4_glm_mean_sd_deviances_', method, suffix, '.csv'))
  
  # retrieve AIC
  aic <- dev_df$AIC
  
  # calculate deviance improvement 
  dev_diff <- dev_df$null_deviance - dev_df$residual_deviance
  
  # calculate chi-square from deviance to get model significance 
  # source: https://stats.stackexchange.com/questions/340489/interpretation-of-deviance-in-logistic-model
  # source: https://www.r-bloggers.com/2022/05/calculate-the-p-value-from-chi-square-statistic-in-r/
  dev_sig <- pchisq(dev_diff, dev_df$null_df - dev_df$residual_df, lower.tail = F)
  
  # check if pvalues below 0.05
  #if(dev_sig <=0.05){dev_sig <- 'sig'}else{dev_sig <- 'not sig'}
  
  # load and retrieve VIF 
  vif_df <- read.csv(paste0('output/', ID, '/', week, '/6_e5_glm_mean_sd_vif_', method, suffix, '.csv'), row.names = 1, header = T)[,2]
  names(vif_df) <- 'vif_results'
  
  # retrieve date of week 
  dfile <- read.csv(paste0(data_path, '2_a1_step_extents_LUT_', method, '_newPathWithCV', '.csv'), row.names = 1)
  date <- dfile$start_date[1]
  
  # retrieve coefs glm 
  c <- read.csv(paste0(output_path, '6_e3_glm_mean_sd_coefs_random_path_custom_distr', suffix, '.csv'))
  c <- c[2:nrow(c),]
  c_clr <- read.csv(paste0(output_path, '6_c1_clr_mean_sd_coefs_random_path_custom_distr', suffix, '.csv'), header = T)
  names(c_clr) <- c('predictor', 'coef', 'exp.coef.', 'se.coef.', 'z', 'Pr...z..')
  # reorder rows to match order of GLM predictors 
  # source: https://www.statology.org/dplyr-arrange-custom-order/
  c_clr <- c_clr %>% arrange(match(predictor, c('ndvi_mean_scaled', 'ndvi_rate_mean_scaled', 'ndvi_rate_sd_scaled', 'ndvi_sd_scaled')))
  
  # load validation results 
  test_results <- read.csv(paste0(output_path, '6_e6_glm_mean_sd_test_results_random_path_custom_distr', suffix, '.csv'), header = T)[,2:10]
  
  # specify if sig or not with 90% confidence (so pvalue < 0.1)
  # c$Pr...z..[as.numeric(c$Pr...z..) < 0.1] <- 'sig'
  # c$Pr...z..[as.numeric(c$Pr...z..) >= 0.1] <- 'not sig'
  # c_clr$Pr...z..[as.numeric(c_clr$Pr...z..) < 0.1] <- 'sig'
  # c_clr$Pr...z..[as.numeric(c_clr$Pr...z..) >= 0.1] <- 'not sig'
  
  # create new entry with coefs and pvalues 
  entry <- data.frame(ID = ID, week = week, date = date, method = method, downscaling = downscaling_setting, predictor = c[,'predictor'],
                     VIF = vif_df, AIC = aic, deviance_improvement = dev_diff, model_sig = dev_sig, 
                      glm_value = c$Estimate, glm_significance = c$Pr...z.., test_results, 
                      clr_value = c_clr$coef, clr_significance = c_clr$Pr...z..)
  
  downscaling_coef <- rbind(downscaling_coef, entry)
}




# manually adapt the dates in case they don't match per week (this happens when there is missing data)
# downscaling_coef$date[downscaling_coef$week == 2278] <- downscaling_coef$date[37]
d <- data.frame()

for(i in unique(downscaling_coef$week)){
  df <- downscaling_coef$date[downscaling_coef$week == i]
  entry <- data.frame(week = i, n = length(unique(df)))
  d <- rbind(d, entry)
}

for(i in downscaling_coef$week[d$n > 1]){
  downscaling_coef$date[downscaling_coef$week == i] <- min(downscaling_coef$date[downscaling_coef$week == i])
}
print(length(unique(downscaling_coef$week)) == length(unique(downscaling_coef$date)))

# change label names of predictors 
downscaling_coef$predictor[downscaling_coef$predictor == 'ndvi_mean_scaled'] <- 'Avg. NDVI'
downscaling_coef$predictor[downscaling_coef$predictor == 'ndvi_sd_scaled'] <- 'Dev. NDVI'
downscaling_coef$predictor[downscaling_coef$predictor == 'ndvi_rate_mean_scaled'] <- 'Avg. NDVI Growth Rate'
downscaling_coef$predictor[downscaling_coef$predictor == 'ndvi_rate_sd_scaled'] <- 'Dev. NDVI Growth Rate' 

# change label for downscaling status to be more indicative 
downscaling_coef$downscaling[downscaling_coef$downscaling == T] <- '30 m'
downscaling_coef$downscaling[downscaling_coef$downscaling == F] <- '250 m'

# create new column for seasons (to make it easier than dates)
downscaling_coef$seasons[downscaling_coef$week == 2260] <- 'April'
downscaling_coef$seasons[downscaling_coef$week == 2267] <- 'June'
downscaling_coef$seasons[downscaling_coef$week == 2278] <- 'August'

# rename the elephant IDs 
downscaling_coef$ID[downscaling_coef$ID == 'LA14'] <- 'Elephant LA14'
downscaling_coef$ID[downscaling_coef$ID == 'LA26'] <- 'Elephant LA26'

# try log scale the pvalue
#downscaling_coef$model_sig_log <- log(downscaling_coef$model_sig)
# 
# # look at pvalue diff 
# downscaling_coef$combo_id <- paste0(downscaling_coef$ID, downscaling_coef$week)
# 
# a <- downscaling_coef[downscaling_coef$downscaling == '250 m',c('combo_id', 'model_sig')]
# b <- downscaling_coef[downscaling_coef$downscaling == '30 m',c('combo_id', 'model_sig')]
# c <- merge(a, b, by = 'combo_id')
# c <- c[!duplicated(c),]
# c$diff <- c$model_sig.y - c$model_sig.x
# names(c)[2] <- 'sig 250m'
# names(c)[3] <- 'sig 30m'
# 
# 
# # quick visualization to compare chisquare pvalues between model types
# library(ggplot2)
# ggplot(data = downscaling_coef, aes(x = factor(seasons, level = c('April', 'June', 'August')), y = model_sig_log, fill = downscaling)) + 
#   geom_bar(position = position_dodge(preserve = 'single'), stat = 'identity') + 
#   scale_fill_manual(name = 'Spatial Resolution', values = c('250 m' = '#bce784', '30 m' = '#43A5C5'), 
#                     breaks = c('250 m', '30 m'), labels = c('250 m', '30 m'), 
#                     guide = guide_legend(override.aes = list(pattern = "none"))) + 
#   #geom_hline(yintercept = 0.01, linetype = 'dashed') +
#   # source: https://www.statology.org/ggplot-facet-order/
#   facet_grid(c(predictor) ~ c(ID)) +
#   #coord_cartesian(ylim = c(0, 0.015)) +
#   xlab('Study Period') + ylab('Coefficient') + #ggtitle('Comparison of estimated coefficients for GLM models trained with 250m and 30m data', subtitle = paste0(elephant, ' from ', start_date, ' to ', end_date)) + 
#   theme_minimal() 

# add column in dataset for custom spacing for text in plot based on value range 
# source: https://stackoverflow.com/questions/51568656/find-difference-between-maximum-value-of-group-and-current-row-with-r
library(dplyr)
downscaling_coef <- downscaling_coef %>% group_by(predictor) %>% mutate(justify = round((max(glm_value) - min(glm_value))/10, 2))
downscaling_coef <- downscaling_coef %>% group_by(predictor) %>% mutate(justify_vif = round((max(VIF) - min(VIF))/5, 2))

# add column to determine if pvalue is sig based on 0.05 threshold (for text coloring)
downscaling_coef$glm_significance_bool <- ifelse(downscaling_coef$glm_significance <= 0.05, 'significant', 'not significant')
downscaling_coef$model_sig_bool <- ifelse(downscaling_coef$model_sig <= 0.05, 'significant', 'not significant')


# only keep model significance for one predictor (no need to replicate 4 times)
#downscaling_coef$model_sig[downscaling_coef$predictor != 'Dev. NDVI Growth Rate'] <- NA

# sort the dataset to have same order of appearance of spatial res --> decreasing = F because dealing with characters not numbers
downscaling_coef <- downscaling_coef[order(downscaling_coef$ID, downscaling_coef$week),]

# save STS coef table 
write.csv(downscaling_coef, 'output/downscaling_df_results_newPathWith10fCV_final.csv')




downscaling_coef <- read.csv('output/downscaling_df_results_newPathWith10fCV_final.csv', row.names = 1)

# # define start and end dates for plotting 
# start_date <- min(downscaling_coef$date)
# end_date <- max(downscaling_coef$date)
# elephant <- 'LA14 and LA26'

# plot results 
if(!('ggpattern') %in% installed.packages()){install.packages('ggpattern')} # to read rasters
library(ggpattern)
library(ggplot2)

# png('output/downscaling_plot_glm_coef.png')
# ggplot(data = downscaling_coef, aes(x = seasons, y = glm_value, fill = downscaling, pattern = glm_significance)) + 
#   #geom_bar(data= downscaling_coef[downscaling_coef$downscaling == '250 m',], aes(y = glm_value, fill = glm_significance), stat = 'identity') + 
#   #geom_bar(data= downscaling_coef[downscaling_coef$downscaling == '30 m',], aes(y = glm_value, fill = glm_significance), stat = 'identity') + 
#   # source: https://stackoverflow.com/questions/20060949/ggplot2-multiple-sub-groups-of-a-bar-chart
#   #geom_bar(aes(x = interaction(seasons, downscaling), y = glm_value, fill = glm_significance, group = downscaling), stat = 'identity') + 
#   # source: https://guslipkin.medium.com/grouped-and-stacked-bar-charts-in-r-e5f5ac5637de
#   # source: https://stackoverflow.com/questions/62393159/how-can-i-add-hatches-stripes-or-another-pattern-or-texture-to-a-barplot-in-ggp
#   geom_bar_pattern(position = position_dodge(preserve = "single"), stat = 'identity', pattern_fill = "grey60",
#                    pattern_angle = 45, pattern_density = 0.05,
#                    pattern_spacing = 0.05, pattern_key_scale_factor = 0.6) + 
#   geom_hline(yintercept = 0, linetype = 'dashed') +
#   # source: https://ggplot2.tidyverse.org/reference/scale_manual.html
#   scale_fill_manual(name = 'Spatial Resolution', values = c('250 m' = '#bce784', '30 m' = '#43A5C5'), 
#                     breaks = c('250 m', '30 m'), labels = c('250 m', '30 m'), 
#                     guide = guide_legend(override.aes = list(pattern = "none"))) + 
#   # source: https://stackoverflow.com/questions/67164758/adding-hatches-or-patterns-to-ggplot-bars
#   scale_pattern_manual(name = 'Coefficient Significance', values = c('sig' = "none", 'not sig' = "stripe"), 
#                        breaks = c('sig', 'not sig'), labels = c('Significant', 'Not Significant'),
#                        guide = guide_legend(override.aes = list(fill = "white", color = 'black'))) +
#   # source: https://www.statology.org/ggplot-facet-order/
#   facet_grid(c(predictor) ~ c(ID), scale = 'free') +
#   xlab('Study Period') + ylab('Coefficient') + ggtitle('Comparison of estimated coefficients for GLM models trained with 250m and 30m data', subtitle = paste0(elephant, ' from ', start_date, ' to ', end_date)) + 
#   theme_minimal() 
# 
# dev.off()


pdf('output/downscaling_plot_glm_coef_newPathWith10fCV_final.pdf',  width = 12, height = 8.5)
ggplot(data = downscaling_coef, aes(x = factor(seasons, level = c('April', 'June', 'August')), 
                                    y = glm_value, fill = factor(downscaling, level = c('250 m', '30 m')), 
                                    color = glm_significance_bool)) + 
  #geom_bar(data= downscaling_coef[downscaling_coef$downscaling == '250 m',], aes(y = glm_value, fill = glm_significance), stat = 'identity') + 
  #geom_bar(data= downscaling_coef[downscaling_coef$downscaling == '30 m',], aes(y = glm_value, fill = glm_significance), stat = 'identity') + 
  # source: https://stackoverflow.com/questions/20060949/ggplot2-multiple-sub-groups-of-a-bar-chart
  #geom_bar(aes(x = interaction(seasons, downscaling), y = glm_value, fill = glm_significance, group = downscaling), stat = 'identity') + 
  # source: https://guslipkin.medium.com/grouped-and-stacked-bar-charts-in-r-e5f5ac5637de
  geom_bar(position = position_dodge(preserve = "single"), stat = 'identity', color = NA) + 
  # source: https://www.geeksforgeeks.org/formatting-numbers-and-strings-in-r-programming-format-function/
  # note: set color at beginning to keep correct order of text (see source)
  # source: https://stackoverflow.com/questions/51734285/ggplot-adding-color-aesthetic-changes-stack-order
  geom_text(aes(label=format(glm_significance, scientific = T, digits = 2),
                y = glm_value + justify * sign(glm_value)), position=position_dodge(.9), size = 4) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey40') +
  # source: https://ggplot2.tidyverse.org/reference/scale_manual.html
  # source: https://stackoverflow.com/questions/41701960/ggplot2-adding-text-on-a-multiple-barplot
  scale_fill_manual(name = 'Spatial Resolution', values = c('250 m' = '#bce784', '30 m' = '#43A5C5'), 
                    breaks = c('250 m', '30 m'), labels = c('250 m', '30 m'), 
                    guide = guide_legend(override.aes = list(pattern = "none"))) + 
  scale_color_manual(name = 'Predictor Significance', values = c('significant' = 'blue', 'not significant' = 'black')) +
  # source: https://www.statology.org/ggplot-facet-order/
  facet_grid(c(predictor) ~ c(ID), scale = 'free') +
  xlab('Study Period') + ylab('Coefficient') + #ggtitle('Comparison of estimated coefficients for GLM models trained with 250m and 30m data', subtitle = paste0(elephant, ' from ', start_date, ' to ', end_date)) + 
  theme_minimal() + 
  theme(text=element_text(size=12)) #15 for png 12 for pdf
dev.off()



# png('output/downscaling_plot_clr_coef.png')
# ggplot(data = downscaling_coef, aes(x = seasons, y = clr_value, fill = downscaling, pattern = clr_significance)) + 
#   # source: https://stackoverflow.com/questions/62393159/how-can-i-add-hatches-stripes-or-another-pattern-or-texture-to-a-barplot-in-ggp
#   geom_bar_pattern(position = position_dodge(preserve = "single"), stat = 'identity', pattern_fill = "grey60",
#                    pattern_angle = 45, pattern_density = 0.05,
#                    pattern_spacing = 0.05, pattern_key_scale_factor = 0.6) + 
#   geom_hline(yintercept = 0, linetype = 'dashed') +
#   # source: https://ggplot2.tidyverse.org/reference/scale_manual.html
#   scale_fill_manual(name = 'Spatial Resolution', values = c('250 m' = '#bce784', '30 m' = '#43A5C5'), 
#                     breaks = c('250 m', '30 m'), labels = c('250 m', '30 m'), 
#                     guide = guide_legend(override.aes = list(pattern = "none"))) + 
#   # source: https://stackoverflow.com/questions/67164758/adding-hatches-or-patterns-to-ggplot-bars
#   scale_pattern_manual(name = 'Coefficient Significance', values = c('sig' = "none", 'not sig' = "stripe"), 
#                        guide = guide_legend(override.aes = list(fill = "white", color = 'black'))) +
#   # source: https://www.statology.org/ggplot-facet-order/
#   facet_grid(c(predictor) ~ c(ID), scale = 'free') +
#   xlab('Study Period') + ylab('Coefficient') + ggtitle('Comparison of estimated coefficients for CLR models trained with 250m and 30m data', subtitle = paste0(elephant, ' from ', start_date, ' to ', end_date)) + 
#   theme_minimal() 
# dev.off()

# 
# png('output/downscaling_plot_vif.png')
# ggplot(data = downscaling_coef, aes(x = seasons, y = VIF, fill = downscaling, pattern = model_sig)) + 
#   # source: https://stackoverflow.com/questions/62393159/how-can-i-add-hatches-stripes-or-another-pattern-or-texture-to-a-barplot-in-ggp
#   geom_bar_pattern(position = position_dodge(preserve = "single"), stat = 'identity', pattern_fill = "grey60",
#                    pattern_angle = 45, pattern_density = 0.05,
#                    pattern_spacing = 0.05, pattern_key_scale_factor = 0.6) + 
#   geom_hline(yintercept = 0, linetype = 'dashed') +
#   # source: https://ggplot2.tidyverse.org/reference/scale_manual.html
#   scale_fill_manual(name = 'Spatial Resolution', values = c('250 m' = '#bce784', '30 m' = '#43A5C5'), 
#                     breaks = c('250 m', '30 m'), labels = c('250 m', '30 m'), 
#                     guide = guide_legend(override.aes = list(pattern = "none"))) + 
#   # source: https://stackoverflow.com/questions/67164758/adding-hatches-or-patterns-to-ggplot-bars
#   scale_pattern_manual(name = 'Coefficient Significance', values = c('sig' = "none", 'not sig' = "stripe"), 
#                        breaks = c('sig', 'not sig'), labels = c('Significant', 'Not Significant'),
#                        guide = guide_legend(override.aes = list(fill = "white", color = 'black'))) +
#   # source: https://www.statology.org/ggplot-facet-order/
#   facet_grid(c(predictor) ~ c(ID), scale = 'free') +
#   xlab('Study Period') + ylab('VIF') + ggtitle('Comparison of VIF for models trained with 250m and 30m data', subtitle = paste0(elephant, ' from ', start_date, ' to ', end_date)) + 
#   theme_minimal() 
# dev.off()


pdf('output/downscaling_plot_glm_vif_newPathWith10fCV_final.pdf',  width = 12, height = 8.5)
ggplot(data = downscaling_coef, aes(x = factor(seasons, level = c('April', 'June', 'August')), 
                                    y = VIF, fill = factor(downscaling, level = c('250 m', '30 m')), 
                                    color = model_sig_bool)) + 
  #geom_bar(data= downscaling_coef[downscaling_coef$downscaling == '250 m',], aes(y = glm_value, fill = glm_significance), stat = 'identity') + 
  #geom_bar(data= downscaling_coef[downscaling_coef$downscaling == '30 m',], aes(y = glm_value, fill = glm_significance), stat = 'identity') + 
  # source: https://stackoverflow.com/questions/20060949/ggplot2-multiple-sub-groups-of-a-bar-chart
  #geom_bar(aes(x = interaction(seasons, downscaling), y = glm_value, fill = glm_significance, group = downscaling), stat = 'identity') + 
  # source: https://guslipkin.medium.com/grouped-and-stacked-bar-charts-in-r-e5f5ac5637de
  geom_bar(position = position_dodge(preserve = "single"), stat = 'identity', color = NA) + 
  # source: https://www.geeksforgeeks.org/formatting-numbers-and-strings-in-r-programming-format-function/
  # note: set color at beginning to keep correct order of text (see source)
  # source: https://stackoverflow.com/questions/51734285/ggplot-adding-color-aesthetic-changes-stack-order
  geom_text(data = downscaling_coef[downscaling_coef$predictor == 'Dev. NDVI Growth Rate',], aes(label=format(model_sig, scientific = T, digits = 2),
                y = VIF + justify_vif * sign(VIF)), position=position_dodge(.9), size = 4) +
  geom_hline(data = data.frame(threshold = 5, predictor =c('Dev. NDVI', 'Dev. NDVI Growth Rate')), aes(yintercept = threshold), linetype = 'dashed', color = 'grey40') +
  # source: https://ggplot2.tidyverse.org/reference/scale_manual.html
  # source: https://stackoverflow.com/questions/41701960/ggplot2-adding-text-on-a-multiple-barplot
  scale_fill_manual(name = 'Spatial Resolution', values = c('250 m' = '#bce784', '30 m' = '#43A5C5'), 
                    breaks = c('250 m', '30 m'), labels = c('250 m', '30 m'), 
                    guide = guide_legend(override.aes = list(pattern = "none"))) + 
  scale_color_manual(name = 'Model Significance', values = c('significant' = 'blue', 'not significant' = 'black')) +
  # source: https://www.statology.org/ggplot-facet-order/
  facet_grid(c(predictor) ~ c(ID), scale = 'free') +
  xlab('Study Period') + ylab('VIF') + #ggtitle('Comparison of estimated coefficients for GLM models trained with 250m and 30m data', subtitle = paste0(elephant, ' from ', start_date, ' to ', end_date)) + 
  theme_minimal() + 
  theme(text=element_text(size=15)) #15 for png 12 for pdf
dev.off()




# quick reading of results table to see how many models had high VIF
r <- read.csv('output/LTS_df_results_aggregated.csv', row.names = 1)
r$run <- paste0(r$ID, "_", r$week)
length(unique(r$run))
v <- r[r$VIF > 5,]

# count how many vif per elephant and per month
t <- data.frame(run = v$run, id = v$ID, date = v$date)
u <- t[duplicated(t),]
table((format(as.Date(u$date, tz = 'Africa/Maputo'), "%m")))
table(u$id)

# quick reading of dates for weeks that have 100% model sig 
ms <- unique(r$date[r$Sig_Model_Proportion == 100])

# see which week had smallest interquartile range 
r$GLM_IQR <- r$GLM_Q3 - r$GLM_Q1
m <- r[r$predictor == "Avg. NDVI",]
mean(m$glm_value, na.rm = T)
hist(m$glm_value)
sd(m$glm_value, na.rm = T)

# calculate overall proportion of sig models per predictor
m <- r[r$predictor == "Dev. NDVI",]
nrow(m[m$glm_significance == 'sig',])/nrow(m)

# count how many models set to NA 
n <- r[is.na(r$glm_value),]
length(unique(n$run))
t <- data.frame(run = n$run, id = n$ID, date = n$date)
u <- t[duplicated(t),]
table((u$id))
table((format(as.Date(u$date, tz = 'Africa/Maputo'), "%Y-%m-%d")))

# find when coef becomes neg and get stats
t <- r[r$predictor == 'Dev. NDVI Growth',c('combo', 'ID', 'week', 'date', 'glm_value', 'GLM_Mean', 'Sig_GLM_Proportion')]
# t <- t[t$week %in% seq(2065, 2168), ]
# t <- t[t$week %in% seq(2169, 2194), ]
t <- t[t$week %in% seq(2065, 2139), ]
t <- t[t$week %in% seq(2140, 2194), ]
mean(t$GLM_Mean)
sd(t$GLM_Mean)
hist(t$glm_value)

t <- t[t$week == 2144,]


# quick reading of downscaling results 
r <- read.csv('output/downscaling_df_results.csv', row.names = 1)

# compare VIF betw res 
v250 <- r[r$downscaling == '250 m',]
v30 <- r[r$downscaling == '30 m',]  
v250 <- r[r$downscaling == '250 m' & r$predictor == 'Avg. NDVI',]
v30 <- r[r$downscaling == '30 m' & r$predictor == 'Avg. NDVI',]
v <- v250[,c('ID', 'week', 'date')]
v$VIF_250 <- v250$VIF
v$VIF_30 <- v30$VIF
v$diff <- v$VIF_30 - v$VIF_250
summary(v$diff)
sd(v$diff)

r <- r[, c('ID', 'week', 'date', 'downscaling', 'predictor', 'glm_significance')]



















######################## step 5 multivisualization #######################
r <- read.csv('data/run_settings_all_runs_old_path_with_CV.csv', row.names = 1)
row.names(r) <- 1:nrow(r)
# create short run table for datasets that want to combine 
run_table <- r[c(195, 476, 594, 602, 409, 422, 436, 444), ]

# create table of max extents of datasets
extent_LUT <- data.frame()

for(i in 1:nrow(run_table)){
  
  ID <- run_table$ID[i]
  week <- run_table$week[i]
  input_filepath <- paste0('data/', ID, '/', week, '/')
  random_data_method <- 'random_path_custom_distr'
  downscaling = 'NULL'
  downscaling_model = 'ranger_full_selection'
  title = 'Elephant movement on mean NDVI'
  
  # retrieve step extent LUT 
  lut <- read.csv(paste0(input_filepath, '2_a1_step_extents_LUT_', random_data_method, suffix, '.csv'), row.names = 1)
  
  entry <- data.frame(ID = ID, week = week, lut[nrow(lut),])
  
  extent_LUT <- rbind(extent_LUT, entry)
}  

# generate max extent when combining all datasets --> to have all images on same extent 
max_ext <- c(xmin = min(extent_LUT$xmin), ymin = min(extent_LUT$ymin), xmax = max(extent_LUT$xmax), ymax = max(extent_LUT$ymax))

# create empty list to store maps 
l_maps <- list()

for(i in 1:nrow(run_table)){
  
  ID <- run_table$ID[i]
  week <- run_table$week[i]
  input_filepath <- paste0('data/', ID, '/', week, '/')
  random_data_method <- 'random_path_custom_distr'
  downscaling = 'NULL'
  downscaling_model = 'ranger_full_selection'
  title = 'Elephant movement on mean NDVI'

  # define data directory and suffix
  if(downscaling == 'NULL'){
    modis_directory <- paste0(input_filepath, '3_a1_modis_images_', random_data_method, input_suffix, '/')

    suffix <- input_suffix

  }else if(downscaling == T){
    modis_directory <- paste0(input_filepath, '3_g1_downscaled_modis_images_30m_', downscaling_model, input_suffix, '/')

    suffix <- paste0('_downscaling_modis_30m', input_suffix)

  }else if(downscaling == F){
    modis_directory <- paste0(input_filepath, '3_b1_modis_images_downscaling_', random_data_method, input_suffix, '/')

    suffix <- paste0('_downscaling_modis_250m', input_suffix)

  }else{stop('Incorrect term set for downscaling parameter. Should be one of the following: NULL, T, F.')}

  # read NDVI dataset
  ndvi_data <- rast(paste0(modis_directory, 'mean_ndvi.tif'))
  ext(ndvi_data) <- max_ext
  names(ndvi_data) <- 'ndvi'

  # read step dataset
  dat <- read.csv(paste0(input_filepath, '4_a1_cov_resp_dataset_', random_data_method, suffix, '.csv'), row.names = 1)
  dat$random_id_ <- as.factor(dat$random_id_)
  #
  # add new step ID column that restarts count at each burst (so doesn't connect the different paths) --> consistency in dataset
  dat$stepID <- NA

  for(b in unique(dat$burst_)){
    # select rows of that burst that are true
    steps <- dat[dat$burst_ == b & dat$case_ == T,]
    dat$stepID[min(as.numeric(steps$step_id_)):max(as.numeric(steps$step_id_))] <- 1:nrow(steps)
  }

  # transfer the step ID of random steps to new column
  # NOTE: could move the code to some other script (unless the columns are useful)
  dat$stepID[dat$case_ == F] <- dat$step_id_[dat$case_ == F]

  # new column for pathID --> every different path (true and false) has a different ID, necessary for plotting paths separately
  dat$pathID <- NA

  # create new column for row names
  dat$rowNames <- rownames(dat)

  # find start of new path
  smin <- as.numeric(dat$rowNames[dat$step_id_ == min(dat$step_id_)])

  # assign new path ID to start of each new path
  dat$pathID[smin] <- 1:length(smin)

  # fill the column with values from above
  # package: tidyr
  # source: https://tidyr.tidyverse.org/reference/fill.html#ref-examples
  dat <- dat %>% fill(pathID)
  dat$pathID <- as.factor(dat$pathID)

  # rename the case column (necessary for plotting order)
  dat$case_[dat$case_ == T] <- 2
  dat$case_[dat$case_ == F] <- 1

  dat$case_ <- as.factor(dat$case_)


  ## PLOTTING

  # make elephant movement on NDVI map without legends
  image_map <- ggplot() +
    # source: https://dieghernan.github.io/tidyterra/reference/geom_spatraster.html
    geom_spatraster(data = ndvi_data, aes(fill = ndvi), alpha = 0.6, show.legend = F) +
    scale_fill_terrain_c(name = 'NDVI', limits = c(0,0.6), breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), alpha = 0.6) +
    geom_path(data = subset(dat, case_ == 1), aes(x = x1_, y = y1_, group = pathID), colour = 'grey30', linetype = 2, linewidth = 0.4) +
    geom_path(data = subset(dat, case_ == 2), aes(x = x1_, y = y1_, group = pathID,), colour = 'red', linetype = 1, linewidth = 0.7) +
    labs(title = paste0('Elephant ', ID, ' - ', format(as.Date(min(dat$t1_), tz = 'Africa/Maputo', '%B %d %Y')))) +
    annotation_north_arrow(location = 'tl', which_north = 'true',
                           pad_x = unit(0.5, "cm"), pad_y = unit(0.6, "cm"),
                           style = north_arrow_fancy_orienteering()) +
    annotation_scale(location = 'br', pad_x = unit(1.0, "cm"), pad_y = unit(0.5, "cm")) +
    theme_minimal() +
    theme(legend.position = 'none')
  
  l_maps <- append(l_maps, list(image_map))

}

# create legend elements from random dataset
# make NDVI map with legend
# source: https://dieghernan.github.io/tidyterra/reference/geom_spatraster.html
modis_ndvi_map <- ggplot() +
  geom_spatraster(data = ndvi_data, aes(fill = ndvi), alpha = 0.6, show.legend = T) +
  scale_fill_terrain_c(name = 'NDVI', limits = c(0,0.6), breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), alpha = 0.6)

# make rough path graph with legend (legend is what's important here)
path_map <- ggplot(data = dat, aes(x = x1_, y = y1_, colour = case_, group = pathID, linetype = case_)) +
  # source: https://stackoverflow.com/questions/27003991/how-can-i-define-line-plotting-order-in-ggplot2-for-grouped-lines
  geom_path(data = subset(dat, case_ == 1), linewidth = 0.4) +
  geom_path(data = subset(dat, case_ == 2), linewidth = 0.7) +
  # source: https://www.geeksforgeeks.org/control-line-color-and-type-in-ggplot2-plot-legend-in-r/
  scale_linetype_manual(name = "Elephant Path", labels = c('pseudo-absence', 'presence'), values = c(2,1)) +
  scale_color_manual(name = "Elephant Path", labels = c('pseudo-absence', 'presence'), values = c('grey30', '#e60000')) +
  theme_minimal()

# retrieve legends
ndvi_legend <- get_legend(modis_ndvi_map)
path_legend <- get_legend(path_map)

# create blank plot
blank_p <- plot_spacer() + theme_void()

# combine legends and plot
legends <- plot_grid(blank_p, path_legend, ndvi_legend, blank_p, nrow = 4)
final_map <- plot_grid(l_maps[[1]], legends, l_maps[[2]], ncol = 2, nrow = 2, rel_widths = c(1, 0.3))
final_map














# # define data directory and suffix
# if(downscaling == 'NULL'){
#   modis_directory <- paste0(input_filepath, '3_a1_modis_images_', random_data_method, input_suffix, '/')
#   
#   suffix <- input_suffix
#   
# }else if(downscaling == T){
#   modis_directory <- paste0(input_filepath, '3_g1_downscaled_modis_images_30m_', downscaling_model, input_suffix, '/')
#   
#   suffix <- paste0('_downscaling_modis_30m', input_suffix)
#   
# }else if(downscaling == F){
#   modis_directory <- paste0(input_filepath, '3_b1_modis_images_downscaling_', random_data_method, input_suffix, '/')
#   
#   suffix <- paste0('_downscaling_modis_250m', input_suffix)
#   
# }else{stop('Incorrect term set for downscaling parameter. Should be one of the following: NULL, T, F.')}
# 
# # read NDVI dataset
# ndvi_data <- rast(paste0(modis_directory, 'mean_ndvi.tif'))
# names(ndvi_data) <- 'ndvi'
# 
# # read step dataset
# dat <- read.csv(paste0(input_filepath, '4_a1_cov_resp_dataset_', random_data_method, suffix, '.csv'), row.names = 1)
# dat$random_id_ <- as.factor(dat$random_id_)
# # 
# # # add new step ID column that restarts count at each burst (so doesn't connect the different paths) --> consistency in dataset
# # dat$stepID <- NA
# # 
# # for(b in unique(dat$burst_)){
# #   # select rows of that burst that are true
# #   steps <- dat[dat$burst_ == b & dat$case_ == T,]
# #   dat$stepID[min(as.numeric(steps$step_id_)):max(as.numeric(steps$step_id_))] <- 1:nrow(steps)
# # }
# # 
# # # transfer the step ID of random steps to new column 
# # # NOTE: could move the code to some other script (unless the columns are useful)
# # dat$stepID[dat$case_ == F] <- dat$step_id_[dat$case_ == F]
# 
# # new column for pathID --> every different path (true and false) has a different ID, necessary for plotting paths separately 
# dat$pathID <- NA
# 
# # create new column for row names 
# dat$rowNames <- rownames(dat)
# 
# # find start of new path 
# smin <- as.numeric(dat$rowNames[dat$step_id_ == min(dat$step_id_)])
# 
# # assign new path ID to start of each new path 
# dat$pathID[smin] <- 1:length(smin)
# 
# # fill the column with values from above 
# # package: tidyr 
# # source: https://tidyr.tidyverse.org/reference/fill.html#ref-examples
# dat <- dat %>% fill(pathID)
# dat$pathID <- as.factor(dat$pathID)
# 
# # rename the case column (necessary for plotting order)
# dat$case_[dat$case_ == T] <- 2
# dat$case_[dat$case_ == F] <- 1
# 
# dat$case_ <- as.factor(dat$case_)
# 
# 
# ## PLOTTING
# 
# # make NDVI map with legend
# # source: https://dieghernan.github.io/tidyterra/reference/geom_spatraster.html
# modis_ndvi_map <- ggplot() +
#   geom_spatraster(data = ndvi_data, aes(fill = ndvi), alpha = 0.6, show.legend = T) +
#   scale_fill_terrain_c(name = 'NDVI', limits = c(0,0.6), breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), alpha = 0.6)
# modis_ndvi_map
# 
# # make rough path graph with legend (legend is what's important here)
# path_map <- ggplot(data = dat, aes(x = x1_, y = y1_, colour = case_, group = pathID, linetype = case_)) +
#   # source: https://stackoverflow.com/questions/27003991/how-can-i-define-line-plotting-order-in-ggplot2-for-grouped-lines
#   geom_path(data = subset(dat, case_ == 1), linewidth = 0.4) + 
#   geom_path(data = subset(dat, case_ == 2), linewidth = 0.7) +
#   # source: https://www.geeksforgeeks.org/control-line-color-and-type-in-ggplot2-plot-legend-in-r/
#   scale_linetype_manual(name = "Elephant Path", labels = c('pseudo-absence', 'presence'), values = c(2,1)) +
#   scale_color_manual(name = "Elephant Path", labels = c('pseudo-absence', 'presence'), values = c('grey30', '#e60000')) + 
#   theme_minimal() 
# path_map
# # make elephant movement on NDVI map without legends 
# image_map <- ggplot() +
#   geom_spatraster(data = ndvi_data, aes(fill = ndvi), alpha = 0.6, show.legend = F) +
#   scale_fill_terrain_c(name = 'NDVI', limits = c(0,0.6), breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), alpha = 0.6) +
#   geom_path(data = subset(dat, case_ == 1), aes(x = x1_, y = y1_, group = pathID), colour = 'grey30', linetype = 2, linewidth = 0.4) + 
#   geom_path(data = subset(dat, case_ == 2), aes(x = x1_, y = y1_, group = pathID,), colour = 'red', linetype = 1, linewidth = 0.7) +
#   labs(title = title, subtitle = paste0('Elephant ', ID, ' from ', as.Date(min(dat$t1_), tz = 'Africa/Maputo') , ' to ', 
#                                         as.Date(max(dat$t2_), tz = 'Africa/Maputo'),' (week ', week, ')'), x = "Longitude", y = "Latitude") +
#   annotation_north_arrow(location = 'tl', which_north = 'true', 
#                          pad_x = unit(0.5, "cm"), pad_y = unit(0.6, "cm"),
#                          style = north_arrow_fancy_orienteering()) +
#   annotation_scale(location = 'br', pad_x = unit(1.0, "cm"), pad_y = unit(0.5, "cm")) +
#   theme_minimal() + 
#   theme(legend.position = 'none')
# image_map
# # retrieve legends 
# ndvi_legend <- get_legend(modis_ndvi_map)
# path_legend <- get_legend(path_map)
# 
# # create blank plot
# blank_p <- plot_spacer() + theme_void()
# 
# # combine legends and plot
# legends <- plot_grid(blank_p, path_legend, ndvi_legend, blank_p, nrow = 4)
# final_map <- plot_grid(image_map, legends, nrow = 1, align = 'h', axis = 't', rel_widths = c(1, 0.3))
# final_map
# 