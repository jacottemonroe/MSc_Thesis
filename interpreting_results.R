## interpreting results 


# create empty summary results table 
summary_results <- data.frame()

# get table of runs 
run_settings <- read.csv('data/run_settings.csv', row.names = 1)

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
  entry <- data.frame(ID = ID, week = week, #pseudo_abs_method = pseudo_abs_method, 
                      VIF_full = NA, full_glm_sig_coef = NA, full_glm_deviance = NA, full_glm_sig = NA, full_clr_sig_coef = NA, full_clr_concord = NA, full_clr_concord_se = NA, 
                      VIF_sub = NA, sub_glm_sig_coef = NA, sub_glm_deviance = NA, sub_glm_sig = NA, sub_clr_sig_coef = NA, sub_clr_concord = NA, sub_clr_concord_se = NA)
  
  
  ## are the VIF values of the model acceptable? 
  # retrieve the dataset
  full_df <- read.csv(paste0(run_filepath, '6_a5_glm_full_vif_', pseudo_abs_method, '.csv'))
  sub_df <- read.csv(paste0(run_filepath, '6_b5_glm_50p_sd_vif_', pseudo_abs_method, '.csv'))
  
  # check if vif values below 5
  if(any(full_df$vif_results <5)){entry$VIF_full <- T}else{entry$VIF_full <- F}
  if(any(sub_df$vif_results <5)){entry$VIF_sub <- T}else{entry$VIF_sub <- F}
  
  
  ## does the glm model have a significant coefficient? 
  # retrieve dataset
  full_df <- read.csv(paste0(run_filepath, '6_a3_glm_full_coefs_', pseudo_abs_method, '.csv'))
  sub_df <- read.csv(paste0(run_filepath, '6_b3_glm_50p_sd_coefs', pseudo_abs_method, '.csv'))
  
  # exclude intercept 
  full_df <- full_df[full_df$X != '(Intercept)', ]
  sub_df <- sub_df[sub_df$X != '(Intercept)', ]
  
  # check if pvalues below 0.05
  if(any(full_df$Pr...z.. <=0.05)){entry$full_glm_sig_coef <- T}else{entry$full_glm_sig_coef <- F}
  if(any(sub_df$Pr...z.. <=0.05)){entry$sub_glm_sig_coef <- T}else{entry$sub_glm_sig_coef <- F}
  
  
  ## how high is the deviance of the fitted model? is the model significant? 
  # retrieve dataset
  full_df <- read.csv(paste0(run_filepath, '6_a4_glm_full_deviances_', pseudo_abs_method, '.csv'))
  sub_df <- read.csv(paste0(run_filepath, '6_b4_glm_50p_sd_deviances_', pseudo_abs_method, '.csv'))
  
  # select residual deviance 
  entry$full_glm_deviance <- full_df$residual_deviance
  entry$sub_glm_deviance <- sub_df$residual_deviance
  
  # derive p-value of model from chi-square stat
  # source: https://stats.stackexchange.com/questions/340489/interpretation-of-deviance-in-logistic-model
  # source: https://www.r-bloggers.com/2022/05/calculate-the-p-value-from-chi-square-statistic-in-r/
  full_pval <- pchisq(full_df$null_deviance - full_df$residual_deviance, full_df$null_df - full_df$residual_df, lower.tail = F)
  sub_pval <- pchisq(sub_df$null_deviance - sub_df$residual_deviance, sub_df$null_df - sub_df$residual_df, lower.tail = F)
  
  # check if pvalues below 0.05
  if(full_pval <=0.05){entry$full_glm_sig <- T}else{entry$full_glm_sig <- F}
  if(sub_pval <=0.05){entry$sub_glm_sig <- T}else{entry$sub_glm_sig <- F}
  
  
  ## does the clr model have a significant coefficient? 
  # retrieve dataset
  full_df <- read.csv(paste0(run_filepath, '6_a1_clr_full_coefs_', pseudo_abs_method, '.csv'))
  sub_df <- read.csv(paste0(run_filepath, '6_b1_clr_50p_sd_coefs_', pseudo_abs_method, '.csv'))
  
  # check if pvalues below 0.05
  if(any(full_df$Pr...z.. <=0.05)){entry$full_clr_sig_coef <- T}else{entry$full_clr_sig_coef <- F}
  if(any(sub_df$Pr...z.. <=0.05)){entry$sub_clr_sig_coef <- T}else{entry$sub_clr_sig_coef <- F}
  
  
  ## what is the concordance? how much does it vary by? 
  # retrieve dataset
  full_df <- read.csv(paste0(run_filepath, '6_a2_clr_full_tests_', pseudo_abs_method, '.csv'))
  sub_df <- read.csv(paste0(run_filepath, '6_b2_clr_50p_sd_tests_', pseudo_abs_method, '.csv'))
  
  # select concordance and SE
  entry$full_clr_concord <- full_df$concordance[1]
  entry$full_clr_concord_se <- full_df$concordance[4]
  entry$sub_clr_concord <- sub_df$concordance[1]
  entry$sub_clr_concord_se <- sub_df$concordance[4]
  
  # rbind the entry to the summary table 
  summary_results <- rbind(summary_results, entry)
}


# # create results data entry 
# entry <- data.frame(ID = ID, week = week, #pseudo_abs_method = pseudo_abs_method, 
#                     VIF_full = NA, full_glm_sig_coef = NA, full_glm_deviance = NA, full_glm_sig = NA, full_clr_sig_coef = NA, full_clr_concord = NA, full_clr_concord_se = NA, 
#                     VIF_sub = NA, sub_glm_sig_coef = NA, sub_glm_deviance = NA, sub_glm_sig = NA, sub_clr_sig_coef = NA, sub_clr_concord = NA, sub_clr_concord_se = NA)
# 
# 
# ## are the VIF values of the model acceptable? 
# # retrieve the dataset
# full_df <- read.csv(paste0(run_filepath, '6_a5_glm_full_vif.csv')) #_', pseudo_abs_method, '.csv'))
# sub_df <- read.csv(paste0(run_filepath, '6_b5_glm_50p_sd_vif.csv')) #_', pseudo_abs_method, '.csv'))
# 
# # check if vif values below 5
# if(any(full_df$vif_results <5)){entry$VIF_full <- T}else{entry$VIF_full <- F}
# if(any(sub_df$vif_results <5)){entry$VIF_sub <- T}else{entry$VIF_sub <- F}
# 
# 
# ## does the glm model have a significant coefficient? 
# # retrieve dataset
# full_df <- read.csv(paste0(run_filepath, '6_a3_glm_full_coefs.csv')) #_', pseudo_abs_method, '.csv'))
# sub_df <- read.csv(paste0(run_filepath, '6_b3_glm_50p_sd_coefs.csv')) #_', pseudo_abs_method, '.csv'))
# 
# # exclude intercept 
# full_df <- full_df[full_df$X != '(Intercept)', ]
# sub_df <- sub_df[sub_df$X != '(Intercept)', ]
# 
# # check if pvalues below 0.05
# if(any(full_df$Pr...z.. <=0.05)){entry$full_glm_sig_coef <- T}else{entry$full_glm_sig_coef <- F}
# if(any(sub_df$Pr...z.. <=0.05)){entry$sub_glm_sig_coef <- T}else{entry$sub_glm_sig_coef <- F}
# 
# 
# ## how high is the deviance of the fitted model? is the model significant? 
# # retrieve dataset
# full_df <- read.csv(paste0(run_filepath, '6_a4_glm_full_deviances.csv')) #_', pseudo_abs_method, '.csv'))
# sub_df <- read.csv(paste0(run_filepath, '6_b4_glm_50p_sd_deviances.csv')) #_', pseudo_abs_method, '.csv'))
# 
# # select residual deviance 
# entry$full_glm_deviance <- full_df$residual_deviance
# entry$sub_glm_deviance <- sub_df$residual_deviance
# 
# # derive p-value of model from chi-square stat
# # source: https://stats.stackexchange.com/questions/340489/interpretation-of-deviance-in-logistic-model
# # source: https://www.r-bloggers.com/2022/05/calculate-the-p-value-from-chi-square-statistic-in-r/
# full_pval <- pchisq(full_df$null_deviance - full_df$residual_deviance, full_df$null_df - full_df$residual_df, lower.tail = F)
# sub_pval <- pchisq(sub_df$null_deviance - sub_df$residual_deviance, sub_df$null_df - sub_df$residual_df, lower.tail = F)
# 
# # check if pvalues below 0.05
# if(full_pval <=0.05){entry$full_glm_sig <- T}else{entry$full_glm_sig <- F}
# if(sub_pval <=0.05){entry$sub_glm_sig <- T}else{entry$sub_glm_sig <- F}
# 
# 
# ## does the clr model have a significant coefficient? 
# # retrieve dataset
# full_df <- read.csv(paste0(run_filepath, '6_a1_clr_full_coefs.csv')) #_', pseudo_abs_method, '.csv'))
# sub_df <- read.csv(paste0(run_filepath, '6_b1_clr_50p_sd_coefs.csv')) #_', pseudo_abs_method, '.csv'))
# 
# # check if pvalues below 0.05
# if(any(full_df$Pr...z.. <=0.05)){entry$full_clr_sig_coef <- T}else{entry$full_clr_sig_coef <- F}
# if(any(sub_df$Pr...z.. <=0.05)){entry$sub_clr_sig_coef <- T}else{entry$sub_clr_sig_coef <- F}
# 
# 
# ## what is the concordance? how much does it vary by? 
# # retrieve dataset
# full_df <- read.csv(paste0(run_filepath, '6_a2_clr_full_tests.csv')) #_', pseudo_abs_method, '.csv'))
# sub_df <- read.csv(paste0(run_filepath, '6_b2_clr_50p_sd_tests.csv')) #_', pseudo_abs_method, '.csv'))
# 
# # select concordance and SE
# entry$full_clr_concord <- full_df$concordance[1]
# entry$full_clr_concord_se <- full_df$concordance[4]
# entry$sub_clr_concord <- sub_df$concordance[1]
# entry$sub_clr_concord_se <- sub_df$concordance[4]
