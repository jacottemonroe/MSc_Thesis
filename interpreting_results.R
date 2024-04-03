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
  entry <- data.frame(ID = ID, week = week, pseudo_abs_method = pseudo_abs_method, 
                      VIF_full = NA, full_glm_sig_coef = NA, full_glm_sig_coef_which = NA, full_glm_deviance = NA, full_glm_sig = NA, full_clr_sig_coef = NA, full_clr_sig_coef_which = NA, full_clr_concord = NA, full_clr_concord_se = NA, 
                      VIF_sub = NA, sub_glm_sig_coef = NA, sub_glm_sig_coef_which = NA, sub_glm_deviance = NA, sub_glm_sig = NA, sub_clr_sig_coef = NA, sub_clr_sig_coef_which = NA, sub_clr_concord = NA, sub_clr_concord_se = NA)
  
  
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
  sub_df <- read.csv(paste0(run_filepath, '6_b3_glm_50p_sd_coefs_', pseudo_abs_method, '.csv'))
  
  # exclude intercept 
  full_df <- full_df[full_df$X != '(Intercept)', ]
  sub_df <- sub_df[sub_df$X != '(Intercept)', ]
  
  # check if pvalues below 0.05
  if(any(full_df$Pr...z.. <=0.05)){entry$full_glm_sig_coef <- T}else{entry$full_glm_sig_coef <- F}
  if(any(sub_df$Pr...z.. <=0.05)){entry$sub_glm_sig_coef <- T}else{entry$sub_glm_sig_coef <- F}
  
  # select predictors that are significant with 95% confidence 
  # source: https://sparkbyexamples.com/r-programming/r-merge-vector-to-string/
  entry$full_glm_sig_coef_which <- paste(full_df$X[full_df$Pr...z.. < 0.05], collapse = '; ')
  entry$sub_glm_sig_coef_which <- paste(sub_df$X[sub_df$Pr...z.. < 0.05], collapse = '; ')

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
  
  # select predictors that are significant with 95% confidence 
  # source: https://sparkbyexamples.com/r-programming/r-merge-vector-to-string/
  entry$full_clr_sig_coef_which <- paste(full_df$X[full_df$Pr...z.. < 0.05], collapse = '; ')
  entry$sub_clr_sig_coef_which <- paste(sub_df$X[sub_df$Pr...z.. < 0.05], collapse = '; ')

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

# results for running 13 elephants on week 2075 with three methods 
write.csv(summary_results, 'output/summary_results_13E_w2075_3M.csv')




#### inspecting all runs 

# select method type 
m <- summary_results[summary_results$pseudo_abs_method == 'random_step',]

print(paste('Total number of model:', nrow(m)))
print(paste('Number of full models with VIF <5:', sum(m$VIF_full == T)))
print(paste('Number of subset models with VIF <5:', sum(m$VIF_sub == T)))

mf <- m[m$VIF_full == T,]
ms <- m[m$VIF_sub == T,]

print(paste('Number of full models with significant GLM (based on deviance):', sum(mf$full_glm_sig == T)))
print(paste('Number of subset models with significant GLM (based on deviance):', sum(ms$sub_glm_sig == T)))

print(paste('Number of full models with significant CLR coef:', sum(mf$full_clr_sig_coef == T)))
print(paste('Number of subset models with significant CLR coef:', sum(ms$sub_clr_sig_coef == T)))

mfg <- mf[mf$full_glm_sig == T,]
msg <- ms[ms$sub_glm_sig == T,]

print(paste('Number of full models with significant GLM coef:', sum(mfg$full_glm_sig_coef == T)))
print(paste('Number of subset models with significant GLM coef:', sum(msg$sub_glm_sig_coef == T)))

mf <- mf[mf$full_clr_sig_coef == T,]
ms <- ms[ms$sub_clr_sig_coef == T,]

mfg <- mfg[mfg$full_glm_sig_coef == T,]
msg <- msg[msg$sub_glm_sig_coef == T,]

print(paste('Significant GLM predictors in full models:', mfg$full_glm_sig_coef_which))
print(paste('Significant GLM predictors in subset models:', msg$sub_glm_sig_coef_which))

print(paste('Significant CLR predictors in full models:', mf$full_clr_sig_coef_which))
print(paste('Significant CLR predictors in subset models:', ms$sub_clr_sig_coef_which))

print(paste('CLR concordance of full models:', 'MIN:', min(mf$full_clr_concord), 
            'MEAN:', mean(mf$full_clr_concord), 'MAX', max(mf$full_clr_concord)))
print(paste('CLR concordance of subset models:', 'MIN:', min(ms$sub_clr_concord), 
            'MEAN:', mean(ms$sub_clr_concord), 'MAX', max(ms$sub_clr_concord)))






method_cd <- summary_results[summary_results$pseudo_abs_method == 'random_path_custom_distr' & summary_results$sub_clr_sig_coef == T,]
method_bp <- summary_results[summary_results$pseudo_abs_method == 'random_path_buffer_point' & summary_results$sub_clr_sig_coef == T,]
method_s <- summary_results[summary_results$pseudo_abs_method == 'random_step' & summary_results$sub_glm_clr_coef == T,]

c_cd <- mean(method_cd$sub_clr_concord)
c_bp <- mean(method_bp$sub_clr_concord)
c_s <- mean(method_s$sub_clr_concord)

sig_clr <- summary_results[summary_results$sub_clr_sig_coef ==T & summary_results$pseudo_abs_method == 'random_path_buffer_point',]
c <- mean(sig_clr$sub_clr_concord)

which_pred <- table(sig_clr$sub_clr_sig_coef_which)





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





##################### plot model (s curve plot)
# tutorial: https://www.theanalysisfactor.com/r-glm-plotting/

# retrieve dataset 
dat <- read.csv('data/LA14/2260/4_a1_cov_resp_dataset_random_path_custom_distr_downscaling_modis_250m.csv', row.names = 1)
cov <- dat[,c('ndvi_50', 'ndvi_sd', 'ndvi_rate_50', 'ndvi_rate_sd')]
# retrieve model 
model <- readRDS(paste0('output/LA14/2260/6_b0_glm_50p_sd_model_random_path_custom_distr_downscaling_modis_250m.RDS'))
summary(model)

# create dataframe of values 
dat_new <- data.frame(ndvi_50 = seq(min(dat$ndvi_50), max(dat$ndvi_50), length.out = 60), 
                      ndvi_sd = seq(min(dat$ndvi_sd), max(dat$ndvi_sd), length.out = 60), 
                      ndvi_rate_50 = seq(min(dat$ndvi_rate_50), max(dat$ndvi_rate_50), length.out = 60), 
                      ndvi_rate_sd = seq(min(dat$ndvi_rate_sd), max(dat$ndvi_rate_sd), length.out = 60))

# set axis for plotting 
xvalues <- seq(min(dat_new), max(dat_new), length.out = 60)
yvalues <- predict(model, newdata = dat_new, type = 'response')

# create s curve plot
plot(xvalues, yvalues)
plot(x=seq(20, 65, by=5), predict( mlogit, 
                                   newdata=data.frame(Drug="Y", Environment="H", Ethnicity="White",
                                                      Age=seq(20, 65, by=5) ), type="response" ) )

# plot model 
plot(xvalues, yvalues)

plot(mtcars$disp, mtcars$vs, pch = 16, xlab = "DISPLACEMENT (cubic inches)", ylab = "VS")





