## MSc_Thesis
## Jacotte Monroe 
## 19/03/24

## R script that runs function to fit statistical models on the movement steps dataset.
## Performs plots the correlation matrix of predictors for a manual multicolinearity check. 
## Fits general and conditional logistic regressions onto the data. 
## Runs the models with all the predictors and with only 50th percentiles and standard deviations as predictors. 
## For each model, the Variance Inflation Factors (VIF) is calculated and the model outputs (coefficients, deviance, concordance, tests) are saved. 
## Conditional logistic regression outputs stat tests: Likelihood ratio, Wald, score tests. 
## Script outputs: 
##    6_a0: Correlation matrix of predictors from step dataset, saved as png. (final output)
##    6_a1: Conditional logistic regression model (CLR) coefficients after fitting model with all predictors, saved as csv. (final output)
##    6_a2: CLR stat tests and concordance results after fitting model with all predictors, saved as csv. (final output)
##    6_a3: General logistic regression model (GLM) coefficients after fitting model with all predictors, saved as csv. (final output)
##    6_a4: GLM deviances after fitting model with all predictors, saved as csv. (final output)
##    6_a5: GLM VIF values for each predictor after fitting model with all predictors, saved as csv. (final output)
##
##    6_b1: Conditional logistic regression model (CLR) coefficients after fitting model with subset of predictors, saved as csv. (final output)
##    6_b2: CLR stat tests and concordance results after fitting model with subset of predictors, saved as csv. (final output)
##    6_b3: General logistic regression model (GLM) coefficients after fitting model with subset of predictors, saved as csv. (final output)
##    6_b4: GLM deviances after fitting model with subset of predictors, saved as csv. (final output)
##    6_b5: GLM VIF values for each predictor after fitting model with subset of predictors, saved as csv. (final output)



###########
## Read run settings 
###########

run_settings <- read.csv('data/single_run_settings.csv', header = F, row.names = 1)

# define elephant ID
ID <- run_settings[[1]]

# define week to test 
week <- run_settings[[2]]

# define pseudo-absence path generator method --> Choices are 1) random_path_custom_distr 2) random_path_buffer_point 3) random_step
pseudo_abs_method <- run_settings[[3]]

# set downscaling parameter
downscaling_setting <- run_settings[[4]]

# define run filepath 
run_filepath <- paste0('data/', ID, '/', week, '/')

print(paste('Now fitting models for elephant', ID, 'of week', week, '...'))



###########
## Fit models on dataset with all predictors, plot correlation matrix, and save model outputs
###########
# load function
source('functions_elephant_ssf/6_a_fittingModels.R')

# load necessary packages 
if(!('dplyr') %in% installed.packages()){install.packages('dplyr')}
library(dplyr) 
if(!('ggplot2') %in% installed.packages()){install.packages('ggplot2')}
library(ggplot2) # needed for loading GGally package
if(!('GGally') %in% installed.packages()){install.packages('GGally')}
library(GGally)
if(!('amt') %in% installed.packages()){install.packages('amt')}
library(amt)
if(!('car') %in% installed.packages()){install.packages('car')}
library(car) # needed to run vif()

# run function
#fitSSFModel(run_filepath, ID, week, pseudo_abs_method, downscaling = downscaling_setting, multicolinearity_check = T, full = T, output_directory = 'output/')



###########
## Fit models on dataset with subset of predictors and save model outputs
###########
# run function
fitSSFModel(run_filepath, ID, week, pseudo_abs_method, downscaling = downscaling_setting, multicolinearity_check = T, full = F, output_directory = 'output/')

print(paste('(DONE) Fitting models for elephant', ID, 'of week', week))
print(paste('(COMPLETE) Elephant', ID, 'has been successfully processed for week', week))







# 
# 
# 
# ID <- 'LA26'
# week <- 2260
# pseudo_abs_method <- 'random_path_custom_distr'
# downscaling_setting <- F
# 
# # define run filepath 
# run_filepath <- paste0('data/', ID, '/', week, '/')
# 
# input_directory <- run_filepath
# random_data_method <- pseudo_abs_method
# downscaling <- downscaling_setting
# multicolinearity_check <- F
# full <- T
# output_directory <- 'output/'
# 
# # create output filepath 
# output_filepath <- paste0(output_directory, ID, '/', week, '/')
# 
# if(downscaling == 'NULL'){
#   suffix <- ''
#   
# }else if(downscaling == T){
#   suffix <- '_downscaling_modis_30m'
#   
# }else if(downscaling == F){
#   suffix <- '_downscaling_modis_250m'
#   
# }else{stop('Incorrect term set for downscaling parameter. Should be one of the following: NULL, T, F.')}
# 
# # read step dataset
# step_dataset <- read.csv(paste0(input_directory, '4_a1_cov_resp_dataset_', random_data_method, suffix, '.csv'))
# 
# 
# # fit conditional logistic regression and general logistic regression models 
# if(full == T){
#   output_number = '6_a'
#   model_type = '_full_'
#   
#   # source: https://cran.r-project.org/web/packages/amt/vignettes/p4_SSF.html
#   clr_model <- fit_clogit(step_dataset, case_ ~ ndvi_10 + ndvi_50 + ndvi_90 + ndvi_sd + 
#                             ndvi_rate_10 + ndvi_rate_50 + ndvi_rate_90 + ndvi_rate_sd + strata(step_id_))
#   
#   # source: https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/
#   glm_model <- glm(case_ ~ ndvi_10 + ndvi_50 + ndvi_90 + ndvi_sd + ndvi_rate_10 + ndvi_rate_50 +
#                      ndvi_rate_90 + ndvi_rate_sd, family = binomial(link = 'logit'), data = step_dataset)
#   
# }else{
#   output_number = '6_b'
#   model_type = '_50p_sd_'
#   
#   # source: https://cran.r-project.org/web/packages/amt/vignettes/p4_SSF.html
#   clr_model <- fit_clogit(step_dataset, case_ ~ ndvi_50 + ndvi_sd + 
#                             ndvi_rate_50 + ndvi_rate_sd + strata(step_id_))
#   
#   # source: https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/
#   glm_model <- glm(case_ ~ ndvi_50 + ndvi_sd + ndvi_rate_50 + ndvi_rate_sd, 
#                    family = binomial(link = 'logit'), data = step_dataset)
# }
# 
# 
# 
# # get model summaries
# clr_summary <- summary(clr_model)
# glm_summary <- summary(glm_model)
# 
# # get VIF from GLM
# vif_results <- vif(glm_model)
# 
# # print information about the dataset and the model
# print(paste('Dataset elephant:', ID))
# print(paste('Week:', as.character(week)))
# print(paste('Pseudo-absence method:', random_data_method))
# print(paste('Number of total observations:', as.character(nrow(step_dataset))))
# print(paste('Number of observations included in model:', clr_summary$n))
# print(paste('Sets of steps:', clr_summary$nevent))
# print(clr_summary)
# print(glm_summary)
# print(vif_results)
# 
# # get model coefficients 
# # source: https://stackoverflow.com/questions/61482594/export-coxph-summary-from-r-to-csv
# clr_coef <- data.frame(clr_summary$coefficients)
# glm_coef <- data.frame(glm_summary$coefficients)
# 
# # create dataframe with statistical test results of CLR, the concordance and its standard error 
# clr_tests <- data.frame(log_likelihood = clr_summary$logtest, score = clr_summary$sctest, wald = clr_summary$waldtest)
# clr_tests <- rbind(clr_tests, SE_concordance = c(NA))
# clr_tests <- cbind(clr_tests, concordance = c(clr_summary$concordance[1], NA, NA, clr_summary$concordance[2]))
# 
# # create dataframe of deviance and vif results from GLM
# glm_deviances <- data.frame(null_deviance = glm_summary$null.deviance, null_df = glm_summary$df.null, 
#                             residual_deviance = glm_summary$deviance, residual_df = glm_summary$df.residual)
# glm_vif <- data.frame(vif_results)
# 
# # save model results as csv 
# write.csv(clr_coef, paste0(output_filepath, output_number, '1_clr', model_type, 'coefs_', random_data_method, suffix, '.csv'))
# write.csv(clr_tests, paste0(output_filepath, output_number, '2_clr', model_type, 'tests_', random_data_method, suffix, '.csv'))
# 
# write.csv(glm_coef, paste0(output_filepath, output_number, '3_glm', model_type, 'coefs_', random_data_method, suffix, '.csv'))
# write.csv(glm_deviances, paste0(output_filepath, output_number, '4_glm', model_type, 'deviances_', random_data_method, suffix, '.csv'))
# write.csv(glm_vif, paste0(output_filepath, output_number, '5_glm', model_type, 'vif_', random_data_method, suffix, '.csv'))
# 
# 
# paste0(output_filepath, output_number, '1_clr', model_type, 'coefs_', random_data_method, suffix, '.csv')
# paste0(output_filepath, output_number, '2_clr', model_type, 'tests_', random_data_method, suffix, '.csv')
