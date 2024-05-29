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

# replace NA from suffix columns of run settings to empty strings
if(is.na(run_settings[[6]])){run_settings[[6]] <- ''}
if(is.na(run_settings[[7]])){run_settings[[7]] <- ''}

# define input and output suffixes
input_suffix <- run_settings[[6]]
output_suffix <- run_settings[[7]]

print(paste('Now fitting models for elephant', ID, 'of week', week, '...'))



###########
## Fit models on dataset with all predictors, plot correlation matrix, and save model outputs
###########
# load function
source('functions_elephant_ssf/6_a_fittingModels.R')

# load necessary packages 
if(!('dplyr') %in% installed.packages()){install.packages('dplyr')}
library(dplyr) # group data
if(!('caret') %in% installed.packages()){install.packages('caret')}
library(caret) # GLM modeling and cross-validation
# if(!('amt') %in% installed.packages()){install.packages('amt')}
# library(amt) # CLR modeling 
if(!('car') %in% installed.packages()){install.packages('car')}
library(car) # needed to run vif()
if(!('cutpointr') %in% installed.packages()){install.packages('cutpointr')}
library(cutpointr) # for hyperparameter tunning on test set 
# if(!('ggplot2') %in% installed.packages()){install.packages('ggplot2')}
# library(ggplot2) # needed for loading GGally package for confusion matrix
# if(!('GGally') %in% installed.packages()){install.packages('GGally')}
# library(GGally)

# # run function 
# fitMovementModel(run_filepath, ID, week, pseudo_abs_method, downscaling = downscaling_setting, 
#                  input_suffix = input_suffix, output_suffix = output_suffix)


# fitMovementModel(run_filepath, ID, week, pseudo_abs_method, downscaling = downscaling_setting, 
#                  input_suffix = input_suffix, output_suffix = output_suffix)

fitMovementModelWith10fCV(run_filepath, ID, week, pseudo_abs_method, downscaling = downscaling_setting, 
                 input_suffix = input_suffix, output_suffix = output_suffix)

# OUTDATED: run function with more settings (confusion matrix, all predictors, not scaled...)
#fitSSFModel_AdditionalSettings(run_filepath, ID, week, pseudo_abs_method, downscaling = downscaling_setting, 
#             multicolinearity_check = T, full = T, output_directory = 'output/')
# fitSSFModel(run_filepath, ID, week, pseudo_abs_method, downscaling = downscaling_setting, 
#             multicolinearity_check = T, full = F, scaled = T, output_directory = 'output/')


print(paste('(DONE) Fitting models for elephant', ID, 'of week', week))
print(paste('(COMPLETE) Elephant', ID, 'has been successfully processed for week', week))








# 
# 
# run_settings <- read.csv('data/run_settings_LTS_rerun.csv', row.names = 1)[8:8,]
# #run_settings <- run_settings[!(run_settings$ID == 'LA4' & run_settings$week == 2072),]
# 
# for(i in 1:nrow(run_settings)){
# 
#   # define elephant ID
#   ID <- run_settings$ID[i]
# 
#   # define week to test
#   week <- run_settings$week[i]
# 
#   # define pseudo-absence path generator method --> Choices are 1) random_path_custom_distr 2) random_path_buffer_point 3) random_step
#   pseudo_abs_method <- run_settings$pseudo_abs_method[i]
# 
#   # set downscaling parameter
#   downscaling_setting <- run_settings$downscaling[i]
# 
#   # define run filepath
#   run_filepath <- paste0('data/', ID, '/', week, '/')
# 
#   # define input and output suffixes
#   input_suffix <- '_newPathWithCV'
#   output_suffix <- '_newPathWithCV'
# 
#   print(paste('Now fitting models for elephant', ID, 'of week', week, '...'))
# 
# 
# 
#   ###########
#   ## Fit models on dataset with all predictors, plot correlation matrix, and save model outputs
#   ###########
#   # load function
#   source('functions_elephant_ssf/6_a_fittingModels.R')
# 
#   # load necessary packages
#   if(!('dplyr') %in% installed.packages()){install.packages('dplyr')}
#   library(dplyr) # group data
#   if(!('caret') %in% installed.packages()){install.packages('caret')}
#   library(caret) # GLM modeling and cross-validation
#   if(!('amt') %in% installed.packages()){install.packages('amt')}
#   library(amt) # CLR modeling
#   if(!('car') %in% installed.packages()){install.packages('car')}
#   library(car) # needed to run vif()
# 
#   # # run function
#   fitMovementModel(run_filepath, ID, week, pseudo_abs_method, downscaling = downscaling_setting,
#                    input_suffix = input_suffix, output_suffix = output_suffix)
# 
#   print(paste('(DONE) Fitting models for elephant', ID, 'of week', week))
#   print(paste('(COMPLETE) Elephant', ID, 'has been successfully processed for week', week))
# 
# 
# 
# }
# 
# 



# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # define elephant ID
# ID <- run_settings$ID #LA26
# 
# # define week to test
# week <- 2247 #run_settings$week #2247
# 
# # define pseudo-absence path generator method --> Choices are 1) random_path_custom_distr 2) random_path_buffer_point 3) random_step
# pseudo_abs_method <- run_settings$pseudo_abs_method
# 
# # set downscaling parameter
# downscaling_setting <- run_settings$downscaling
# 
# # define run filepath
# run_filepath <- paste0('data/', ID, '/', week, '/')
# 
# # define input and output suffixes
# input_suffix <- '_newPathWithCV'
# output_suffix <- '_newPathWithCV'
# 
# input_directory <- run_filepath
# random_data_method <- pseudo_abs_method
# downscaling <- downscaling_setting
# 
# output_directory <- 'output/'
# 
# 
# 
# ######## STEP 0: Set-Up
# 
# 
# # create output filepath 
# output_filepath <- paste0(output_directory, ID, '/', week, '/')
# 
# # create data directory if it does not yet exist
# if(!dir.exists(output_filepath)){dir.create(output_filepath, recursive = T)}
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
# # define model specifics to avoid overwriting previous results 
# output_number = '6_c'
# model_type = '_mean_sd_' # refers to model built no selection of predictors
# 
# # read step dataset
# step_dataset <- read.csv(paste0(input_directory, '4_a1_cov_resp_dataset_', random_data_method, 
#                                 suffix, input_suffix, '.csv'), row.names = 1)
# 
# # duplicate the case outcome column (to keep a version with boolean)
# step_dataset$case_bool <- step_dataset$case_
# 
# # rename case outcome T = presence; F = absence 
# step_dataset$case_[step_dataset$case_ == T] <- 'presence'
# step_dataset$case_[step_dataset$case_ == F] <- 'absence'
# 
# # # fix the step ID column to restart step count for every burst 
# # step_dataset$stepID <- NA
# # 
# # for(b in unique(step_dataset$burst_)){
# #   # select rows of that burst that are true
# #   steps <- step_dataset[step_dataset$burst_ == b & step_dataset$case_ == 'presence',]
# #   step_dataset$stepID[min(as.numeric(steps$step_id_)):max(as.numeric(steps$step_id_))] <- 1:nrow(steps)
# # }
# # 
# # # transfer the step ID of random steps to new column 
# # # NOTE: could move the code to some other script (unless the columns are useful)
# # step_dataset$stepID[step_dataset$case_ == 'absence'] <- step_dataset$step_id_[step_dataset$case_ == 'absence']
# # 
# # create new column that groups true/false cases for corresponding step in burst 
# # package: dplyr
# # source: https://stackoverflow.com/questions/24119599/how-to-assign-a-unique-id-number-to-each-group-of-identical-values-in-a-column
# step_dataset <- step_dataset %>% group_by(burst_, step_id_) %>% mutate(pair_id=cur_group_id())
# 
# # set the class to factor type instead of string
# step_dataset$case_ <- factor(step_dataset$case_, levels = c('absence', 'presence'))
# 
# 
# ######## STEP 1: Split step dataset into training and testing sets 
# 
# 
# # get list of pair IDs (all pair groups have 1 T and 20 F)
# pair_id_list <- unique(step_dataset$pair_id)
# 
# # generate random split in indices of step pairs
# # package: caret
# # source: https://rforhr.com/kfold.html
# set.seed(152)
# partition <- sample(pair_id_list, size = floor(0.7*length(pair_id_list)), replace = F)
# #partition <- unname(createDataPartition(pair_id_list, p=.7, list=F))
# 
# # split dataset into training and testing based on partition
# training_steps <- step_dataset[step_dataset$pair_id %in% partition,]
# test_steps <- step_dataset[!(step_dataset$pair_id %in% partition),]
# 
# # convert outcome column to factor (necessary to define classes)
# training_steps$case_ <- factor(training_steps$case_, levels = c('absence', 'presence'))
# test_steps$case_ <- factor(test_steps$case_, levels = c('absence', 'presence'))
# 
# # remove any steps that contain NA in test steps (normally should not have any but for reason some slipped through so this is a fail safe)
# test_steps <- test_steps[complete.cases(test_steps[,c('case_', 'ndvi_mean_scaled', 'ndvi_sd_scaled', 
#                                                       'ndvi_rate_mean_scaled', 'ndvi_rate_sd_scaled')]),]
# 
# 
# ######## STEP 2: Implement cross-validation and fit GLM model on training step dataset
# 
# 
# # retrieve custom GLM model
# source('functions_elephant_ssf/6_a_buildingCustomGLM.R')
# custom_glm <- buildCustomGLM()
# 
# # define cross-validation settings 
# # Note: Applying a 10-fold cross-validation on the training data 
# # Note: Using custom performance metric (from 6_a_buildingCustomGLM.R script) for hyperparameter tuning
# # package: caret
# cv_settings_custom <- trainControl(method = 'cv', classProbs = T, summaryFunction = performanceFunction)
# 
# # fit a custom GLM model through the training data with hyperparameter tuning of threshold probability
# # source: https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/
# # source: https://www.rdocumentation.org/packages/traineR/versions/2.2.0/topics/train.glm
# # source: https://cran.r-project.org/web/packages/caret/vignettes/caret.html
# # source: https://stackoverflow.com/questions/38250440/error-in-na-fail-default-missing-values-in-object-but-no-missing-values
# # Note: 20 threshold values are tested in the parameter tuning 
# # Note: Dist used as custom performance metric for tuning
# # package: caret 
# glm_model_custom <- train(case_ ~ ndvi_mean_scaled + ndvi_sd_scaled + ndvi_rate_mean_scaled + ndvi_rate_sd_scaled, 
#                           data = training_steps, method = custom_glm, family = binomial(link = 'logit'),
#                           metric = 'Dist', maximize = F, tuneLength = 20, trControl = cv_settings_custom, na.action = na.pass)
# 
# # also fit conditional logistic regression and general logistic regression models (without cross-validation)
# # note: models trained on full step dataset (no split into training and validation)
# # package: amt
# # source: https://cran.r-project.org/web/packages/amt/vignettes/p4_SSF.html
# clr_model <- fit_clogit(step_dataset, case_bool ~ ndvi_mean_scaled + ndvi_sd_scaled + 
#                           ndvi_rate_mean_scaled + ndvi_rate_sd_scaled + strata(step_id_))
# 
# # package: caret
# # source: https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/
# glm_model <- glm(case_ ~ ndvi_mean_scaled + ndvi_sd_scaled + ndvi_rate_mean_scaled + ndvi_rate_sd_scaled, 
#                  family = binomial(link = 'logit'), data = step_dataset)
# 
# 
# ######## STEP 3: Predict test set with trained, optimized, and validated GLM model
# 
# # predict likelihood of elephant occurrence on step
# # Note: Type 'raw' outputs the class directly instead of the probability based on the tuned cutoff value from trained model
# test_steps$prediction  <- predict(glm_model_custom, newdata = test_steps , type = "raw")
# 
# # create confusion matrix
# # source: https://stackoverflow.com/questions/46028360/confusionmatrix-for-logistic-regression-in-r
# # package: caret
# cm <- confusionMatrix(data = test_steps$prediction, reference = test_steps$case_, positive = 'presence')
# 
# 
# ######## STEP 4: Retrieve model results and performance metrics 
# 
# 
# # get VIF from GLM (not applicable to train object = cross-validated GLM model or CLR)
# # package: car
# glm_vif <- data.frame(vif(glm_model))
# 
# # get model summaries
# clr_summary <- summary(clr_model)
# glm_summary <- summary(glm_model)
# glm_summary_c <- summary(glm_model_custom)
# 
# # get model coefficients 
# # source: https://stackoverflow.com/questions/61482594/export-coxph-summary-from-r-to-csv
# clr_coef <- data.frame(clr_summary$coefficients)
# glm_coef <- data.frame(glm_summary$coefficients)
# glm_coef_c <- data.frame(glm_summary_c$coefficients)
# 
# # create dataframe with statistical test results of CLR, the concordance and its standard error 
# clr_tests <- data.frame(log_likelihood = clr_summary$logtest, score = clr_summary$sctest, wald = clr_summary$waldtest)
# clr_tests <- rbind(clr_tests, SE_concordance = c(NA))
# clr_tests <- cbind(clr_tests, concordance = c(clr_summary$concordance[1], NA, NA, clr_summary$concordance[2]))
# 
# # create dataframe of deviance and AIC for GLM models 
# glm_deviances <- data.frame(AIC = glm_summary$aic, null_deviance = glm_summary$null.deviance, null_df = glm_summary$df.null, 
#                             residual_deviance = glm_summary$deviance, residual_df = glm_summary$df.residual)
# glm_deviances_c <- data.frame(AIC = glm_summary_c$aic, null_deviance = glm_summary_c$null.deviance, null_df = glm_summary_c$df.null, 
#                               residual_deviance = glm_summary_c$deviance, residual_df = glm_summary_c$df.residual)
# 
# # create dataframe with optimal cross-validation results from custom GLM
# glm_c_validation <- glm_model_custom$results[glm_model_custom$results$threshold == glm_model_custom$bestTune$threshold,]
# 
# # create dataframe with custom GLM performance results on test set 
# glm_c_test <- data.frame(cm$byClass)
# 
# 
# ### save outputs 
# 
# suffix <- paste0(suffix, output_suffix)
# 
# # save model 
# saveRDS(clr_model, paste0(output_filepath, output_number, '0_clr', model_type, 'model_', random_data_method, suffix, '.RDS'))
# saveRDS(glm_model, paste0(output_filepath, output_number, '0_glm', model_type, 'model_', random_data_method, suffix, '.RDS'))
# saveRDS(glm_model_custom, paste0(output_filepath, output_number, '0_glm_custom', model_type, 'model_', random_data_method, suffix, '.RDS'))
# 
# # save model results as csv 
# write.csv(clr_coef, paste0(output_filepath, output_number, '1_clr', model_type, 'coefs_', random_data_method, suffix, '.csv'))
# write.csv(clr_tests, paste0(output_filepath, output_number, '2_clr', model_type, 'tests_', random_data_method, suffix, '.csv'))
# 
# write.csv(glm_coef, paste0(output_filepath, output_number, '3_glm', model_type, 'coefs_', random_data_method, suffix, '.csv'))
# write.csv(glm_deviances, paste0(output_filepath, output_number, '4_glm', model_type, 'deviances_', random_data_method, suffix, '.csv'))
# write.csv(glm_vif, paste0(output_filepath, output_number, '5_glm', model_type, 'vif_', random_data_method, suffix, '.csv'))
# 
# write.csv(glm_coef_c, paste0(output_filepath, output_number, '3_glm_custom', model_type, 'coefs_', random_data_method, suffix, '.csv'))
# write.csv(glm_deviances_c, paste0(output_filepath, output_number, '4_glm_custom', model_type, 'deviances_', random_data_method, suffix, '.csv'))
# write.csv(glm_c_validation, paste0(output_filepath, output_number, '6_glm_custom', model_type, 'CV_results_', random_data_method, suffix, '.csv'))
# write.csv(glm_c_test, paste0(output_filepath, output_number, '7_glm_custom', model_type, 'test_results_', random_data_method, suffix, '.csv'))
# 
# # save confusion matrix from custom GLM
# saveRDS(cm, paste0(output_filepath, output_number, '8_glm_custom', model_type, 'confusion_matrix_', random_data_method, suffix, '.RDS'))
#   
# 
# 
# 
# 
# 
# 
# 
# 
# 
