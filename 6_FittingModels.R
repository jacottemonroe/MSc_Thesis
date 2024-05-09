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
if(!('amt') %in% installed.packages()){install.packages('amt')}
library(amt) # CLR modeling 
if(!('car') %in% installed.packages()){install.packages('car')}
library(car) # needed to run vif()
# if(!('ggplot2') %in% installed.packages()){install.packages('ggplot2')}
# library(ggplot2) # needed for loading GGally package for confusion matrix
# if(!('GGally') %in% installed.packages()){install.packages('GGally')}
# library(GGally)

# run function 
fitMovementModel(run_filepath, ID, week, pseudo_abs_method, downscaling = downscaling_setting, 
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
# ###### full process to integrate in function 
# 
# # necessary packages
# if(!('dplyr') %in% installed.packages()){install.packages('dplyr')}
# library(dplyr) # group data
# if(!('caret') %in% installed.packages()){install.packages('caret')}
# library(caret) # GLM modeling and cross-validation
# if(!('amt') %in% installed.packages()){install.packages('amt')}
# library(amt) # CLR modeling 
# if(!('car') %in% installed.packages()){install.packages('car')}
# library(car) # needed to run vif()
# 
# ### set up - not included in function itself (just temporary)
# 
# ID <- 'LA3'
# week <- 2089
# pseudo_abs_method <- 'random_path_custom_distr'
# downscaling_setting <- 'NULL'
# input_suffix <- '_withMean'
# 
# # define run filepath
# run_filepath <- paste0('data/', ID, '/', week, '/')
# 
# # specify function parameters
# input_directory <- run_filepath
# random_data_method <- pseudo_abs_method
# downscaling <- downscaling_setting
# multicolinearity_check <- F
# full <- T
# output_directory <- 'output/'
# 
# 
# ### set up - included in function (preprocessing)
# 
# # determine suffix based on input parameters
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
# step_dataset <- read.csv(paste0(input_directory, '4_a1_cov_resp_dataset_', random_data_method, suffix, input_suffix, '.csv'), row.names = 1)
# 
# # duplicate the case outcome column (to keep a version with boolean)
# step_dataset$case_bool <- step_dataset$case_
# 
# # rename case outcome T = presence; F = absence
# step_dataset$case_[step_dataset$case_ == T] <- 'presence'
# step_dataset$case_[step_dataset$case_ == F] <- 'absence'
# 
# # fix the step ID column to restart step count for every burst
# step_dataset$stepID <- NA
# 
# for(b in unique(step_dataset$burst_)){
#   # select rows of that burst that are true
#   steps <- step_dataset[step_dataset$burst_ == b & step_dataset$case_ == 'presence',]
#   step_dataset$stepID[min(as.numeric(steps$step_id_)):max(as.numeric(steps$step_id_))] <- 1:nrow(steps)
# }
# 
# # transfer the step ID of random steps to new column
# # NOTE: could move the code to some other script (unless the columns are useful)
# step_dataset$stepID[step_dataset$case_ == 'absence'] <- step_dataset$step_id_[step_dataset$case_ == 'absence']
# 
# # create new column that groups true/false cases for corresponding step in burst
# # package: dplyr
# # source: https://stackoverflow.com/questions/24119599/how-to-assign-a-unique-id-number-to-each-group-of-identical-values-in-a-column
# step_dataset <- step_dataset %>% group_by(burst_, step_id_) %>% mutate(pair_id=cur_group_id())
# 
# # set the class to factor type instead of string
# step_dataset$case_ <- factor(step_dataset$case_, levels = c('absence', 'presence'))
# 
# 
# ### split step dataset into training and testing sets
# 
# # get list of pair IDs (all pair groups have 1 T and 20 F)
# pair_id_list <- unique(step_dataset$pair_id)
# 
# # generate random split in indices of step pairs
# # package: caret
# # source: https://rforhr.com/kfold.html
# set.seed(152)
# partition <- unname(createDataPartition(pair_id_list, p=.7, list=F, times=1))
# 
# # split dataset into training and testing based on partition
# training_steps <- step_dataset[step_dataset$pair_id %in% partition,]
# test_steps <- step_dataset[!(step_dataset$pair_id %in% partition),]
# 
# # convert outcome column to factor (necessary to define classes)
# training_steps$case_ <- factor(training_steps$case_, levels = c('absence', 'presence'))
# test_steps$case_ <- factor(test_steps$case_, levels = c('absence', 'presence'))
# 
# 
# ### build custom GLM model to implement threshold probability tuning in cross-validation
# 
# ## Step by step code used as reference: http://rstudio-pubs-static.s3.amazonaws.com/145252_b241fc4c9cc640a185e721694648ad31.html
# ## The reference script modified the settings of the default rf model to incorporate threshold probability tuning in cross-validation.
# ## Taking the online tutorial and the glm documentation (getModelInfo) as a guideline, I did the same but for the glm model.
# ## The following functions were modified from the original glm model:
# ##    model$type
# ##    model$parameters
# ##    model$grid
# ##    model$predict
# 
# # get GLM model information
# thresh_glm <- getModelInfo("glm", regex = FALSE)[[1]]
# 
# # specify the model type as classification
# thresh_glm$type <- c('Classification')
# 
# # add threshold as tuning parameter
# thresh_glm$parameters <- data.frame(parameter = c('threshold'), class = c('numeric'), label = c('Probability Cutoff'))
# 
# # set tuning grid
# thresh_glm$grid <- function(x, y, len = NULL, search = 'grid'){
#   if(search == 'grid'){
#     grid <- expand.grid(threshold = seq(0.01, 0.99, length = len))
#   } else{
#     grid <- expand.grid(threshold = runif(1, 0, size = len))
#   }
#   grid
# }
# 
# # modify the predict function to test custom threshold
# thresh_glm$predict <- function(modelFit, newdata, submodels = NULL){
#   if (!is.data.frame(newdata)) {
#     newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
#   }
# 
#   if (modelFit$problemType == "Classification") {
#     probs <- predict(modelFit, newdata, type = "response")
#     out <- ifelse(probs >= modelFit$tuneValue$threshold,
#                   modelFit$obsLevel[1], modelFit$obsLevel[2])
#   }
#   else {
#     out <- predict(modelFit, newdata, type = "response")
#   }
# 
#   out
# }
# 
# # # remove some part of prob function to just return the class
# # thresh_glm$prob <- function (modelFit, newdata, submodels = NULL)
# # {
# #   if (!is.data.frame(newdata))
# #     newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
# #   out <- predict(modelFit, newdata, type = "response")
# #   out
# # }
# 
# # write custom summaryFunction for trControl
# # still based on tutorial: http://rstudio-pubs-static.s3.amazonaws.com/145252_b241fc4c9cc640a185e721694648ad31.html
# # Note: twoClassSummary automatically returns ROC, sensitivity, and specificity metrics
# # Note: Dist = distance of specificity and sensitivity metrics to optimal performance (=1), the smaller the distance the better the model
# # source: https://cran.r-project.org/web/packages/caret/vignettes/caret.html
# performanceStats <- function(data, lev = levels(data$obs), model = NULL){
#   out <- c(twoClassSummary(data, lev = levels(data$obs), model = NULL))
#   #out2 <- c(prSummary(data, lev = levels(data$obs), model = NULL))
#   coords <- matrix(c(1, 1, out["Spec"], out["Sens"]), ncol = 2, byrow = T)
#   colnames(coords) <- c('Spec', 'Sens')
#   rownames(coords) <- c('Best', 'Current')
#   c(out, Dist = dist(coords)[1])
# }
# 
# 
# ### implement cross-validation and fit GLM model on training step dataset
# 
# # define cross-validation settings
# # Note: Applying a 10-fold cross-validation on the training data
# 
# # # Option1: with default performance metric used for parameter tuning
# # cv_settings_default <- trainControl(method = 'cv', classProbs = T, summaryFunction = twoClassSummary)
# 
# # Option 2: with custom performance metric used for parameter tuning
# # package: caret
# cv_settings_custom <- trainControl(method = 'cv', classProbs = T, summaryFunction = performanceStats)
# 
# # # ONLY FOR OPTION 1: Reverse level order of classes!
# # # Note: By default, absence (= 0) is first level, that's the negative outcome.
# # #       This doesn't match hardcoded 'first level = positive outcome' rule of CV function (twoClassSummary).
# # #       Need to reverse so presence (=1) is first level and read in CV function as positive outcome.
# # # source: https://stackoverflow.com/questions/45333029/specifying-positive-class-of-an-outcome-variable-in-caret-train
# # # Note: also applying this so test set (even though CV function not used) for consistency
# # training_steps_rev <- training_steps
# # training_steps_rev$case_ <- factor(training_steps_rev$case_, levels=rev(levels(as.factor(training_steps_rev$case_))))
# # test_steps_rev <- test_steps
# # test_steps_rev$case_ <- factor(test_steps_rev$case_, levels=rev(levels(as.factor(test_steps_rev$case_))))
# 
# # fit a custom GLM model through the training data with hyperparameter tuning of threshold probability
# # source: https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/
# # source: https://www.rdocumentation.org/packages/traineR/versions/2.2.0/topics/train.glm
# # source: https://cran.r-project.org/web/packages/caret/vignettes/caret.html
# # Note: 20 threshold values are tested in the parameter tuning
# #
# # # Option 1: fit default GLM that uses AUC as performance metric
# # glm_model_default <- train(case_ ~ ndvi_50_scaled + ndvi_sd_scaled + ndvi_rate_50_scaled + ndvi_rate_sd_scaled,
# #                    data = training_steps_rev, method = 'glm', family = binomial(link = 'logit'),
# #                    metric = 'ROC', maximize = F, tuneLength = 20, trControl = cv_settings_default)
# 
# # Option 2: fit custom GLM that uses Dist as performance metric
# # package: caret
# # Note: the custom metric Dist is used for tuning threshold probability
# glm_model_custom <- train(case_ ~ ndvi_50_scaled + ndvi_sd_scaled + ndvi_rate_50_scaled + ndvi_rate_sd_scaled,
#            data = training_steps, method = thresh_glm, family = binomial(link = 'logit'),
#            metric = 'Dist', maximize = F, tuneLength = 20, trControl = cv_settings_custom)
# 
# # also fit conditional logistic regression and general logistic regression models (without cross-validation)
# # note: models trained on full step dataset (no split into training and validation)
# # package: amt
# # source: https://cran.r-project.org/web/packages/amt/vignettes/p4_SSF.html
# clr_model <- fit_clogit(step_dataset, case_bool ~ ndvi_50_scaled + ndvi_sd_scaled +
#                           ndvi_rate_50_scaled + ndvi_rate_sd_scaled + strata(stepID))
# 
# # package: caret
# # source: https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/
# glm_model <- glm(case_ ~ ndvi_50_scaled + ndvi_sd_scaled + ndvi_rate_50_scaled + ndvi_rate_sd_scaled,
#                  family = binomial(link = 'logit'), data = step_dataset)
# 
# 
# ### predict test set with trained, optimized, and validated GLM model
# 
# # predict likelihood of elephant occurrence on step
# # Note: Type 'raw' outputs the class directly instead of the probability based on the tuned cutoff value from trained model
# test_steps$prediction  <- predict(glm_model_custom, newdata = test_steps , type = "raw" )
# 
# # create confusion matrix
# # source: https://stackoverflow.com/questions/46028360/confusionmatrix-for-logistic-regression-in-r
# cm <- confusionMatrix(data = test_steps$prediction, reference = test_steps$case_, positive = 'presence')
# 
# 
# ### Retrieve model performance metrics and results
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
#                             residual_deviance = glm_summary_c$deviance, residual_df = glm_summary_c$df.residual)
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
# saveRDS(cm, paste0(output_filepath, output_number, '8_glm_custom', model_type, 'confusion_matrix_', random_data_method, suffix, '.csv'))
# 
# 
# 
# 
# 



# 
# 
# 
# ### analyze trained model 
# 
# glm_model
# # get ROC spec sens diff by selecting values base on threshold best tuned 
# s <- summary(glm_model)
# s$aic
# 
# # metrics <- glm_model$results[, c(1, 3:5)]
# # if(!('reshape2') %in% installed.packages()){install.packages('reshape2')}
# # library(reshape2)
# # metrics <- melt(metrics, id.vars = "threshold",
# #                 variable.name = "Resampled",
# #                 value.name = "Data")
# # 
# # ggplot(metrics, aes(x = threshold, y = Data, color = Resampled)) +
# #   geom_line() +
# #   ylab('Performance Score') + xlab("Threshold Probability") +
# #   ggtitle('Threshold Probability Tuning') +
# #   # source: https://stackoverflow.com/questions/14622421/how-to-change-legend-title-in-ggplot
# #   labs(color = 'Performance Metrics') +
# #   theme_minimal() +
# #   theme(legend.position = "top")
# 
# # retrieve threshold value
# new_threshold <- glm_model$bestTune[[1]]
# 
# 
# ### predict the test outcome with trained model and new threshold value
# 
# # predict likelihood of elephant occurrence on step
# #test_steps$prediction  <- predict(glm_model, newdata = test_steps, type = "response")
# training_steps$prediction <- predict( glm_model, newdata = training_steps, type = "raw" )
# test_steps$prediction  <- predict( glm_model, newdata = test_steps , type = "raw" )
# 
# # create confusion matrix of predicted values based on new threshold value
# # source: https://stackoverflow.com/questions/46028360/confusionmatrix-for-logistic-regression-in-r
# # source: https://stackoverflow.com/questions/39803667/r-create-factor-based-on-condition
# confusionMatrix(data = factor(ifelse(test_steps$prediction$presence > 0.5, 'presence', 'absence')), 
#                 reference = test_steps$case_, positive = 'presence')
# 
# confusionMatrix(data = factor(ifelse(test_steps$prediction$presence > new_threshold, 'presence', 'absence')), 
#                 reference = test_steps$case_, positive = 'presence')
# 
# cm <- confusionMatrix(data = test_steps$prediction, reference = test_steps$case_, positive = 'presence')
# cm$byClass
# 
# ggplot( training_steps, aes( prediction, color = as.factor(case_) ) ) + 
#   geom_density( linewidth = 1 ) +
#   ggtitle( "Training Set's Predicted Score" ) + 
#   scale_color_manual( name = "data", values = c( "absence" = 'orange', "presence" = 'blue'), labels = c('absence', 'presence')) + 
#   theme_minimal()
# 
# ggplot( test_steps, aes( prediction, color = as.factor(case_) ) ) + 
#   geom_density( linewidth = 1 ) +
#   ggtitle( "Test Set's Predicted Score" ) + 
#   scale_color_manual( name = "data", values = c( "absence" = 'orange', "presence" = 'blue'), labels = c('absence', 'presence')) + 
#   theme_minimal()
# 
# # iterate to find optimal cutoff value
# # source: https://cran.r-project.org/web/packages/cutpointr/vignettes/cutpointr.html
# if(!('cutpointr') %in% installed.packages()){install.packages('cutpointr')}
# library(cutpointr)
# # source: https://stats.stackexchange.com/questions/61521/cut-off-point-in-a-roc-curve-is-there-a-simple-function
# cp <- cutpointr(training_steps, prediction, case_, method = maximize_metric, metric = F1_score, pos_class = 'presence', direction = '>=')
# summary(cp)
# cp$F1_score
# cp$AUC
# plot(cp)
# plot_metric(cp)
# 
# 
# 
# 
# 
# 
# 
# 
# # 
# # 
# # 
# 
# 
# 
# 
# ## iterate for many datasets to see how variable cut off values are
# 
# # load results dataset
# r <- read.csv('output/LTS_df_results_aggregated.csv', row.names = 1)
# r2 <- read.csv('output/STS_df_results_aggregated.csv', row.names = 1)
# r3 <- rbind(r, r2)
# rs <- r3[,c('ID', 'week', 'date', 'method', 'model_sig')]
# rs <- rs[!duplicated(rs),]
# 
# # create new columns for relevant performance metrics 
# rs$cutoff <- NA
# rs$f1_score <- NA
# rs$AUC <- NA
# rs$sensitivity <- NA
# rs$specificity <- NA
# 
# # loop for each elephant run (data entry)
# for(i in 1:nrow(rs)){
#   
#   ID <- rs$ID[i]
#   week <- rs$week[i]
#   pseudo_abs_method <- rs$method[i]
#   downscaling_setting <- 'NULL'
#   
#   # define run filepath
#   run_filepath <- paste0('data/', ID, '/', week, '/')
#   
#   input_directory <- run_filepath
#   random_data_method <- pseudo_abs_method
#   downscaling <- downscaling_setting
#   multicolinearity_check <- F
#   full <- T
#   output_directory <- 'output/'
#   
#   # 
#   # # create output filepath 
#   # output_filepath <- paste0(output_directory, ID, '/', week, '/')
#   # 
#   if(downscaling == 'NULL'){
#     suffix <- ''
#     
#   }else if(downscaling == T){
#     suffix <- '_downscaling_modis_30m'
#     
#   }else if(downscaling == F){
#     suffix <- '_downscaling_modis_250m'
#     
#   }else{stop('Incorrect term set for downscaling parameter. Should be one of the following: NULL, T, F.')}
#   
#   # read step dataset
#   step_dataset <- read.csv(paste0(input_directory, '4_a1_cov_resp_dataset_', random_data_method, suffix, '.csv'), row.names = 1)
#   
#   # rename case outcome T = presence; F = absence 
#   step_dataset$case_[step_dataset$case_ == T] <- 1
#   step_dataset$case_[step_dataset$case_ == F] <- 0
#   
#   # fix the step ID column to restart step count for every burst 
#   step_dataset$stepID <- NA
#   
#   # source: https://stackoverflow.com/questions/38768398/r-continue-loop-on-error
#   try({ 
#     for(b in unique(step_dataset$burst_)){
#       # select rows of that burst that are true
#       steps <- step_dataset[step_dataset$burst_ == b & step_dataset$case_ == 1,]
#       step_dataset$stepID[min(as.numeric(steps$step_id_)):max(as.numeric(steps$step_id_))] <- 1:nrow(steps)
#     }
#     
#     # transfer the step ID of random steps to new column 
#     # NOTE: could move the code to some other script (unless the columns are useful)
#     step_dataset$stepID[step_dataset$case_ == 0] <- step_dataset$step_id_[step_dataset$case_ == 0]
#     
#     # create new column that groups true/false cases for corresponding step in burst 
#     #library(dplyr)
#     # source: https://stackoverflow.com/questions/24119599/how-to-assign-a-unique-id-number-to-each-group-of-identical-values-in-a-column
#     step_dataset <- step_dataset %>% group_by(burst_, stepID) %>% mutate(pair_id=cur_group_id())
#     
#     # get list of pair IDs (all pair groups have 1 T and 20 F)
#     pair_id_list <- unique(step_dataset$pair_id)
#     
#     # randomly split the step pairs into training and test sets
#     # if(!('caret') %in% installed.packages()){install.packages('caret')} # to read rasters
#     # library(caret)
#     # source: https://rforhr.com/kfold.html
#     set.seed(152)
#     partition <- unname(createDataPartition(pair_id_list, p=.7, list=F, times=1))
#     
#     training_steps <- step_dataset[step_dataset$pair_id %in% partition,]
#     test_steps <- step_dataset[!(step_dataset$pair_id %in% partition),]
#     
#     # convert outcome column to factor 
#     training_steps$case_ <- as.factor(training_steps$case_)
#     test_steps$case_ <- as.factor(test_steps$case_)
#     
#     # look at imbalance of dataset
#     # Note: imbalance ratio is the same for training and test subsets because i decided not to split pairs
#     prop.table(table(step_dataset$case_))
#     
#     # fit logistic regression model on training data 
#     # source: https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/
#     glm_model <- glm(case_ ~ ndvi_50_scaled + ndvi_sd_scaled + ndvi_rate_50_scaled + ndvi_rate_sd_scaled, 
#                      family = binomial(link = 'logit'), data = training_steps)
#     
#     s <- summary(glm_model)
#     
#     # predict outcomes based on training and test sets and trained GLM
#     training_steps$prediction <- predict( glm_model, newdata = training_steps, type = "response" )
#     test_steps$prediction  <- predict( glm_model, newdata = test_steps , type = "response" )
#     
#     # NOTE: currently the cutoff value is at default 0.5 
# 
#     # iterate to find optimal cutoff value
#     # source: https://cran.r-project.org/web/packages/cutpointr/vignettes/cutpointr.html
#     # if(!('cutpointr') %in% installed.packages()){install.packages('cutpointr')}
#     # library(cutpointr)
#     # source: https://stats.stackexchange.com/questions/61521/cut-off-point-in-a-roc-curve-is-there-a-simple-function
#     cp <- cutpointr(test_steps, prediction, case_, method = maximize_metric, metric = F1_score, 
#                     direction = '>=', pos_class = 1, na.rm = T)
#     
#     rs$cutoff[i] <- cp$optimal_cutpoint
#     rs$f1_score[i] <- cp$F1_score
#     rs$AUC[i] <- cp$AUC
#     rs$sensitivity[i] <- cp$sensitivity
#     rs$specificity[i] <- cp$specificity
#     }, silent = T)
# }
# 
# sum(is.na(rs$cutoff))
# hist(rs$cutoff)
# hist(rs$f1_score)
# hist(rs$AUC)
# 
# z <- rs
# z <- strftime(as.Date(rs$date, tz = 'Africa/Maputo'), '%d-%m')
# 
# ggplot(data = rs, aes(x = as.Date(strftime(as.Date(rs$date, tz = 'Africa/Maputo'), '%d-%m'), 
#                                   format = '%d-%m', tz = 'Africa/Maputo'), y = cutoff, 
#                       color = strftime(as.Date(rs$date, tz = 'Africa/Maputo'), '%m'))) + 
#   geom_point() + 
#   scale_x_date(date_labels = '%B') + 
#   xlab('Month of Year') + ylab('Cutoff Value') + ggtitle('Cutoff Values of ELephant Presence Likelihood for All Models') +
#   theme(legend.position="none")
# 
# ggplot(data = rs, aes(x = as.Date(strftime(as.Date(rs$date, tz = 'Africa/Maputo'), '%d-%m'), 
#                                   format = '%d-%m', tz = 'Africa/Maputo'), y = f1_score, 
#                       color = strftime(as.Date(rs$date, tz = 'Africa/Maputo'), '%m'))) + 
#   geom_hline(yintercept = 0.5, linetype = 'dashed', color = 'grey50') +
#   geom_point() + 
#   scale_x_date(date_labels = '%B') + 
#   xlab('Month of Year') + ylab('Cutoff Value') + ggtitle('F1 Score at Optimal Cutoff Value of ELephant Presence Likelihood for All Models') +
#   theme(legend.position="none")
# 
# ggplot(data = rs, aes(x = as.Date(strftime(as.Date(rs$date, tz = 'Africa/Maputo'), '%d-%m'), 
#                                   format = '%d-%m', tz = 'Africa/Maputo'), y = AUC, 
#                       color = strftime(as.Date(rs$date, tz = 'Africa/Maputo'), '%m'))) + 
#   geom_hline(yintercept = 0.5, linetype = 'dashed', color = 'grey50') +
#   geom_point() + 
#   scale_x_date(date_labels = '%B') + 
#   xlab('Month of Year') + ylab('Cutoff Value') + ggtitle('AUC at Optimal Cutoff Value of ELephant Presence Likelihood for All Models') +
#   theme(legend.position="none")
# 
# 
# 
# 
# 
# 
# 
# 
# #### implement cross validation with parameter tuning 
# 
# ID <- 'LA12'
# week <- 2172
# pseudo_abs_method <- 'random_path_custom_distr'
# downscaling_setting <- 'NULL'
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
# # 
# # # create output filepath 
# # output_filepath <- paste0(output_directory, ID, '/', week, '/')
# # 
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
# step_dataset <- read.csv(paste0(input_directory, '4_a1_cov_resp_dataset_', random_data_method, suffix, '.csv'), row.names = 1)
# 
# # rename case outcome T = presence; F = absence 
# step_dataset$case_[step_dataset$case_ == T] <- 'presence'
# step_dataset$case_[step_dataset$case_ == F] <- 'absence'
# 
# # fix the step ID column to restart step count for every burst 
# step_dataset$stepID <- NA
# 
# for(b in unique(step_dataset$burst_)){
#   # select rows of that burst that are true
#   steps <- step_dataset[step_dataset$burst_ == b & step_dataset$case_ == 'presence',]
#   step_dataset$stepID[min(as.numeric(steps$step_id_)):max(as.numeric(steps$step_id_))] <- 1:nrow(steps)
# }
# 
# # transfer the step ID of random steps to new column 
# # NOTE: could move the code to some other script (unless the columns are useful)
# step_dataset$stepID[step_dataset$case_ == 'absence'] <- step_dataset$step_id_[step_dataset$case_ == 'absence']
# 
# # create new column that groups true/false cases for corresponding step in burst 
# library(dplyr)
# # source: https://stackoverflow.com/questions/24119599/how-to-assign-a-unique-id-number-to-each-group-of-identical-values-in-a-column
# step_dataset <- step_dataset %>% group_by(burst_, stepID) %>% mutate(pair_id=cur_group_id())
# 
# # get list of pair IDs (all pair groups have 1 T and 20 F)
# pair_id_list <- unique(step_dataset$pair_id)
# 
# # randomly split the step pairs into training and test sets
# if(!('caret') %in% installed.packages()){install.packages('caret')} # to read rasters
# library(caret)
# # source: https://rforhr.com/kfold.html
# set.seed(152)
# partition <- unname(createDataPartition(pair_id_list, p=.7, list=F, times=1))
# 
# training_steps <- step_dataset[step_dataset$pair_id %in% partition,]
# test_steps <- step_dataset[!(step_dataset$pair_id %in% partition),]
# 
# # convert outcome column to factor (necessary to define classes)
# # Note: By default, absence (= 0) is first level, that's the negative outcome. 
# #       This doesn't match hardcoded 'first level = positive outcome' rule of CV function (twoClassSummary). 
# #       Need to reverse so presence (=1) is first level and read in CV function as positive outcome. 
# # source: https://stackoverflow.com/questions/45333029/specifying-positive-class-of-an-outcome-variable-in-caret-train
# # Note: also applying this so test set (even though CV function not used) for consistency
# training_steps$case_ <- factor(training_steps$case_, levels=rev(levels(as.factor(training_steps$case_))))
# test_steps$case_ <- factor(test_steps$case_, levels=rev(levels(as.factor(test_steps$case_))))
# 
# # custom function for evaluating performance at custom cutoff based on improvement compared to ideal model --> based on ROC
# fourStats <- function (data, lev = levels(data$obs), model = NULL) {
#   ## This code will get use the area under the ROC curve and the
#   ## sensitivity and specificity values using the current candidate
#   ## value of the probability threshold.
#   out <- c(twoClassSummary(data, lev = levels(data$obs), model = NULL))
#   
#   ## The best possible model has sensitivity of 1 and specifity of 1. 
#   ## How far are we from that value?
#   coords <- matrix(c(1, 1, out["Spec"], out["Sens"]), 
#                    ncol = 2, 
#                    byrow = TRUE)
#   colnames(coords) <- c("Spec", "Sens")
#   rownames(coords) <- c("Best", "Current")
#   c(out, Dist = dist(coords)[1])
# }
# 
#  
# 
# 
# # define cross-validation parameters 
# # Note: Applying a 10-fold cross-validation on the training data 
# # Note: Additional parameter to return a specified performance metric that was tuned 
# # Note: twoClassSummary automatically returns ROC, sensitivity, and specificity metrics
# # source: https://cran.r-project.org/web/packages/caret/vignettes/caret.html
# if(!('MLmetrics') %in% installed.packages()){install.packages('MLmetrics')} # to read rasters
# library(MLmetrics)
# # package required for prSummary (only, not twoClassSummary or other)
# cv_settings <- trainControl(method = 'cv', number = 10, classProbs = T, summaryFunction = prSummary) #twoClassSummary or prSummary
# #cv_settings <- trainControl(method = 'cv', number = 10, classProbs = T, summaryFunction = f1)
# 
# # fit logistic regression model on training data 
# # source: https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/
# # source: https://www.rdocumentation.org/packages/traineR/versions/2.2.0/topics/train.glm
# # glm_model <- glm(case_ ~ ndvi_50_scaled + ndvi_sd_scaled + ndvi_rate_50_scaled + ndvi_rate_sd_scaled, 
# #                  family = binomial(link = 'logit'), data = training_steps)
# # source: https://cran.r-project.org/web/packages/caret/vignettes/caret.html
# glm_model <- train(case_ ~ ndvi_50_scaled + ndvi_sd_scaled + ndvi_rate_50_scaled + ndvi_rate_sd_scaled, 
#                    data = training_steps, family = binomial(link = 'logit'), method = 'glm', 
#                    trControl = cv_settings, metric = 'AUC', tuneLength = 20) #ROC or AUC
# 
# glm_model <- train(case_ ~ ndvi_50_scaled + ndvi_sd_scaled + ndvi_rate_50_scaled + ndvi_rate_sd_scaled, 
#                    data = training_steps, family = binomial(link = 'logit'), method = 'glm', 
#                    weights = model_weights, trControl = cv_settings, metric = 'ROC')
# 
# 
# 
# glm_model <- glm(case_ ~ ndvi_50_scaled + ndvi_sd_scaled + ndvi_rate_50_scaled + ndvi_rate_sd_scaled, 
#                 family = binomial(link = 'logit'), data = training_steps, weights = model_weights)
# 
# glm_model <- train(case_ ~ ndvi_50_scaled + ndvi_sd_scaled + ndvi_rate_50_scaled + ndvi_rate_sd_scaled, 
#                    data = training_steps, family = binomial(link = 'logit'), method = 'glm', 
#                    weights = model_weights)
# #glm_model
# summary(glm_model)
# 
# sub_pval <- pchisq(glm_model$null.deviance - glm_model$deviance, glm_model$df.null - glm_model$df.residual, lower.tail = F)
# sub_pval
# glm_model$res
# 
# s <- summary(glm_model)
# s
# # calculate pseudo R value
# round(1 - (s$deviance/s$null.deviance),2)
# 
# # predict outcomes based on training and test sets and trained GLM
# training_steps$prediction <- predict( glm_model, newdata = training_steps, type = "response" )
# test_steps$prediction  <- predict( glm_model, newdata = test_steps , type = "response" )
# 
# ggplot( training_steps, aes( prediction, color = as.factor(case_) ) ) + 
#   geom_density( linewidth = 1 ) +
#   ggtitle( "Training Set's Predicted Score" ) + 
#   scale_color_manual( name = "data", values = c( "0" = 'orange', "1" = 'blue'), labels = c('absence', 'presence')) + 
#   theme_minimal()
# 
# 
# # NOTE: currently the cutoff value is at default 0.5 
# 
# # create confusion matrix of predicted values based on current cutoff value
# # source: https://stackoverflow.com/questions/46028360/confusionmatrix-for-logistic-regression-in-r
# confusionMatrix(data = factor(as.numeric(test_steps$prediction > 0.15), levels = 0:1), 
#                 reference = test_steps$case_, positive = '1') #$byClass['F1']
# 
# # iterate to find optimal cutoff value
# # source: https://cran.r-project.org/web/packages/cutpointr/vignettes/cutpointr.html
# if(!('cutpointr') %in% installed.packages()){install.packages('cutpointr')}
# library(cutpointr)
# # source: https://stats.stackexchange.com/questions/61521/cut-off-point-in-a-roc-curve-is-there-a-simple-function
# cp <- cutpointr(training_steps, prediction, case_, method = maximize_metric, metric = F1_score, pos_class = 1, direction = '>=')
# summary(cp)
# cp$F1_score
# cp$AUC
# plot(cp)
# plot_metric(cp)
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
# a <- readRDS('data/LA12/2172/1_b1_all_steps_random_path_custom_distr.RDS')
# 
# # fit conditional logistic regression and general logistic regression models 
# # Note: subset of predictors used (not full), covariates have been rescaled, includes cross-validation
# output_number = '6_c'
# model_type = '_50p_sd_'
# suffix <- paste0(suffix, '_scaled_withCV')
# 
# # source: https://cran.r-project.org/web/packages/amt/vignettes/p4_SSF.html
# clr_model <- fit_clogit(step_dataset, case_ ~ ndvi_50_scaled + ndvi_sd_scaled + 
#                           ndvi_rate_50_scaled + ndvi_rate_sd_scaled + strata(step_id_))
# 
# # source: https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/
# glm_model <- glm(case_ ~ ndvi_50_scaled + ndvi_sd_scaled + ndvi_rate_50_scaled + ndvi_rate_sd_scaled, 
#                  family = binomial(link = 'logit'), data = step_dataset)
# 
# # save model 
# saveRDS(clr_model, paste0(output_filepath, output_number, '0_clr', model_type, 'model_', random_data_method, suffix, '.RDS'))
# saveRDS(glm_model, paste0(output_filepath, output_number, '0_glm', model_type, 'model_', random_data_method, suffix, '.RDS'))
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
# 
# 
# 
# 
# 
# 
# 
# ##############################################################################
# ############# Attempting something crazy - Build own Model ###################
# ##############################################################################
# 
# 
# training_steps <- step_dataset[step_dataset$pair_id %in% partition,]
# test_steps <- step_dataset[!(step_dataset$pair_id %in% partition),]
# 
# # convert outcome column to factor (necessary to define classes)
# # Note: By default, absence (= 0) is first level, that's the negative outcome. 
# #       This doesn't match hardcoded 'first level = positive outcome' rule of CV function (twoClassSummary). 
# #       Need to reverse so presence (=1) is first level and read in CV function as positive outcome. 
# # source: https://stackoverflow.com/questions/45333029/specifying-positive-class-of-an-outcome-variable-in-caret-train
# # Note: also applying this so test set (even though CV function not used) for consistency
# training_steps$case_ <- as.factor(training_steps$case_)
# test_steps$case_ <- as.factor(test_steps$case_)
# # training_steps$case_ <- factor(training_steps$case_, levels=rev(levels(as.factor(training_steps$case_))))
# # test_steps$case_ <- factor(test_steps$case_, levels=rev(levels(as.factor(test_steps$case_))))
# 
# 
# ## Existing script that builds on RF model for threshold tuning: http://rstudio-pubs-static.s3.amazonaws.com/145252_b241fc4c9cc640a185e721694648ad31.html
# ## I want to do the same but for GLM instead of RF 
# ## I want to change as little as possible in the model, but I want it to allow threshold tunning 
# 
# # get GLM model information
# thresh_ref <- getModelInfo("glm", regex = FALSE)[[1]]
# thresh_glm <- getModelInfo("glm", regex = FALSE)[[1]]
# 
# # specify the model type as classification 
# thresh_glm$type <- c('Classification')
# 
# # add threshold as tuning parameter 
# thresh_glm$parameters <- data.frame(parameter = c('threshold'), class = c('numeric'), label = c('Probability Cutoff'))
# 
# # set tuning grid
# thresh_glm$grid <- function(x, y, len = NULL, search = 'grid'){
#   if(search == 'grid'){
#     grid <- expand.grid(threshold = seq(0.01, 0.99, length = len))
#   } else{
#     grid <- expand.grid(threshold = runif(1, 0, size = len))
#   }
#   grid
# }
# 
# # # create loop
# # thresh_glm$loop <- function(grid){
# #   submodels <- vector(mode = "list", length = 1)
# #   submodels[[1]] <- data.frame(threshold = grid$threshold)
# #   list(loop = 1, submodels = submodels)
# # }
# 
# # modify the predict function to test custom threshold
# thresh_glm$predict <- function(modelFit, newdata, submodels = NULL){
#   if (!is.data.frame(newdata)) {
#     newdata <- as.data.frame(newdata, stringsAsFactors = TRUE) 
#   }
# 
#   if (modelFit$problemType == "Classification") {
#     probs <- predict(modelFit, newdata, type = "response")
#     out <- ifelse(probs >= modelFit$tuneValue$threshold, 
#                   modelFit$obsLevel[1], modelFit$obsLevel[2])
#   }
#   else {
#     out <- predict(modelFit, newdata, type = "response")
#   }
# 
#   out
# }
# 
# # adapt prob function? 
# thresh_glm$prob <- function(modelFit, newdata, submodels = NULL){
#   
#   if (!is.data.frame(newdata)) {
#     newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
#   }
# 
#   out <- predict(modelFit, newdata, type = "response")
#   out <- cbind(1 - out, out)
#   dimnames(out)[[2]] <- modelFit$obsLevels
#   out
# 
# }
# 
# # write custom summaryFunction for trControl
# performanceStats <- function(data, lev = levels(data$obs), model = NULL){
#   out <- c(twoClassSummary(data, lev = levels(data$obs), model = NULL))
#   coords <- matrix(c(1, 1, out["Spec"], out["Sens"]), ncol = 2, byrow = T)
#   colnames(coords) <- c('Spec', 'Sens')
#   rownames(coords) <- c('Best', 'Current')
#   c(out, Dist = dist(coords)[1])
# }
# 
# # define cross-validation settings 
# cv_settings <- trainControl(method = 'cv', classProbs = T, summaryFunction = performanceStats)
# 
# # fit a GLM model through the training data with hyperparameter tuning of threshold probability
# m <- train(case_ ~ ndvi_50_scaled + ndvi_sd_scaled + ndvi_rate_50_scaled + ndvi_rate_sd_scaled, 
#            data = training_steps, method = thresh_glm, family = binomial(link = 'logit'),
#            metric = 'Dist', maximize = F, tuneLength = 20, trControl = cv_settings)
# 
# 
# m
# s <- summary(m)
# 
# metrics <- m$results[, c(1, 3:5)]
# if(!('reshape2') %in% installed.packages()){install.packages('reshape2')}
# library(reshape2)
# metrics <- melt(metrics, id.vars = "threshold",
#                 variable.name = "Resampled",
#                 value.name = "Data")
# 
# ggplot(metrics, aes(x = threshold, y = Data, color = Resampled)) +
#   geom_line() +
#   ylab("") + xlab("Probability Cutoff") +
#   theme(legend.position = "top")
# 
# 
# 
# 
# 
# glm_model
# thresh_glm$prob
# thresh_code$prob
# thresh_glm$predict
# 
# ## compare to RF 
# thresh_code <- getModelInfo("rf", regex = FALSE)[[1]]
# thresh_code$loop
# 
# 
# 
# a <- function(x, y, len = NULL, search = "grid") {  #...3
#   p <- ncol(x)
#   if(search == "grid") {
#     grid <- expand.grid(mtry = floor(sqrt(p)),
#                         threshold = seq(.01, .99, length = len))
#   } else {
#     grid <- expand.grid(mtry = sample(1:p, size = len),
#                         threshold = runif(1, 0, size = len))
#   }
#   grid
# }
# 
# 
# thresh_code$loop = function(grid) {    #...4
#   library(plyr)
#   loop <- ddply(grid, c("mtry"),
#                 function(x) c(threshold = max(x$threshold)))
#   submodels <- vector(mode = "list", length = nrow(loop))
#   for(i in seq(along = loop$threshold)) {
#     index <- which(grid$mtry == loop$mtry[i])
#     cuts <- grid[index, "threshold"]
#     submodels[[i]] <- data.frame(threshold = cuts[cuts != loop$threshold[i]])
#   }
#   list(loop = loop, submodels = submodels)
# }
# 
# 
# p <- 10
# grid <- expand.grid(mtry = floor(sqrt(p)),
#                     threshold = seq(.01, .99, length = 20))
# 
# submodels <- vector(mode = "list", length = 1)
# submodels[[1]] <- data.frame(threshold = grid$threshold)
# l <- list(loop = 1, submodels = submodels)
# 
# r <- randomForest(training_steps$ndvi_50_scaled, training_steps$case_)
# 
# 
# 
# 
# 
# 
# 
# ######## leftover code 
# 
# 
# # define weights for presence/absence data (to deal with data imbalance)
# # source: https://www.r-bloggers.com/2016/12/handling-class-imbalance-with-r-and-caret-an-introduction/
# model_weights <- ifelse(training_steps$case_ == "presence",
#                         (1/table(training_steps$case_)[1]) * 0.5,
#                         (1/table(training_steps$case_)[2]) * 0.5)
# 
# # define function to calculate F1 Score from predicted values 
# # Note: this function will be embeded in other functions for cross-validation therefore needs to fit certain format (hence the NULL arguments)
# # f1 <- function (data, lev = NULL, model = NULL) {
# #   precision <- precision(data = data$pred, reference = data$obs, relevant = 'presence')
# #   recall  <- recall(data = data$pred, reference = data$obs, relevant = 'presence')
# #   f1_val <- (2 * precision * recall) / (precision + recall)
# #   names(f1_val) <- c("F1")
# #   f1_val
# # } 