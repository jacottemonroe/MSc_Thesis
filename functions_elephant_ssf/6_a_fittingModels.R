## MSc Thesis 
## Jacotte Monroe 
## Functions SSF script 

# Function that takes the elephant step dataset with its covariates and fits the step-selection function to it. 
# 
# Input: input dataset repository, elephant ID and week of interest, output directory.
# 
# Output: printed information about dataset and summary of fitted model, model saved as RDS.

# Packages: dplyr, caret, amt, car 



# this first function is simplified/cleaned version of second function with less options to try but with cross-validation

fitMovementModel <- function(input_directory = 'data/', ID, week, random_data_method = 'random_path_custom_distr', 
                                   downscaling = 'NULL', input_suffix = '', output_directory = 'output/', output_suffix = ''){
  
  
  ######## STEP 0: Set-Up
  
  
  # create output filepath 
  output_filepath <- paste0(output_directory, ID, '/', week, '/')
  
  # create data directory if it does not yet exist
  if(!dir.exists(output_filepath)){dir.create(output_filepath, recursive = T)}
  
  if(downscaling == 'NULL'){
    suffix <- ''
    
  }else if(downscaling == T){
    suffix <- '_downscaling_modis_30m'
    
  }else if(downscaling == F){
    suffix <- '_downscaling_modis_250m'
    
  }else{stop('Incorrect term set for downscaling parameter. Should be one of the following: NULL, T, F.')}
  
  # define model specifics to avoid overwriting previous results 
  output_number = '6_c'
  model_type = '_mean_sd_' # refers to model built no selection of predictors
  
  # read step dataset
  step_dataset <- read.csv(paste0(input_directory, '4_a1_cov_resp_dataset_', random_data_method, 
                                  suffix, input_suffix, '.csv'), row.names = 1)
  
  # duplicate the case outcome column (to keep a version with boolean)
  step_dataset$case_bool <- step_dataset$case_
  
  # rename case outcome T = presence; F = absence 
  step_dataset$case_[step_dataset$case_ == T] <- 'presence'
  step_dataset$case_[step_dataset$case_ == F] <- 'absence'
  
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
  # create new column that groups true/false cases for corresponding step in burst 
  # package: dplyr
  # source: https://stackoverflow.com/questions/24119599/how-to-assign-a-unique-id-number-to-each-group-of-identical-values-in-a-column
  step_dataset <- step_dataset %>% group_by(burst_, step_id_) %>% mutate(pair_id=cur_group_id())
  
  # set the class to factor type instead of string
  step_dataset$case_ <- factor(step_dataset$case_, levels = c('absence', 'presence'))
  
  
  ######## STEP 1: Split step dataset into training and testing sets 
  
  
  # get list of pair IDs (all pair groups have 1 T and 20 F)
  pair_id_list <- unique(step_dataset$pair_id)
  
  # generate random split in indices of step pairs
  # package: caret
  # source: https://rforhr.com/kfold.html
  # Note: the CreateDataPartition function didn't work on very small sample size --> had to write alternative command that simulated this function
  set.seed(152)
  #partition <- unname(createDataPartition(pair_id_list, p=.7, list=F, times=1))
  partition <- sample(pair_id_list, size = floor(0.7*length(pair_id_list)), replace = F)
  
  # split dataset into training and testing based on partition
  training_steps <- step_dataset[step_dataset$pair_id %in% partition,]
  test_steps <- step_dataset[!(step_dataset$pair_id %in% partition),]
  
  # convert outcome column to factor (necessary to define classes)
  training_steps$case_ <- factor(training_steps$case_, levels = c('absence', 'presence'))
  test_steps$case_ <- factor(test_steps$case_, levels = c('absence', 'presence'))
  
  # remove any steps that contain NA in test steps (normally should not have any but for reason some slipped through so this is a fail safe)
  test_steps <- test_steps[complete.cases(test_steps[,c('case_', 'ndvi_mean_scaled', 'ndvi_sd_scaled', 
                                                        'ndvi_rate_mean_scaled', 'ndvi_rate_sd_scaled')]),]
  
  
  ######## STEP 2: Implement cross-validation and fit GLM model on training step dataset
  
  
  # retrieve custom GLM model
  source('functions_elephant_ssf/6_a_buildingCustomGLM.R')
  custom_glm <- buildCustomGLM()
  
  # define cross-validation settings 
  # Note: Applying a 10-fold cross-validation on the training data 
  # Note: Using custom performance metric (from 6_a_buildingCustomGLM.R script) for hyperparameter tuning
  # package: caret
  cv_settings_custom <- trainControl(method = 'cv', classProbs = T, summaryFunction = performanceFunction)
   
  # fit a custom GLM model through the training data with hyperparameter tuning of threshold probability
  # source: https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/
  # source: https://www.rdocumentation.org/packages/traineR/versions/2.2.0/topics/train.glm
  # source: https://cran.r-project.org/web/packages/caret/vignettes/caret.html
  # source: https://stackoverflow.com/questions/38250440/error-in-na-fail-default-missing-values-in-object-but-no-missing-values
  # Note: 20 threshold values are tested in the parameter tuning 
  # Note: Dist used as custom performance metric for tuning
  # package: caret 
  glm_model_custom <- train(case_ ~ ndvi_mean_scaled + ndvi_sd_scaled + ndvi_rate_mean_scaled + ndvi_rate_sd_scaled, 
                            data = training_steps, method = custom_glm, family = binomial(link = 'logit'),
                            metric = 'Dist', maximize = F, tuneLength = 20, trControl = cv_settings_custom, na.action = na.pass)
  
  # also fit conditional logistic regression and general logistic regression models (without cross-validation)
  # note: models trained on full step dataset (no split into training and validation)
  # package: amt
  # source: https://cran.r-project.org/web/packages/amt/vignettes/p4_SSF.html
  clr_model <- fit_clogit(step_dataset, case_bool ~ ndvi_mean_scaled + ndvi_sd_scaled + 
                            ndvi_rate_mean_scaled + ndvi_rate_sd_scaled + strata(step_id_))
  
  # package: caret
  # source: https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/
  glm_model <- glm(case_ ~ ndvi_mean_scaled + ndvi_sd_scaled + ndvi_rate_mean_scaled + ndvi_rate_sd_scaled, 
                   family = binomial(link = 'logit'), data = step_dataset)
  
  
  ######## STEP 3: Predict test set with trained, optimized, and validated GLM model
  
  # predict likelihood of elephant occurrence on step
  # Note: Type 'raw' outputs the class directly instead of the probability based on the tuned cutoff value from trained model
  test_steps$prediction  <- predict(glm_model_custom, newdata = test_steps , type = "raw")
  
  # create confusion matrix
  # source: https://stackoverflow.com/questions/46028360/confusionmatrix-for-logistic-regression-in-r
  # package: caret
  cm <- confusionMatrix(data = test_steps$prediction, reference = test_steps$case_, positive = 'presence')
  
  
  ######## STEP 4: Retrieve model results and performance metrics 
  
  
  # get VIF from GLM (not applicable to train object = cross-validated GLM model or CLR)
  # package: car
  glm_vif <- data.frame(vif(glm_model))
  
  # get model summaries
  clr_summary <- summary(clr_model)
  glm_summary <- summary(glm_model)
  glm_summary_c <- summary(glm_model_custom)
  
  # get model coefficients 
  # source: https://stackoverflow.com/questions/61482594/export-coxph-summary-from-r-to-csv
  clr_coef <- data.frame(clr_summary$coefficients)
  glm_coef <- data.frame(glm_summary$coefficients)
  glm_coef_c <- data.frame(glm_summary_c$coefficients)
  
  # create dataframe with statistical test results of CLR, the concordance and its standard error 
  clr_tests <- data.frame(log_likelihood = clr_summary$logtest, score = clr_summary$sctest, wald = clr_summary$waldtest)
  clr_tests <- rbind(clr_tests, SE_concordance = c(NA))
  clr_tests <- cbind(clr_tests, concordance = c(clr_summary$concordance[1], NA, NA, clr_summary$concordance[2]))
  
  # create dataframe of deviance and AIC for GLM models 
  glm_deviances <- data.frame(AIC = glm_summary$aic, null_deviance = glm_summary$null.deviance, null_df = glm_summary$df.null, 
                              residual_deviance = glm_summary$deviance, residual_df = glm_summary$df.residual)
  glm_deviances_c <- data.frame(AIC = glm_summary_c$aic, null_deviance = glm_summary_c$null.deviance, null_df = glm_summary_c$df.null, 
                                residual_deviance = glm_summary_c$deviance, residual_df = glm_summary_c$df.residual)
  
  # create dataframe with optimal cross-validation results from custom GLM
  glm_c_validation <- glm_model_custom$results[glm_model_custom$results$threshold == glm_model_custom$bestTune$threshold,]
  
  # create dataframe with custom GLM performance results on test set 
  glm_c_test <- data.frame(cm$byClass)
  
  
  ### save outputs 
  
  suffix <- paste0(suffix, output_suffix)
  
  # save model 
  saveRDS(clr_model, paste0(output_filepath, output_number, '0_clr', model_type, 'model_', random_data_method, suffix, '.RDS'))
  saveRDS(glm_model, paste0(output_filepath, output_number, '0_glm', model_type, 'model_', random_data_method, suffix, '.RDS'))
  saveRDS(glm_model_custom, paste0(output_filepath, output_number, '0_glm_custom', model_type, 'model_', random_data_method, suffix, '.RDS'))
  
  # save model results as csv 
  write.csv(clr_coef, paste0(output_filepath, output_number, '1_clr', model_type, 'coefs_', random_data_method, suffix, '.csv'))
  write.csv(clr_tests, paste0(output_filepath, output_number, '2_clr', model_type, 'tests_', random_data_method, suffix, '.csv'))
  
  write.csv(glm_coef, paste0(output_filepath, output_number, '3_glm', model_type, 'coefs_', random_data_method, suffix, '.csv'))
  write.csv(glm_deviances, paste0(output_filepath, output_number, '4_glm', model_type, 'deviances_', random_data_method, suffix, '.csv'))
  write.csv(glm_vif, paste0(output_filepath, output_number, '5_glm', model_type, 'vif_', random_data_method, suffix, '.csv'))
  
  write.csv(glm_coef_c, paste0(output_filepath, output_number, '3_glm_custom', model_type, 'coefs_', random_data_method, suffix, '.csv'))
  write.csv(glm_deviances_c, paste0(output_filepath, output_number, '4_glm_custom', model_type, 'deviances_', random_data_method, suffix, '.csv'))
  write.csv(glm_c_validation, paste0(output_filepath, output_number, '6_glm_custom', model_type, 'CV_results_', random_data_method, suffix, '.csv'))
  write.csv(glm_c_test, paste0(output_filepath, output_number, '7_glm_custom', model_type, 'test_results_', random_data_method, suffix, '.csv'))
  
  # save confusion matrix from custom GLM
  saveRDS(cm, paste0(output_filepath, output_number, '8_glm_custom', model_type, 'confusion_matrix_', random_data_method, suffix, '.RDS'))
  
}















## previously when testing different model types, settings, combinations.... no cross-validation done 

fitSSFModel_AdditionalSettings <- function(input_directory = 'data/', ID, week, random_data_method = 'random_path_custom_distr', downscaling = 'NULL',
                        multicolinearity_check = F, full = T, scaled = F, output_directory = 'output/'){
  
  # create output filepath 
  output_filepath <- paste0(output_directory, ID, '/', week, '/')
  
  # create data directory if it does not yet exist
  if(!dir.exists(output_filepath)){dir.create(output_filepath, recursive = T)}
  
  if(downscaling == 'NULL'){
    suffix <- ''
    
  }else if(downscaling == T){
    suffix <- '_downscaling_modis_30m'
    
  }else if(downscaling == F){
    suffix <- '_downscaling_modis_250m'

  }else{stop('Incorrect term set for downscaling parameter. Should be one of the following: NULL, T, F.')}
  
  # read step dataset
  step_dataset <- read.csv(paste0(input_directory, '4_a1_cov_resp_dataset_', random_data_method, suffix, '.csv'))
  
  # generate correlation matrix 
  if(multicolinearity_check == T){

    # select columns with predictors only 
    # source: https://stackoverflow.com/questions/25923392/select-columns-based-on-string-match-dplyrselect
    covariates <- step_dataset %>% select(contains(c('case_', 'ndvi')))
    
    # save matrix as png
    png(filename = paste0(output_filepath, '6_a1_correlation_matrix_', random_data_method, suffix, '.png'), width = 850, height = 350)
    
    # source: # source: https://r-charts.com/correlation/ggpairs/?utm_content=cmp-true
    correlation_matrix <- ggpairs(covariates, columns = 2:ncol(covariates), aes(color = as.factor(case_), alpha = 0.5))    
    
    # add print statement for graph to save as png inside a function 
    # source: https://stackoverflow.com/questions/9206110/using-png-function-not-working-when-called-within-a-function
    print(correlation_matrix)
    dev.off()
  }
  
  # fit conditional logistic regression and general logistic regression models 
  if(full == T & scaled == F){
    output_number = '6_a'
    model_type = '_full_'
    
    # source: https://cran.r-project.org/web/packages/amt/vignettes/p4_SSF.html
    clr_model <- fit_clogit(step_dataset, case_ ~ ndvi_10 + ndvi_50 + ndvi_90 + ndvi_sd + 
                              ndvi_rate_10 + ndvi_rate_50 + ndvi_rate_90 + ndvi_rate_sd + strata(step_id_))
    
    # source: https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/
    glm_model <- glm(case_ ~ ndvi_10 + ndvi_50 + ndvi_90 + ndvi_sd + ndvi_rate_10 + ndvi_rate_50 +
                       ndvi_rate_90 + ndvi_rate_sd, family = binomial(link = 'logit'), data = step_dataset)
    
  }else if(full == F & scaled == F){
    output_number = '6_b'
    model_type = '_50p_sd_'
    
    # source: https://cran.r-project.org/web/packages/amt/vignettes/p4_SSF.html
    clr_model <- fit_clogit(step_dataset, case_ ~ ndvi_50 + ndvi_sd + 
                              ndvi_rate_50 + ndvi_rate_sd + strata(step_id_))
    
    # source: https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/
    glm_model <- glm(case_ ~ ndvi_50 + ndvi_sd + ndvi_rate_50 + ndvi_rate_sd, 
                     family = binomial(link = 'logit'), data = step_dataset)
  }else if(full == T & scaled == T){
    output_number = '6_a'
    model_type = '_full_'
    suffix <- paste0(suffix, '_scaled')
    
    # source: https://cran.r-project.org/web/packages/amt/vignettes/p4_SSF.html
    clr_model <- fit_clogit(step_dataset, case_ ~ ndvi_10_scaled + ndvi_50_scaled + ndvi_90_scaled + ndvi_sd_scaled + 
                              ndvi_rate_10_scaled + ndvi_rate_50_scaled + ndvi_rate_90_scaled + ndvi_rate_sd_scaled + strata(step_id_))
    
    # source: https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/
    glm_model <- glm(case_ ~ ndvi_10_scaled + ndvi_50_scaled + ndvi_90_scaled + ndvi_sd_scaled + ndvi_rate_10_scaled + ndvi_rate_50_scaled +
                       ndvi_rate_90_scaled + ndvi_rate_sd_scaled, family = binomial(link = 'logit'), data = step_dataset)
  }else{
    output_number = '6_b'
    model_type = '_50p_sd_'
    suffix <- paste0(suffix, '_scaled')
    
    # source: https://cran.r-project.org/web/packages/amt/vignettes/p4_SSF.html
    clr_model <- fit_clogit(step_dataset, case_ ~ ndvi_50_scaled + ndvi_sd_scaled + 
                              ndvi_rate_50_scaled + ndvi_rate_sd_scaled + strata(step_id_))
    
    # source: https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/
    glm_model <- glm(case_ ~ ndvi_50_scaled + ndvi_sd_scaled + ndvi_rate_50_scaled + ndvi_rate_sd_scaled, 
                     family = binomial(link = 'logit'), data = step_dataset)
  }
  
  # save model 
  saveRDS(clr_model, paste0(output_filepath, output_number, '0_clr', model_type, 'model_', random_data_method, suffix, '.RDS'))
  saveRDS(glm_model, paste0(output_filepath, output_number, '0_glm', model_type, 'model_', random_data_method, suffix, '.RDS'))
  
  # get model summaries
  clr_summary <- summary(clr_model)
  glm_summary <- summary(glm_model)
  
  # get VIF from GLM
  vif_results <- vif(glm_model)
  
  # print information about the dataset and the model
  print(paste('Dataset elephant:', ID))
  print(paste('Week:', as.character(week)))
  print(paste('Pseudo-absence method:', random_data_method))
  print(paste('Number of total observations:', as.character(nrow(step_dataset))))
  print(paste('Number of observations included in model:', clr_summary$n))
  print(paste('Sets of steps:', clr_summary$nevent))
  print(clr_summary)
  print(glm_summary)
  print(vif_results)
  
  # get model coefficients 
  # source: https://stackoverflow.com/questions/61482594/export-coxph-summary-from-r-to-csv
  clr_coef <- data.frame(clr_summary$coefficients)
  glm_coef <- data.frame(glm_summary$coefficients)
  
  # create dataframe with statistical test results of CLR, the concordance and its standard error 
  clr_tests <- data.frame(log_likelihood = clr_summary$logtest, score = clr_summary$sctest, wald = clr_summary$waldtest)
  clr_tests <- rbind(clr_tests, SE_concordance = c(NA))
  clr_tests <- cbind(clr_tests, concordance = c(clr_summary$concordance[1], NA, NA, clr_summary$concordance[2]))
  
  # create dataframe of deviance and vif results from GLM
  glm_deviances <- data.frame(null_deviance = glm_summary$null.deviance, null_df = glm_summary$df.null, 
                              residual_deviance = glm_summary$deviance, residual_df = glm_summary$df.residual)
  glm_vif <- data.frame(vif_results)
  
  # save model results as csv 
  write.csv(clr_coef, paste0(output_filepath, output_number, '1_clr', model_type, 'coefs_', random_data_method, suffix, '.csv'))
  write.csv(clr_tests, paste0(output_filepath, output_number, '2_clr', model_type, 'tests_', random_data_method, suffix, '.csv'))
  
  write.csv(glm_coef, paste0(output_filepath, output_number, '3_glm', model_type, 'coefs_', random_data_method, suffix, '.csv'))
  write.csv(glm_deviances, paste0(output_filepath, output_number, '4_glm', model_type, 'deviances_', random_data_method, suffix, '.csv'))
  write.csv(glm_vif, paste0(output_filepath, output_number, '5_glm', model_type, 'vif_', random_data_method, suffix, '.csv'))
}



























fitMovementModelWithoutCV <- function(input_directory = 'data/', ID, week, random_data_method = 'random_path_custom_distr', 
                             downscaling = 'NULL', input_suffix = '', output_directory = 'output/', output_suffix = ''){
  
  
  ######## STEP 0: Set-Up
  
  
  # create output filepath 
  output_filepath <- paste0(output_directory, ID, '/', week, '/')
  
  # create data directory if it does not yet exist
  if(!dir.exists(output_filepath)){dir.create(output_filepath, recursive = T)}
  
  if(downscaling == 'NULL'){
    suffix <- ''
    
  }else if(downscaling == T){
    suffix <- '_downscaling_modis_30m'
    
  }else if(downscaling == F){
    suffix <- '_downscaling_modis_250m'
    
  }else{stop('Incorrect term set for downscaling parameter. Should be one of the following: NULL, T, F.')}
  
  # define model specifics to avoid overwriting previous results 
  output_number = '6_d'
  model_type = '_mean_sd_' # refers to model built no selection of predictors
  
  # read step dataset
  step_dataset <- read.csv(paste0(input_directory, '4_a1_cov_resp_dataset_', random_data_method, 
                                  suffix, input_suffix, '.csv'), row.names = 1)
  
  # duplicate the case outcome column (to keep a version with boolean)
  step_dataset$case_bool <- step_dataset$case_
  
  # rename case outcome T = presence; F = absence 
  step_dataset$case_[step_dataset$case_ == T] <- 'presence'
  step_dataset$case_[step_dataset$case_ == F] <- 'absence'
  
  # create new column that groups true/false cases for corresponding step in burst 
  # package: dplyr
  # source: https://stackoverflow.com/questions/24119599/how-to-assign-a-unique-id-number-to-each-group-of-identical-values-in-a-column
  step_dataset <- step_dataset %>% group_by(burst_, step_id_) %>% mutate(pair_id=cur_group_id())
  
  # set the class to factor type instead of string
  step_dataset$case_ <- factor(step_dataset$case_, levels = c('absence', 'presence'))
  
  
  ######## STEP 1: Split step dataset into training and testing sets 
  
  
  # get list of pair IDs (all pair groups have 1 T and 20 F)
  pair_id_list <- unique(step_dataset$pair_id)
  
  # generate random split in indices of step pairs
  # package: caret
  # source: https://rforhr.com/kfold.html
  # Note: the CreateDataPartition function didn't work on very small sample size --> had to write alternative command that simulated this function
  set.seed(152)
  #partition <- unname(createDataPartition(pair_id_list, p=.7, list=F, times=1))
  partition <- sample(pair_id_list, size = floor(0.7*length(pair_id_list)), replace = F)
  
  # split dataset into training and testing based on partition
  training_steps <- step_dataset[step_dataset$pair_id %in% partition,]
  test_steps <- step_dataset[!(step_dataset$pair_id %in% partition),]
  
  # convert outcome column to factor (necessary to define classes)
  training_steps$case_ <- factor(training_steps$case_, levels = c('absence', 'presence'))
  test_steps$case_ <- factor(test_steps$case_, levels = c('absence', 'presence'))
  
  # remove any steps that contain NA in test steps (normally should not have any but for reason some slipped through so this is a fail safe)
  test_steps <- test_steps[complete.cases(test_steps[,c('case_', 'ndvi_mean_scaled', 'ndvi_sd_scaled', 
                                                        'ndvi_rate_mean_scaled', 'ndvi_rate_sd_scaled')]),]
  
  
  ######## STEP 2: Implement cross-validation and fit GLM model on training step dataset
  
  # define cross-validation settings 
  # Note: Applying a 10-fold cross-validation on the training data 
  # package: caret
  #cv_settings <- trainControl(method = 'cv', classProbs = T, number = 10, summaryFunction = prSummary(data, lev = levels(data$obs)))
  
  # fit a custom GLM model through the training data with hyperparameter tuning of threshold probability
  # source: https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/
  # source: https://www.rdocumentation.org/packages/traineR/versions/2.2.0/topics/train.glm
  # source: https://cran.r-project.org/web/packages/caret/vignettes/caret.html
  # source: https://stackoverflow.com/questions/38250440/error-in-na-fail-default-missing-values-in-object-but-no-missing-values
  # Note: 20 threshold values are tested in the parameter tuning 
  # Note: Dist used as custom performance metric for tuning
  # package: caret 
  # glm_model_custom <- train(case_ ~ ndvi_mean_scaled + ndvi_sd_scaled + ndvi_rate_mean_scaled + ndvi_rate_sd_scaled, 
  #                           data = training_steps, method = custom_glm, family = binomial(link = 'logit'),
  #                           metric = 'Dist', maximize = F, tuneLength = 20, trControl = cv_settings_custom, na.action = na.pass)
  
  # package: caret
  # source: https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/
  glm_model <- glm(case_ ~ ndvi_mean_scaled + ndvi_sd_scaled + ndvi_rate_mean_scaled + ndvi_rate_sd_scaled, 
                   family = binomial(link = 'logit'), data = training_steps)
  
  
  ######## STEP 3: Predict test set with trained, optimized, and validated GLM model
  
  # predict likelihood of elephant occurrence on step
  # Note: Type 'raw' outputs the class directly instead of the probability based on the tuned cutoff value from trained model
  test_steps$prediction  <- predict(glm_model, newdata = test_steps , type = "response")
  
  # create confusion matrix
  # source: https://stackoverflow.com/questions/46028360/confusionmatrix-for-logistic-regression-in-r
  # package: caret
  #cm <- confusionMatrix(data = test_steps$prediction, reference = test_steps$case_, positive = 'presence')
  
  # iterate to find optimal cutoff value to generate performance metrics 
  # source: https://cran.r-project.org/web/packages/cutpointr/vignettes/cutpointr.html
  # package: cutpointr 
  # source: https://stats.stackexchange.com/questions/61521/cut-off-point-in-a-roc-curve-is-there-a-simple-function
  cp <- cutpointr(test_steps, prediction, case_, method = maximize_metric, metric = F1_score, pos_class = 'presence', direction = '>=')
  
  
  ######## STEP 4: Retrieve model results and performance metrics 
  
  
  # get VIF from GLM (not applicable to train object = cross-validated GLM model or CLR)
  # package: car
  glm_vif <- data.frame(vif(glm_model))
  
  # get model summaries
  glm_summary <- summary(glm_model)
  
  # get model coefficients 
  # source: https://stackoverflow.com/questions/61482594/export-coxph-summary-from-r-to-csv
  glm_coef <- data.frame(glm_summary$coefficients)
  
  # create dataframe of deviance and AIC for GLM models 
  glm_deviances <- data.frame(AIC = glm_summary$aic, null_deviance = glm_summary$null.deviance, null_df = glm_summary$df.null, 
                              residual_deviance = glm_summary$deviance, residual_df = glm_summary$df.residual)
  
  # # create dataframe with optimal cross-validation results from custom GLM
  # glm_validation <- glm_model_custom$results[glm_model_custom$results$threshold == glm_model_custom$bestTune$threshold,]
  # 
  # # create dataframe with custom GLM performance results on test set 
  # glm_c_test <- data.frame(cm$byClass)
  # 
  
  cm <- data.frame(summary(cp)$confusion_matrix)
  precision <- cm$tp/(cm$tp + cm$fp)
  
  glm_validation <- data.frame(test_AUC = cp$AUC, test_accuracy = cp$acc, test_F1_score = cp$F1_score, 
                               test_cutoff = cp$optimal_cutpoint, test_recall = cp$sensitivity, 
                               test_specificity = cp$specificity, test_precision = precision, 
                               test_positive_class = cp$pos_class, test_prevalence = cp$prevalence)
  
  ### save outputs 
  
  suffix <- paste0(suffix, output_suffix)
  
  # save model 
  saveRDS(glm_model, paste0(output_filepath, output_number, '0_glm', model_type, 'model_', random_data_method, suffix, '.RDS'))
  
  # save model results as csv 
  write.csv(glm_coef, paste0(output_filepath, output_number, '3_glm', model_type, 'coefs_', random_data_method, suffix, '.csv'))
  write.csv(glm_deviances, paste0(output_filepath, output_number, '4_glm', model_type, 'deviances_', random_data_method, suffix, '.csv'))
  write.csv(glm_vif, paste0(output_filepath, output_number, '5_glm', model_type, 'vif_', random_data_method, suffix, '.csv'))
  write.csv(glm_validation, paste0(output_filepath, output_number, '6_glm', model_type, 'test_results_', random_data_method, suffix, '.csv'))
  
}













fitMovementModelWith10fCV <- function(input_directory = 'data/', ID, week, random_data_method = 'random_path_custom_distr', 
                                      downscaling = 'NULL', input_suffix = '', output_directory = 'output/', output_suffix = ''){
  
  
  ######## STEP 0: Set-Up
  
  
  # create output filepath 
  output_filepath <- paste0(output_directory, ID, '/', week, '/')
  
  # create data directory if it does not yet exist
  if(!dir.exists(output_filepath)){dir.create(output_filepath, recursive = T)}
  
  if(downscaling == 'NULL'){
    suffix <- ''
    
  }else if(downscaling == T){
    suffix <- '_downscaling_modis_30m'
    
  }else if(downscaling == F){
    suffix <- '_downscaling_modis_250m'
    
  }else{stop('Incorrect term set for downscaling parameter. Should be one of the following: NULL, T, F.')}
  
  # define model specifics to avoid overwriting previous results 
  output_number = '6_e'
  model_type = '_mean_sd_' # refers to model built no selection of predictors
  
  # read step dataset
  step_dataset <- read.csv(paste0(input_directory, '4_a1_cov_resp_dataset_', random_data_method, 
                                  suffix, input_suffix, '.csv'), row.names = 1)
  
  # duplicate the case outcome column (to keep a version with boolean)
  step_dataset$case_bool <- step_dataset$case_
  
  # rename case outcome T = presence; F = absence 
  step_dataset$case_[step_dataset$case_ == T] <- 'presence'
  step_dataset$case_[step_dataset$case_ == F] <- 'absence'
  
  # create new column that groups true/false cases for corresponding step in burst 
  # package: dplyr
  # source: https://stackoverflow.com/questions/24119599/how-to-assign-a-unique-id-number-to-each-group-of-identical-values-in-a-column
  step_dataset <- step_dataset %>% group_by(burst_, step_id_) %>% mutate(pair_id=cur_group_id())
  
  # set the class to factor type instead of string
  step_dataset$case_ <- factor(step_dataset$case_, levels = c('absence', 'presence'))
  
  # remove any rows containing missing data in predictor and response variable columns 
  step_dataset <- step_dataset[complete.cases(step_dataset[,c('case_', 'ndvi_mean_scaled', 'ndvi_sd_scaled',
                                                              'ndvi_rate_mean_scaled', 'ndvi_rate_sd_scaled')]),]
  
  # create fixed ID for each individual data point to find it again later in processing 
  step_dataset$occurrence_ID <- 1:nrow(step_dataset)
  
  
  ######## STEP 1: Split dataset into 10 folds and train GLM model (predict left out fold)
  
  
  # get list of pair IDs (all pair groups have 1 T and 20 F)
  pair_id_list <- unique(step_dataset$pair_id)
  
  # partitions the step pairs into (max) 10 folds of approx equal size 
  # returns the list of indices used to test the model (all other indices used to train the model) --> all step IDs appear once in test set (left out fold)
  # package: caret
  # source: https://rforhr.com/kfold.html
  set.seed(152)
  partition <- createFolds(pair_id_list, k = 10, list = TRUE, returnTrain = F)
  
  # loop each fold iteration = manual k fold cross validation 
  predicted_occurrences <- data.frame()
  glm_all_vif <- data.frame()
  glm_all_coef <- data.frame()
  glm_all_deviances <- data.frame()
  
  for(i in 1:length(partition)){
    
    # split dataset into training and testing based on partition
    training_steps <- step_dataset[!(step_dataset$pair_id %in% partition[[i]]),]
    test_steps <- step_dataset[step_dataset$pair_id %in% partition[[i]],]
    
    # convert outcome column to factor (necessary to define classes)
    training_steps$case_ <- factor(training_steps$case_, levels = c('absence', 'presence'))
    test_steps$case_ <- factor(test_steps$case_, levels = c('absence', 'presence'))
    
    # fit model on training set 
    # package: caret
    # source: https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/
    glm_model <- glm(case_ ~ ndvi_mean_scaled + ndvi_sd_scaled + ndvi_rate_mean_scaled + ndvi_rate_sd_scaled, 
                     family = binomial(link = 'logit'), data = training_steps)
    
    # get VIF from GLM
    # package: car
    glm_vif <- data.frame(vif(glm_model))
    glm_vif$predictor <- row.names(glm_vif)
    
    glm_all_vif <- rbind(glm_all_vif, glm_vif)
    
    # get model coefficients 
    glm_summary <- summary(glm_model)
    glm_coef <- data.frame(glm_summary$coefficients)
    glm_coef$predictor <- row.names(glm_coef)
    
    glm_all_coef <- rbind(glm_all_coef, glm_coef)
    
    # get model deviance and AIC
    glm_deviances <- data.frame(AIC = glm_summary$aic, null_deviance = glm_summary$null.deviance, null_df = glm_summary$df.null,
                                residual_deviance = glm_summary$deviance, residual_df = glm_summary$df.residual)
    
    glm_all_deviances <- rbind(glm_all_deviances, glm_deviances)
    
    # predict likelihood of elephant occurrence on step 
    test_steps$prediction  <- predict(glm_model, newdata = test_steps , type = "response")
    
    # append the predictions to dataframe 
    predicted_occurrences <- rbind(predicted_occurrences, test_steps[,c('occurrence_ID', 'prediction')])
    
  }
  
  # aggregate model VIF coefs and deviance results --> average 
  glm_vif <- glm_all_vif %>% group_by(predictor) %>% summarise_all(mean)
  glm_coef <- glm_all_coef %>% group_by(predictor) %>% summarise_all(mean)
  glm_deviances <- glm_all_deviances %>% summarise_all(mean)
  
  # match the predicted occurrence the rows of step dataset 
  step_dataset <- merge(step_dataset, predicted_occurrences, by = 'occurrence_ID')
  
  
  
  ######## STEP 3: Tune model for optimal cutoff probability value and derive model performance
  
  # iterate to find optimal cutoff value to generate performance metrics 
  # source: https://cran.r-project.org/web/packages/cutpointr/vignettes/cutpointr.html
  # package: cutpointr 
  # source: https://stats.stackexchange.com/questions/61521/cut-off-point-in-a-roc-curve-is-there-a-simple-function
  cp <- cutpointr(test_steps, prediction, case_, method = maximize_metric, metric = F1_score, pos_class = 'presence', direction = '>=')
  
  # calculate precision 
  cm <- data.frame(summary(cp)$confusion_matrix)
  precision <- cm$tp/(cm$tp + cm$fp)
  
  # summarize model performance results into table 
  glm_validation <- data.frame(AUC = cp$AUC, accuracy = cp$acc, F1_score = cp$F1_score, 
                               cutoff = cp$optimal_cutpoint, recall = cp$sensitivity, 
                               specificity = cp$specificity, precision = precision, 
                               positive_class = cp$pos_class, prevalence = cp$prevalence)
  
  ######## STEP 4: Save outputs
  
  suffix <- paste0(suffix, output_suffix)
  
  # save model --> not possible since aggregating iterated models 
  #saveRDS(glm_model, paste0(output_filepath, output_number, '0_glm', model_type, 'model_', random_data_method, suffix, '.RDS'))
  
  # save model results as csv 
  write.csv(glm_coef, paste0(output_filepath, output_number, '3_glm', model_type, 'coefs_', random_data_method, suffix, '.csv'))
  write.csv(glm_deviances, paste0(output_filepath, output_number, '4_glm', model_type, 'deviances_', random_data_method, suffix, '.csv'))
  write.csv(glm_vif, paste0(output_filepath, output_number, '5_glm', model_type, 'vif_', random_data_method, suffix, '.csv'))
  write.csv(glm_validation, paste0(output_filepath, output_number, '6_glm', model_type, 'test_results_', random_data_method, suffix, '.csv'))
  
}




# 
# 
# 
# ######## STEP 0: Set-Up
# run_settings <- read.csv('data/run_settings_STS_final.csv', row.names = 1)[1,]
# 
# # define elephant ID
# ID <- run_settings[[1]]
# 
# # define week to test
# week <- 2084
# 
# # define pseudo-absence path generator method --> Choices are 1) random_path_custom_distr 2) random_path_buffer_point 3) random_step
# random_data_method <- run_settings[[3]]
# 
# # set downscaling parameter
# downscaling <- run_settings[[4]]
# 
# # define run filepath
# run_filepath <- paste0('data/', ID, '/', week, '/')
# 
# # replace NA from suffix columns of run settings to empty strings
# if(is.na(run_settings[[6]])){run_settings[[6]] <- ''}
# if(is.na(run_settings[[7]])){run_settings[[7]] <- ''}
# 
# # define input and output suffixes
# input_suffix <- run_settings[[6]]
# output_suffix <- '_newPathWith10fCV'
# 
# 
# input_directory <- run_filepath
# output_directory <- 'output/'
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
# output_number = '6_e'
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
# # create new column that groups true/false cases for corresponding step in burst
# # package: dplyr
# library(dplyr)
# # source: https://stackoverflow.com/questions/24119599/how-to-assign-a-unique-id-number-to-each-group-of-identical-values-in-a-column
# step_dataset <- step_dataset %>% group_by(burst_, step_id_) %>% mutate(pair_id=cur_group_id())
# 
# # set the class to factor type instead of string
# step_dataset$case_ <- factor(step_dataset$case_, levels = c('absence', 'presence'))
# 
# # remove any rows containing missing data in predictor and response variable columns 
# step_dataset <- step_dataset[complete.cases(step_dataset[,c('case_', 'ndvi_mean_scaled', 'ndvi_sd_scaled',
#                                         'ndvi_rate_mean_scaled', 'ndvi_rate_sd_scaled')]),]
# 
# # create fixed ID for each individual data point to find it again later in processing 
# step_dataset$occurrence_ID <- 1:nrow(step_dataset)
# 
# 
# ######## STEP 1: Split step dataset into training and testing sets
# 
# 
# # get list of pair IDs (all pair groups have 1 T and 20 F)
# pair_id_list <- unique(step_dataset$pair_id)
# 
# # partitions the step pairs into (max) 10 folds of approx equal size 
# # returns the list of indices used to test the model (all other indices used to train the model) --> all step IDs appear once in test set (left out fold)
# # package: caret
# # source: https://rforhr.com/kfold.html
# set.seed(152)
# partition <- createFolds(pair_id_list, k = 10, list = TRUE, returnTrain = F)
# 
# # loop each fold iteration = manual k fold cross validation 
# predicted_occurrences <- data.frame()
# glm_all_vif <- data.frame()
# glm_all_coef <- data.frame()
# glm_all_deviances <- data.frame()
# 
# for(i in 1:length(partition)){
#   
#   # split dataset into training and testing based on partition
#   training_steps <- step_dataset[!(step_dataset$pair_id %in% partition[[i]]),]
#   test_steps <- step_dataset[step_dataset$pair_id %in% partition[[i]],]
#   
#   # convert outcome column to factor (necessary to define classes)
#   training_steps$case_ <- factor(training_steps$case_, levels = c('absence', 'presence'))
#   test_steps$case_ <- factor(test_steps$case_, levels = c('absence', 'presence'))
#   
#   # fit model on training set 
#   glm_model <- glm(case_ ~ ndvi_mean_scaled + ndvi_sd_scaled + ndvi_rate_mean_scaled + ndvi_rate_sd_scaled, 
#                    family = binomial(link = 'logit'), data = training_steps)
#   
#   # get VIF from GLM
#   # package: car
#   glm_vif <- data.frame(vif(glm_model))
#   glm_vif$predictor <- row.names(glm_vif)
#   
#   glm_all_vif <- rbind(glm_all_vif, glm_vif)
#   
#   # get model coefficients 
#   glm_summary <- summary(glm_model)
#   glm_coef <- data.frame(glm_summary$coefficients)
#   glm_coef$predictor <- row.names(glm_coef)
#   
#   glm_all_coef <- rbind(glm_all_coef, glm_coef)
#   
#   # get model deviance and AIC
#   glm_deviances <- data.frame(AIC = glm_summary$aic, null_deviance = glm_summary$null.deviance, null_df = glm_summary$df.null,
#                               residual_deviance = glm_summary$deviance, residual_df = glm_summary$df.residual)
#   
#   glm_all_deviances <- rbind(glm_all_deviances, glm_deviances)
#   
#   # predict likelihood of elephant occurrence on step 
#   test_steps$prediction  <- predict(glm_model, newdata = test_steps , type = "response")
#   
#   # append the predictions to dataframe 
#   predicted_occurrences <- rbind(predicted_occurrences, test_steps[,c('occurrence_ID', 'prediction')])
#   
# }
# 
# # aggregate model VIF coefs and deviance results --> average 
# glm_vif <- glm_all_vif %>% group_by(predictor) %>% summarise_all(mean)
# 
# glm_coef <- glm_all_coef %>% group_by(predictor) %>% summarise_all(mean)
# 
# glm_deviances <- glm_all_deviances %>% summarise_all(mean)
# 
# # match the predicted occurrence the rows of step dataset 
# step_dataset <- merge(step_dataset, predicted_occurrences, by = 'occurrence_ID')
# 
# # iterate to find optimal cutoff value to generate performance metrics 
# # source: https://cran.r-project.org/web/packages/cutpointr/vignettes/cutpointr.html
# # package: cutpointr 
# # source: https://stats.stackexchange.com/questions/61521/cut-off-point-in-a-roc-curve-is-there-a-simple-function
# cp <- cutpointr(step_dataset, prediction, case_, method = maximize_metric, metric = F1_score, pos_class = 'presence', direction = '>=')
# 
# # calculate precision 
# cm <- data.frame(summary(cp)$confusion_matrix)
# precision <- cm$tp/(cm$tp + cm$fp)
# 
# # summarize model performance results into table 
# glm_validation <- data.frame(AUC = cp$AUC, accuracy = cp$acc, F1_score = cp$F1_score, 
#                              cutoff = cp$optimal_cutpoint, recall = cp$sensitivity, 
#                              specificity = cp$specificity, precision = precision, 
#                              positive_class = cp$pos_class, prevalence = cp$prevalence)
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
# # read step dataset
# step_dataset2 <- read.csv(paste0(input_directory, '4_a1_cov_resp_dataset_', random_data_method,
#                                 suffix, input_suffix, '.csv'), row.names = 1)
# 
# # duplicate the case outcome column (to keep a version with boolean)
# step_dataset2$case_bool <- step_dataset2$case_
# 
# # rename case outcome T = presence; F = absence
# step_dataset2$case_[step_dataset2$case_ == T] <- 'presence'
# step_dataset2$case_[step_dataset2$case_ == F] <- 'absence'
# 
# # create new column that groups true/false cases for corresponding step in burst
# # package: dplyr
# library(dplyr)
# # source: https://stackoverflow.com/questions/24119599/how-to-assign-a-unique-id-number-to-each-group-of-identical-values-in-a-column
# step_dataset2 <- step_dataset2 %>% group_by(burst_, step_id_) %>% mutate(pair_id=cur_group_id())
# 
# # set the class to factor type instead of string
# step_dataset2$case_ <- factor(step_dataset2$case_, levels = c('presence', 'absence'))
# 
# 
# ######## STEP 1: Split step dataset into training and testing sets
# 
# 
# # get list of pair IDs (all pair groups have 1 T and 20 F)
# pair_id_list2 <- unique(step_dataset2$pair_id)
# 
# # generate random split in indices of step pairs
# # package: caret
# # source: https://rforhr.com/kfold.html
# # Note: the CreateDataPartition function didn't work on very small sample size --> had to write alternative command that simulated this function
# set.seed(152)
# #partition <- unname(createDataPartition(pair_id_list, p=.7, list=F, times=1))
# partition2 <- sample(pair_id_list2, size = floor(0.7*length(pair_id_list2)), replace = F)
# 
# # split dataset into training and testing based on partition
# training_steps2 <- step_dataset2[step_dataset2$pair_id %in% partition2,]
# test_steps2 <- step_dataset2[!(step_dataset2$pair_id %in% partition2),]
# 
# # convert outcome column to factor (necessary to define classes)
# training_steps2$case_ <- factor(training_steps2$case_, levels = c('presence', 'absence'))
# test_steps2$case_ <- factor(test_steps2$case_, levels = c('presence', 'absence'))
# 
# # remove any steps that contain NA in test steps (normally should not have any but for reason some slipped through so this is a fail safe)
# test_steps2 <- test_steps2[complete.cases(test_steps2[,c('case_', 'ndvi_mean_scaled', 'ndvi_sd_scaled',
#                                                       'ndvi_rate_mean_scaled', 'ndvi_rate_sd_scaled')]),]
# 
# 
# ######## STEP 2: Implement cross-validation and fit GLM model on training step dataset
# 
# # define cross-validation settings
# # Note: Applying a 10-fold cross-validation on the training data
# # package: caret
# library(caret)
# # cv_settings <- trainControl(method = 'cv', classProbs = T, summaryFunction = prSummary)
# cv_settings2 <- trainControl(method = 'cv', number = 10, classProbs = T, summaryFunction = prSummary)
# 
# # # fit a custom GLM model through the training data with hyperparameter tuning of threshold probability
# # # source: https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/
# # # source: https://www.rdocumentation.org/packages/traineR/versions/2.2.0/topics/train.glm
# # # source: https://cran.r-project.org/web/packages/caret/vignettes/caret.html
# # # source: https://stackoverflow.com/questions/38250440/error-in-na-fail-default-missing-values-in-object-but-no-missing-values
# # # Note: 20 threshold values are tested in the parameter tuning
# # # Note: Dist used as custom performance metric for tuning
# # # package: caret
# # glm_model_custom <- train(case_ ~ ndvi_mean_scaled + ndvi_sd_scaled + ndvi_rate_mean_scaled + ndvi_rate_sd_scaled,
# #                           data = training_steps, method = custom_glm, family = binomial(link = 'logit'),
# #                           metric = 'Dist', maximize = F, tuneLength = 20, trControl = cv_settings_custom, na.action = na.pass)
# 
# # package: caret
# # source: https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/
# glm_model2 <- train(case_ ~ ndvi_mean_scaled + ndvi_sd_scaled + ndvi_rate_mean_scaled + ndvi_rate_sd_scaled, method = 'glm',
#                    family = binomial(link = 'logit'), data = training_steps2, trControl = cv_settings2, metric = 'AUC', na.action = na.pass)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # predict likelihood of elephant occurrence on step
# #test_steps$prediction  <- predict(glm_model, newdata = test_steps, type = "response")
# training_steps$prediction <- predict( glm_model2, newdata = training_steps, type = "prob" )
# test_steps$prediction  <- predict( glm_model2, newdata = test_steps , type = "prob" )
# training_steps$predClass <- predict( glm_model2, newdata = training_steps, type = "raw" )
# test_steps$predClass  <- predict( glm_model2, newdata = test_steps , type = "raw" )
# 
# # create confusion matrix of predicted values based on new threshold value
# # source: https://stackoverflow.com/questions/46028360/confusionmatrix-for-logistic-regression-in-r
# # source: https://stackoverflow.com/questions/39803667/r-create-factor-based-on-condition
# confusionMatrix(data = factor(ifelse(test_steps$prediction$presence > 0.5, 'presence', 'absence')),
#                 reference = test_steps$case_, positive = 'presence')
# 
# confusionMatrix(data = factor(ifelse(test_steps$prediction$presence > 0.06, 'presence', 'absence')),
#                 reference = test_steps$case_, positive = 'presence')
# 
# # cm <- confusionMatrix(data = test_steps$predClass, reference = test_steps$case_, positive = 'presence')
# # cm$byClass
# # 
# # training_steps <- 
# 
# ggplot( training_steps, aes( predClass, color = as.factor(case_) ) ) +
#   geom_density( linewidth = 1 ) +
#   ggtitle( "Training Set's Predicted Score" ) +
#   scale_color_manual( name = "data", values = c( "absence" = 'orange', "presence" = 'blue'), labels = c('absence', 'presence')) +
#   theme_minimal()
# 
# ggplot( test_steps, aes( pred, color = as.factor(case_) ) ) +
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
# cp <- cutpointr(training_steps, predClass, case_, method = maximize_metric, metric = F1_score, pos_class = 'presence', direction = '>=')
# summary(cp)
# cp$F1_score
# cp$AUC
# plot(cp)
# plot_metric(cp)
# 
# 
# 
# 
# # 
# # # read step dataset
# # step_dataset3 <- read.csv(paste0(input_directory, '4_a1_cov_resp_dataset_', random_data_method,
# #                                  suffix, input_suffix, '.csv'), row.names = 1)
# # 
# # # duplicate the case outcome column (to keep a version with boolean)
# # step_dataset3$case_bool <- step_dataset3$case_
# # 
# # # rename case outcome T = presence; F = absence
# # step_dataset3$case_[step_dataset3$case_ == T] <- 'presence'
# # step_dataset3$case_[step_dataset3$case_ == F] <- 'absence'
# # 
# # # create new column that groups true/false cases for corresponding step in burst
# # # package: dplyr
# # library(dplyr)
# # # source: https://stackoverflow.com/questions/24119599/how-to-assign-a-unique-id-number-to-each-group-of-identical-values-in-a-column
# # step_dataset3 <- step_dataset3 %>% group_by(burst_, step_id_) %>% mutate(pair_id=cur_group_id())
# # 
# # # set the class to factor type instead of string
# # step_dataset3$case_ <- factor(step_dataset3$case_, levels = c('absence', 'presence'))
# # 
# # 
# # ######## STEP 1: Split step dataset into training and testing sets
# # 
# # 
# # # get list of pair IDs (all pair groups have 1 T and 20 F)
# # pair_id_list3 <- unique(step_dataset3$pair_id)
# # 
# # # generate random split in indices of step pairs
# # # package: caret
# # # source: https://rforhr.com/kfold.html
# # # Note: the CreateDataPartition function didn't work on very small sample size --> had to write alternative command that simulated this function
# # set.seed(152)
# # #partition <- unname(createDataPartition(pair_id_list, p=.7, list=F, times=1))
# # partition3 <- sample(pair_id_list3, size = floor(0.7*length(pair_id_list3)), replace = F)
# # 
# # # split dataset into training and testing based on partition
# # training_steps3 <- step_dataset3[step_dataset3$pair_id %in% partition3,]
# # test_steps3 <- step_dataset3[!(step_dataset3$pair_id %in% partition3),]
# # 
# # 
# # # ONLY FOR OPTION 1: Reverse level order of classes!
# # # Note: By default, absence (= 0) is first level, that's the negative outcome.
# # #       This doesn't match hardcoded 'first level = positive outcome' rule of CV function (twoClassSummary).
# # #       Need to reverse so presence (=1) is first level and read in CV function as positive outcome.
# # # source: https://stackoverflow.com/questions/45333029/specifying-positive-class-of-an-outcome-variable-in-caret-train
# # # Note: also applying this so test set (even though CV function not used) for consistency
# # training_steps_rev <- training_steps3
# # training_steps_rev$case_ <- factor(training_steps_rev$case_, levels=rev(levels(as.factor(training_steps_rev$case_))))
# # test_steps_rev <- test_steps3
# # test_steps_rev$case_ <- factor(test_steps_rev$case_, levels=rev(levels(as.factor(test_steps_rev$case_))))
# # # 
# # # 
# # # # convert outcome column to factor (necessary to define classes)
# # # training_steps2$case_ <- factor(training_steps2$case_, levels = c('presence', 'absence'))
# # # test_steps2$case_ <- factor(test_steps2$case_, levels = c('presence', 'absence'))
# # 
# # # remove any steps that contain NA in test steps (normally should not have any but for reason some slipped through so this is a fail safe)
# # test_steps3 <- test_steps3[complete.cases(test_steps3[,c('case_', 'ndvi_mean_scaled', 'ndvi_sd_scaled',
# #                                                          'ndvi_rate_mean_scaled', 'ndvi_rate_sd_scaled')]),]
# # 
# # 
# # ######## STEP 2: Implement cross-validation and fit GLM model on training step dataset
# # 
# # # define cross-validation settings
# # # Note: Applying a 10-fold cross-validation on the training data
# # # package: caret
# # library(caret)
# # # cv_settings <- trainControl(method = 'cv', classProbs = T, summaryFunction = prSummary)
# # cv_settings3 <- trainControl(method = 'cv', number = 10, classProbs = T, summaryFunction = prSummary)
# # 
# # # # fit a custom GLM model through the training data with hyperparameter tuning of threshold probability
# # # # source: https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/
# # # # source: https://www.rdocumentation.org/packages/traineR/versions/2.2.0/topics/train.glm
# # # # source: https://cran.r-project.org/web/packages/caret/vignettes/caret.html
# # # # source: https://stackoverflow.com/questions/38250440/error-in-na-fail-default-missing-values-in-object-but-no-missing-values
# # # # Note: 20 threshold values are tested in the parameter tuning
# # # # Note: Dist used as custom performance metric for tuning
# # # # package: caret
# # # glm_model_custom <- train(case_ ~ ndvi_mean_scaled + ndvi_sd_scaled + ndvi_rate_mean_scaled + ndvi_rate_sd_scaled,
# # #                           data = training_steps, method = custom_glm, family = binomial(link = 'logit'),
# # #                           metric = 'Dist', maximize = F, tuneLength = 20, trControl = cv_settings_custom, na.action = na.pass)
# # 
# # # package: caret
# # # source: https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/
# # glm_model3 <- train(case_ ~ ndvi_mean_scaled + ndvi_sd_scaled + ndvi_rate_mean_scaled + ndvi_rate_sd_scaled, method = 'glm',
# #                     family = binomial(link = 'logit'), data = training_steps3, trControl = cv_settings3, metric = 'AUC', na.action = na.pass)
# # 
# 
# 
# 
# glm_m <- glm(case_ ~ ndvi_mean_scaled + ndvi_sd_scaled + ndvi_rate_mean_scaled + ndvi_rate_sd_scaled, 
#                  family = binomial(link = 'logit'), data = step_dataset)
# glm_m
# 
# 
# 
# 
# 
# 
# 
# # predict likelihood of elephant occurrence on step
# #test_steps$prediction  <- predict(glm_model, newdata = test_steps, type = "response")
# training_steps$prediction <- predict( glm_m, newdata = training_steps, type = "response" )
# test_steps$prediction  <- predict( glm_m, newdata = test_steps , type = "response" )
# training_steps$predClass <- predict( glm_m, newdata = training_steps, type = "link" )
# test_steps$predClass  <- predict( glm_m, newdata = test_steps , type = "terms" )
# 
# # create confusion matrix of predicted values based on new threshold value
# # source: https://stackoverflow.com/questions/46028360/confusionmatrix-for-logistic-regression-in-r
# # source: https://stackoverflow.com/questions/39803667/r-create-factor-based-on-condition
# confusionMatrix(data = factor(ifelse(test_steps$prediction > 0.5, 'presence', 'absence')),
#                 reference = test_steps$case_, positive = 'presence')
# 
# confusionMatrix(data = factor(ifelse(test_steps$prediction > 0.06, 'presence', 'absence')),
#                 reference = test_steps$case_, positive = 'presence')
# 
# cm <- confusionMatrix(data = test_steps$prediction, reference = test_steps$case_, positive = 'presence')
# cm$byClass
# # 
# # training_steps <- 
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
# cp <- cutpointr(test_steps, prediction, case_, method = maximize_metric, metric = sens_constrain, pos_class = 'presence', direction = '>=')
# summary(cp)
# cp$F1_score
# cp$AUC
# plot(cp)
# plot_metric(cp)
