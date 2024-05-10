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
  set.seed(152)
  partition <- unname(createDataPartition(pair_id_list, p=.7, list=F, times=1))
  
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






