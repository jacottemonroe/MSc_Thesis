## MSc Thesis 
## Jacotte Monroe 
## Functions SSF script 

# Function that takes the elephant step dataset with its covariates and fits the step-selection function to it. 
# 
# Input: input dataset repository, elephant ID and week of interest, output directory.
# 
# Output: printed information about dataset and summary of fitted model, model saved as RDS.



fitSSFModel <- function(input_directory = 'data/', ID, week, random_data_method = 'random_path_custom_distr', 
                        multicolinearity_check = F, full = T, output_directory = 'output/'){
  
  # create output filepath 
  output_filepath <- paste0(output_directory, ID, '/', week, '/')
  
  # create data directory if it does not yet exist
  if(!dir.exists(output_filepath)){dir.create(output_filepath, recursive = T)}
  
  # read step dataset
  step_dataset <- read.csv(paste0(input_directory, '4_a1_cov_resp_dataset_', random_data_method, '.csv'))
  
  # generate correlation matrix 
  if(multicolinearity_check == T){

    # select columns with predictors only 
    # source: https://stackoverflow.com/questions/25923392/select-columns-based-on-string-match-dplyrselect
    covariates <- step_dataset %>% select(contains(c('case_', 'ndvi')))
    
    # save matrix as png
    png(filename = paste0(output_filepath, '6_a1_correlation_matrix_', random_data_method, '.png'), width = 850, height = 350)
    
    # source: # source: https://r-charts.com/correlation/ggpairs/?utm_content=cmp-true
    correlation_matrix <- ggpairs(covariates, columns = 2:ncol(covariates), aes(color = as.factor(case_), alpha = 0.5))    
    
    # add print statement for graph to save as png inside a function 
    # source: https://stackoverflow.com/questions/9206110/using-png-function-not-working-when-called-within-a-function
    print(correlation_matrix)
    dev.off()
  }
  
  # fit conditional logistic regression and general logistic regression models 
  if(full == T){
    output_number = '6_a'
    model_type = '_full_'
    
    # source: https://cran.r-project.org/web/packages/amt/vignettes/p4_SSF.html
    clr_model <- fit_clogit(step_dataset, case_ ~ ndvi_10 + ndvi_50 + ndvi_90 + ndvi_sd + 
                              ndvi_rate_10 + ndvi_rate_50 + ndvi_rate_90 + ndvi_rate_sd + strata(step_id_))
    
    # source: https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/
    glm_model <- glm(case_ ~ ndvi_10 + ndvi_50 + ndvi_90 + ndvi_sd + ndvi_rate_10 + ndvi_rate_50 +
                       ndvi_rate_90 + ndvi_rate_sd, family = binomial(link = 'logit'), data = step_dataset)
    
  }else{
    output_number = '6_b'
    model_type = '_50p_sd_'
    
    # source: https://cran.r-project.org/web/packages/amt/vignettes/p4_SSF.html
    clr_model <- fit_clogit(step_dataset, case_ ~ ndvi_50 + ndvi_sd + 
                              ndvi_rate_50 + ndvi_rate_sd + strata(step_id_))
    
    # source: https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/
    glm_model <- glm(case_ ~ ndvi_50 + ndvi_sd + ndvi_rate_50 + ndvi_rate_sd, 
                     family = binomial(link = 'logit'), data = step_dataset)
  }
  
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
  write.csv(clr_coef, paste0(output_filepath, output_number, '1_clr', model_type, 'coefs_', random_data_method, '.csv'))
  write.csv(clr_tests, paste0(output_filepath, output_number, '2_clr', model_type, 'tests_', random_data_method, '.csv'))
  
  write.csv(glm_coef, paste0(output_filepath, output_number, '3_glm', model_type, 'coefs_', random_data_method, '.csv'))
  write.csv(glm_deviances, paste0(output_filepath, output_number, '4_glm', model_type, 'deviances_', random_data_method, '.csv'))
  write.csv(glm_vif, paste0(output_filepath, output_number, '5_glm', model_type, 'vif_', random_data_method, '.csv'))
}