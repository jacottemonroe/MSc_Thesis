## MSc Thesis 
## Jacotte Monroe 
## Functions SSF script 

# Function that takes the elephant step dataset with its covariates and fits the step-selection function to it. 
# 
# Input: input dataset repository, elephant ID and week of interest, output directory.
# 
# Output: printed information about dataset and summary of fitted model, model saved as RDS.



fitSSFModel <- function(input_repository = 'output/elephant_etosha/', ID, week, full = T, output_directory = 'output/ssf_models/'){
  
  # read step dataset
  step_dataset <- read.csv(paste0(input_repository, ID, '_', as.character(week), '_step_dataset.csv'))
  
  # select only rows with NA
  step_dataset_NA <- step_dataset[!complete.cases(step_dataset),]
  
  # fit SSF model 
  if(full == T){
    model_type = 'clr_full_'
    
    ssf_model <- fit_clogit(step_dataset, case_ ~ ndvi_10 + ndvi_50 + ndvi_90 + ndvi_sd + 
                              ndvi_rate_10 + ndvi_rate_50 + ndvi_rate_90 + ndvi_rate_sd + strata(step_id_))
    
  }else{
    model_type = 'clr_50p_sd_'
    
    ssf_model <- fit_clogit(step_dataset, case_ ~ ndvi_50 + ndvi_sd + 
                              ndvi_rate_50 + ndvi_rate_sd + strata(step_id_))
  }
  
  # get model summary
  model_summary <- summary(ssf_model)
  
  # print information about the dataset and the model
  print(paste('Dataset elephant:', ID))
  print(paste('Week:', as.character(week)))
  print(paste('Number of total observations:', as.character(nrow(step_dataset))))
  print(paste('Number of observations included in model:', model_summary$n))
  print(paste('Sets of steps:', model_summary$nevent))
  print(model_summary)
  
  # get model coefficients 
  # source: https://stackoverflow.com/questions/61482594/export-coxph-summary-from-r-to-csv
  model_coef <- data.frame(model_summary$coefficients)
  
  # create dataframe with statistical test results, the concordance and its standard error 
  model_tests <- data.frame(log_likelihood = model_summary$logtest, score = model_summary$sctest, wald = model_summary$waldtest)
  model_tests <- rbind(model_tests, SE_concordance = c(NA))
  model_tests <- cbind(model_tests, concordance = c(model_summary$concordance[1], NA, NA, model_summary$concordance[2]))
  
  # create output filepath 
  output_filepath <- paste0(output_directory, ID, '/', week, '/')
  
  # create data directory if it does not yet exist
  if(!dir.exists(output_filepath)){dir.create(output_filepath, recursive = T)}
  
  # save model results as csv 
  write.csv(model_coef, paste0(output_filepath, paste0(model_type, 'coefs.csv')))
  write.csv(model_tests, paste0(output_filepath, paste0(model_type, 'tests.csv')))
  
  print(paste('Done processing elephant', ID, 'week', week))
}