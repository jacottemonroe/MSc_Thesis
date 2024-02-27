## MSc Thesis 
## Jacotte Monroe 
## Functions SSF script 

# Function that takes the elephant step dataset with its covariates and fits the step-selection function to it. 
# 
# Input: input dataset repository, elephant ID and week of interest, output directory.
# 
# Output: printed information about dataset and summary of fitted model, model saved as RDS.



fitSSFModel <- function(input_repository = 'output/elephant_etosha/', ID, week, output_directory = 'output/ssf_models/'){
  
  # read step dataset
  step_dataset <- read.csv(paste0(input_repository, ID, '_', as.character(week), '_step_dataset.csv'))
  
  # select only rows with NA
  step_dataset_NA <- step_dataset[!complete.cases(step_dataset),]
  
  # print information about the dataset 
  print(paste('Dataset elephant:', ID, '\n', 
              'Week:', as.character(week), '\n', 
              'Number of total steps:', as.character(nrow(step_dataset)), '\n', 
              'Number of observed steps:', as.character(length(unique(step_dataset$step_id_))), '\n', 
              'Number of steps with NA:', as.character(length(unique(step_dataset_NA$step_id_))), '\n', 
              'Number of observed steps with NA:', as.character(sum(step_dataset_NA$case_ == T))))
  
  # fit SSF model 
  ssf_model <- fit_clogit(step_dataset, case_ ~ ndvi_10 + ndvi_50 + ndvi_90 + ndvi_sd + 
                            ndvi_rate_10 + ndvi_rate_50 + ndvi_rate_90 + ndvi_rate_sd + strata(step_id_))
  
  # print model summary
  print(summary(ssf_model))
  
  # save model as RDS 
  saveRDS(ssf_model, file = paste0(output_directory, ID, '_', as.character(week), '_ssf_model.rds'))
  
}