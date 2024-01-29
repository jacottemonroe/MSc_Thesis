## MSc Thesis 
## Jacotte Monroe 

## Predict MODIS NDVI by running image through model
## Input: Spatraster (with covariates and response as layers), model
## Models: Linear regression, Cubist regression, Random Forest regression
## Output: Predicted NDVI spatraster, summary of absolute error 

if(!('ranger') %in% installed.packages()){install.packages('ranger')} # to predict with ranger model
library(ranger)

predictModelOnData <- function(test_data, model, validation = F){
  
  # subset covariates from validation set
  test_cov <- test_data['B.']
  
  # checks which model it is 
  model_type_check <- class(model)
  
  if(model_type_check == 'ranger'){
    # get model covariates 
    model_cov <- model$forest$independent.variable.names
    
    # subset test covariates to only the ones that the model has been trained on 
    test_cov_selected <- subset(test_cov, model_cov)
    
    # check that covariates are the same 
    if(any(names(test_cov_selected) != model_cov)){
      stop('Data should contain same covariates as those trained on the model. Check covariates!')
    }else{
      # predict using random forest model 
      predicted <- predict(test_cov_selected, model, fun = function(...) predict(...)$predictions, na.rm = T)
    }
    
  }else if(model_type_check == 'lm'){
    # get model covariates
    model_elements <- names(model$model)
    # source: https://stackoverflow.com/questions/69759984/how-can-i-subset-a-list-in-r-by-extracting-the-elements-that-contain-a-string
    model_cov <- model_elements[grep('B.', model_elements)]
    
    # subset test covariates to only the ones that the model has been trained on 
    test_cov_selected <- subset(test_cov, model_cov)
    
    # check that covariates are the same
    if(any(names(test_cov_selected) != model_cov)){
      stop('Data should contain same covariates as those trained on the model. Check covariates!')
    }else{
      # predict using linear regression model 
      predicted <- predict(test_cov_selected, model)
    }
    
  }else if(model_type_check == 'cubist'){
    # get model covariates 
    model_cov <- model$var$all
    
    # subset test covariates to only the ones that the model has been trained on 
    test_cov_selected <- subset(test_cov, model_cov)
    
    # check that covariates are the same 
    if(any(names(test_cov_selected) != model_cov)){
      stop('Data should contain same covariates as those trained on the model. Check covariates!')
    }else{
      # predict using linear regression model 
      predicted <- predict(test_cov_selected, model)
    }
    
  }else{
    stop('This function is not compatible with this type of model. Either change models or update function.')
  }
  
  # generate output visualizations 
  
  if(validation == T){
    par(mfrow = c(2,1))
    true_plot <- plot(test_data$ndvi_modis, main = 'MODIS True NDVI')
    true_plot
    pred_plot <- plot(predicted, main = 'MODIS Predicted NDVI')
    pred_plot
    
    # plot errors 
    error <- test_data$ndvi_modis - predicted
    error_plot <- plot(error, main = 'Error Between True and Predicted NDVI')
    error_plot
    
    abs_error <- abs(error)
    abs_error_plot <- plot(abs_error, main = 'Absolute Error Between True and Predicted NDVI')
    abs_error_plot 
    
    return(list(model_predicted_output = predicted, error_summary = summary(abs_error)))
  }else{
    par(mfrow = c(1,1))
    pred_plot <- plot(predicted, main = 'MODIS Predicted NDVI')
    pred_plot
    
    return(predicted)
  }
} 