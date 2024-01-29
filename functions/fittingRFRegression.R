## MSc Thesis 
## Jacotte Monroe 

## Fit Random Forest Regression using the trainingset 
## Input: trainingset, boolean to specify if want variable importance or not 
## Output: trained RF model (and optionally list of variables ordered by importance)

# packages 
if(!('ranger') %in% installed.packages()){install.packages('ranger')} # fit Random Forest regression 
library(ranger)



fitRandomForestRegression <- function(training_set, importance = "permutation"){
  
  # turn training set into data frame and separate covariates and response variable
  d_train_cov <- data.frame(training_set['B.'])
  d_train_resp <- data.frame(training_set['ndvi_modis'])
  
  # fit random forest regression 
  model <- ranger(x = d_train_cov, y = d_train_resp$ndvi_modis, num.trees = 100,
                  importance = importance, seed = 0xfedbeef)
  
  if(importance != 'none'){
    
    var_importance <- data.frame(cov = names(model$variable.importance), imp = model$variable.importance, row.names = NULL)
    
    # source: https://sparkbyexamples.com/r-programming/sort-data-frame-in-r/
    var_importance <- var_importance[order(var_importance$imp,decreasing=TRUE),]
    
    return(list(RF_model = model, variable_importance = var_importance))
    
  }else{
    
    return(model)
  }
}