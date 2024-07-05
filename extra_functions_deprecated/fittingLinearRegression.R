## MSc Thesis 
## Jacotte Monroe 

## Fit (train) Linear Regression Model through training dataset
## Input: training set, boolean to indicate if wish to run stepwise AIC or take full model
## Ouput: generated model(s)

fitLinearRegression <- function(training_set, full_model = F, stepwise_AIC = T){
  lr <- lm(ndvi_modis ~ ., data = training_set)
  
  if(full_model == T & stepwise_AIC == T){
    # run stepwise AIC to see which model is best 
    # source: https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/step
    best_model <- step(lr)
    
    # check assumptions 
    # source: # source: http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/
    par(mfrow = c(2, 2))
    plot(lr)
    plot(best_model)
    
    return(list(full_model = lr, best_model = best_model))
    
  }else if(full_model == F & stepwise_AIC == T){
    # run stepwise AIC to see which model is best 
    # source: https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/step
    best_model <- step(lr)
    
    # check assumptions 
    # source: # source: http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/
    par(mfrow = c(2, 2))
    plot(best_model)
    return(best_model)
  }
  else{
    par(mfrow = c(2, 2))
    plot(lr)
    return(lr)}
}