## MSc Thesis 
## Jacotte Monroe 

## Fit Cubist Regression through training set 
## Input: trainingset, boolean for which package to use (Cubist or Caret)
## Ouput: the trained cubist regression model 


fitCubistRegression <- function(training_set, with_caret_package = F){
  
  # turn training set into data frame and separate covariates and response variable
  d_train_cov <- data.frame(training_set['B.'])
  d_train_resp <- data.frame(training_set['ndvi_modis'])
  
  # fit cubist regression 
  if(with_caret_package == T){
    
    if(!('caret') %in% installed.packages()){install.packages('caret')} # ALTERNATIVE to fit cubist regression 
    library(caret)
    
    # Caret package has more parameters to tweak?
    # source: https://pierreroudier.github.io/teaching/20171014-DSM-Masterclass-Hamilton/2017-10-09-dsm-with-covariates.html
    grid <- expand.grid(committees = 1, neighbors = 0)
    
    model <- train(d_train_cov, d_train_resp$ndvi_modis, method = 'cubist', 
                   trControl = trainControl(method = 'none'), tuneGrid = grid) # using Caret package
  }else{
    
    if(!('lattice') %in% installed.packages()){install.packages('lattice')} # required for Cubist package
    library(lattice)
    
    if(!('Cubist') %in% installed.packages()){install.packages('Cubist')} # to fit cubist regression 
    library(Cubist)
    
    # source: # source: https://koalatea.io/r-cubist-regression/
    model <- cubist(d_train_cov, d_train_resp$ndvi_modis) # using Cubist package 
  }
  
  return(model)
}