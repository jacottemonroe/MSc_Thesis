## MSc Thesis 
## Jacotte Monroe 

## Script of functions used to build a sutom GLM model
## The idea is to make a GLM model that tunes the threshold probability to handle imbalanced data
## This step is done during cross-validation 

## Step by step code used as reference: http://rstudio-pubs-static.s3.amazonaws.com/145252_b241fc4c9cc640a185e721694648ad31.html
## The reference script modified the settings of the default rf model to incorporate threshold probability tuning in cross-validation. 
## Taking the online tutorial and the glm documentation (getModelInfo) as a guideline, I did the same but for the glm model.
## The following functions were modified from the original glm model:
##    model$type
##    model$parameters
##    model$grid
##    model$predict
## Output: Custom GLM model with modified structure 

## Second function creates a custom metric for tuning the (newly included) threshold probability parameter of the custom GLM model.
## Input: Standard structure of input for summaryFunction fed to trControl function of caret package.
## Output: Desired performance metrics to tune and display in custom model


buildCustomGLM <- function(){
  
  # get GLM model information
  model <- getModelInfo("glm", regex = FALSE)[[1]]
  
  # specify the model type as classification 
  model$type <- c('Classification')
  
  # add threshold as tuning parameter 
  model$parameters <- data.frame(parameter = c('threshold'), class = c('numeric'), label = c('Probability Cutoff'))
  
  # set tuning grid
  model$grid <- function(x, y, len = NULL, search = 'grid'){
    if(search == 'grid'){
      grid <- expand.grid(threshold = seq(0.01, 0.99, length = len))
    } else{
      grid <- expand.grid(threshold = runif(1, 0, size = len))
    }
    grid
  }
  
  # modify the predict function to test custom threshold
  model$predict <- function(modelFit, newdata, submodels = NULL){
    if (!is.data.frame(newdata)) {
      newdata <- as.data.frame(newdata, stringsAsFactors = TRUE) 
    }
    
    if (modelFit$problemType == "Classification") {
      probs <- predict(modelFit, newdata, type = "response")
      out <- ifelse(probs >= modelFit$tuneValue$threshold, 
                    modelFit$obsLevel[1], modelFit$obsLevel[2])
    }
    else {
      out <- predict(modelFit, newdata, type = "response")
    }
    out
  }
  
  return(model)
}



# build custom summaryFunction for trControl
# still based on tutorial: http://rstudio-pubs-static.s3.amazonaws.com/145252_b241fc4c9cc640a185e721694648ad31.html
# Note: twoClassSummary automatically returns ROC, sensitivity, and specificity metrics
# Note: Dist = distance of specificity and sensitivity metrics to optimal performance (=1), the smaller the distance the better the model
# source: https://cran.r-project.org/web/packages/caret/vignettes/caret.html
performanceFunction <- function(data, lev = levels(data$obs), model = NULL){
  out <- c(twoClassSummary(data, lev = levels(data$obs), model = NULL))
  #out2 <- c(prSummary(data, lev = levels(data$obs), model = NULL))
  coords <- matrix(c(1, 1, out["Spec"], out["Sens"]), ncol = 2, byrow = T)
  colnames(coords) <- c('Spec', 'Sens')
  rownames(coords) <- c('Best', 'Current')
  c(out, Dist = dist(coords)[1])
}
