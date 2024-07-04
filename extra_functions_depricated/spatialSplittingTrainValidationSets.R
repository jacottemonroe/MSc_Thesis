## MSc Thesis 
## Jacotte Monroe 

## Spatially splits the dataset into training and validation sets 

spatialSplitTrainValidationSets <- function(dataset, training_size = 0.7){
  
  # create new extents for spatial split 
  # source: https://stackoverflow.com/questions/75278218/how-to-generate-a-spatvector-from-spatextent-in-r-terra
  d_ext_train <- ext(dataset)
  d_ext_train[3] <- d_ext_train[4] - ((d_ext_train[4] - d_ext_train[3])*training_size)
  
  d_ext_test <- ext(dataset)
  d_ext_test[4] <- d_ext_test[3] + (d_ext_test[4] - d_ext_test[3])*(1-training_size)
  
  # create training and validation spatrasters
  # source: https://rspatial.github.io/terra/reference/crop.html
  d_train <- crop(dataset, d_ext_train)
  
  d_test <- crop(dataset, d_ext_test)
  
  return(list(trainingset = d_train, validationset = d_test))
}