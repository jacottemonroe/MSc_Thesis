## MSc Thesis 
## Jacotte Monroe 
## Downscaling Script

## Functions that take the dataset and sampling points and fit different regressions (linear, cubist, or random forest). 
## There is an option to run the model with all predictors or to set certain predictors to run. 
## It is also possible to run a forward feature selection to select the optimal model. 
## Each model is trained on the sampled points and validated through a 9-fold cross-validation. 
## The full image is then predicted and the R², RMSE, and MAE are calculated. 
## Necessary packages: caret, car, terra, ranger
## Inputs: The filepath to the correct directory, elephant ID, week of interest, date of interest, 
##          any suffix to add after input dataset name, optional subset of predictors to include as covariates (as vector of string elements), 
##          if there should be feature selection (boolean), the type of regression model to fit (lm, cubist, or rf), the output directory to store outputs
## Outputs: 
##      3_f1: the trained model (as RDS)
##      3_f2: the model results/calculated metrics based on cross-validation (MAE, MSE, RMSE, R2) (as csv)
##      3_f3: the trained model coefficients (as csv)
##      3_f4: the variable importances for the trained model (as csv)
##      3_f5: the VIF values for each predictor from the trained model (as csv) 
##      3_f6: the predicted raster when running the full image through the fitted model (as tif)
##      3_f7: the calculated metrics when comparing the predicted image to the original dataset (as csv)
##      3_f8: the error and absolute error between the predicted image and original dataset (as png)


fitRegression <- function(input_filepath, ID, week, modis_date, input_dataset_suffix = '', 
                                subset_predictors = NULL, feature_selection = F, regression_type = 'lm', output_directory = 'data/'){
  
  # define output filepath 
  output_filepath <- paste0(output_directory, ID, '/', week, '/')
  
  # create output filepath if it does not yet exist
  if(!dir.exists(output_filepath)){dir.create(output_filepath, recursive = T)}
  
  ## MODEL TRAINING
  
  # load sampled points and the cross-validation folds 
  sample_points <- readRDS(paste0(input_filepath, '3_e1_', modis_date, '_trainingPoints.RDS'))
  cv_folds <- readRDS(paste0(input_filepath, '3_e2_', modis_date, '_CVFolds.RDS'))
  
  # retrieve covariates from sample points and store in a new dataframe
  covs <- data.frame(sample_points)
  if(!is.null(subset_predictors)){
    covs <- covs[, subset_predictors]
  }else{
    covs <- covs[, grep('B.*', colnames(covs))]
  }
  
  # check which grid settings to specify depending on the model name 
  if(regression_type == 'lm'){
    grid_setting <- expand.grid(intercept = T)
    
  }else if(regression_type == 'cubist'){
    # source: https://pierreroudier.github.io/teaching/20171014-DSM-Masterclass-Hamilton/2017-10-09-dsm-with-covariates.html
    grid_setting <- expand.grid(committees = 1, neighbors = 0)
    
  }else if(regression_type == 'ranger'){
    # set ranger parameters to default apart from mtry 
    # source: https://cran.r-project.org/web/packages/ranger/ranger.pdf
    # source: https://stackoverflow.com/questions/48334929/r-using-ranger-with-caret-tunegrid-argument
    grid_setting <- expand.grid(mtry = 1:28, splitrule = 'variance', min.node.size = 5)
    
  }else{print('Specified regression model type is not valid for this function. Try: lm, cubist, or ranger')}
  
  # train the regression model on the sampling points 
  if(feature_selection == F){
    # fit model with full range of specified predictors (no feature selection)
    # source: https://www.statology.org/k-fold-cross-validation-in-r/
    # source: https://cran.r-hub.io/web/packages/CAST/vignettes/CAST-intro.html
    if(regression_type == 'ranger'){
      model <- train(x = covs, y = sample_points$ndvi, method = regression_type, 
                     trControl = trainControl(method = 'cv', index = cv_folds$index), 
                     tuneGrid = grid_setting, num.trees = 100, importance = 'permutation')
      
      #final_model <- model$finalModel
      
    }else{
      model <- train(x = covs, y = sample_points$ndvi, method = regression_type, 
                     trControl = trainControl(method = 'cv', index = cv_folds$index), 
                     tuneGrid = grid_setting)
      
      #final_model <- model$finalModel$coefficients
    }
    
    model_type <- 'full'
    
  }else{
    if(regression_type == 'ranger'){
      # fit model with forward feature selection
      # source: https://cran.r-hub.io/web/packages/CAST/vignettes/CAST-intro.html
      model <- ffs(predictors = covs, response = sample_points$ndvi, method = regression_type, 
                     trControl = trainControl(method = 'cv', index = cv_folds$index), 
                     tuneGrid = grid_setting, num.trees = 100, importance = 'permutation')
      
      #final_model <- model$finalModel
      
    }else{
      model <- ffs(predictors = covs, response = sample_points$ndvi, method = regression_type, 
                     trControl = trainControl(method = 'cv', index = cv_folds$index), 
                     tuneGrid = grid_setting)
      
      #final_model <- model$finalModel$coefficients
    }
    
    model_type <- 'ffs'

  }
  
  # store and save results
  print(model)
  saveRDS(model, paste0(output_filepath, '3_f1_', modis_date, '_', regression_type, '_', model_type, '_model.RDS'))
  write.csv(data.frame(model$results), paste0(output_filepath, '3_f2_', modis_date, '_', regression_type, '_', model_type, '_results.csv'))
  #write.csv(data.frame(final_model), paste0(output_filepath, '3_f3_', modis_date, '_', regression_type, '_', model_type, '_finalModel.csv'))
  write.csv(t(data.frame(varImp(model)$importance)), paste0(output_filepath, '3_f4_', modis_date, '_', regression_type, '_', model_type, '_importances.csv'))
  if(regression_type == 'lm'){
    write.csv(t(data.frame(VIF = sort(vif(model$finalModel)))), paste0(output_filepath, '3_f5_', modis_date, '_', regression_type, '_', model_type, '_vif.csv'))
  }

  ## MODEL TESTING 
  
  # load covariates/response dataset 
  dataset <- rast(paste0(input_filepath,'3_d1_', modis_date, '_dataset', input_dataset_suffix, '.tif'))
  
  # retrieve the dataset covariates to match those used in training the model
  dataset_covs <- dataset[[names(dataset) %in% colnames(covs)]]
  
  # predict the full MODIS NDVI image using the trained linear regression model 
  dataset_predicted <- predict(dataset_covs, model)
  names(dataset_predicted) <- 'ndvi_pred'
  
  # calculate metrics and store in dataframe
  num_obs <- ncol(dataset_predicted) * nrow(dataset_predicted)
  
  # source: https://gis.stackexchange.com/questions/4802/rmse-between-two-rasters-step-by-step
  error <- dataset_predicted$ndvi_pred - dataset$ndvi
  
  # source: https://rdrr.io/cran/terra/man/global.html#google_vignette
  mse <- global(error**2, 'sum', na.rm = T)[[1]]/num_obs
  rmse <- sqrt(mse)
  mae <- global(abs(error), 'sum', na.rm = T)[[1]]/num_obs
  
  # source: https://stackoverflow.com/questions/63335671/correct-way-of-determining-r2-between-two-rasters-in-r#:~:text=R2%20%3D%20r%20*%20r.,of%202%20to%20get%20R2.
  r2 <- cor(values(dataset_predicted$ndvi_pred), values(dataset$ndvi), use="complete.obs", method = 'pearson')[[1]]
  
  results <- data.frame(MSE = mse, RMSE = rmse, MAE = mae, R2 = r2)
  errors <- data.frame(summary(error))[,3]
  abs_errors <- data.frame(summary(abs(error)))[,3]

  # source: https://stackoverflow.com/questions/41056639/r-get-value-from-summary
  summary_errors <- data.frame('Label' = sub(':.*', '', errors), 'Error' = as.numeric(sub('.*:', '', errors)), 
                               'Abs(Error)' = as.numeric(sub('.*:', '', abs_errors)))
  
  # save outputs 
  writeRaster(dataset_predicted, paste0(output_filepath, '3_f6_', modis_date, '_', regression_type, '_', model_type, '_predNDVI_250m.tif'))
  write.csv(results, paste0(output_filepath, '3_f7_', modis_date, '_', regression_type, '_', model_type, '_predNDVI_250m_results.csv'))
  write.csv(summary_errors, paste0(output_filepath, '3_f8_', modis_date, '_', regression_type, '_', model_type, '_predNDVI_250m_error_summary.csv'))
  png(paste0(output_filepath, '3_f9_', modis_date, '_', regression_type, '_', model_type, '_predNDVI_250m_errors.png'))
  par(mfrow = c(3, 1))
  plot(error)
  plot(error < 0)
  plot(abs(error))
  dev.off()
}
