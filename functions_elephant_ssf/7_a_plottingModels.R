## MSc_Thesis
## Jacotte Monroe 

## Function that plots the elephant movement path 
## Necessary packages: sjPlot (coef plot), 
# NOTE FOR README: this requires installation of SjPlot package --> depending on R version that have might have issues installing 
# Details: sjPlot package requires sjstats package which requires emmeans package which requires estimability package which requires new R version
# The packages will not install if have estimability v1.5 and now R v4.3.0 
# Solution: install estimability v1.4.1 instead of the latest --> 'https://cran.r-project.org/src/contrib/Archive/estimability/estimability_1.4.1.tar.gz'
# source: https://stackoverflow.com/questions/68172470/i-want-to-install-old-version-package-in-r
# Once older version installed, the rest should be able to install fine like a chain 




plotModels <- function(directory = 'output/', ID, week, random_data_method = 'random_path_custom_distr', downscaling = 'NULL', full = F){
  
  if(downscaling == 'NULL'){
    suffix <- ''
    modis_label <- ''
    
  }else if(downscaling == T){
    suffix <- '_downscaling_modis_30m'
    modis_label <- 'MODIS 30m'
    
  }else if(downscaling == F){
    suffix <- '_downscaling_modis_250m'
    modis_label <- 'MODIS 250m'
    
  }else{stop('Incorrect term set for downscaling parameter. Should be one of the following: NULL, T, F.')}
  
  if(full == T){
    letter <- 'a'
    model_type <- 'full'
  }else{
    letter <- 'b'
    model_type <- '50p_sd'}
  
  # define filepath 
  filepath <- paste0(directory, ID, '/', week, '/')
  
  # retrieve fitted model 
  model <- readRDS(paste0(filepath, '6_', letter, '0_glm_', model_type, '_model_', random_data_method, suffix, '.RDS'))
  
  # plot model odds ratios and log odds 
  # source: https://strengejacke.github.io/sjPlot/articles/plot_model_estimates.html
  plot_title <- paste('Elephant', ID, 'week', week, modis_label)
  logodds_plot <- plot_model(model, transform = NULL, show.values = T, vline.color = 'red', title = plot_title)
  save_plot(paste0(filepath, '7_', letter, '1_', model_type, '_plot_log_odds_', random_data_method, suffix, '.png'), 
            logodds_plot)
  
  plot_odd_ratio <- function(model, plot_title, filepath, letter, model_type, random_data_method, suffix){
    logratios_plot <- plot_model(model, show.values = T, vline.color = 'red', title = plot_title)
    save_plot(paste0(filepath, '7_', letter, '2_', model_type, '_plot_odd_ratios_', random_data_method, suffix, '.png'), 
              logratios_plot) 
  }
  
  try(plot_odd_ratio(model, plot_title, filepath, letter, model_type, random_data_method, suffix))
  
  # plot curve 
  df <- data.frame(predicted = predict(model, type = 'response'), 
                   residuals = residuals(model, type = 'response'), 
                   linearized = model$linear.predictors, 
                   weights = model$weights)
  
  #define a function to plot the model
  # source: https://stackoverflow.com/questions/55418724/how-to-plot-the-s-curve-for-my-logistic-regression-model
  gra.tot <- function(dat, varLin, varY, weights, fitModel, devModel, dfModel, devNull, dfNull, aicModel, nameX, nameY) {
    ggplot(dat, aes(x = varLin, y = varY)) +
      geom_point(aes(size=weights), color = 'blue3', alpha=.3) +
      geom_text(x= -Inf, y=Inf, hjust=0, label=paste( 'predicted ~ linearized', '\nres. deviance: ', round(devModel,2) , 
                                                      ' (df: ', round(dfModel,2),')',  '\nnull deviance: ', round(devNull,2), 
                                                      ' (df: ', round(dfNull,2),')', '\nAIC: ', round(aicModel,2) ), col='grey40', size=3, fontface='italic') +
      xlab(as.character(nameX)) +
      ylab(as.character(nameY)) +
      theme_bw()
  }
  
  #plot the model
  tot <- gra.tot(df, df$linearized, df$predicted, df$weights, model$fitted, model$deviance, model$df.residual, model$null.deviance, model$df.null, model$aic, 'linearized_predictors', 'logit probability')
  png(paste0(filepath, '7_', letter, '3_', model_type, '_plot_curve_', random_data_method, suffix, '.png'))
  print(tot)
  dev.off()
  
}
