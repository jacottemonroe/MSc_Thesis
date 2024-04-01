## MSc_Thesis
## Jacotte Monroe 
## 19/03/24

## R script that runs function to fit statistical models on the movement steps dataset.
## Performs plots the correlation matrix of predictors for a manual multicolinearity check. 
## Fits general and conditional logistic regressions onto the data. 
## Runs the models with all the predictors and with only 50th percentiles and standard deviations as predictors. 
## For each model, the Variance Inflation Factors (VIF) is calculated and the model outputs (coefficients, deviance, concordance, tests) are saved. 
## Conditional logistic regression outputs stat tests: Likelihood ratio, Wald, score tests. 
## Script outputs: 
##    6_a0: Correlation matrix of predictors from step dataset, saved as png. (final output)
##    6_a1: Conditional logistic regression model (CLR) coefficients after fitting model with all predictors, saved as csv. (final output)
##    6_a2: CLR stat tests and concordance results after fitting model with all predictors, saved as csv. (final output)
##    6_a3: General logistic regression model (GLM) coefficients after fitting model with all predictors, saved as csv. (final output)
##    6_a4: GLM deviances after fitting model with all predictors, saved as csv. (final output)
##    6_a5: GLM VIF values for each predictor after fitting model with all predictors, saved as csv. (final output)
##
##    6_b1: Conditional logistic regression model (CLR) coefficients after fitting model with subset of predictors, saved as csv. (final output)
##    6_b2: CLR stat tests and concordance results after fitting model with subset of predictors, saved as csv. (final output)
##    6_b3: General logistic regression model (GLM) coefficients after fitting model with subset of predictors, saved as csv. (final output)
##    6_b4: GLM deviances after fitting model with subset of predictors, saved as csv. (final output)
##    6_b5: GLM VIF values for each predictor after fitting model with subset of predictors, saved as csv. (final output)



###########
## Read run settings 
###########

run_settings <- read.csv('data/single_run_settings.csv', header = F, row.names = 1)

# define elephant ID
ID <- run_settings[[1]]

# define week to test 
week <- run_settings[[2]]

# define pseudo-absence path generator method --> Choices are 1) random_path_custom_distr 2) random_path_buffer_point 3) random_step
pseudo_abs_method <- run_settings[[3]]

# set downscaling parameter
downscaling_setting <- run_settings[[4]]

# define run filepath 
run_filepath <- paste0('data/', ID, '/', week, '/')



###########
## Fit models on dataset with all predictors, plot correlation matrix, and save model outputs
###########
# load function
source('functions_elephant_ssf/6_a_fittingModels.R')

# load necessary packages 
if(!('dplyr') %in% installed.packages()){install.packages('dplyr')}
library(dplyr) 
if(!('ggplot2') %in% installed.packages()){install.packages('ggplot2')}
library(ggplot2) # needed for loading GGally package
if(!('GGally') %in% installed.packages()){install.packages('GGally')}
library(GGally)
if(!('amt') %in% installed.packages()){install.packages('amt')}
library(amt)
if(!('car') %in% installed.packages()){install.packages('car')}
library(car) # needed to run vif()

# run function
fitSSFModel(run_filepath, ID, week, pseudo_abs_method, downscaling = downscaling_setting, multicolinearity_check = T, full = T, 'output/')



###########
## Fit models on dataset with subset of predictors and save model outputs
###########
# run function
fitSSFModel(run_filepath, ID, week, pseudo_abs_method, downscaling = downscaling_setting, multicolinearity_check = F, full = F, 'output/')

print(paste('(DONE) Fitting models for elephant', ID, 'of week', week))
print(paste('(COMPLETE) Elephant', ID, 'has been successfully processed for week', week))