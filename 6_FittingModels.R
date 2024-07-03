## MSc_Thesis
## Jacotte Monroe 
## 19/03/24

## R script that runs function to fit statistical models on the movement steps dataset.
## Following a k-fold cross-validation scheme, trains a generalized linear model (GLM or unconditional logistic regression). 
## (Optional) Can also fit a conditional logistic regression onto the data (without cross-validation). 
## The mean and standard deviation NDVI and NDVI change rate are used as the four predictors.
## For each model, the Variance Inflation Factors (VIF) is calculated and the model outputs (coefficients, deviance, concordance, tests) are saved. 
## Script outputs: 
##    6_a1: GLM estimated coefficients, saved as csv.
##    6_a2: GLM resulting AIC and deviance metrics, saved as csv.
##    6_a3: GLM VIF values for each predictor after fitting model, saved as csv.
##    6_a4: GLM performance metrics after model validation, including AUC, F1_score, precision, recall, specificity, 
##            the cutoff probability value (after tunning), and more... Saved as csv.
##    6_b0: Full Conditional Logistic Regression (CLR) model, saved as RDS. 
##    6_b1: CLR estimated coefficients, saved as csv. 
##    6_b2: CLR concordance and other statistical tests after fitting model, saved as csv. 




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

# replace NA from suffix columns of run settings to empty strings
if(is.na(run_settings[[6]])){run_settings[[6]] <- ''}
if(is.na(run_settings[[7]])){run_settings[[7]] <- ''}

# define input and output suffixes
input_suffix <- run_settings[[6]]
output_suffix <- run_settings[[7]]

print(paste('Now fitting models for elephant', ID, 'of week', week, '...'))



###########
## Fit models on dataset with all predictors, plot correlation matrix, and save model outputs
###########
# load function
source('functions/6_a_fittingModels.R')

# load necessary packages 
if(!('dplyr') %in% installed.packages()){install.packages('dplyr')}
library(dplyr) # group data
if(!('caret') %in% installed.packages()){install.packages('caret')}
library(caret) # GLM modeling and cross-validation
if(!('car') %in% installed.packages()){install.packages('car')}
library(car) # needed to run vif()
if(!('cutpointr') %in% installed.packages()){install.packages('cutpointr')}
library(cutpointr) # for hyperparameter tunning on test set 

# specific for CLR
# if(!('amt') %in% installed.packages()){install.packages('amt')}
# library(amt) # CLR modeling


# # run function to fit GLM model
fitGLMModel(run_filepath, ID, week, pseudo_abs_method, downscaling = downscaling_setting, 
                 input_suffix = input_suffix, output_suffix = output_suffix)

# run function fit CLR model
# fitCLRModel(run_filepath, ID, week, pseudo_abs_method, downscaling = downscaling_setting, input_suffix = input_suffix, 
#             output_suffix = output_suffix)

print(paste('(DONE) Fitting models for elephant', ID, 'of week', week))
print(paste('(COMPLETE) Elephant', ID, 'has been successfully processed for week', week))



