## interpreting results 


# create empty summary results table 
summary_results <- data.frame()

# get table of runs 
run_settings <- read.csv('data/run_settings_RQ2.csv', row.names = 1)
run_settings <- run_settings[1:77,]
#run_settings <- run_settings[run_settings$ID == 'LA14' & run_settings$downscaling == F,]

suffix <- '' #'_downscaling_modis_250m'
# loop over each run 
for(i in 1:nrow(run_settings)){
  # retrieve the run
  run <- run_settings[i,]
  
  # define elephant ID
  ID <- run[[1]]
  
  # define week to test 
  week <- run[[2]]
  
  # define pseudo-absence path generator method --> Choices are 1) random_path_custom_distr 2) random_path_buffer_point 3) random_step
  pseudo_abs_method <- run[[3]]
  
  # define run filepath 
  run_filepath <- paste0('output/', ID, '/', week, '/')
  
  # create results data entry 
  entry <- data.frame(ID = ID, week = week, pseudo_abs_method = pseudo_abs_method, 
                      VIF_full = NA, full_glm_sig_coef = NA, full_glm_sig_coef_which = NA, full_glm_deviance = NA, full_glm_sig = NA, full_clr_sig_coef = NA, full_clr_sig_coef_which = NA, full_clr_concord = NA, full_clr_concord_se = NA, 
                      VIF_sub = NA, sub_glm_sig_coef = NA, sub_glm_sig_coef_which = NA, sub_glm_deviance = NA, sub_glm_sig = NA, sub_clr_sig_coef = NA, sub_clr_sig_coef_which = NA, sub_clr_concord = NA, sub_clr_concord_se = NA)
  
  
  ## are the VIF values of the model acceptable? 
  # retrieve the dataset
  #full_df <- read.csv(paste0(run_filepath, '6_a5_glm_full_vif_', pseudo_abs_method, '.csv'))
  sub_df <- read.csv(paste0(run_filepath, '6_b5_glm_50p_sd_vif_', pseudo_abs_method, suffix, '.csv'))
  
  # check if vif values below 5
  #if(any(full_df$vif_results <5)){entry$VIF_full <- T}else{entry$VIF_full <- F}
  if(any(sub_df$vif_results <5)){entry$VIF_sub <- T}else{entry$VIF_sub <- F}
  
  
  ## does the glm model have a significant coefficient? 
  # retrieve dataset
  #full_df <- read.csv(paste0(run_filepath, '6_a3_glm_full_coefs_', pseudo_abs_method, '.csv'))
  sub_df <- read.csv(paste0(run_filepath, '6_b3_glm_50p_sd_coefs_', pseudo_abs_method, suffix, '.csv'))
  
  # exclude intercept 
  #full_df <- full_df[full_df$X != '(Intercept)', ]
  sub_df <- sub_df[sub_df$X != '(Intercept)', ]
  
  # check if pvalues below 0.05
  #if(any(full_df$Pr...z.. <=0.05)){entry$full_glm_sig_coef <- T}else{entry$full_glm_sig_coef <- F}
  if(any(sub_df$Pr...z.. <=0.05)){entry$sub_glm_sig_coef <- T}else{entry$sub_glm_sig_coef <- F}
  
  # select predictors that are significant with 95% confidence 
  # source: https://sparkbyexamples.com/r-programming/r-merge-vector-to-string/
  #entry$full_glm_sig_coef_which <- paste(full_df$X[full_df$Pr...z.. < 0.05], collapse = '; ')
  entry$sub_glm_sig_coef_which <- paste(sub_df$X[sub_df$Pr...z.. < 0.05], collapse = '; ')

  ## how high is the deviance of the fitted model? is the model significant? 
  # retrieve dataset
  #full_df <- read.csv(paste0(run_filepath, '6_a4_glm_full_deviances_', pseudo_abs_method, '.csv'))
  sub_df <- read.csv(paste0(run_filepath, '6_b4_glm_50p_sd_deviances_', pseudo_abs_method, suffix, '.csv'))
  
  # select residual deviance 
  #entry$full_glm_deviance <- full_df$residual_deviance
  entry$sub_glm_deviance <- sub_df$residual_deviance
  
  # derive p-value of model from chi-square stat
  # source: https://stats.stackexchange.com/questions/340489/interpretation-of-deviance-in-logistic-model
  # source: https://www.r-bloggers.com/2022/05/calculate-the-p-value-from-chi-square-statistic-in-r/
  #full_pval <- pchisq(full_df$null_deviance - full_df$residual_deviance, full_df$null_df - full_df$residual_df, lower.tail = F)
  sub_pval <- pchisq(sub_df$null_deviance - sub_df$residual_deviance, sub_df$null_df - sub_df$residual_df, lower.tail = F)
  
  # check if pvalues below 0.05
  #if(full_pval <=0.05){entry$full_glm_sig <- T}else{entry$full_glm_sig <- F}
  if(sub_pval <=0.05){entry$sub_glm_sig <- T}else{entry$sub_glm_sig <- F}
  
  
  ## does the clr model have a significant coefficient? 
  # retrieve dataset
  #full_df <- read.csv(paste0(run_filepath, '6_a1_clr_full_coefs_', pseudo_abs_method, '.csv'))
  sub_df <- read.csv(paste0(run_filepath, '6_b1_clr_50p_sd_coefs_', pseudo_abs_method, suffix, '.csv'))
  
  # check if pvalues below 0.05
  #if(any(full_df$Pr...z.. <=0.05)){entry$full_clr_sig_coef <- T}else{entry$full_clr_sig_coef <- F}
  if(any(sub_df$Pr...z.. <=0.05)){entry$sub_clr_sig_coef <- T}else{entry$sub_clr_sig_coef <- F}
  
  # select predictors that are significant with 95% confidence 
  # source: https://sparkbyexamples.com/r-programming/r-merge-vector-to-string/
  #entry$full_clr_sig_coef_which <- paste(full_df$X[full_df$Pr...z.. < 0.05], collapse = '; ')
  entry$sub_clr_sig_coef_which <- paste(sub_df$X[sub_df$Pr...z.. < 0.05], collapse = '; ')

  ## what is the concordance? how much does it vary by? 
  # retrieve dataset
  #full_df <- read.csv(paste0(run_filepath, '6_a2_clr_full_tests_', pseudo_abs_method, '.csv'))
  sub_df <- read.csv(paste0(run_filepath, '6_b2_clr_50p_sd_tests_', pseudo_abs_method, suffix, '.csv'))
  
  # select concordance and SE
  # entry$full_clr_concord <- full_df$concordance[1]
  # entry$full_clr_concord_se <- full_df$concordance[4]
  entry$sub_clr_concord <- sub_df$concordance[1]
  entry$sub_clr_concord_se <- sub_df$concordance[4]
  
  # rbind the entry to the summary table 
  summary_results <- rbind(summary_results, entry)
}

# results for running 13 elephants on week 2075 with three methods 
write.csv(summary_results, 'output/summary_results_RQ2.csv')


##### FOR RQ2 TIMESERIES

# retrieves summary results for only sig models GLM 
srsig <- summary_results[summary_results$sub_glm_sig == T,] # these are the models i want to retrieve coefs for 

# get weeks of sig models 
sigw <- srsig$week

# get run table for only weeks that are sig 
rsig <- run_settings[run_settings$week %in% sigw,]

tsdf <- data.frame()

for(i in 1:nrow(rsig)){
  
  # get run settings
  ID <- rsig$ID[i]
  week <- rsig$week[i]
  method <- rsig$pseudo_abs_method[i]
  
  # define filepaths
  data_path <- paste0('data/', ID, '/', week, '/')
  output_path <- paste0('output/', ID, '/', week, '/')
  
  # retrieve date of week 
  dfile <- read.csv(paste0(data_path, '2_a1_step_extents_LUT_', method, '.csv'), row.names = 1)
  date <- dfile$start_date[1]
  
  # retrieve coefs glm 
  c <- read.csv(paste0(output_path, '6_b3_glm_50p_sd_coefs_random_path_custom_distr', suffix, '.csv'))
  c <- c[2:nrow(c),]
  
  # specify if sig or not with 90% confidence (so pvalue < 0.1)
  c$Pr...z..[as.numeric(c$Pr...z..) < 0.1] <- 'sig'
  c$Pr...z..[as.numeric(c$Pr...z..) >= 0.1] <- 'not sig'
  
  # create new entry with coefs and pvalues 
  # entry <- data.frame(ID = ID, week = week, method = method, date = date, ndvi_50 = c$Estimate[1], 
  #                     ndvi_sd = c$Estimate[2], ndvi_rate_50 = c$Estimate[3], ndvi_rate_sd = c$Estimate[4], 
  #                     pval.ndvi_50 = c$Pr...z..[1], pval.ndvi_sd = c$Pr...z..[2], pval.ndvi_rate_50 = c$Pr...z..[3], pval.ndvi_rate_sd = c$Pr...z..[4])
  entry <- data.frame(ID = ID, week = week, method = method, date = date, predictor = c[,1], value = c$Estimate, significance = c$Pr...z..)
  
  tsdf <- rbind(tsdf, entry)
}


#library(ggplot2)
ggplot(data = tsdf, aes(x = as.Date(date, tz = 'Africa/Maputo'), y = value, group = predictor, color = predictor)) + 
  geom_line() + geom_point(data = tsdf[tsdf$significance == 'sig',], aes(y = value), shape = 8) + 
  geom_hline(yintercept = 0) + 
  # source: https://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(vars(predictor), scale = 'free') + 
  xlab('Time') + ylab('Coefficient Value') + ggtitle('Timeseries of predictors for LA14 from August 2009 to June 2010')
  




######################################### end RQ2 code 




#### inspecting all runs 

# select method type 
m <- summary_results[summary_results$pseudo_abs_method == 'random_step',]

print(paste('Total number of model:', nrow(m)))
print(paste('Number of full models with VIF <5:', sum(m$VIF_full == T)))
print(paste('Number of subset models with VIF <5:', sum(m$VIF_sub == T)))

mf <- m[m$VIF_full == T,]
ms <- m[m$VIF_sub == T,]

print(paste('Number of full models with significant GLM (based on deviance):', sum(mf$full_glm_sig == T)))
print(paste('Number of subset models with significant GLM (based on deviance):', sum(ms$sub_glm_sig == T)))

print(paste('Number of full models with significant CLR coef:', sum(mf$full_clr_sig_coef == T)))
print(paste('Number of subset models with significant CLR coef:', sum(ms$sub_clr_sig_coef == T)))

mfg <- mf[mf$full_glm_sig == T,]
msg <- ms[ms$sub_glm_sig == T,]

print(paste('Number of full models with significant GLM coef:', sum(mfg$full_glm_sig_coef == T)))
print(paste('Number of subset models with significant GLM coef:', sum(msg$sub_glm_sig_coef == T)))

mf <- mf[mf$full_clr_sig_coef == T,]
ms <- ms[ms$sub_clr_sig_coef == T,]

mfg <- mfg[mfg$full_glm_sig_coef == T,]
msg <- msg[msg$sub_glm_sig_coef == T,]

print(paste('Significant GLM predictors in full models:', mfg$full_glm_sig_coef_which))
print(paste('Significant GLM predictors in subset models:', msg$sub_glm_sig_coef_which))

print(paste('Significant CLR predictors in full models:', mf$full_clr_sig_coef_which))
print(paste('Significant CLR predictors in subset models:', ms$sub_clr_sig_coef_which))

print(paste('CLR concordance of full models:', 'MIN:', min(mf$full_clr_concord), 
            'MEAN:', mean(mf$full_clr_concord), 'MAX', max(mf$full_clr_concord)))
print(paste('CLR concordance of subset models:', 'MIN:', min(ms$sub_clr_concord), 
            'MEAN:', mean(ms$sub_clr_concord), 'MAX', max(ms$sub_clr_concord)))






method_cd <- summary_results[summary_results$pseudo_abs_method == 'random_path_custom_distr' & summary_results$sub_clr_sig_coef == T,]
method_bp <- summary_results[summary_results$pseudo_abs_method == 'random_path_buffer_point' & summary_results$sub_clr_sig_coef == T,]
method_s <- summary_results[summary_results$pseudo_abs_method == 'random_step' & summary_results$sub_glm_clr_coef == T,]

c_cd <- mean(method_cd$sub_clr_concord)
c_bp <- mean(method_bp$sub_clr_concord)
c_s <- mean(method_s$sub_clr_concord)

sig_clr <- summary_results[summary_results$sub_clr_sig_coef ==T & summary_results$pseudo_abs_method == 'random_path_buffer_point',]
c <- mean(sig_clr$sub_clr_concord)

which_pred <- table(sig_clr$sub_clr_sig_coef_which)





# # create results data entry 
# entry <- data.frame(ID = ID, week = week, #pseudo_abs_method = pseudo_abs_method, 
#                     VIF_full = NA, full_glm_sig_coef = NA, full_glm_deviance = NA, full_glm_sig = NA, full_clr_sig_coef = NA, full_clr_concord = NA, full_clr_concord_se = NA, 
#                     VIF_sub = NA, sub_glm_sig_coef = NA, sub_glm_deviance = NA, sub_glm_sig = NA, sub_clr_sig_coef = NA, sub_clr_concord = NA, sub_clr_concord_se = NA)
# 
# 
# ## are the VIF values of the model acceptable? 
# # retrieve the dataset
# full_df <- read.csv(paste0(run_filepath, '6_a5_glm_full_vif.csv')) #_', pseudo_abs_method, '.csv'))
# sub_df <- read.csv(paste0(run_filepath, '6_b5_glm_50p_sd_vif.csv')) #_', pseudo_abs_method, '.csv'))
# 
# # check if vif values below 5
# if(any(full_df$vif_results <5)){entry$VIF_full <- T}else{entry$VIF_full <- F}
# if(any(sub_df$vif_results <5)){entry$VIF_sub <- T}else{entry$VIF_sub <- F}
# 
# 
# ## does the glm model have a significant coefficient? 
# # retrieve dataset
# full_df <- read.csv(paste0(run_filepath, '6_a3_glm_full_coefs.csv')) #_', pseudo_abs_method, '.csv'))
# sub_df <- read.csv(paste0(run_filepath, '6_b3_glm_50p_sd_coefs.csv')) #_', pseudo_abs_method, '.csv'))
# 
# # exclude intercept 
# full_df <- full_df[full_df$X != '(Intercept)', ]
# sub_df <- sub_df[sub_df$X != '(Intercept)', ]
# 
# # check if pvalues below 0.05
# if(any(full_df$Pr...z.. <=0.05)){entry$full_glm_sig_coef <- T}else{entry$full_glm_sig_coef <- F}
# if(any(sub_df$Pr...z.. <=0.05)){entry$sub_glm_sig_coef <- T}else{entry$sub_glm_sig_coef <- F}
# 
# 
# ## how high is the deviance of the fitted model? is the model significant? 
# # retrieve dataset
# full_df <- read.csv(paste0(run_filepath, '6_a4_glm_full_deviances.csv')) #_', pseudo_abs_method, '.csv'))
# sub_df <- read.csv(paste0(run_filepath, '6_b4_glm_50p_sd_deviances.csv')) #_', pseudo_abs_method, '.csv'))
# 
# # select residual deviance 
# entry$full_glm_deviance <- full_df$residual_deviance
# entry$sub_glm_deviance <- sub_df$residual_deviance
# 
# # derive p-value of model from chi-square stat
# # source: https://stats.stackexchange.com/questions/340489/interpretation-of-deviance-in-logistic-model
# # source: https://www.r-bloggers.com/2022/05/calculate-the-p-value-from-chi-square-statistic-in-r/
# full_pval <- pchisq(full_df$null_deviance - full_df$residual_deviance, full_df$null_df - full_df$residual_df, lower.tail = F)
# sub_pval <- pchisq(sub_df$null_deviance - sub_df$residual_deviance, sub_df$null_df - sub_df$residual_df, lower.tail = F)
# 
# # check if pvalues below 0.05
# if(full_pval <=0.05){entry$full_glm_sig <- T}else{entry$full_glm_sig <- F}
# if(sub_pval <=0.05){entry$sub_glm_sig <- T}else{entry$sub_glm_sig <- F}
# 
# 
# ## does the clr model have a significant coefficient? 
# # retrieve dataset
# full_df <- read.csv(paste0(run_filepath, '6_a1_clr_full_coefs.csv')) #_', pseudo_abs_method, '.csv'))
# sub_df <- read.csv(paste0(run_filepath, '6_b1_clr_50p_sd_coefs.csv')) #_', pseudo_abs_method, '.csv'))
# 
# # check if pvalues below 0.05
# if(any(full_df$Pr...z.. <=0.05)){entry$full_clr_sig_coef <- T}else{entry$full_clr_sig_coef <- F}
# if(any(sub_df$Pr...z.. <=0.05)){entry$sub_clr_sig_coef <- T}else{entry$sub_clr_sig_coef <- F}
# 
# 
# ## what is the concordance? how much does it vary by? 
# # retrieve dataset
# full_df <- read.csv(paste0(run_filepath, '6_a2_clr_full_tests.csv')) #_', pseudo_abs_method, '.csv'))
# sub_df <- read.csv(paste0(run_filepath, '6_b2_clr_50p_sd_tests.csv')) #_', pseudo_abs_method, '.csv'))
# 
# # select concordance and SE
# entry$full_clr_concord <- full_df$concordance[1]
# entry$full_clr_concord_se <- full_df$concordance[4]
# entry$sub_clr_concord <- sub_df$concordance[1]
# entry$sub_clr_concord_se <- sub_df$concordance[4]





##################### plot model (s curve plot)
# tutorial: https://www.theanalysisfactor.com/r-glm-plotting/ --> now its a mix of sources

# retrieve dataset 
dat <- read.csv('data/LA14/2260/4_a1_cov_resp_dataset_random_path_custom_distr_downscaling_modis_250m.csv', row.names = 1)
cov <- dat[,c('ndvi_50', 'ndvi_sd', 'ndvi_rate_50', 'ndvi_rate_sd')]
# retrieve model 
#model <- readRDS(paste0('output/LA14/2260/6_b0_glm_50p_sd_model_random_path_custom_distr_downscaling_modis_250m.RDS'))
model <- glm(case_ ~ ndvi_50, data = dat, family = binomial)
summary(model)
xvalues <- seq(min(dat$ndvi_50), max(dat$ndvi_50), length.out = 60)
yvalues <- predict(model, newdata = list(ndvi_50 = xvalues), type = 'response')
plot(dat$ndvi_50, dat$case_, xlab = "Mean NDVI in a Step", ylab = 'Probability of Elephant Passage')
lines(xvalues, yvalues)


# create dataframe of values 
dat_new <- data.frame(ndvi_50 = seq(min(dat$ndvi_50), max(dat$ndvi_50), length.out = 60), 
                      ndvi_sd = seq(min(dat$ndvi_sd), max(dat$ndvi_sd), length.out = 60), 
                      ndvi_rate_50 = seq(min(dat$ndvi_rate_50), max(dat$ndvi_rate_50), length.out = 60), 
                      ndvi_rate_sd = seq(min(dat$ndvi_rate_sd), max(dat$ndvi_rate_sd), length.out = 60))

# set axis for plotting 
xvalues <- seq(min(dat_new), max(dat_new), length.out = 60)
yvalues <- predict(model, newdata = dat_new, type = 'response')

# create s curve plot
plot(xvalues, yvalues, xlab = "Mean NDVI + StDev NDVI + Mean Rate NDVI + StDev Rate NDVI", ylab = 'Probability of Elephant Passage')
plot(x=seq(20, 65, by=5), predict( mlogit, 
                                   newdata=data.frame(Drug="Y", Environment="H", Ethnicity="White",
                                                      Age=seq(20, 65, by=5) ), type="response" ) )

# plot model 
plot(xvalues, yvalues)

plot(mtcars$disp, mtcars$vs, pch = 16, xlab = "DISPLACEMENT (cubic inches)", ylab = "VS")


### try another approach 

# intercept & beta coefficients  
intercept <- as.numeric( coef( model )[1] )
beta_ndvi_50 <- as.numeric( coef( model )[2] )
beta_ndvi_sd <- as.numeric( coef( model )[3] )
beta_ndvi_rate_50 <- as.numeric( coef( model )[4] )
beta_ndvi_rate_sd <- as.numeric( coef( model )[5] )


# range of variables
range <- seq(min(cov), max(cov), length.out = 60)

# log odds
logodds_ndvi_50 <- intercept + beta_ndvi_50 * range
logodds_ndvi_sd <- intercept + beta_ndvi_sd * range
logodds_ndvi_rate_50 <- intercept + beta_ndvi_rate_50 * range
logodds_ndvi_rate_sd <- intercept + beta_ndvi_rate_sd * range

# probabilities 
prob_ndvi_50 <- exp(logodds_ndvi_50) / (1+exp(logodds_ndvi_50))
prob_ndvi_sd <- exp(logodds_ndvi_sd) / (1+exp(logodds_ndvi_sd))
prob_ndvi_rate_50 <- exp(logodds_ndvi_rate_50) / (1+exp(logodds_ndvi_rate_50))
prob_ndvi_rate_sd <- exp(logodds_ndvi_rate_sd) / (1+exp(logodds_ndvi_rate_sd))

# curves
plot( x=range, y=prob_ndvi_50, type="l" )
plot( x=range, y=prob_var_2, type="l" )







## plot coef of model

# NOTE FOR README: this requires installation of SjPlot package --> depending on R version that have might have issues installing 
# Details: sjPlot package requires sjstats package which requires emmeans package which requires estimability package which requires new R version
# The packages will not install if have estimability v1.5 and now R v4.3.0 
# Solution: install estimability v1.4.1 instead of the latest --> 'https://cran.r-project.org/src/contrib/Archive/estimability/estimability_1.4.1.tar.gz'
# source: https://stackoverflow.com/questions/68172470/i-want-to-install-old-version-package-in-r
# Once older version installed, the rest should be able to install fine like a chain 

# link <- 'https://cran.r-project.org/src/contrib/Archive/estimability/estimability_1.4.1.tar.gz'
# install.packages(link, repos = NULL, type = 'source')
# 
# if(!('estimability') %in% installed.packages()){install.packages('estimability')} 
# if(!('emmeans') %in% installed.packages()){install.packages('emmeans')} 
if(!('sjPlot') %in% installed.packages()){install.packages('sjPlot')} 
library(sjPlot)

# retrieve fitted model 
model <- readRDS(paste0('output/LA14/2260/6_b0_glm_50p_sd_model_random_path_custom_distr_downscaling_modis_30m.RDS'))
#model <- readRDS(paste0('output/LA26/2267/6_b0_glm_50p_sd_model_random_path_custom_distr_downscaling_modis_30m.RDS'))
summary(model)

plot_model(model, transform = NULL, show.values = T, vline.color = 'red')
plot_model(model, show.values = T, vline.color = 'red')


if(!('terra') %in% installed.packages()){install.packages('terra')} #for north arrow
library(terra)
if(!('tidyterra') %in% installed.packages()){install.packages('tidyterra')} #for north arrow
library(tidyterra)
if(!('ggspatial') %in% installed.packages()){install.packages('ggspatial')} #for north arrow
library(ggspatial)
library(tidyr)

ID <- 'LA14'
week <- 2260
input_filepath <- paste0('data/', ID, '/', week, '/')
random_data_method <- 'random_path_custom_distr'
downscaling = T
downscaling_model = 'ranger_full_selection'
title = 'Elephant movement on mean NDVI'

# define data directory and suffix
if(downscaling == 'NULL'){
  modis_directory <- paste0(input_filepath, '3_a1_modis_images_', random_data_method, '/')

  suffix <- ''

}else if(downscaling == T){
  modis_directory <- paste0(input_filepath, '3_g1_downscaled_modis_images_30m_', downscaling_model, '/')

  suffix <- '_downscaling_modis_30m'

}else if(downscaling == F){
  modis_directory <- paste0(input_filepath, '3_b1_modis_images_downscaling_', random_data_method, '/')

  suffix <- '_downscaling_modis_250m'

}else{stop('Incorrect term set for downscaling parameter. Should be one of the following: NULL, T, F.')}


# read NDVI dataset
ndvi_data <- rast(paste0(modis_directory, 'mean_ndvi.tif'))
names(ndvi_data) <- 'ndvi'



# read step dataset
dat <- read.csv(paste0(input_filepath, '4_a1_cov_resp_dataset_', random_data_method, suffix, '.csv'))
dat$random_id_ <- as.factor(dat$random_id_)

# plot histograms 
hist(dat$ndvi_50)
hist(dat$ndvi_sd)
hist(dat$ndvi_rate_50)
hist(dat$ndvi_rate_sd)

s <- dat[dat$ndvi_sd > 0.08,]
ss <- dat[dat$ndvi_rate_sd > 0.015,]

# add new step ID column that restarts count at each burst (so doesn't connect the different paths)
dat$stepID <- NA
steps <- dat[dat$burst_ == 1 & dat$case_ == T,]
dat$stepID[min(steps$X):max(steps$X)] <- 1:as.numeric(nrow(steps))
for(b in unique(dat$burst_)){
  # select rows of that burst that are true
  steps <- dat[dat$burst_ == b & dat$case_ == T,]
  dat$stepID[min(steps$X):max(steps$X)] <- 1:as.numeric(nrow(steps))
}

# transfer the step ID of random steps to new column
# NOTE: could move the code to some other script (unless the columns are useful)
dat$stepID[dat$case_ == F] <- dat$step_id_[dat$case_ == F]

# new column for pathID
dat$pathID <- NA

# find start of new path
smin <- dat$X[dat$stepID == min(dat$stepID)]

# assign new path ID to start of each new path
dat$pathID[smin] <- 1:length(smin)

# fill the column with values from above
# source: https://tidyr.tidyverse.org/reference/fill.html#ref-examples
dat <- dat %>% fill(pathID)
dat$pathID <- as.factor(dat$pathID)

# rename the case column (necessary for plotting order)
dat$case_[dat$case_ == T] <- 2
dat$case_[dat$case_ == F] <- 1

dat$case_ <- as.factor(dat$case_)


# make visual map without legends
image_map <- ggplot() +
  geom_spatraster(data = ndvi_data, aes(fill = ndvi), show.legend = F) +
  scale_fill_terrain_c(name = 'NDVI', limits = c(0,0.6)) +
  geom_path(data = subset(dat, case_ == 1), aes(x = x1_, y = y1_, group = pathID), colour = 'grey50', linetype = 2, linewidth = 0.4) +
  geom_path(data = subset(dat, case_ == 2), aes(x = x1_, y = y1_, group = pathID,), colour = 'darkred', linetype = 1, linewidth = 0.4) +
  geom_path(data = dat[c(453,454),], aes(x = x1_, y = y1_), colour = 'blue', linetype = 1, linewidth = 0.4) + 
  #geom_path(data = dat[c(250,251),], aes(x = x1_, y = y1_), colour = 'blue', linetype = 1, linewidth = 0.4) + 
  labs(title = title, subtitle = paste0('Elephant ', ID, ' from ', as.Date(min(dat$t1_), tz = 'Africa/Maputo') , ' to ',
                                        as.Date(max(dat$t2_), tz = 'Africa/Maputo'),' (week ', week, ')'), x = "Longitude", y = "Latitude") +
  annotation_north_arrow(location = 'tl', which_north = 'true',
                         pad_x = unit(0.5, "cm"), pad_y = unit(0.6, "cm"),
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = 'br', pad_x = unit(1.0, "cm"), pad_y = unit(0.5, "cm")) +
  theme_minimal() +
  theme(legend.position = 'none')

image_map


round(exp(coef(model)[4]), 8)

model[2]
coef(model)[2]











# plot s curve 

m50 <- glm(case_ ~ ndvi_50 , family = binomial(link = 'logit'), data = dat)
msd <- glm(case_ ~ ndvi_sd , family = binomial(link = 'logit'), data = dat)
mr50 <- glm(case_ ~ ndvi_rate_50 , family = binomial(link = 'logit'), data = dat)
mrsd <- glm(case_ ~ ndvi_rate_sd, family = binomial(link = 'logit'), data = dat)

xval <- data.frame(ndvi_50 = seq(from = min(dat$ndvi_50), to = max(dat$ndvi_50), length.out = 60))

#predict gives the predicted value in terms of logits
yval <- predict(m50, xval)

d <- data.frame(X = xval, Y = yval)
names(d) <- c('x', 'y')
ggplot(data = d, aes(x = x, y = y)) + geom_line()
#convert those logit values to probabilities
yor <- exp(yval)/(1+exp(yval))

d <- data.frame(X = xval, Y = yor)
names(d) <- c('x', 'y')

ggplot()+ 
  #geom_point(data = dat, aes(x=ndvi_50, y=case_)) +
  geom_line(data = d, aes(x=x, y=y))

ggplot(data = d) + 
  geom_line(aes(x = ndvi_50, y = yor))



# derive p-value of model from chi-square stat
# source: https://stats.stackexchange.com/questions/340489/interpretation-of-deviance-in-logistic-model
# source: https://www.r-bloggers.com/2022/05/calculate-the-p-value-from-chi-square-statistic-in-r/
pch <- pchisq(model$null.deviance - model$deviance, model$df.null - model$df.residual, lower.tail = F)
pch




# new df 
zdat <- read.csv(paste0(input_filepath, '4_a1_cov_resp_dataset_', random_data_method, suffix, '.csv'))

df <- data.frame(predicted = predict(model, type = 'response'), 
                 residuals = residuals(model, type = 'response'), 
                 linearized = model$linear.predictors, 
                 weights = model$weights, 
                 path = as.numeric(zdat$case_))

df$id <- seq(1,nrow(df))
# this is assuming the decision threshold is set at 0.5
mism <- df %>% filter(path != round(predicted))
max(df$predicted)
for (i in 1:nrow(df)) {
  if (!is.na(match(df$id[i],mism$id))) m <- 1
  else m <- 0
  df$mismatched[i] <- m
}

df$id <- NULL

#define a function to plot the model
gra.tot <- function(dat, varLin, varY, weights, group, fitModel, devModel, dfModel, devNull, dfNull, aicModel, nameX, nameY) {
  ggplot(dat, aes(x = varLin, y = varY)) +
    geom_point(aes(size=weights, color=as.factor(group)), alpha=.3) +
    scale_colour_manual(name="mismatched", values = c("grey30", "red3")) +
    geom_text(x= min(varLin) + (0.1 * (max(varLin) - min(varLin))) , y=0.5, hjust=0, label=paste( 'predicted ~ linearized', "\nmismatched: ",sum(group),"/",length(fitModel), '\nres. deviance: ', round(devModel,2) , ' (df: ', round(dfModel,2),')',  '\nnull deviance: ', round(devNull,2), ' (df: ', round(dfNull,2),')', '\nAIC: ', round(aicModel,2) ), col='grey40', size=3, fontface='italic') +
    xlab(as.character(nameX)) +
    ylab(as.character(nameY)) +
    theme_bw()
}

#plot the model
tot <- gra.tot(df, df$linearized, df$predicted, df$weights, df$mismatched, model$fitted, model$deviance, model$df.residual, model$null.deviance, model$df.null, model$aic, 'linearized_predictors', 'logit_risk')
tot















############### CREATE SUMMARY TABLE OF GLM SIGNIFICANCE AND DEVIANCE AND VIF
run_table <- read.csv('data/run_settings_downscaling.csv', row.names = 1)

df <- data.frame()
for(r in 1:nrow(run_table)){
  r
  # get settings
  ID <- run_table$ID[r]
  week <- run_table$week[r]
  pseudo_abs_method <- run_table$pseudo_abs_method[r]
  downscaling_setting <- run_table$downscaling[r]
  
  # get model specifications
  if(downscaling_setting == 'NULL'){
    suffix <- ''
    modis_label <- 'MODIS 250m'
    
  }else if(downscaling_setting == T){
    suffix <- '_downscaling_modis_30m'
    modis_label <- 'MODIS 30m (D)'
    
  }else if(downscaling_setting == F){
    suffix <- '_downscaling_modis_250m'
    modis_label <- 'MODIS 250m (D)'

  }else{stop('Incorrect term set for downscaling parameter. Should be one of the following: NULL, T, F.')}
  
  # load dataset 
  dev_df <- read.csv(paste0('output/', ID, '/', week, '/6_b4_glm_50p_sd_deviances_', pseudo_abs_method, suffix, '.csv'))
  vif_df <- read.csv(paste0('output/', ID, '/', week, '/6_b5_glm_50p_sd_vif_', pseudo_abs_method, suffix, '.csv'), row.names = 1, header = T)
  
  # retrieve the deviance for the model 
  pval <- pchisq(dev_df$null_deviance - dev_df$residual_deviance, dev_df$null_df - dev_df$residual_df, lower.tail = F)
  
  if(pval < 0.001){sig = '99.9%'}else if(pval >= 0.001 & pval <0.01){sig = '99%'}else if(pval >= 0.01 & pval < 0.05){sig = '95%'}else if(pval >= 0.05 & pval < 0.1){sig = '90%'}else if(pval >= 0.1 & pval < 1){sig = 'not sig'}else{sig = '?'}
  
  entry <- data.frame(ID = ID, week = week, method = pseudo_abs_method, modis = modis_label, 
                      vif_ndvi_50 = vif_df$vif_results[1], vif_ndvi_sd = vif_df$vif_results[2], 
                      vif_ndvi_rate_50 = vif_df$vif_results[3], vif_ndvi_srate_d = vif_df$vif_results[4], 
                      deviance = dev_df$residual_deviance, p_value = pval, significance = sig)

  df <- rbind(df, entry)
}

write.csv(df, file = 'output/summary_deviances_downscalingRQ3.csv')


a <- read.csv('data/run_settings_moreRQ1.csv', row.names = 1)
a <- a[a$pseudo_abs_method == 'random_path_custom_distr',]
a$week <- 2066
a$downscaling <- 'NULL'
a$downscaling_model <- 'NULL'

write.csv(a, 'data/run_settings_moreRQ1.csv')





## check if elements created correctly 
r <- read.csv('data/run_settings_RQ2.csv', row.names = 1)

df <- data.frame()

for(i in 1:nrow(r)){
  
  # get run settings
  ID <- r$ID[i]
  week <- r$week[i]
  method <- r$pseudo_abs_method[i]
  
  # define filepaths
  data_path <- paste0('data/', ID, '/', week, '/')
  output_path <- paste0('output/', ID, '/', week, '/')
  
  # get files for data and output paths
  data_files <- list.files(data_path)
  output_files <- list.files(output_path)
  
  # define files that should have for each step
  step1_files <- c('1_a1_elephant_full_track_xyt.RDS', '1_a2_elephant_track_xyt.RDS', '1_b1_all_steps_random_path_custom_distr.RDS')
  step2_files <- c('2_a1_step_extents_LUT_random_path_custom_distr.csv')
  step3_files <- c('3_a1_modis_images_random_path_custom_distr')
  step4_files <- c('4_a1_cov_resp_dataset_random_path_custom_distr.csv')
  step5_files <- c('5_a1_elephant_movement_map_random_path_custom_distr.png')
  step6_files <- c('6_b0_clr_50p_sd_model_random_path_custom_distr.RDS', '6_b0_glm_50p_sd_model_random_path_custom_distr.RDS', 
                   '6_b1_clr_50p_sd_coefs_random_path_custom_distr.csv', '6_b2_clr_50p_sd_tests_random_path_custom_distr.csv', 
                   '6_a1_correlation_matrix_random_path_custom_distr.png', '6_b3_glm_50p_sd_coefs_random_path_custom_distr.csv', 
                   '6_b4_glm_50p_sd_deviances_random_path_custom_distr.csv', '6_b5_glm_50p_sd_vif_random_path_custom_distr.csv')
  step7_files <- c('7_b1_50p_sd_plot_log_odds_random_path_custom_distr.png', '7_b2_50p_sd_plot_odd_ratios_random_path_custom_distr.png', 
                   '7_b3_50p_sd_plot_curve_random_path_custom_distr.png')
  
  # check if each folder has the correct files and mark the answer in table 
  if(all(step1_files %in% data_files)){step1 = T}else{step1 = F}
  if(all(step2_files %in% data_files)){step2 = T}else{step2 = F}
  if(all(step3_files %in% data_files) & length(list.files(paste0(data_path, step3_files))) == 15){step3 = T}else{step3 = F}
  if(all(step4_files %in% data_files)){step4 = T}else{step4 = F}
  if(all(step5_files %in% output_files)){step5 = T}else{step5 = F}
  if(all(step6_files %in% output_files)){step6 = T}else{step6 = F}
  if(all(step7_files %in% output_files)){step7 = T}else{step7 = F}
  
  # check if all steps complete 
  if(all(c(step1, step2, step3, step4, step5, step6, step7) == T)){complete = T}else{complete = F}
  
  # fill entry 
  entry <- data.frame(ID = ID, week = week, method = method, step1 = step1, step2 = step2, step3 = step3, step4 = step4, step5 = step5, step6 = step6, step7 = step7, complete = complete)
  
  df <- rbind(df, entry)
  
}

dfr <- df$week[df$step3 == F]
print(dfr)

rr <- r[r$week %in% dfr,]

write.csv(rr, 'data/run_settings_RQ2_rerun.csv')

# 
# s <- readRDS('data/LA14/2112/1_b1_all_steps_random_path_custom_distr.RDS')
# ss <- readRDS('data/LA14/2112/1_a2_elephant_track_xyt.RDS')
# 
# 
# 
# r <- r[2:nrow(r),]
# rownames(r) <- 1:nrow(r)
# write.csv(r, 'data/run_settings_RQ2.csv')
# r <- read.csv('data/run_settings_RQ2.csv', row.names = 1)
# r$X <- NULL

