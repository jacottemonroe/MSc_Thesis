#!/usr/bin/bash

## MSc_Thesis 
## Jacotte Monroe 

## Main script that runs the rest of the processing steps (after MODIS image retrieval, since have to do that in JN)

# Create directories
# source: https://www.geeksforgeeks.org/mkdir-cannot-create-directory-file-exists/
mkdir -p data output 

# loop over entries of csv
# source: https://www.baeldung.com/linux/csv-parsing
exec < data/run_settings_RQ2_rerun.csv
read header
while read line
do
  # save the settings for the run in a new csv to be read in R scripts
  # source: https://supakon-k.medium.com/how-to-generate-csv-file-by-bash-script-15a7f526b83c
   echo "$line" > data/single_run_settings.csv
   
  # extract the NDVI values for each step and store in covariate/response dataframe
  Rscript 4_ExtractingCovariates.R
  
  # visualize movement paths of elephant for week of interest
  Rscript 5_VisualizingPaths.R
  
  # fit general and conditional logistic regression models and save the model outputs
  Rscript 6_FittingModels.R
  
  # visualize model restults 
  Rscript 7_VisualizingModels.R
done 
