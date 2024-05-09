#!/usr/bin/bash

## MSc_Thesis 
## Jacotte Monroe 

## Main script that runs the prreprocessing and first processing steps (until MODIS image retrieval, since have to do that in JN)

# Create directories
# source: https://www.geeksforgeeks.org/mkdir-cannot-create-directory-file-exists/
mkdir -p data output 

# create new environment from yaml file 

# activate environment (important for running Python!!)

# run elephant preprocessing script 


# read csv with all runs to do
#cat data/run_settings.csv

# loop over entries of csv 
# source: https://www.baeldung.com/linux/csv-parsing
#exec < data/run_settings_temp.csv
exec < data/run_settings_new_path_with_CV_rerun.csv
read header
while read line
do
  # save the settings for the run in a new csv to be read in R scripts
  # source: https://supakon-k.medium.com/how-to-generate-csv-file-by-bash-script-15a7f526b83c
   echo "$line" > data/single_run_settings.csv
   
  # retrieve corresponding elephant data for run and transform into a dataset of movement steps
  # source: https://stackoverflow.com/questions/31201561/running-r-commands-using-a-bash-script
  Rscript 1_LoadDataAndGenerateSteps.R
  
  # create a look-up table of spatial extents for the elephant dataset of that week for image retrieval
  Rscript 2_CreateStepExtentsLUT.R
  
done 
