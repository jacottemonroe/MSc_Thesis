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
exec < data/run_settings.csv
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
  
  # retrieves MODIS images for dates and spatial extents of steps from elephant and week of interest
  #python -c 'import I3_RetrievingMODISImages_test; I3_RetrievingMODISImages_test.retrieveMODISImages()'
  
  #python -c 'import test_script2; test_script2.test_function()'
  #python test_script.py 
  
  #jupyter nbconvert --execute 0_a_InitializingGEEAPI.ipynb
  # jupyter nbconvert --to python 0_a_InitializingGEEAPI.ipynb 
  # python 0_a_InitializingGEEAPI.py
  # 
  # !earthengine set_project my-project
  #python -c "import ee; ee.Initialize()"
  #earthengine authenticate
  #runipy image_retrieval_ssf.ipynb
done 
