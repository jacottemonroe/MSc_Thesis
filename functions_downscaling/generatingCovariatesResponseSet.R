## MSc Thesis 
## Jacotte Monroe 

## Function that creates a multiraster dataset of all covariates and the response variable
## Input: landsat spatraster with all relevant bands, modis spatraster with all relevant bands 
## Output: dataset with all landsat bands, their band ratios, and the modis ndvi (as response variable)

getCovariatesResponseSet <- function(landsat_dataset, modis_dataset = NULL){
  dataset <- landsat_dataset
  
  if(!is.null(modis_dataset)){
    # calculate NDVI 
    dataset$ndvi_modis <- (modis_dataset$B2 - modis_dataset$B1)/(modis_dataset$B2 + modis_dataset$B1)
  }
  
  # get all NA values in same place 
  # source: https://stackoverflow.com/questions/73719011/mask-layer-of-a-raster-stack-using-other-layer-using-terra
  mask <- any(is.na(dataset))
  dataset <- mask(dataset, mask, maskvalues = T)
  
  # get dataframe of all band combinations
  # source: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/expand.grid
  band_combinations <- expand.grid(bandA = names(dataset['B.']), bandB = names(dataset['B.']), KEEP.OUT.ATTRS = F, stringsAsFactors = F)
  
  # calculate landsat band ratios 
  for(i in 1:nrow(band_combinations)){
    # get band name from combination dataframe 
    bandA <- band_combinations[i,1]
    bandB <- band_combinations[i,2]
    
    # get band number 
    # source: https://www.statology.org/r-extract-number-from-string/
    bandA_number <- as.numeric(gsub("\\D", "", bandA))
    bandB_number <- as.numeric(gsub("\\D", "", bandB))
    
    if(bandA_number>bandB_number){
      # calculate ratio 
      ratio <- (dataset[[bandA]]-dataset[[bandB]])/(dataset[[bandA]]+dataset[[bandB]])
      
      # add band ratio to dataset
      layer_name <- paste0(bandA,'.',bandB)
      dataset[[layer_name]] <- ratio
    }
  }
  
  return(dataset)
}