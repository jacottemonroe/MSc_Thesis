## MSc Thesis 
## Jacotte Monroe 

## Function that removes spatraster layers based on a vector (list) of bands to remove
## Input: dataset with all layers and vector list of layers to remove
## Output: dataset without said layers


removeSpatRasterLayers <- function(dataset, layers_to_remove){
  layer_names_to_keep <- names(dataset)[!names(dataset) %in% layers_to_remove]
  dataset <- dataset[[layer_names_to_keep]]
  return(dataset)
}