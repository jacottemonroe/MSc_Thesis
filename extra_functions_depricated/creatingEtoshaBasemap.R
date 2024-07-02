## MSc Thesis 
## Data Exploration 

## Function to create Etosha basemap 

if(!('ggplot2') %in% installed.packages()){install.packages('ggplot2')} # for maps
library(ggplot2)

if(!('ggspatial') %in% installed.packages()){install.packages('ggspatial')} #for north arrow
library(ggspatial)

createBasemapEtosha <- function(etosha_sa, etosha_lc){
  basemap <- ggplot() +
    geom_raster(data = etosha_lc, aes(x = x, y = y, fill = landcover), show.legend = F) + 
    geom_sf(data = etosha_sa$geometry, fill = NA, color = 'black', lwd = 1) +
    scale_fill_manual(values = c('grey94', 'grey16', 'grey32', 'grey65', 'grey80', 'grey90', 'grey25', 'grey20')) +
    annotation_north_arrow(location = 'tl', which_north = 'true', 
                           pad_x = unit(0.04, "in"), pad_y = unit(0.3, "in"),
                           style = north_arrow_fancy_orienteering()) +
    annotation_scale(location = 'tl') +
    theme_minimal()
  
  return(basemap)
}