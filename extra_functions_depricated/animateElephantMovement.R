## Depricated code 

## Script to map and ANIMATE elephant movement 

# Works after having run the preprocessing script










# study_area_filename <- 'data/etosha_geometry.geojson'
# landcover_filename <- 'data/etosha_landcover_dataframe_cop.csv'

# # load study area --> need SF package for this
# sa <- st_read(study_area_filename)
# 
# # retrieve the landcover dataframe of etosha for plotting 
# lc_etoshaNP <- read.csv(landcover_filename, sep = ',')
# 
# # create basemap of Etosha (for later mapping)
# source("functions/creatingEtoshaBasemap.R")
# basemap_Etosha <- createBasemapEtosha(sa, lc_etoshaNP)
# basemap_Etosha








#### plot data

mov_map <- ggplot() +
  geom_raster(data = lc_etoshaNP, aes(x = x, y = y, fill = landcover), show.legend = F) + 
  geom_sf(data = sa$geometry, fill = NA, color = 'black', lwd = 1) +
  #scale_fill_grey(start = 1, end = 0.7) +
  scale_fill_manual(values = c('grey94', 'grey16', 'grey32', 'grey65', 'grey80', 'grey90', 'grey25', 'grey20')) +
  #scale_fill_manual(name = 'Land cover', values = lc_lut$lc.color, labels = lc_lut$lc.class, na.translate = F) +
  #labs(title = "Elephant Movement", subtitle = "2008 - 2014", x = "Longitude", y = "Latitude") +
  geom_path(data = LA5_daily, aes(x = location.long, y = location.lat, color = 'red3', group=1), linewidth = 0.1, show.legend = F) +
  #annotation_north_arrow(location = 'tl', which_north = 'true', 
  # pad_x = unit(0.04, "in"), pad_y = unit(0.3, "in"),
  # style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = 'tl') +
  theme_minimal()

mov_map



lc_map <- ggplot() +
  geom_raster(data = lc_etosha_df, aes(x = x, y = y, fill = lc_etosha_df$landcover), show.legend = F) + 
  geom_sf(data = sa$geometry, fill = NA, color = 'black', lwd = 1) +
  scale_fill_manual(name = 'Land cover', values = lc_lut$lc.color, labels = lc_lut$lc.class, na.translate = F) +
  labs(title = "Etosha National Park",
       subtitle = "Namibia",
       x = "Longitude",
       y = "Latitude") +
  annotation_north_arrow(location = 'tl', which_north = 'true', 
                         pad_x = unit(0.04, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = 'tl') +
  #annotation_custom(grob = ggplotGrob(mini_map), xmin = 17, xmax = 17.5, ymin = 18.6, ymax = 18.8) +
  theme_minimal() + 
  theme(plot.margin=unit(c(1,1,4,0.5),"cm")) + 
  theme(legend.position = c(1.2, .1))
lc_map


# tutorial: https://conservancy.umn.edu/bitstream/handle/11299/220339/time-maps-tutorial-v2.html?sequence=3&isAllowed=y
# tutorial: https://hansenjohnson.org/post/animate-movement-in-r/

# plot basemap 
base_map <- ggplot() +
  # source: https://bookdown.org/mcwimberly/gdswr-book/raster-geospatial-data---discrete.html --> can't get it to work because of the different land cover data structure
  #geom_raster(data = lc_masked, show.legend = F, aes(fill = discrete_classification)) +
  #scale_fill_manual(name = 'Land cover', values = lc_colors, labels = lc_classes, na.translate = F)
  geom_sf(data = sa$geometry, color = "black") #, fill = "white") 

#base_map

# plot data on basemap 
map_with_data <- base_map +
  #geom_point(data = LA5_daily, aes(x = location.long, y = location.lat, group=date), color = 'royalblue1', size = 0.8) +
  geom_path(data = LA5_daily, aes(x = location.long, y = location.lat, color = tag, group=1), linewidth = 0.1)

#map_with_data

# zoom in 
min_long <- min(LA5_daily$location.long)
max_long <- max(LA5_daily$location.long)
min_lat <- min(LA5_daily$location.lat)
max_lat <- max(LA5_daily$location.lat)

map_with_data <- map_with_data +
  coord_sf(xlim = c(min_long, max_long),  ylim = c(min_lat, max_lat))

map_with_data

# animate 
map_with_animation <- mov_map #map_with_data +
transition_reveal(along = as.Date(timestamp)) +
  ggtitle('Date: {frame_along}',
          subtitle = 'Frame {frame} of {nframes}')

map_with_shadow <- map_with_animation + 
  shadow_mark()

days <- unique(LA5_daily$date)
ndays <- max(days) - min(days) + 1

animate(map_with_shadow, nframes = ndays, fps = 100)
anim_save('output/elephant_LA14_timelapse.gif', animation = last_animation())