rm(list = ls())

library(sf)
library(ggrepel)
library(tidyverse)

df <- read_rds('data/data_main.rds')

## Define map theme

theme_map <- function(...) {
  theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "white", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA), 
      panel.background = element_rect(fill = "white", color = NA), 
      legend.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank(),
      ...
    )
}

## Get shapefile

fname <- 'shapefiles/vg2500_krs.shp'
shp <- sf::st_read(fname)

## Merge

shp <- shp %>% left_join(., df, by =c('RS' = 'ags_2017'))

## Get list of cities 

cit <- read_csv('shapefiles/cities_de.csv') %>% 
  filter(str_detect(city, 'Berlin|Hamburg|Munich|Cologne|Hannover|Stuttgart')) %>% 
  filter(!str_detect(city, 'Oder'))

## Get state borders

states <- sf::st_read('shapefiles/vg2500_bld.shp')

## Plot

p <- ggplot(shp) +
  # municipality polygons
  geom_sf(data = shp, 
          aes(fill = hannover_dist, color = hannover_dist)) +
  geom_sf(data = shp %>% filter(RS == '03241'),
          fill = NA, color = 'black') +
  geom_sf(data = states ,
          fill = NA, color = 'black') +
  geom_point(data = cit, aes(x = lng, y = lat), 
             shape=21, fill = 'white', size = 2.5) +
  geom_label_repel(data = cit, aes(x = lng, y = lat, label = city), nudge_y = -0.3) +
  theme_map() +
  scale_fill_distiller(palette = 3, 
                       name = 'Distance from standard German',
                       type = 'div',
                       guide = guide_colorbar(
                         direction = "horizontal",
                         barheight = unit(2, units = "mm"),
                         barwidth = unit(52, units = "mm"),
                         draw.ulim = F,
                         title.position = 'top',
                         # some shifting around
                         title.hjust = 0.5,
                         label.hjust = 0.5
                       )) +
  scale_color_distiller(palette = 3, 
                        name = 'Distance from standard German',
                        type = 'div',
                        guide = guide_colorbar(
                          direction = "horizontal",
                          barheight = unit(2, units = "mm"),
                          barwidth = unit(52, units = "mm"),
                          draw.ulim = F,
                          title.position = 'top',
                          # some shifting around
                          title.hjust = 0.5,
                          label.hjust = 0.5
                        )) +
  theme(legend.position = 'bottom')
p
