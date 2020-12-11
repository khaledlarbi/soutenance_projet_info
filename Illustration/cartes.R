library(rgdal)
library("leaflet")

carte_carreaux_paris <- readOGR(paste0(chemin, "\\filosofi_75.shp"))
m <- leaflet() %>% addTiles() %>% setView(lat = 48.8534, lng = 2.3488, zoom = 12.2) %>% 
  addPolygons(data = carte_carreaux_paris)
