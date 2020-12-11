library(rgdal)
library("leaflet")

setwd("C:\\Users\\khale\\OneDrive\\Bureau\\Soutenance projet info 2020")
chemin <- "C:\\Users\\khale\\OneDrive\\Documents\\Ensae\\Python pour le data scientist\\Projets\\dvf_ensae_sbra\\khaled\\data\\filosofi\\metropole"

carte_carreaux_paris <- readOGR(paste0(chemin, "\\filosofi_75.shp"))

binpal <- colorBin("RdYlBu", carte_carreaux_paris@data$Men_pauv, 6, pretty = FALSE)

m <- leaflet() %>% addTiles() %>% setView(lat = 48.8534, lng = 2.3488, zoom = 13) %>% 
  addPolygons(data = carte_carreaux_paris, color = ~binpal(carte_carreaux_paris@data$Men_pauv),
              weight = 2,
              opacity = 1,
              dashArray = "3",
              fillOpacity = 0.7)
m

#Enregistrement de la carte au format html
library(htmlwidgets)
saveWidget(m, file="m.html")

save("m",file = "carte.Rdata")

