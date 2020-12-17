library(rgdal)
library("leaflet")

setwd("C:\\Users\\khale\\OneDrive\\Bureau\\Soutenance projet info 2020")

#Lecture de la table Filosofi
carte_carreaux_paris <- readOGR("data\\metropole\\filosofi_75.shp")


#Calcul niveau de vie par habitant

carte_carreaux_paris@data$niv_par_ind <- 
  carte_carreaux_paris@data$Ind_snv/carte_carreaux_paris@data$Ind 

#Calcul de proportion pour la distrubtion d'âge

ind_0_24 <- rowSums(carte_carreaux_paris@data[, c("Ind_0_3","Ind_4_5","Ind_6_10","Ind_11_17","Ind_18_24")])
ind_25_64 <-  rowSums(carte_carreaux_paris@data[, c("Ind_25_39","Ind_40_54","Ind_55_64")])
ind_65 <-  rowSums(carte_carreaux_paris@data[, c("Ind_65_79","Ind_80p")])

tot_ind <- rowSums(carte_carreaux_paris@data[, c("Ind_0_3","Ind_4_5","Ind_6_10","Ind_11_17","Ind_18_24",
                                                 "Ind_25_39","Ind_40_54","Ind_55_64","Ind_65_79","Ind_80p")])

taux_0_24 <- round(100*ind_0_24/tot_ind,2)
taux_25_64 <- round(100*ind_25_64/tot_ind,2)
taux_65 <- round(100*ind_65/tot_ind,2)

carte_carreaux_paris@data$taux_0_24 <- taux_0_24
carte_carreaux_paris@data$taux_25_64 <- taux_25_64
carte_carreaux_paris@data$taux_65 <- taux_65

carte_carreaux_paris@data$idv <- 100*ind_65/ind_0_24

#Message pop up

label_carreaux <- sprintf(
  "<strong>Niveau de vie par individu : </strong><br/>%g euros</sup>
  <br/>
  <br/>
  <strong>Part de moins de 25 ans :  </strong> %g %%
  <br/>
  <strong>Part de plus de 25 ans et moins de 64 ans :  </strong> %g %%
  <br/>
  <strong>Part de plus de 65 ans :  </strong> %g %%
  <br/>
  <strong>Indice de vieillissement :  </strong> %g %%",
  
  carte_carreaux_paris@data$niv_par_ind, carte_carreaux_paris@data$taux_0_24,carte_carreaux_paris@data$taux_25_64,
  carte_carreaux_paris@data$taux_65,carte_carreaux_paris@data$idv
) %>% lapply(htmltools::HTML)



binpal_rev <- colorBin("YlOrRd", carte_carreaux_paris@data$niv_par_ind, 8, pretty = FALSE)
binpal_idv <- colorBin("YlGnBu", carte_carreaux_paris@data$idv, 5, pretty = FALSE)

m <- leaflet(data = carte_carreaux_paris) %>% addTiles() %>% setView(lat = 48.8534, lng = 2.3488, zoom = 13) %>% 
  addPolygons(color = ~binpal_rev(carte_carreaux_paris@data$niv_par_ind),
              weight = 2,
              opacity = 1,
              dashArray = "3",
              fillOpacity = 0.7, 
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = label_carreaux,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"), group = "Revenu") %>% 
  addLegend("bottomright", pal = binpal_rev, values = carte_carreaux_paris@data$niv_par_ind,
            title = "Niveau de vie",
            labFormat = labelFormat(suffix = "€", big.mark = " ", transform = function(x){signif(x,3)}),
            opacity = 1, group = "Revenu"
  ) %>% 
  addPolygons(color = ~binpal_idv(carte_carreaux_paris@data$idv),
              weight = 2,
              opacity = 1,
              dashArray = "3",
              fillOpacity = 0.7, 
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = label_carreaux,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"), group = "Démographie") %>% 
  addLegend("bottomright", pal = binpal_idv, values = carte_carreaux_paris@data$idv,
            title = "Indice de vieillissement",
            labFormat = labelFormat(big.mark = " ", transform = function(x){signif(x,3)}),
            opacity = 1, group = "Démographie"
  ) %>% 
  addLayersControl(overlayGroups = c("Revenu","Démographie"),
                   options = layersControlOptions(collapsed = FALSE)
  ) %>% hideGroup("Démographie")


m



#Enregistrement de la carte au format html
library(htmlwidgets)
saveWidget(m, file="carte_filosofi.html")

save("m",file = "carte.Rdata")




# Carte BPE
library("data.table")
bpe <- fread("data\\bpe\\bpe19_ensemble_xy.csv")

bpe_paris_geo <- bpe[DEP == "75" & QUALITE_XY %in% c("Bonne", "Acceptable", "Mauvaise")]
bpe_paris_geo

bpe_a_garder <- fread("data\\bpe\\bpe_a_garder.csv",
                      encoding = "UTF-8", header = FALSE)

bpe_a_garder_4 <- bpe_a_garder[nchar(V1) == 4]
bpe_a_garder_2 <- bpe_a_garder[nchar(V1) == 2]

bpe_avec_nom <- merge(bpe_paris_geo, bpe_a_garder_4, by.x = "TYPEQU", by.y = "V1")

bpe_ens_second <- bpe_avec_nom[V2 %in% c("MATERNELLE","CRÈCHE","PRIMAIRE", "COLLÈGE" , "LYCÉE GÉNÉRAL ET TECHNOLOGIQUE" )]
bpe_theatre <- bpe_avec_nom[V2 %in% "THÉATRE",.(LAMBERT_X,LAMBERT_Y)]


library(raster)
#coord is a dataframe of lambert93 coord whose col names are LAMBERT_X and LAMBERT_Y

lambert_to_wgs <- function(coords,n_sample = 100L){
  #On échantillonne n_sample élements
  if(nrow(coords) > n_sample){
    coords <- coords[sample.int(nrow(coords), n_sample, replace = FALSE),]
  }
  coordinates(coords) <- ~ LAMBERT_X + LAMBERT_Y
  proj4string(coords) <- CRS("+init=epsg:2154")
  #Transformation en WGS84
  coords_wgs <- spTransform(coords, CRS("+proj=longlat +ellps=WGS84"))
  return(coords_wgs)
}


leaflet() %>% addTiles() %>% setView(lat = 48.8534, lng = 2.3488, zoom = 13) %>% 
  addMarkers(data = lambert_to_wgs(bpe_theatre))
