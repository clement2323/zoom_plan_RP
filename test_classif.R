library(dplyr)
library(sf)
sf_use_s2(FALSE)
library(igraph)
library(ggplot2)
library(fpc)
library(gridExtra)
library(geomtextpath) #remotes::install_github("allanCameron/geomtextpath")
library(ggrepel)
library(raster)
library(tidyterra)

rm(list =ls())

setwd("/Users/clementguillo/Desktop/travaux_insee/zoom-plans-rp/")
source("src/fonctions.R")

# code pour zoomer des parties  d'un plan où des logements se oncentrent
ilots <- st_read("data/ilots.gpkg")

# import d'un ril du lamentin
ril_lam <- read.csv2(file="data/liste EA Lamentin.csv", skip=1, colClasses = c("ilot"= "character"))

# filtre gr 1 :
ril_lam <- ril_lam  %>% 
  filter(grp_rotation == 1) %>% 
  select(id_rp, ilot, depcom, x, y) %>% 
  mutate(adr_rang = sapply(strsplit(id_rp," "),"[[",5))%>%
  st_as_sf(coords = c("x","y"), crs=5490, remove = FALSE)

# donner la possibilité de ne pas électionner tel ou tel zoom ?
# code_ilot = "0902"; code_ilot = "0941"; "0770";"0751";


unique(ril_lam$ilot)%>% sort()

gerer_ilot <- function(code_ident_ilot){
  # code_ident_ilot = "972130950"
  contour_ilot <- ilots %>% filter(ident_ilot==code_ident_ilot)
  # On pourrait afficher le résultat de la classif avec des numéros
  # Puis mettre le numéro du groupe dans le titre  
  ril_ilot <- ril_lam %>% 
    filter(ilot == substr(code_ident_ilot,6,9)) %>% 
    select(id_rp, ilot, depcom, x, y,adr_rang) 
  
  #plot(ril_ilot$geometry,col= "red")
  
  res_classif <- classif_kmeans_seq(ril_ilot)

  ril_ilot_avec_grp <- res_classif$ril
  graph <- res_classif$plot_final

  #ploter le ril_ilot_complet :
  p_globale <- representer_sous_ril(sous_ril = ril_ilot_avec_grp, contour_ilot = contour_ilot)
  # puis ploter les différents groupes
  liste_sous_ril <- split(ril_ilot_avec_grp,ril_ilot_avec_grp$groupe)

  liste_plot <- lapply(liste_sous_ril, function(x) representer_sous_ril(sous_ril = x, contour_ilot = contour_ilot))

  liste_plot <- c(list(p_globale,graph),liste_plot)
  # tout mettre dans le même pdf fusionner les PDF
  # save the plot in a same pdf file
  pdf(file = paste0("zoom_",code_ident_ilot,".pdf"), width = 10, height = 10)
  for (i in 1:length(liste_plot)){
    print(liste_plot[[i]])
  }
  dev.off()

}
# je mets le résultat dans un pdf avec les 2 à côté
representer_sous_ril <- function(sous_ril,contour_ilot){
# sous_ril <- liste_sous_ril[[2]];  contour_ilot <- contour_ilot
# Et là je peux sortir l'artillerie lourde pour faire un beau plan
  bbox <- st_bbox(sous_ril %>% st_transform(4326))
  
  bbox_large <- bbox

  offset <- 0.005
  bbox_large["xmin"] <- bbox["xmin"] - offset   
  bbox_large["xmax"] <- bbox["xmax"] + offset
  bbox_large["ymin"] <- bbox["ymin"] - offset 
  bbox_large["ymax"] <- bbox["ymax"] + offset

  url_ortho <- get_url_ortho(bbox_large,CRS = "4326")
  url_plan <- get_url_plan(bbox_large,CRS = "4326")
  url_voie <- get_url_voie(bbox_large,CRS = "4326")
  
  # récupération des voies nommées
  voies <- st_read(url_voie) %>% 
    mutate(name =tolower(nom_initial_troncon)) %>% 
    rename(geometry = geometrie) %>% 
    dplyr::select(name)

  bbox_polygon <- generer_polygone_from_bbox(bbox)

  voies <- st_crop(voies%>% st_cast("MULTILINESTRING")%>% st_cast("LINESTRING"),bbox_polygon)

  contour_ilot <- st_crop(contour_ilot,bbox_polygon)

  raster_data <- obtain_raster_from_url(url_ortho,bbox)

  p <- dessiner_plan(raster_data, voies, bbox_large,
                sous_ril, contour_ilot,
                couleur_nom_voie = "white",
                titre = paste0("Plan Ortho ",contour_ilot$ident_ilot[1]," groupe : ",sous_ril$groupe[1]),
                vjust_voie = -0.00005, offset = 0.00000)
  p

}
