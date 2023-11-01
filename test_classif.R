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
ril_ville <- read.csv2(file="data/liste EA Lamentin.csv", skip=1, colClasses = c("ilot"= "character"))

ril_ville <- read.csv2(file="data/liste EA Trinité.csv", skip=1, colClasses = c("ilot"= "character"))

ril_ville <- read.csv2(file="data/liste EA Le François.csv", skip=1, colClasses = c("ilot"= "character"))

ril_ville <- read.csv2(file="data/liste EA Ducos.csv", skip=1, colClasses = c("ilot"= "character"))

# filtre gr 1 :
ril_ville<- ril_ville  %>% 
  filter(grp_rotation == 1) %>% 
  select(id_rp, ilot, depcom, x, y) %>% 
  mutate(adr_rang = sapply(strsplit(id_rp," "),"[[",5))%>%
  st_as_sf(coords = c("x","y"), crs=5490, remove = FALSE)

depcom <- substr(ril_ville$id_rp,1,5)%>% unique()
# donner la possibilité de ne pas électionner tel ou tel zoom ?
# code_ilot = "0902"; code_ilot = "0941"; "0770";"0751";

s<-Sys.time()
liste_ilot <- 
  paste0(depcom,unique(ril_ville$ilot))
lapply(liste_ilot,function(ilot){
  writeLines(ilot, "log.txt",append =TRUE)
  gerer_ilot(ilot, ril_ville = ril_ville,ilots = ilots)
})
e <-Sys.time()

e-s

#TODO
# debug les cas bizarre
# gérer titres pour graphs principaux
# gérer les couleurs, ie mettre les numéros de couleur des logements hors groupe en bleu à minima
# relancer éventuellement avec des seuils de distance + faible ou plus fort dans certains cas
# pour que le decoupage se fasse quand même ou non
# branchement éventuel d'autres images Dynamis -> test d'obtention
# expliqquer tout ça à Loulou pour qu'il prenne le bb




