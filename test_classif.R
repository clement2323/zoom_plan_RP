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
lapply(liste_ilot,gerer_ilot, ril_ville = ril_ville,ilots = ilots)
e <-Sys.time()

e-s




# filtre gr 1 :
ril_tri <- ril_tri  %>% 
  filter(grp_rotation == 1) %>% 
  select(id_rp, ilot, depcom, x, y) %>% 
  mutate(adr_rang = sapply(strsplit(id_rp," "),"[[",5))%>%
  st_as_sf(coords = c("x","y"), crs=5490, remove = FALSE)

s<-Sys.time()
liste_ilot <- 
  paste0("97230",unique(ril_tri$ilot))

lapply(liste_ilot,gerer_ilot, ril_ville = ril_tri,ilots = ilots)
lapply(liste_ilot[sample(length(liste_ilot))],gerer_ilot, ril_ville = ril_tri,ilots = ilots)
e <-Sys.time()

pirnt(s,e)

