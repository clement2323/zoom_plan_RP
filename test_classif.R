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

liste_ilot <- 
  paste0("97213",c(c("0902","0941","0770","0751"),unique(ril_lam$ilot)))

lapply(liste_ilot,gerer_ilot, ril_ville = ril_lam,ilots = ilots)


