
# seuil_distance, distance max que l'on voudrait sur le plan en moyenne
classif_kmeans_seq <- function(ril_ilot, seuil_distance = 200, n_class_max = 5){

  dist_max <- max(st_distance(ril_ilot))
  n_class <- round(dist_max/seuil_distance) %>% as.numeric()
  n_class <- min(n_class,n_class_max) # maximum de classes = 10 ..

  ril_restant <- ril_ilot # intiitialisation du ril restant on va attribuer les points à des classes séquentiellement
  n_class_en_cours <- n_class
  ril_final_groupe <- data.frame()
  compteur_groupe <- 1  
  liste_plot <- list()


while(nrow(ril_restant)>0){
  

   if (nrow(ril_restant)==1){ 
      indice <- which.min(st_distance(ril_final_groupe,ril_restant))
      ril_restant$groupe <- ril_final_groupe[indice,]$groupe
      ril_final_groupe <- rbind(
        ril_final_groupe,
        ril_restant
      )
      ril_restant <-data.frame()
      break
    }
  res_kmeans <- kmeans(
    st_coordinates(ril_restant),
     centers = n_class_en_cours
     )$cluster

  ril_restant$groupe_kmeans <- res_kmeans
  
  # on récupère le groupe le plus gros
  gros_groupe <- ril_restant$groupe_kmeans %>% 
    table() %>% 
    which.max() %>% 
    names()
  
  bb <- st_bbox(
    ril_restant %>% filter(groupe_kmeans == gros_groupe)
    )

  # on récupère tous les ponts dans la bbox pour le plan
  ril_select <- ril_restant %>% 
    filter (
    x >= bb["xmin"],
    x <= bb["xmax"],
    y <= bb["ymax"],
    y >= bb["ymin"]
    ) %>% 
    mutate(groupe = compteur_groupe)
  
  ril_final_groupe <- rbind(
    ril_final_groupe,
    ril_select
    )
  
  ril_restant <- ril_restant %>% 
    filter(!id_rp %in% ril_select$id_rp)

  # Create a new plot for this iteration
  plotit <- ggplot(data = bind_rows(ril_final_groupe,ril_restant))+
    geom_sf(aes(col = as.factor(groupe)), size = 5, show.legend = FALSE)+
    theme_classic() +
    ggtitle(paste0("Iteration ", compteur_groupe - 1))+
    labs(x = NULL, y = NULL) +
    theme(axis.text = element_blank())

  liste_plot <- c(liste_plot,list(plotit))
  n_class_en_cours <- n_class_en_cours - 1
  compteur_groupe <- compteur_groupe + 1 
}

p_final <- ggplot(data =ril_final_groupe)+
    geom_sf(aes(col = as.factor(groupe)), size = 5, show.legend = TRUE)+
    theme_classic() +
    ggtitle(paste0("Iteration ", compteur_groupe - 1))+
    labs(x = NULL, y = NULL) +
    theme(axis.text = element_blank())

liste_plot <- c(liste_plot,list(p_final))
nom_ilot <- ril_ilot$ilot %>% unique()

grid_plots <- do.call(grid.arrange, c(liste_plot, ncol = 3))
ggsave(plot = grid_plots, paste0("res_kmeans_ilot_",nom_ilot,".png"))

return(list(ril=ril_final_groupe,plot_final = p_final))
}




get_url_voie <- function(bbox, CRS ="4326") {
  contour_ign <- paste0(bbox[c("ymin", "xmin", "ymax", "xmax")], collapse = ",")
  url <- paste0(
    "https://wxs.ign.fr/",
    "topographie",
    "/geoportail/",
    "wfs?SERVICE=WFS",
    "&VERSION=2.0.0",
    "&REQUEST=getfeature",
    "&typename=BDTOPO_V3:voie_nommee",
    "&CRS=EPSG:",CRS,
    "&BBOX=",contour_ign
  )
  url
}


get_url_plan <- function(bbox, width = 1100 , height = 1300, CRS ="4326") {
  contour_ign <- paste0(bbox[c("ymin", "xmin", "ymax", "xmax")], collapse = ",")
  url <- paste0(
    "https://wxs.ign.fr/",
    "cartes",
    "/geoportail/r/wms?",
    "LAYERS=GEOGRAPHICALGRIDSYSTEMS.MAPS.BDUNI.J1",
    "&EXCEPTIONS=text/xml",
    "&FORMAT=image/geotiff",
    "&SERVICE=WMS",
    "&VERSION=1.3.0",
    "&REQUEST=GetMap",
    "&WIDTH=",width,
    "&HEIGHT=",height,
    "&STYLES=",
    "&CRS=EPSG:",CRS,
    "&BBOX=",
    contour_ign
  )
  url
}


get_url_ortho <- function(bbox, width = 1100 , height = 1300, CRS = "4326") {
  #bbox = bb_ilot
  contour_ign <- paste0(bbox[c("ymin", "xmin", "ymax", "xmax")], collapse = ",")
  url <- paste0(
    "https://wxs.ign.fr/",
    "ortho", # "satellite",
    "/geoportail/r/wms?",
    "LAYERS=ORTHOIMAGERY.ORTHOPHOTOS.BDORTHO", # SI JE PRENDS ORTHOIMAGERY.ORTHOPHOTOS C4EST TROP LOURd
    "&EXCEPTIONS=text/xml",
    "&FORMAT=image/geotiff",
    "&SERVICE=WMS",
    "&VERSION=1.3.0",
    "&REQUEST=GetMap",
    "&WIDTH=",width,
    "&HEIGHT=",height,
    "&STYLES=",
    "&CRS=EPSG:",CRS,
    "&BBOX=",
    contour_ign)
  #   ,"&HEIGHT = 1000,",
  #   "&WIDTH = 1000"
  # )
  url
}





obtain_raster_from_url <- function(url, bbox) {
  # url <- url_ortho
  download.file(url = url, destfile = "ilot.tif", mode = "wb")
  raster_data <- stack("ilot.tif") # equivalent de raster mais pour multibandes !!
  bb <- bbox[c("xmin", "xmax", "ymin", "ymax")]
  extent(raster_data) <- bb
  raster_data <- terra::rast(raster_data)
  raster_data
}

generer_polygone_from_bbox <- function(bbox){
  
  xmin <- bbox["xmin"]    
  xmax <- bbox["xmax"] 
  ymin <- bbox["ymin"] 
  ymax <- bbox["ymax"] 
  
  bbox_polygon <- st_polygon(list(matrix(c(xmin, ymin,
                                           xmax, ymin,
                                           xmax, ymax,
                                           xmin, ymax,
                                           xmin, ymin), 
                                         ncol=2, byrow=TRUE)))
  
  bbox_polygon <- st_sf(geometry = st_sfc(bbox_polygon, crs = 4326)) 
  
  bbox_polygon
  
}

obtain_raster_from_url <- function(url,bbox){
    # url <- url_ortho
    download.file(url=url,destfile="image.tif",mode="wb")
    
    raster_data <- stack("image.tif") # equivalent de raster mais pour multibandes !!
    bb <- bbox[c("xmin","xmax","ymin","ymax")]
    extent(raster_data) <- bb
    raster_data <- terra::rast(raster_data)
    raster_data
}



dessiner_plan <- function(raster_data, voies, bbox,
                          ril, ilot,
                          couleur_nom_voie,
                          titre,
                          vjust_voie = -0.00005, offset = 0.00000){

  # ril <- sous_ril; ilot <- contour_ilot                             
  # vjust_voie = -0.00005
  # couleur_nom_voie = "blue"
  # titre = "Plan Ortho"
  # offset <- 0.00005
  # raster_data <- obtain_raster_from_url(url_ortho,bbox)
  # Chargement de la bibliothèque ggplot2
  # Création du graphique
  out_gg <- ggplot() +
    geom_spatraster_rgb(data = raster_data) 
  
  ril <-ril%>%st_transform(4326)
  coords <- st_coordinates(ril)
  colnames(coords)<- c("x_gps","y_gps")

  ril <- cbind(ril,coords)

if (nrow(voies) != 0) out_gg <- out_gg +  geom_sf(data = voies, color = alpha("#ec09d9", 1), size = 10) 
  
   out_gg +
    geom_textsf(data = voies,
                aes(label = name), text_smoothing = 65, linecolour = "#ad88b3", 
                color = couleur_nom_voie, vjust = vjust_voie, fill = "#E6F0B3", 
                alpha = 0.5, fontface = 6, size = 4) +
    geom_sf(data = ril, col = "black", size = 3) +
    geom_sf(data = ilot, col= "red", size = 4, alpha = 0)+
    geom_label(data = ril,
               aes(x = x_gps, y = y_gps, label = adr_rang), color = "red", fill= alpha("white", 0.6), 
               size = 3, label.size = NA, label.padding = unit(0.01, "npc"), 
               nudge_x = 0.00004,
               nudge_y = 0.00004,
               fontface ="bold",
               label.r = unit(0.3, "lines")
               )+
    coord_sf( # réduire la zone grise autour de la photo
      xlim = c(bbox["xmin"] + offset, bbox["xmax"] - offset),
      ylim = c(bbox["ymin"] + offset, bbox["ymax"] - offset)
    ) +
    theme( # supprimer la grille  dderrière 
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank()
    ) +
    theme( # enlever les graduations
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    xlab("") + # enlever le titre de l'axe X
    ylab("") +  # enlever le titre de l'axe Y
    #theme(aspect.ratio = aspect_ratio) +
    ggtitle(titre)+
    scale_y_continuous(expand = c(-0.00001,-0.00001)) + scale_x_continuous(expand = c(-0.00001,-0.00001))+
    theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_blank(),
    )
}




# je mets le résultat dans un pdf avec les 2 à côté
representer_sous_ril <- function(sous_ril,contour_ilot){
# sous_ril <- liste_sous_ril[[2]];  contour_ilot <- contour_ilot
# Et là je peux sortir l'artillerie lourde pour faire un beau plan
  bbox <- st_bbox(sous_ril %>% st_transform(4326))
  
  bbox_large <- bbox

  offset <- 0.001
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

  bbox_polygon <- generer_polygone_from_bbox(bbox_large)

  voies <- st_crop(voies%>% 
  st_cast("MULTILINESTRING")%>% 
  st_cast("LINESTRING"),bbox_polygon)

  contour_ilot <- st_crop(contour_ilot,bbox_polygon)

  raster_data <- obtain_raster_from_url(url_ortho,bbox)

  p <- dessiner_plan(raster_data, voies, bbox_large,
                sous_ril, contour_ilot,
                couleur_nom_voie = "white",
                titre = paste0("Plan Ortho ",contour_ilot$ident_ilot[1]," groupe : ",sous_ril$groupe[1]),
                vjust_voie = -0.00005, offset = 0.00000)
  p

}
