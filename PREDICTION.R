
##INTERTIDAL HABITAT CLASSIFICATION WITH MULTISPECTRAL AND TOPOGRAPHIC DATA
#MARTA ROMÁN

rm(list = ls())
Sys.setenv(LANG = "en")


library(tidyterra)
library(terra)
library(sf)
library(sp)
library(tidyverse)
library(tidymodels)
library(janitor)
library(rsample)
library(DALEX)
library(DALEXtra)
library(ggspatial)
library(stacks)
library(future)
library(doFuture)
library(tune)
library(dplyr)
library(rsample)
library(purrr)
library(ggpubr)
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(terra::buffer)
parallel::detectCores(logical = TRUE)
parallel::detectCores(logical = FALSE)

# registerDoFuture()
future::plan(sequential)


setwd("")


#PREDICTION map------

#CARGAR MODELO-----

load("CONF_MAT.RData")
load("RECIPE.RData")
load("FINAL_MODEL.RData")
load("FINAL_WF.RData")



##RAPACARALLOS 2023----
#LOAD RASTERS
plan()

TRI_rapa_23<-rast("OUTPUT/TRI_rapa_23_nw.tiff")
TPI_rapa_23<-rast("OUTPUT/TPI_rapa_23_nw.tiff")
slope_rapa_23<-rast("OUTPUT/slope_rapa23_nw.tiff")
aspect_rapa_23<-rast("OUTPUT/aspect_rapa_23_nw.tiff")
multi_rapa_23<-rast("OUTPUT/MULTI_RAPA23_NW.tiff")



#MERGE ALL RASTERS, TRANSFORM INTO DF, CLEAN NAS AND SORT INTO TILES
comb_multi_topo_23<-c(multi_rapa_23,slope_rapa_23,aspect_rapa_23,TRI_rapa_23,TPI_rapa_23) 
plot(comb_multi_topo_23)
comb_multi_topo_23


summary(recipe)
names(comb_multi_topo_23)

#CALCULATE NRGI
comb_multi_topo_23$B5_B4<-(comb_multi_topo_23$Vigo_Site1_DualMX_32629_corrected_5-
                             comb_multi_topo_23$Vigo_Site1_DualMX_32629_corrected_4)/
  (comb_multi_topo_23$Vigo_Site1_DualMX_32629_corrected_5+
     comb_multi_topo_23$Vigo_Site1_DualMX_32629_corrected_4)

plot(comb_multi_topo_23$B5_B4)


# CALCULATE AUS

comb_multi_topo_23$AUS <- ((560-531)*((comb_multi_topo_23$Vigo_Site1_DualMX_32629_corrected_3 +comb_multi_topo_23$Vigo_Site1_DualMX_32629_corrected_4)/2) + 
                 (650-560)*((comb_multi_topo_23$Vigo_Site1_DualMX_32629_corrected_4 + comb_multi_topo_23$Vigo_Site1_DualMX_32629_corrected_5)/2) + 
                 (668-650)*((comb_multi_topo_23$Vigo_Site1_DualMX_32629_corrected_5 + comb_multi_topo_23$Vigo_Site1_DualMX_32629_corrected_6)/2))

plot(comb_multi_topo_23$AUS)
names(comb_multi_topo_23)

writeRaster(comb_multi_topo_23,"OUTPUT/RASTERS FOR PREDICTION/RAPA 23/RAPA_23.tiff",overwrite=TRUE)

#SEPARAR EN TILES
comb_multi_topo_23<-rast("OUTPUT/RASTERS FOR PREDICTION/RAPA 23/RAPA_23.tiff")

library(purrr)
nx <- ncol(comb_multi_topo_23)
ny <- nrow(comb_multi_topo_23)
template<-rast(comb_multi_topo_23)

options(digits = 22)


tile_size_pixels <- 2000
pixel_res <- res(comb_multi_topo_23)[1]
tile_size <- tile_size_pixels * pixel_res

output_dir <- "OUTPUT/RASTERS FOR PREDICTION/RAPA 23/tiles_output"
dir.create(output_dir, showWarnings = FALSE)
tiles_files <- makeTiles(comb_multi_topo_23, tile_size_pixels, filename=file.path(output_dir, "tile_d.tif"))

print(head(tiles_files)) 




####LOOP FOR PASSING INTO DF, PREPROCESS AND PREDICT


class_id<-seq(1,9)
.pred_class<-c("other_barnacles","mussel_spat","adult_mussels","goose_barnacle","red_algae","brown_algae","green_algae","bare_rock","water")
class_table<-tibble(class_id,.pred_class)

dir.create("OUTPUT/tiles_predictions/RAPA_23", recursive = TRUE, showWarnings = FALSE)
tile_files<-tile_files[[28]]


for (tile_file in tile_files) {
  
  tile_name <- tools::file_path_sans_ext(basename(tile_file))
  cat("Processing:", tile_name, "\n")
  


  r <- rast(tile_file)
  
  df <- as.data.frame(r, xy = TRUE, na.rm = FALSE)
  
  df <- df %>% drop_na()
  
  if (nrow(df) == 0) {
    message("Skipping ", tile_file)
    next
  }
  

  
  
df$tile_id <- basename(tile_file)
  
  df<-df%>%
    mutate(ID="0") %>% 
    mutate(ID=as.numeric(ID))
  
  df$ID<-paste0("rapa23_",df$ID)

  df_ok<-df %>% 
    rownames_to_column(var="PixelID") %>% 
    mutate(PixelID = paste0("rapa23_", PixelID))%>%
    dplyr::rename(b1=Vigo_Site1_DualMX_32629_corrected_1,
                  b2=Vigo_Site1_DualMX_32629_corrected_2,
                  b3=Vigo_Site1_DualMX_32629_corrected_3,
                  b4=Vigo_Site1_DualMX_32629_corrected_4,
                  b5=Vigo_Site1_DualMX_32629_corrected_5,
                  b6=Vigo_Site1_DualMX_32629_corrected_6,
                  b7=Vigo_Site1_DualMX_32629_corrected_7,
                  b8=Vigo_Site1_DualMX_32629_corrected_8,
                  b9=Vigo_Site1_DualMX_32629_corrected_9,
                  b10=Vigo_Site1_DualMX_32629_corrected_10 ) %>% 
    na.omit()
  
  df_final_ok_features<-df_ok %>% 
    dplyr::select(x,y,PixelID,ID,slope,aspect,TRI,TPI,
                  b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,AUS,B5_B4)
  
  
  data_long<-df_ok %>% 
    dplyr::select(-slope,-aspect,-TRI,-TPI,-AUS,-B5_B4) %>% 
    pivot_longer(-c(PixelID,ID,x,y,tile_id),names_to = "band",values_to = "reflectance") 
  
  # STANDARDIZE
  #(ri-min)/(max-min)
  
  labels_long_std<-data_long %>%
    group_by(PixelID) %>% 
    filter(!all(is.na(reflectance))) %>% 
    
    mutate(std.ref = ((reflectance-min(reflectance))/(max(reflectance)-min(reflectance)))) %>% 
    ungroup() %>% 
    mutate(band=case_when(band=="b1"~"b1_std",
                          band=="b2"~"b2_std",
                          band=="b3"~"b3_std",
                          band=="b4"~"b4_std",
                          band=="b5"~"b5_std",
                          band=="b6"~"b6_std",
                          band=="b7"~"b7_std",
                          band=="b8"~"b8_std",
                          band=="b9"~"b9_std",
                          band=="b10"~"b10_std"))
  
  which(is.na(labels_long_std))
  
  labels_std_ok<-labels_long_std%>%
    pivot_wider(id_cols=PixelID,names_from = band, values_from = std.ref) %>% 
    left_join(df_final_ok_features)
  

  
  labels_std_ok<-labels_std_ok%>%
    dplyr::select(-PixelID,-b1,-b2,-b3,-b4,-b5,-b6,-b7,-b8,-b9,-b10) %>%
    na.omit()
  
  
  # #PREDICT
  # 
  prediction <- labels_std_ok %>%
    mutate(predict(final_model, .),
           .pred_class = factor(.pred_class)) %>%
    left_join(class_table, by = ".pred_class")
  
  # # Extract coordinates and prediction
  prediction_points <- prediction %>%
    dplyr::select(x, y, class_id) %>%
    dplyr::filter(!is.na(class_id))  # Remove NA class predictions
  # 
  points_vect <- terra::vect(prediction_points, geom = c("x", "y"), crs = crs(r))
  # 
  prediction_tif <- terra::rasterize(points_vect, r, field = "class_id")
  # 
  
  output_file <- file.path("OUTPUT/tiles_predictions/RAPA_23", paste0("Prediction_", tile_name, ".tif"))
  writeRaster(prediction_tif, output_file, overwrite = TRUE)
  
  cat("Saved:", output_file, "\n\n")
  


}

##MAP GENERAL CLASSIFICATION

#load tiles 
tile_path <- list.files("OUTPUT/tiles_predictions/RAPA_23", pattern = "\\.tif$", full.names = TRUE)
tile_path
tiles<-lapply(tile_path,rast)

clasificacion_total_rapa_23 <- do.call(merge, tiles)
plot(clasificacion_total_rapa_23)
names(clasificacion_total_rapa_23)

#rgb
rgb_rapa_23<-rast("DATA/voles_2023/Lidar/Photogrammetry RGB/Vigo_site1_RGB_photogrammetry_32629.tif")



plotRGB(rgb_rapa_23)

clases  <- data.frame(
  value = 1:9,
  label = c("Other barnacles","Mussel spat","Adult mussels","Goose barnacle",  "Red algae", "Brown algae", "Green algae", 
            "Bare rock", "Water"))
clasificacion_total_rapa_23 <- classify(clasificacion_total_rapa_23, rcl = cbind(1:9, 1:9))
levels(clasificacion_total_rapa_23) <- clases

names(clasificacion_total_rapa_23)
clasificacion_ok_rapa_23<-clasificacion_total_rapa_23 %>% 
  filter(!label=="Water")

writeRaster(clasificacion_total_rapa_23,"OUTPUT/clasificacion_total_rapa_23.tiff",overwrite=TRUE)

 

extent_raster <- terra::ext(rgb_rapa_23)


prediction_plot_rapa_23<-ggplot() +
  geom_spatraster_rgb(data = rgb_rapa_23) +
  geom_spatraster(data = clasificacion_ok_rapa_23, aes(fill = label)) +
   
  scale_fill_manual(name = "Class", breaks = c("Goose barnacle","Other barnacles","Adult mussels","Mussel spat",
                                               "Red algae", "Brown algae", "Green algae",  "Bare rock"),
                    values = c( "orange", "pink", "blue", "grey3", "red", "goldenrod4", "green", "grey" ),na.value = NA) +
  theme_bw() +
  coord_sf(xlim = c(extent_raster[1], extent_raster[2]),
               ylim = c(extent_raster[3], extent_raster[4]))+
  annotation_scale(location = "br", width_hint = 0.3,text_cex = 1.5)+
  theme(axis.text = element_blank(),legend.text=element_text(size=14,color="black"),
        legend.title = element_text(size=14,color = "black"),panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),plot.title = element_text(size=14,color="black"),
        legend.position = "none")+ggtitle("A) Rapacarallos 2023")+
  scale_x_continuous(breaks = seq(-8.864, -8.862, by = 0.0005))+
  scale_y_continuous(breaks=seq(42.120,42.122,by=0.0004))

prediction_plot_rapa_23

rgb_plot_rapa_23<-ggplot() +
  geom_spatraster_rgb(data = rgb_rapa_23) +
 
 theme_bw() +
  coord_sf(xlim = c(extent_raster[1], extent_raster[2]),
           ylim = c(extent_raster[3], extent_raster[4]))+
  annotation_scale(location = "br", width_hint = 0.3,text_cex = 1.5)+
  theme(axis.text = element_text(size=14,color = "black"),legend.text=element_text(size=14,color="black"),
       panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),plot.title = element_text(size=14,color="black"))+ggtitle("B)")+
  scale_x_continuous(breaks = seq(-8.864, -8.862, by = 0.0005))+
  scale_y_continuous(breaks=seq(42.120,42.122,by=0.0004))

windows();rgb_plot_rapa_23



rapacarallos_23<-ggarrange(prediction_plot_rapa_23,rgb_plot_rapa_23,ncol=2, align="hv")
rapacarallos_23

ggsave("prediction_plot_rapa_23.svg",plot=prediction_plot_rapa_23,device="svg", path="FIGURES",width=8.5,height=12,units="in")


##RAPACARALLOS 2024----
#LOAD RASTERS
plan()

TRI_rapa_24<-rast("OUTPUT/TRI_rapa_24_nw.tiff")
TPI_rapa_24<-rast("OUTPUT/TPI_rapa_24_nw.tiff")
slope_rapa_24<-rast("OUTPUT/slope_rapa_24_nw.tiff")
aspect_rapa_24<-rast("OUTPUT/aspect_rapa_24_nw.tiff")
multi_rapa_24<-rast("OUTPUT/MULTI_RAPA24_NW.tiff")



#MERGE ALL RASTERS, TRANSFORM INTO DF, CLEAN NAS AND SORT INTO TILES
comb_multi_topo_24<-c(multi_rapa_24,slope_rapa_24,aspect_rapa_24,TRI_rapa_24,TPI_rapa_24) 
plot(comb_multi_topo_24)
comb_multi_topo_24


summary(recipe)
names(comb_multi_topo_24)

#CALCULATE NRGI
comb_multi_topo_24$B5_B4<-(comb_multi_topo_24$Vigo_1_2024_MULTI_5-
                             comb_multi_topo_24$Vigo_1_2024_MULTI_4)/
  (comb_multi_topo_24$Vigo_1_2024_MULTI_5+
     comb_multi_topo_24$Vigo_1_2024_MULTI_4)

plot(comb_multi_topo_24$B5_B4)


# CALCULATE AUS

comb_multi_topo_24$AUS <- ((560-531)*((comb_multi_topo_24$Vigo_1_2024_MULTI_3 +comb_multi_topo_24$Vigo_1_2024_MULTI_4)/2) + 
                             (650-560)*((comb_multi_topo_24$Vigo_1_2024_MULTI_4 + comb_multi_topo_24$Vigo_1_2024_MULTI_5)/2) + 
                             (668-650)*((comb_multi_topo_24$Vigo_1_2024_MULTI_5 + comb_multi_topo_24$Vigo_1_2024_MULTI_6)/2))

plot(comb_multi_topo_24$AUS)
names(comb_multi_topo_24)

writeRaster(comb_multi_topo_24,"OUTPUT/RASTERS FOR PREDICTION/RAPA 24/RAPA_24.tiff",overwrite=TRUE)

#SEPARATE INTO TILES
comb_multi_topo_24<-rast("OUTPUT/RASTERS FOR PREDICTION/RAPA 24/RAPA_24.tiff")

library(purrr)
nx <- ncol(comb_multi_topo_24)
ny <- nrow(comb_multi_topo_24)
template<-rast(comb_multi_topo_24)

options(digits = 22)


tile_size_pixels <- 2000
pixel_res <- res(comb_multi_topo_24)[1]
tile_size <- tile_size_pixels * pixel_res

output_dir <- "OUTPUT/RASTERS FOR PREDICTION/RAPA 24/tiles_output"
dir.create(output_dir, showWarnings = FALSE)
tiles_files <- makeTiles(comb_multi_topo_24, tile_size_pixels, filename=file.path(output_dir, "tile_d.tif"))

print(head(tiles_files)) 



#LOOP FOR PASSING INTO DF, PREPROCESS AND PREDICT

class_id<-seq(1,9)
.pred_class<-c("other_barnacles","mussel_spat","adult_mussels","goose_barnacle","red_algae","brown_algae","green_algae","bare_rock","water")
class_table<-tibble(class_id,.pred_class)

 tile_files <- list.files("OUTPUT/RASTERS FOR PREDICTION/RAPA 24/tiles_output", pattern = "\\.tif$", full.names = TRUE)
dir.create("OUTPUT/tiles_predictions/RAPA_24", recursive = TRUE, showWarnings = FALSE)
 


for (tile_file in tile_files) {
  
  tile_name <- tools::file_path_sans_ext(basename(tile_file))
  cat("Processing:", tile_name, "\n")
  


  r <- rast(tile_file)
  
  df <- as.data.frame(r, xy = TRUE, na.rm = FALSE)
  
  df <- df %>% drop_na()
  
  # Skip if no rows left
  if (nrow(df) == 0) {
    message("Skipping  ", tile_file)
    next
  }
  
  
  
  
  df$tile_id <- basename(tile_file)
  
  df<-df%>%
    mutate(ID="0") %>% 
    mutate(ID=as.numeric(ID))
  
  df$ID<-paste0("rapa24_",df$ID)
  
  df_ok<-df %>% 
    rownames_to_column(var="PixelID") %>% 
    mutate(PixelID = paste0("rapa24_", PixelID))%>%
    dplyr::rename(b1=Vigo_1_2024_MULTI_1,
                  b2=Vigo_1_2024_MULTI_2,
                  b3=Vigo_1_2024_MULTI_3,
                  b4=Vigo_1_2024_MULTI_4,
                  b5=Vigo_1_2024_MULTI_5,
                  b6=Vigo_1_2024_MULTI_6,
                  b7=Vigo_1_2024_MULTI_7,
                  b8=Vigo_1_2024_MULTI_8,
                  b9=Vigo_1_2024_MULTI_9,
                  b10=Vigo_1_2024_MULTI_10 ) %>% 
    na.omit()
  
  df_final_ok_features<-df_ok %>% 
    dplyr::select(x,y,PixelID,ID,slope,aspect,TRI,TPI,
                  b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,AUS,B5_B4)
  
  
  data_long<-df_ok %>% 
    dplyr::select(-slope,-aspect,-TRI,-TPI,-AUS,-B5_B4) %>% 
    pivot_longer(-c(PixelID,ID,x,y,tile_id),names_to = "band",values_to = "reflectance") 
  
  # STANDARDIZE
  #(ri-min)/(max-min)
  
  labels_long_std<-data_long %>%
    group_by(PixelID) %>% 
    filter(!all(is.na(reflectance))) %>% 
    
    mutate(std.ref = ((reflectance-min(reflectance))/(max(reflectance)-min(reflectance)))) %>% 
    ungroup() %>% 
    mutate(band=case_when(band=="b1"~"b1_std",
                          band=="b2"~"b2_std",
                          band=="b3"~"b3_std",
                          band=="b4"~"b4_std",
                          band=="b5"~"b5_std",
                          band=="b6"~"b6_std",
                          band=="b7"~"b7_std",
                          band=="b8"~"b8_std",
                          band=="b9"~"b9_std",
                          band=="b10"~"b10_std"))
  
  which(is.na(labels_long_std))
  
  labels_std_ok<-labels_long_std%>%
    pivot_wider(id_cols=PixelID,names_from = band, values_from = std.ref) %>% 
    left_join(df_final_ok_features)
  
  
  
  labels_std_ok<-labels_std_ok%>%
    dplyr::select(-PixelID,-b1,-b2,-b3,-b4,-b5,-b6,-b7,-b8,-b9,-b10) %>%
    na.omit()
  
  
  # #PREDICT
  # 
  prediction <- labels_std_ok %>%
    mutate(predict(final_model, .),
           .pred_class = factor(.pred_class)) %>%
    left_join(class_table, by = ".pred_class")
  
  prediction_points <- prediction %>%
    dplyr::select(x, y, class_id) %>%
    dplyr::filter(!is.na(class_id))  
  # 
  points_vect <- terra::vect(prediction_points, geom = c("x", "y"), crs = crs(r))
  # 
  prediction_tif <- terra::rasterize(points_vect, r, field = "class_id")
  # 
  
  output_file <- file.path("OUTPUT/tiles_predictions/RAPA_24", paste0("Prediction_", tile_name, ".tif"))
  writeRaster(prediction_tif, output_file, overwrite = TRUE)
  
  cat("Saved:", output_file, "\n\n")
  
  
  
  
}


##MAP GENERAL CLASSIFICATION

#load tiles 
tile_path <- list.files("OUTPUT/tiles_predictions/RAPA_24", pattern = "\\.tif$", full.names = TRUE)
tile_path
tiles<-lapply(tile_path,rast)

clasificacion_total_rapa_24 <- do.call(merge, tiles)
plot(clasificacion_total_rapa_24)
names(clasificacion_total_rapa_24)

#rgb

rgb_rapa_24<-rast("DATA/Voles_2024/Vigo_1_2024_RGB_modified.tif")
plotRGB(rgb_rapa_24)
rgb_rapa_24
clasificacion_total_rapa_24


rgb_proj_rapa_24<-project(rgb_rapa_24,clasificacion_total_rapa_24)
rgb_proj_rapa_24
plotRGB(rgb_proj_rapa_24)



mask_layer<-rgb_proj_rapa_24[[1]]>0
 
plot(mask_layer)
mask_layer
mask_rgb_rapa_24<-mask(rgb_proj_rapa_24,mask_layer,maskvalue = FALSE)
 
plot(mask_rgb_rapa_24)
 

clases  <- data.frame(
  value = 1:9,
  label = c("Other barnacles","Mussel spat","Adult mussels","Goose barnacle",  "Red algae", "Brown algae", "Green algae", 
            "Bare rock", "Water"))
clasificacion_total_rapa_24 <- classify(clasificacion_total_rapa_24, rcl = cbind(1:9, 1:9))
levels(clasificacion_total_rapa_24) <- clases
plot(clasificacion_total_rapa_24)

names(clasificacion_total_rapa_24)

writeRaster(clasificacion_total_rapa_24,"OUTPUT/clasificacion_total_rapa_24.tiff",overwrite=TRUE)

clasificacion_ok_rapa_24<-clasificacion_total_rapa_24 %>% 
  filter(!label=="Water")


extent_raster <- terra::ext(mask_rgb_rapa_24)
 

prediction_plot_rapa_24<-ggplot() +
  geom_spatraster_rgb(data = mask_rgb_rapa_24) +
  geom_spatraster(data = clasificacion_ok_rapa_24, aes(fill = label)) +

  scale_fill_manual(name = "Class", breaks = c("Goose barnacle","Other barnacles","Adult mussels","Mussel spat",
                                               "Red algae", "Brown algae", "Green algae",  "Bare rock"),
                    values = c( "orange", "pink", "blue", "grey3", "red", "goldenrod4", "green", "grey" ),na.value = NA) +
   theme_bw() +
  coord_sf(xlim = c(extent_raster[1], extent_raster[2]),
           ylim = c(extent_raster[3], extent_raster[4]))+
  annotation_scale(location = "br", width_hint = 0.3,text_cex = 1.5)+
  theme(axis.text = element_blank(),legend.text=element_text(size=14,color="black"),
        legend.title = element_text(size=14,color = "black"),panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),plot.title = element_text(size=14,color="black"),
        legend.position = "none")+
  ggtitle("C) Rapacarallos 2024")+
  scale_x_continuous(breaks = seq(-8.8635,8.8625,by=0.0005))+
  scale_y_continuous(breaks = seq(42.1204,42.1212,by=0.0004))


windows();prediction_plot_rapa_24


rgb_plot_rapa_24<-ggplot() +
  geom_spatraster_rgb(data = mask_rgb_rapa_24) +
 
  theme_bw() +
  coord_sf(xlim = c(extent_raster[1], extent_raster[2]),
           ylim = c(extent_raster[3], extent_raster[4]))+
  annotation_scale(location = "br", width_hint = 0.3,text_cex = 1.5)+
  theme(axis.text = element_text(size=14,color = "black"),legend.text=element_text(size=14,color="black"),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),plot.title = element_text(size=14,color="black"))+
  ggtitle("D) ")+
  scale_x_continuous(breaks = seq(-8.8635,8.8625,by=0.0005))+
  scale_y_continuous(breaks = seq(42.1204,42.1212,by=0.0004))


rgb_plot_rapa_24

rapacarallos_24<-ggarrange(prediction_plot_rapa_24,rgb_plot_rapa_24,ncol=2,align="hv")
rapacarallos_24

ggsave("prediction_plot_rapa_24.svg",plot=prediction_plot_rapa_24,device="svg", path="FIGURES",width=8.5,height=12,units="in")


#resample 2023 to 2024

rgb_rapa_23_res<-resample(rgb_rapa_23,mask_rgb_rapa_24)
plotRGB(rgb_rapa_23_res)
clasificacion_ok_rapa_23_res<-resample(clasificacion_ok_rapa_23,clasificacion_ok_rapa_24)
plot(clasificacion_ok_rapa_23_res)
clasificacion_ok_rapa_23_res

extent_raster <- terra::ext(mask_rgb_rapa_24)

prediction_plot_rapa_23_res<-ggplot() +
  geom_spatraster_rgb(data = rgb_rapa_23_res) +
  geom_spatraster(data = clasificacion_ok_rapa_23_res, aes(fill = label)) +
  # geom_sf(data = rect_sf, fill = NA, color = "black", linewidth = 1.2)+
  
  scale_fill_manual(name = "Class", breaks = c("Goose barnacle","Other barnacles","Adult mussels","Mussel spat",
                                               "Red algae", "Brown algae", "Green algae",  "Bare rock"),
                    values = c( "orange", "pink", "blue", "grey3", "red", "goldenrod4", "green", "grey" ),na.value = NA) +
  theme_bw() +
  coord_sf(xlim = c(extent_raster[1], extent_raster[2]),
           ylim = c(extent_raster[3], extent_raster[4]))+
  annotation_scale(location = "br", width_hint = 0.3,text_cex = 1.5)+
  theme(axis.text =element_blank(),legend.text=element_text(size=14,color="black"),
        legend.position="none",panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),plot.title = element_text(size=14,color="black"))+ggtitle("A) Rapacarallos 2023")+
  scale_x_continuous(breaks = seq(-8.8635,8.8625,by=0.0005))+
  scale_y_continuous(breaks = seq(42.1204,42.1212,by=0.0004))

windows();prediction_plot_rapa_23_res

rgb_plot_rapa_23_res<-ggplot() +
  geom_spatraster_rgb(data = rgb_rapa_23_res) +
 
  theme_bw() +
  coord_sf(xlim = c(extent_raster[1], extent_raster[2]),
           ylim = c(extent_raster[3], extent_raster[4]))+
  annotation_scale(location = "br", width_hint = 0.3,text_cex = 1.5)+
  theme(axis.text = element_text(size=14,color = "black"),legend.text=element_text(size=14,color="black"),
        legend.position="none",panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),plot.title = element_text(size=14,color="black"))+ggtitle("A) Rapacarallos 2023")+
  scale_x_continuous(breaks = seq(-8.8635,8.8625,by=0.0005))+
  scale_y_continuous(breaks = seq(42.1204,42.1212,by=0.0004))

windows();rgb_plot_rapa_23_res


ggarrange(prediction_plot_rapa_23,prediction_plot_rapa_24,ncol=2,align="hv",common.legend=TRUE)



##TALASO 2024 (CUNCHALES)-----
plan()

TRI_talaso_24<-rast("OUTPUT/TRI_talaso_nw.tiff")
TPI_talaso_24<-rast("OUTPUT/TPI_talaso_nw.tiff")
slope_talaso_24<-rast("OUTPUT/slope_talaso_nw.tiff")
aspect_talaso_24<-rast("OUTPUT/aspect_talaso_nw.tiff")
multi_talaso_24<-rast("OUTPUT/MULTI_TALASO_NW.tiff")



#MERGE ALL RASTERS, TRANSFORM INTO DF, CLEAN NAS AND SORT INTO TILES
comb_multi_talaso<-c(multi_talaso_24,slope_talaso_24,aspect_talaso_24,TRI_talaso_24,TPI_talaso_24) 
plot(comb_multi_talaso)
comb_multi_talaso


summary(recipe)
names(comb_multi_talaso)

#CALCULATE NRGI
comb_multi_talaso$B5_B4<-(comb_multi_talaso$Vigo_2_2024_MULTI_5-
                            comb_multi_talaso$Vigo_2_2024_MULTI_4)/
  (comb_multi_talaso$Vigo_2_2024_MULTI_5+
     comb_multi_talaso$Vigo_2_2024_MULTI_4)

plot(comb_multi_talaso$B5_B4)


# CALCULATE AUS

comb_multi_talaso$AUS <- ((560-531)*((comb_multi_talaso$Vigo_2_2024_MULTI_3 +comb_multi_talaso$Vigo_2_2024_MULTI_4)/2) + 
                             (650-560)*((comb_multi_talaso$Vigo_2_2024_MULTI_4 + comb_multi_talaso$Vigo_2_2024_MULTI_5)/2) + 
                             (668-650)*((comb_multi_talaso$Vigo_2_2024_MULTI_5 + comb_multi_talaso$Vigo_2_2024_MULTI_6)/2))

plot(comb_multi_talaso$AUS)
names(comb_multi_talaso)

writeRaster(comb_multi_talaso,"OUTPUT/RASTERS FOR PREDICTION/TALASO/TALASO.tiff",overwrite=TRUE)

#SEPARAR EN TILES
comb_multi_talaso<-rast("OUTPUT/RASTERS FOR PREDICTION/TALASO/TALASO.tiff")

library(purrr)
nx <- ncol(comb_multi_talaso)
ny <- nrow(comb_multi_talaso)
template<-rast(comb_multi_talaso)

options(digits = 22)


tile_size_pixels <- 2000
pixel_res <- res(comb_multi_talaso)[1]
tile_size <- tile_size_pixels * pixel_res

output_dir <- "OUTPUT/RASTERS FOR PREDICTION/TALASO/tiles_output"
dir.create(output_dir, showWarnings = FALSE)
tiles_files <- makeTiles(comb_multi_talaso, tile_size_pixels, filename=file.path(output_dir, "tile_d.tif"))

print(head(tiles_files)) 




#LOOP FOR PASING INTO DF, PREPROCESS AND PREDICT


class_id<-seq(1,9)
.pred_class<-c("other_barnacles","mussel_spat","adult_mussels","goose_barnacle","red_algae","brown_algae","green_algae","bare_rock","water")
class_table<-tibble(class_id,.pred_class)

 tile_files <- list.files("OUTPUT/RASTERS FOR PREDICTION/TALASO/tiles_output", pattern = "\\.tif$", full.names = TRUE)
dir.create("OUTPUT/tiles_predictions/TALASO", recursive = TRUE, showWarnings = FALSE)
 


for (tile_file in tile_files) {
  
  tile_name <- tools::file_path_sans_ext(basename(tile_file))
  cat("Processing:", tile_name, "\n")
  

  r <- rast(tile_file)
  
  df <- as.data.frame(r, xy = TRUE, na.rm = FALSE)
  
  df <- df %>% drop_na()
  
  if (nrow(df) == 0) {
    message("Skipping empty raster after removing NA rows: ", tile_file)
    next
  }
  
  
  
  
  df$tile_id <- basename(tile_file)
  
  df<-df%>%
    mutate(ID="0") %>% 
    mutate(ID=as.numeric(ID))
  
  df$ID<-paste0("talaso_",df$ID)
  
  df_ok<-df %>% 
    rownames_to_column(var="PixelID") %>% 
    mutate(PixelID = paste0("talaso_", PixelID))%>%
    dplyr::rename(b1=Vigo_2_2024_MULTI_1,
                  b2=Vigo_2_2024_MULTI_2,
                  b3=Vigo_2_2024_MULTI_3,
                  b4=Vigo_2_2024_MULTI_4,
                  b5=Vigo_2_2024_MULTI_5,
                  b6=Vigo_2_2024_MULTI_6,
                  b7=Vigo_2_2024_MULTI_7,
                  b8=Vigo_2_2024_MULTI_8,
                  b9=Vigo_2_2024_MULTI_9,
                  b10=Vigo_2_2024_MULTI_10 ) %>% 
    na.omit()
  
  df_final_ok_features<-df_ok %>% 
    dplyr::select(x,y,PixelID,ID,slope,aspect,TRI,TPI,
                  b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,AUS,B5_B4)
  
  
  data_long<-df_ok %>% 
    dplyr::select(-slope,-aspect,-TRI,-TPI,-AUS,-B5_B4) %>% 
    pivot_longer(-c(PixelID,ID,x,y,tile_id),names_to = "band",values_to = "reflectance") 
  
  # STANDARDIZE
  #(ri-min)/(max-min)
  
  labels_long_std<-data_long %>%
    group_by(PixelID) %>% 
    filter(!all(is.na(reflectance))) %>% 
    
    mutate(std.ref = ((reflectance-min(reflectance))/(max(reflectance)-min(reflectance)))) %>% 
    ungroup() %>% 
    mutate(band=case_when(band=="b1"~"b1_std",
                          band=="b2"~"b2_std",
                          band=="b3"~"b3_std",
                          band=="b4"~"b4_std",
                          band=="b5"~"b5_std",
                          band=="b6"~"b6_std",
                          band=="b7"~"b7_std",
                          band=="b8"~"b8_std",
                          band=="b9"~"b9_std",
                          band=="b10"~"b10_std"))
  
  which(is.na(labels_long_std))
  
  labels_std_ok<-labels_long_std%>%
    pivot_wider(id_cols=PixelID,names_from = band, values_from = std.ref) %>% 
    left_join(df_final_ok_features)
  
  
  
  labels_std_ok<-labels_std_ok%>%
    dplyr::select(-PixelID,-b1,-b2,-b3,-b4,-b5,-b6,-b7,-b8,-b9,-b10) %>%
    na.omit()
  
  
  # #PREDICT

  prediction <- labels_std_ok %>%
    mutate(predict(final_model, .),
           .pred_class = factor(.pred_class)) %>%
    left_join(class_table, by = ".pred_class")
  
  prediction_points <- prediction %>%
    dplyr::select(x, y, class_id) %>%
    dplyr::filter(!is.na(class_id))  
  # 
  points_vect <- terra::vect(prediction_points, geom = c("x", "y"), crs = crs(r))
  # 
  prediction_tif <- terra::rasterize(points_vect, r, field = "class_id")
  # 
  
  output_file <- file.path("OUTPUT/tiles_predictions/TALASO", paste0("Prediction_", tile_name, ".tif"))
  writeRaster(prediction_tif, output_file, overwrite = TRUE)
  
  cat("Saved:", output_file, "\n\n")
  
  
  
  
}



#MAP GENERAL CLASSIFICATION

#load tiles 
tile_path <- list.files("OUTPUT/tiles_predictions/TALASO", pattern = "\\.tif$", full.names = TRUE)
tile_path
tiles<-lapply(tile_path,rast)

clasificacion_total <- do.call(merge, tiles)
plot(clasificacion_total)
names(clasificacion_total)

#rgb

rgb<-rast("DATA/Voles_2024/Vigo_2_2024_RGB (1)_modified.tif")
plotRGB(rgb)
rgb
clasificacion_total


rgb_proj<-project(rgb,clasificacion_total)
rgb_proj
plotRGB(rgb_proj)



mask_layer<-rgb_proj[[1]]>0
# 
plot(mask_layer)
mask_layer
mask_rgb<-mask(rgb_proj,mask_layer,maskvalue = FALSE)
# 
plot(mask_rgb)
# 
# 
# 

clases  <- data.frame(  value = 1:9,
  label = c("Other barnacles","Mussel spat","Adult mussels","Goose barnacle",  "Red algae", "Brown algae", "Green algae", 
            "Bare rock", "Water"))
clasificacion_total <- classify(clasificacion_total, rcl = cbind(1:9, 1:9))
levels(clasificacion_total) <- clases

names(clasificacion_total)
writeRaster(clasificacion_total,"OUTPUT/clasificacion_total_talaso.tiff",overwrite=TRUE)


clasificacion_ok<-clasificacion_total %>% 
  filter(!label=="Water")

extent_raster <- terra::ext(mask_rgb)



prediction_plot_talaso<-ggplot() +
  geom_spatraster_rgb(data = mask_rgb) +
  geom_spatraster(data = clasificacion_ok, aes(fill = label)) +

  scale_fill_manual(name = "Class", breaks = c("Goose barnacle","Other barnacles","Adult mussels","Mussel spat",
                                               "Red algae", "Brown algae", "Green algae",  "Bare rock"),
                    values = c( "orange", "pink", "blue", "grey3", "red", "goldenrod4", "green", "grey" ),na.value = NA) +
  theme_bw() +
  coord_sf(xlim = c(extent_raster[1], extent_raster[2]),
           ylim = c(extent_raster[3], extent_raster[4]))+
  annotation_scale(location = "br", width_hint = 0.3,text_cex = 1.5)+
  theme(axis.text = element_blank(),legend.text=element_text(size=14,color="black"),
        legend.title = element_text(size=14,color = "black"),panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),plot.title = element_text(size=14,color="black"),
        legend.position = "none")+ggtitle("E) Cunchales 2024")+
  scale_x_continuous(breaks = seq(-8.90,-8.89,by=0.0004))+
  scale_y_continuous(breaks = seq(42.1002,42.1008,by=0.0003))

windows();prediction_plot_talaso


rgb_plot_talaso<-ggplot() +
  geom_spatraster_rgb(data = mask_rgb) +
   theme_bw() +
  coord_sf(xlim = c(extent_raster[1], extent_raster[2]),
           ylim = c(extent_raster[3], extent_raster[4]))+
  annotation_scale(location = "br", width_hint = 0.3,text_cex = 1.5)+
  theme(axis.text = element_text(size=14,color = "black"),legend.text=element_text(size=14,color="black"),
       panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),plot.title = element_text(size=14,color="black"))+ggtitle("F)")+
  scale_x_continuous(breaks = seq(-8.90,-8.89,by=0.0004))+
  scale_y_continuous(breaks = seq(42.1002,42.1008,by=0.0003))
  

windows();rgb_plot_talaso

talaso_2024<-ggarrange(prediction_plot_talaso,rgb_plot_talaso,ncol=2,align="hv")

talaso_2024


intertidal_clasif_plot<-ggarrange(prediction_plot_rapa_23_res,rgb_plot_rapa_23_res,
                                  prediction_plot_rapa_24,rgb_plot_rapa_24,
                                  prediction_plot_talaso,rgb_plot_talaso,
                                  nrow=3,ncol=2,align="hv",common.legend = TRUE,legend="top")

intertidal_clasif_plot


ggsave("intertidal_clasif_plot.svg",plot=intertidal_clasif_plot,device="svg",path="FIGURES",
       width = 12,height = 18,units = "in")




