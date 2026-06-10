#GOOSE BARNACLE COVER IN 50 X 50 CM GRID
#MARTA ROMAN GEADA


rm(list = ls())
Sys.setenv(LANG = "en")
library(readxl)
library(mgcv)
library(tidyverse)
library(ggtext)
library(terra)
library(tidyterra)
library(ggspatial)
library(patchwork)
library(ggpubr)

setwd("")



#RAPACARALLOS 23----

folder<-"OUTPUT/tiles_predictions/RAPA_23"
raster_files <- list.files(folder, pattern = "\\.tif$", full.names = TRUE)
rel_cover_tiles <- list()
df_tot<-data.frame()


target_res <- 0.5

for (file in raster_files) {
  
  goose <- rast(file)
  
  if (!any(values(goose) == 4, na.rm = TRUE)) {
    message("No hay valor 4 en el tile ", basename(file), " - saltando")
    next
  }
  
  r_bin <- ifel(goose == 4, 1, NA)
  
  res_orig <- res(goose)[1]  
  agg_factor <- ceiling(target_res / res_orig)
  
  r_area_clase <- aggregate(r_bin, fact = agg_factor, fun = sum, na.rm = TRUE)
  
  r_cobertura_relativa <- r_area_clase / (agg_factor^2)
  
  rel_cover_tiles[[length(rel_cover_tiles) + 1]] <- r_cobertura_relativa
  
  df <- as.data.frame(r_cobertura_relativa, xy = TRUE, na.rm = TRUE)
  if (nrow(df) == 0) next
  
  names(df)[3] <- "rel_cover_m"
  df$tile <- basename(file)
  
  df_tot <- rbind(df_tot, df)
  
  print(paste("Procesado:", basename(file)))
}

table(df_tot$tile)
df_rapa_23<-df_tot

r_cobertura_relativa

#mosaic relative density goose tiles
if (length(rel_cover_tiles) > 0) {
  cobertura_relativa_total_rapa_23 <- do.call(mosaic, rel_cover_tiles)
 
} else {
  cobertura_relativa_total_rapa_23 <- NULL
  message("No tiles")
}

plot(cobertura_relativa_total_rapa_23)
hist(cobertura_relativa_total_rapa_23)

plot(cobertura_relativa_total_rapa_23)
hist(cobertura_relativa_total_rapa_23)


#FIGURE RAPA 23
rgb_rapa_23<-rast("Vigo_site1_RGB_photogrammetry_32629.tif")

extent_raster <- terra::ext(rgb_rapa_23)

values(cobertura_relativa_total_rapa_23)
cobertura_relativa_total_rapa_23_plot <- cobertura_relativa_total_rapa_23 * 100
cobertura_relativa_total_rapa_23_plot
cobertura_relativa_total_rapa_23_plot[cobertura_relativa_total_rapa_23_plot < 5] <- NA

rel_cover_plot_rapa_23<-ggplot() +
  geom_spatraster_rgb(data = rgb_rapa_23,alpha=.8) +
  geom_spatraster(data = cobertura_relativa_total_rapa_23_plot, aes(fill = last)) +
  scale_fill_viridis_c(name = "Goose barnacle \nCover (%)\n", limits = c(0, 100),na.value = NA,option="magma")+
  theme_bw() +
  coord_sf(xlim = c(extent_raster[1], extent_raster[2]),
           ylim = c(extent_raster[3], extent_raster[4]))+
  annotation_scale(location = "br", width_hint = 0.3,text_cex = 1.5)+
  theme(axis.text = element_text(size=14,color = "black"),legend.text=element_text(size=14,color="black"),
        legend.title = element_text(size=14,color = "black"),panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),plot.title = element_text(size=14,color="black"),
        legend.position = "none")+ggtitle("A) Rapacarallos 2023")+
  scale_x_continuous(breaks = seq(-8.8635,8.8625,by=0.0005))+
  scale_y_continuous(breaks = seq(42.1204,42.1212,by=0.0004))

windows();rel_cover_plot_rapa_23#cover above 5 %



plot(cobertura_relativa_total_rapa_23)

global(cobertura_relativa_total_rapa_23,fun='mean', na.rm=TRUE)
global(cobertura_relativa_total_rapa_23,fun='std', na.rm=TRUE)

hist(cobertura_relativa_total_rapa_23)

#percentiles
quantile(values(cobertura_relativa_total_rapa_23), 
         probs = c(0.25, 0.5, 0.75),
         na.rm = TRUE)
 
boxplot(values(cobertura_relativa_total_rapa_23))

df_rapa_23<-df_rapa_23 %>% 
  mutate(dummy=1) %>% 
  mutate(rel_cover_m=100*rel_cover_m)
mean_rapa_23<-df_rapa_23 %>% 
  reframe(mean=mean(rel_cover_m)) %>% 
  mutate(dummy=1)
  

boxplot_rapa_23<-ggplot()+
  geom_boxplot(data = df_rapa_23,aes(x=dummy,y=rel_cover_m))+
  geom_point(data=mean_rapa_23,aes(x=dummy,y=mean),colour = "blue",size=3)+
  theme_classic()+ scale_y_continuous(name="Cover (%)")+
  theme(axis.text.x=element_blank(),axis.title.x = element_blank(),
        axis.ticks = element_blank(),axis.text = element_text(size=14,color="black"),
        axis.title = element_text(size=14,color="black"),axis.line.x = element_blank())
  
boxplot_rapa_23

plot_rapa23<-rel_cover_plot_rapa_23+annotation_custom(ggplotGrob(boxplot_rapa_23),
                                                      xmin=511253, xmax=511293, ymin=4663225  , ymax=4663265 )
plot_rapa23

# #COVER IN SQUARE METERS 
tile_path <- list.files("", pattern = "\\.tif$", full.names = TRUE)
tile_path
tiles<-lapply(tile_path,rast)
clasificacion_total_rapa_23 <- do.call(merge, tiles)
plot(clasificacion_total_rapa_23)
names(clasificacion_total_rapa_23)
clasificacion_total_rapa_23[clasificacion_total_rapa_23==9]<-NA


num_celdas_goose <- freq(clasificacion_total_rapa_23, value = 4)[, "count"]

area_celda <- res(clasificacion_total_rapa_23)[1] * res(clasificacion_total_rapa_23)[2]

area_total_m2_goose <- num_celdas_goose * area_celda
area_total_m2_goose


#area intertidal
num_celdas_tot<-freq(clasificacion_total_rapa_23)
sum(num_celdas_tot$count)

area_intermareal<-sum(num_celdas_tot$count)*area_celda
area_intermareal

area_total_m2_goose/area_intermareal




#RAPACARALLOS 24----
folder<-"OUTPUT/tiles_predictions/RAPA_24"
raster_files <- list.files(folder, pattern = "\\.tif$", full.names = TRUE)
rel_cover_tiles <- list()
df_tot<-data.frame()


target_res <- 0.5

for (file in raster_files) {
  
  goose <- rast(file)
  
  if (!any(values(goose) == 4, na.rm = TRUE)) {
    message("No hay valor 4 en el tile ", basename(file), " - saltando")
    next
  }
  
  r_bin <- ifel(goose == 4, 1, NA)
  
  res_orig <- res(goose)[1]  
  agg_factor <- ceiling(target_res / res_orig)
  
  r_area_clase <- aggregate(r_bin, fact = agg_factor, fun = sum, na.rm = TRUE)
  
  r_cobertura_relativa <- r_area_clase / (agg_factor^2)
  
  rel_cover_tiles[[length(rel_cover_tiles) + 1]] <- r_cobertura_relativa
  
  df <- as.data.frame(r_cobertura_relativa, xy = TRUE, na.rm = TRUE)
  if (nrow(df) == 0) next
  
  names(df)[3] <- "rel_cover_m"
  df$tile <- basename(file)
  
  df_tot <- rbind(df_tot, df)
  
  print(paste("Procesado:", basename(file)))
}

df_rapa24=df_tot
table(df_rapa24$tile)
summary(df_rapa24)
r_cobertura_relativa



#mosaic relative density goose tiles
if (length(rel_cover_tiles) > 0) {
  cobertura_relativa_total_rapa_24 <- do.call(mosaic, rel_cover_tiles)
} else {
  cobertura_relativa_total_rapa_24 <- NULL
  message("No  tiles ")
}

plot(cobertura_relativa_total_rapa_24)
hist(cobertura_relativa_total_rapa_24)





#FIGURE RAPA 24


#load rgb
mask_rgb_rapa_24<-rast("OUTPUT/mask_rgb_rapa_24.tiff")
extent_raster <- terra::ext(rgb_rapa_23)

values(cobertura_relativa_total_rapa_24)
cobertura_relativa_total_rapa_24_plot <- cobertura_relativa_total_rapa_24 * 100
 
cobertura_relativa_total_rapa_24_plot[cobertura_relativa_total_rapa_24_plot < 5] <- NA
 
rel_cover_plot_rapa_24<-ggplot() +
  geom_spatraster_rgb(data = mask_rgb_rapa_24,alpha=.8, interpolate = TRUE) +
  geom_spatraster(data = cobertura_relativa_total_rapa_24_plot, aes(fill = last)) +
  scale_fill_viridis_c(name = "Goose barnacle \nCover (%)\n", limits = c(0, 100),na.value = NA,option="magma")+
  theme_bw() +
  coord_sf(xlim = c(extent_raster[1], extent_raster[2]),
           ylim = c(extent_raster[3], extent_raster[4]))+
  annotation_scale(location = "br", width_hint = 0.3,text_cex = 1.5)+
  theme(axis.text = element_text(size=14,color = "black"),legend.text=element_text(size=14,color="black"),
        legend.title = element_text(size=14,color = "black"),panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),plot.title = element_text(size=14,color="black"),
        legend.position = "none")+ggtitle("B) Rapacarallos 2024")+
  scale_x_continuous(breaks = seq(-8.8635,8.8625,by=0.0005))+
  scale_y_continuous(breaks = seq(42.1204,42.1212,by=0.0004))

windows();rel_cover_plot_rapa_24

ggsave("rel_cover_plot_rapa_24.svg",plot=rel_cover_plot_rapa_24,device="svg",path="FIGURES",
       width = 14,height = 14,units = "in")

#estadisticas descriptivas de cobertura

plot(cobertura_relativa_total_rapa_24)
#media
global(cobertura_relativa_total_rapa_24,fun='mean', na.rm=TRUE)
global(cobertura_relativa_total_rapa_24,fun='std', na.rm=TRUE)

hist(cobertura_relativa_total_rapa_24)

#percentiles
quantile(values(cobertura_relativa_total_rapa_24), 
         probs = c(0.25, 0.5, 0.75),
         na.rm = TRUE)


boxplot(values(cobertura_relativa_total_rapa_24))

df_rapa_24<-df_rapa24 %>% 
  mutate(dummy=1) %>% 
  mutate(rel_cover_m=100*rel_cover_m)
mean_rapa_24<-df_rapa_24 %>% 
  reframe(mean=mean(rel_cover_m)) %>% 
  mutate(dummy=1)


boxplot_rapa_24<-ggplot()+
  geom_boxplot(data = df_rapa_24,aes(x=dummy,y=rel_cover_m))+
  geom_point(data=mean_rapa_24,aes(x=dummy,y=mean),colour = "blue",size=3)+
  theme_classic()+ scale_y_continuous(name="Cover (%)")+
  theme(axis.text.x=element_blank(),axis.title.x = element_blank(),
        axis.ticks = element_blank(),axis.text = element_text(size=14,color="black"),
        axis.title = element_text(size=14,color="black"),axis.line.x = element_blank())

boxplot_rapa_24

plot_rapa24<-rel_cover_plot_rapa_24+annotation_custom(ggplotGrob(boxplot_rapa_24),
                                                      xmin=511253, xmax=511293, ymin=4663225  , ymax=4663265    )
plot_rapa24

plot_rapa23
ggarrange(plot_rapa23,plot_rapa24,common.legend = TRUE)




# COVER IN SQUARE METERS
tile_path <- list.files("OUTPUT/tiles_predictions/RAPA_24", pattern = "\\.tif$", full.names = TRUE)
tile_path
tiles<-lapply(tile_path,rast)
clasificacion_total_rapa_24 <- do.call(merge, tiles)
plot(clasificacion_total_rapa_24)
names(clasificacion_total_rapa_24)
clasificacion_total_rapa_24[clasificacion_total_rapa_24==9]<-NA


num_celdas_goose <- freq(clasificacion_total_rapa_24, value = 4)[, "count"]

area_celda <- res(clasificacion_total_rapa_24)[1] * res(clasificacion_total_rapa_24)[2]

area_total_m2_goose <- num_celdas_goose * area_celda
area_total_m2_goose


#area intertidal
num_celdas_tot<-freq(clasificacion_total_rapa_24)
sum(num_celdas_tot$count)

area_intermareal<-sum(num_celdas_tot$count)*area_celda
area_intermareal

area_total_m2_goose/area_intermareal


#TALASO (CUNCHALES)-----
folder<-"OUTPUT/tiles_predictions/TALASO"
raster_files <- list.files(folder, pattern = "\\.tif$", full.names = TRUE)
rel_cover_tiles <- list()
df_tot<-data.frame()

target_res <- 0.5

for (file in raster_files) {
  
  goose <- rast(file)
  
  if (!any(values(goose) == 4, na.rm = TRUE)) {
    message("No hay valor 4 en el tile ", basename(file), " - saltando")
    next
  }
  
  r_bin <- ifel(goose == 4, 1, NA)
  
  res_orig <- res(goose)[1] 
  agg_factor <- ceiling(target_res / res_orig)
  
  r_area_clase <- aggregate(r_bin, fact = agg_factor, fun = sum, na.rm = TRUE)
  
  r_cobertura_relativa <- r_area_clase / (agg_factor^2)
  
  rel_cover_tiles[[length(rel_cover_tiles) + 1]] <- r_cobertura_relativa
  
  df <- as.data.frame(r_cobertura_relativa, xy = TRUE, na.rm = TRUE)
  if (nrow(df) == 0) next
  
  names(df)[3] <- "rel_cover_m"
  df$tile <- basename(file)
  
  df_tot <- rbind(df_tot, df)
  
  print(paste("Procesado:", basename(file)))
}


df_talaso=df_tot
table(df_talaso$tile)
summary(df_talaso)
r_cobertura_relativa



#mosaic relative density goose tiles
if (length(rel_cover_tiles) > 0) {
  cobertura_relativa_total_talaso <- do.call(mosaic, rel_cover_tiles)
} else {
  cobertura_relativa_total_talaso <- NULL
  message("No tiles")
}

plot(cobertura_relativa_total_talaso)
hist(cobertura_relativa_total_talaso)


#FIGURE TALASO

mask_rgb_talaso<-rast("OUTPUT/mask_rgb_talaso.tiff")
extent_raster <- terra::ext(cobertura_relativa_total_talaso)

 cobertura_relativa_total_talaso_plot <- cobertura_relativa_total_talaso * 100
 
 cobertura_relativa_total_talaso_plot[cobertura_relativa_total_talaso_plot < 5] <- NA

rel_cover_plot_talaso<- ggplot()+
  geom_spatraster_rgb(data = mask_rgb_talaso,alpha=.8, interpolate = TRUE) +
  geom_spatraster(data = cobertura_relativa_total_talaso_plot, aes(fill = last)) +
  scale_fill_viridis_c(name = "Goose barnacle \nCover (%)\n", limits = c(0, 100),na.value = NA,option="magma")+
  theme_bw() +
  annotation_scale(location = "br", width_hint = 0.3,text_cex = 1.5)+
  theme(axis.text = element_text(size=14,color = "black"),legend.text=element_text(size=14,color="black"),
        legend.title = element_text(size=14,color = "black"),panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),plot.title = element_text(size=14,color="black"),
        legend.position = "none")+ggtitle("C) Cunchales 2024")+
  scale_x_continuous(breaks = seq(-8.8970,8.8965,by=0.0005))+
  scale_y_continuous(breaks = seq(42.1002,42.1008,by=0.0005))

windows();rel_cover_plot_talaso


#estadisticas descriptivas de cobertura

plot(cobertura_relativa_total_talaso)
#media
global(cobertura_relativa_total_talaso,fun='mean', na.rm=TRUE)
global(cobertura_relativa_total_talaso,fun='std', na.rm=TRUE)

hist(cobertura_relativa_total_talaso)

#percentiles
quantile(values(cobertura_relativa_total_talaso), 
         probs = c(0.25, 0.5, 0.75),
         na.rm = TRUE)


boxplot(values(cobertura_relativa_total_talaso))

df_talaso_ok<-df_talaso %>% 
  mutate(dummy=1) %>% 
  mutate(rel_cover_m=100*rel_cover_m)
mean_talaso<-df_talaso_ok %>% 
  reframe(mean=mean(rel_cover_m)) %>% 
  mutate(dummy=1)


boxplot_talaso<-ggplot()+
  geom_boxplot(data = df_talaso_ok,aes(x=dummy,y=rel_cover_m))+
  geom_point(data=mean_talaso,aes(x=dummy,y=mean),colour = "blue",size=3)+
  theme_classic()+ scale_y_continuous(name="Cover (%)")+
  theme(axis.text.x=element_blank(),axis.title.x = element_blank(),
        axis.ticks = element_blank(),axis.text = element_text(size=14,color="black"),
        axis.title = element_text(size=14,color="black"),axis.line.x = element_blank())

boxplot_talaso

plot_talaso<-rel_cover_plot_talaso+annotation_custom(ggplotGrob(boxplot_talaso),
                                                      xmin=508480, xmax=508512, ymin=4660885  , ymax=4660915    )
plot_talaso

# COVER IN SQUARE METERS
tile_path <- list.files("OUTPUT/tiles_predictions/TALASO", pattern = "\\.tif$", full.names = TRUE)
tile_path
tiles<-lapply(tile_path,rast)
clasificacion_total_talaso <- do.call(merge, tiles)
plot(clasificacion_total_talaso)
names(clasificacion_total_talaso)
clasificacion_total_talaso[clasificacion_total_talaso==9]<-NA


num_celdas_goose <- freq(clasificacion_total_talaso, value = 4)[, "count"]

area_celda <- res(clasificacion_total_talaso)[1] * res(clasificacion_total_talaso)[2]

area_total_m2_goose <- num_celdas_goose * area_celda
area_total_m2_goose


#area intertidal
num_celdas_tot<-freq(clasificacion_total_talaso)
sum(num_celdas_tot$count)

area_intermareal<-sum(num_celdas_tot$count)*area_celda
area_intermareal

area_total_m2_goose/area_intermareal


#plot three sites-----
plot_goose_density<-ggarrange(plot_rapa23,plot_rapa24,plot_talaso,nrow=3,align="hv",common.legend = TRUE,legend = "right")
plot_goose_density

ggsave("plot_goose_density.svg",plot=plot_goose_density,device="svg", path="FIGURES",width=7,height=15,units="in")

