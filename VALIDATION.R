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
library(sf)
library(caret)

setwd("E:/POSDOC XUNTA 2023/MULTISPECTRAL LIDAR")




##GENERAR RASTER BINARIO (1=PERCEBE, 0=NO PERCEBE) HACER SOLO UNA VEZ----
#RAPACARALLOS 23

folder<-"OUTPUT/tiles_predictions/RAPA_23"
raster_files <- list.files(folder, pattern = "\\.tif$", full.names = TRUE)
dir.create("OUTPUT/BINARY/RAPA_23", recursive = TRUE, showWarnings = FALSE)

ras<-rast(raster_files[34])
plot(ras)
unique(values(ras))
levels(ras)
cats(ras)
ras

rasters_list <- list()

for (file in raster_files) {
  
  r <- rast(file)
  
  if (!any(values(r) == 4, na.rm = TRUE)) {
    message("No hay valor 4 en el tile ", basename(file), " - saltando")
    next
  }
  
  r_bin <- ifel(r == 4, 1, 0)
  
  output_path <- file.path( "OUTPUT/BINARY/RAPA_23",  paste0("bin_", basename(file)))
  
  writeRaster(r_bin, output_path, overwrite = TRUE)
  
  rasters_list[[basename(file)]] <- r_bin  
  
  print(paste("Procesado:", basename(file)))
}


bin_files <- list.files("OUTPUT/BINARY/RAPA_23", pattern = "\\.tif$", full.names = TRUE)
r_final <- do.call(merge, lapply(bin_files, rast))
plot(r_final)
writeRaster(r_final,"OUTPUT/BINARY/RAPA_23/RAPA_BINARY_23.tiff")


#RAPACARALLOS 2024


folder<-"OUTPUT/tiles_predictions/RAPA_24"
raster_files <- list.files(folder, pattern = "\\.tif$", full.names = TRUE)
dir.create("OUTPUT/BINARY/RAPA_24", recursive = TRUE, showWarnings = FALSE)

ras<-rast(raster_files[24])
plot(ras)
unique(values(ras))
levels(ras)
cats(ras)
ras

rasters_list <- list()

for (file in raster_files) {
  
  r <- rast(file)
  
  if (!any(values(r) == 4, na.rm = TRUE)) {
    message("No hay valor 4 en el tile ", basename(file), " - saltando")
    next
  }
  
  r_bin <- ifel(r == 4, 1, 0)
  
  output_path <- file.path( "OUTPUT/BINARY/RAPA_24",  paste0("bin_", basename(file)))
  
  writeRaster(r_bin, output_path, overwrite = TRUE)
  
  rasters_list[[basename(file)]] <- r_bin  
  
  print(paste("Procesado:", basename(file)))
}


bin_files <- list.files("OUTPUT/BINARY/RAPA_24", pattern = "\\.tif$", full.names = TRUE)
r_final <- do.call(merge, lapply(bin_files, rast))
plot(r_final)
writeRaster(r_final,"OUTPUT/BINARY/RAPA_24/RAPA_BINARY_24.tiff")


#CUNCHALES 2024



folder<-"OUTPUT/tiles_predictions/TALASO"
raster_files <- list.files(folder, pattern = "\\.tif$", full.names = TRUE)
dir.create("OUTPUT/BINARY/CUNCHALES_24", recursive = TRUE, showWarnings = FALSE)

ras<-rast(raster_files[14])
plot(ras)
unique(values(ras))
levels(ras)
cats(ras)
ras

rasters_list <- list()

for (file in raster_files) {
  
  r <- rast(file)
  
  if (!any(values(r) == 4, na.rm = TRUE)) {
    message("No hay valor 4 en el tile ", basename(file), " - saltando")
    next
  }
  
  r_bin <- ifel(r == 4, 1, 0)
  
  output_path <- file.path( "OUTPUT/BINARY/CUNCHALES_24",  paste0("bin_", basename(file)))
  
  writeRaster(r_bin, output_path, overwrite = TRUE)
  
  rasters_list[[basename(file)]] <- r_bin  
  
  print(paste("Procesado:", basename(file)))
}


bin_files <- list.files("OUTPUT/BINARY/CUNCHALES_24", pattern = "\\.tif$", full.names = TRUE)
r_final <- do.call(merge, lapply(bin_files, rast))
plot(r_final)
writeRaster(r_final,"OUTPUT/BINARY/CUNCHALES_24/CUNCHALES_BINARY_24.tiff")

##CARGAR SHAPE DE VALIDACION Y EXTRAER VALORES DEL RASTER----
##2023 RAPACARALLOS----

r_final<-rast("OUTPUT/BINARY/RAPA_23/RAPA_BINARY_23.tiff")
r_final
unique(r_final$last)



###CHECHU----
validation_t<-st_read("DATA/VALIDATION/TXETXU/VALIDATION RAPACARALLOS 2023.shp")
validation_t<-validation_t %>% 
  na.omit()
str(validation_t)
st_crs(validation_t)


plot(validation_t)

validation_t <- validation_t[!st_is_empty(validation_t), ]

valid_vs_pred_2023_t <- terra::extract(r_final, validation_t)
str(valid_vs_pred_2023_t)

i <- 55
valid_vs_pred_2023_t[i, ]
validation_t[2, ]

valid_vs_pred_2023_t$id <- validation_t$id[valid_vs_pred_2023_t$ID]
table(valid_vs_pred_2023_t$id)

valid_vs_pred_2023_t<-valid_vs_pred_2023_t %>% 
  select(-ID)
head(valid_vs_pred_2023_t)
unique(valid_vs_pred_2023_t$id)



###DAVID----
validation_d<-st_read("DATA/VALIDATION/DAVID/VALIDATION RAPACARALLOS 2023.shp")
validation_d<-validation_d %>% 
  na.omit()
str(validation_d)
st_crs(validation_d)
plot(validation_d)
#filter outlier sizes
validation_d$area <- st_area(validation_d)
hist(as.numeric(validation_d$area), breaks = 50)
threshold <- quantile(validation_d$area, 0.8)
validation_d <- validation_d[validation_d$area <= threshold, ]
hist(as.numeric(validation_d$area), breaks = 50)

#negative buffer 
validation_d<-st_buffer(validation_d, dist = -0.02)
unique(validation_d$id)
validation_d<-validation_d %>% 
  dplyr::select(-area)
plot(validation_d)

validation_d <- validation_d[!st_is_empty(validation_d), ]


valid_vs_pred_2023_d <- terra::extract(r_final, validation_d)
str(valid_vs_pred_2023_d)

 

valid_vs_pred_2023_d$id <- validation_d$id[valid_vs_pred_2023_d$ID]
table(valid_vs_pred_2023_d$id)

valid_vs_pred_2023_d<-valid_vs_pred_2023_d %>% 
  select(-ID)
head(valid_vs_pred_2023_d)
unique(valid_vs_pred_2023_d$id)



valid_vs_pred_2023<-rbind(valid_vs_pred_2023_d,
                          valid_vs_pred_2023_t)
table(valid_vs_pred_2023$id)
conf_matrix <- confusionMatrix(factor(valid_vs_pred_2023$last),factor(valid_vs_pred_2023$id), positive = "1")
conf_matrix
 

#RAPACARALLOS 2024----

r_final<-rast("OUTPUT/BINARY/RAPA_24/RAPA_BINARY_24.tiff")
r_final
unique(r_final$last)

##CELIA----
validation_c<-st_read("DATA/VALIDATION/CELIA/VALIDATION RAPACARALLOS 2024.shp")
validation_c


str(validation_c)
st_crs(validation_c)

#filter outlier sizes
validation_c$area <- st_area(validation_c)
hist(as.numeric(validation_c$area), breaks = 50)
threshold <- quantile(validation_c$area, 0.80)
validation_c <- validation_c[validation_c$area <= threshold, ]
hist(as.numeric(validation_c$area), breaks = 50)

#negative buffer 
validation_c<-st_buffer(validation_c, dist = -0.02)

validation_c <- validation_c[!st_is_empty(validation_c), ]

unique(validation_c$id)
validation_c<-validation_c %>% 
  dplyr::select(-area)
plot(validation_c)

valid_vs_pred_2024_c <- terra::extract(r_final, validation_c)

str(valid_vs_pred_2024_c)

valid_vs_pred_2024_c$id <- validation_c$id[valid_vs_pred_2024_c$ID]
str(valid_vs_pred_2024_c)
table(valid_vs_pred_2024_c$id)


valid_vs_pred_2024_c<-valid_vs_pred_2024_c %>% 
  select(-ID)
head(valid_vs_pred_2024_c)
unique(valid_vs_pred_2024_c$id)
plot(valid_vs_pred_2024_c)


###CHECHU----
validation_t<-st_read("DATA/VALIDATION/TXETXU/VALIDATION RAPACARALLOS 2024.shp")
validation_t<-validation_t %>% 
  na.omit()
str(validation_t)
st_crs(validation_t)

#filter outlier sizes
validation_t$area <- st_area(validation_t)
hist(as.numeric(validation_t$area), breaks = 50)
# threshold <- quantile(validation_t$area, 0.9)
# validation_t <- validation_t[validation_t$area <= threshold, ]
# hist(as.numeric(validation_t$area), breaks = 50)

 
# validation_t <- validation_t[!st_is_empty(validation_t), ]

unique(validation_t$id)
validation_t<-validation_t %>% 
  dplyr::select(-area)
plot(validation_t)

valid_vs_pred_2024_t <- terra::extract(r_final, validation_t)
str(valid_vs_pred_2024_t)

 

valid_vs_pred_2024_t$id <- validation_t$id[valid_vs_pred_2024_t$ID]
table(valid_vs_pred_2024_t$id)

valid_vs_pred_2024_t<-valid_vs_pred_2024_t %>% 
  select(-ID)
head(valid_vs_pred_2024_t)
unique(valid_vs_pred_2024_t$id)

conf_matrix <- confusionMatrix(factor(valid_vs_pred_2024_t$last),
                               factor(valid_vs_pred_2024_t$id),positive="1")
conf_matrix

#DAVID
validation_d<-st_read("DATA/VALIDATION/DAVID/VALIDATION RAPACARALLOS 2024.shp")

validation_d$area <- st_area(validation_d)
hist(as.numeric(validation_d$area), breaks = 50)
threshold <- quantile(validation_d$area, 0.80)
validation_d <- validation_d[validation_d$area <= threshold, ]
hist(as.numeric(validation_d$area), breaks = 50)

validation_d<-st_buffer(validation_d, dist = -0.02)
validation_d <- validation_d[!st_is_empty(validation_d), ]

str(validation_d)
unique(validation_d$id)
validation_d<-validation_d %>% 
  dplyr::select(-area)
plot(validation_d)

valid_vs_pred_2024_d <- terra::extract(r_final, validation_d)
str(valid_vs_pred_2024_d)


valid_vs_pred_2024_d$id <- validation_d$id[valid_vs_pred_2024_d$ID]
table(valid_vs_pred_2024_d$id)


valid_vs_pred_2024_d<-valid_vs_pred_2024_d %>% 
  select(-ID)
head(valid_vs_pred_2024_d)
unique(valid_vs_pred_2024_d$id)


valid_vs_pred_2024<-rbind(valid_vs_pred_2024_c,
                          valid_vs_pred_2024_t,valid_vs_pred_2024_d)




##CUNCHALES 2024----

r_final<-rast("OUTPUT/BINARY/CUNCHALES_24/CUNCHALES_BINARY_24.tiff")
r_final
unique(r_final$last)

###CELIA----
validation_c<-st_read("DATA/VALIDATION/CELIA/VALIDATION_CUNCHALES_2024.shp")
validation_c


str(validation_c)
st_crs(validation_c)
graphics.off()

#filter outlier sizes
validation_c$area <- st_area(validation_c)
hist(as.numeric(validation_c$area), breaks = 50)
threshold <- quantile(validation_c$area, 0.80)
validation_c <- validation_c[validation_c$area <= threshold, ]
hist(as.numeric(validation_c$area), breaks = 50)

#negative buffer of 2 cm
validation_c<-st_buffer(validation_c, dist = -0.02)
validation_c <- validation_c[!st_is_empty(validation_c), ]

unique(validation_c$id)
validation_c<-validation_c %>% 
  dplyr::select(-area)
plot(validation_c)

valid_vs_pred_cunchales_c <- terra::extract(r_final, validation_c)

str(valid_vs_pred_cunchales_c)

valid_vs_pred_cunchales_c$id <- validation_c$id[valid_vs_pred_cunchales_c$ID]
str(valid_vs_pred_cunchales_c)
table(valid_vs_pred_cunchales_c$id)


valid_vs_pred_cunchales_c<-valid_vs_pred_cunchales_c %>% 
  select(-ID)
head(valid_vs_pred_cunchales_c)
unique(valid_vs_pred_cunchales_c$id)
plot(valid_vs_pred_cunchales_c)



###BARBARA----
validation_b<-st_read("DATA/VALIDATION/BARBARA/VALIDATION_CUNCHALES_2024.shp")

validation_b$area <- st_area(validation_b)

hist(as.numeric(validation_b$area), breaks = 50)
threshold <- quantile(validation_b$area, 0.80)
validation_b <- validation_b[validation_b$area <= threshold, ]
hist(as.numeric(validation_b$area), breaks = 50)

validation_b<-st_buffer(validation_b, dist = -0.02)
validation_b <- validation_b[!st_is_empty(validation_b), ]


str(validation_b)
unique(validation_b$id)
validation_b<-validation_b %>% 
  dplyr::select(-area)
plot(validation_b)

valid_vs_pred_cunchales_b <- terra::extract(r_final, validation_b)
str(valid_vs_pred_cunchales_b)


valid_vs_pred_cunchales_b$id <- validation_b$id[valid_vs_pred_cunchales_b$ID]
table(valid_vs_pred_cunchales_b$id)


valid_vs_pred_cunchales_b<-valid_vs_pred_cunchales_b %>% 
  select(-ID)
head(valid_vs_pred_cunchales_b)
unique(valid_vs_pred_cunchales_b$id)


#CHECHU----

validation_t<-st_read("DATA/VALIDATION/TXETXU/VALIDATION_CUNCHALES_2024.shp")
validation_t<-validation_t %>% 
  na.omit()
str(validation_t)
st_crs(validation_t)

#filter outlier sizes
validation_t$area <- st_area(validation_t)
hist(as.numeric(validation_t$area), breaks = 50)
# threshold <- quantile(validation_t$area, 0.9)
# validation_t <- validation_t[validation_t$area <= threshold, ]
# hist(as.numeric(validation_t$area), breaks = 50)
# 

unique(validation_t$id)
validation_t<-validation_t %>% 
  dplyr::select(-area)
plot(validation_t)
validation_t <- validation_t[!st_is_empty(validation_t), ]

valid_vs_pred_cunchales_t <- terra::extract(r_final, validation_t)
str(valid_vs_pred_cunchales_t)



valid_vs_pred_cunchales_t$id <- validation_t$id[valid_vs_pred_cunchales_t$ID]
table(valid_vs_pred_cunchales_t$id)

valid_vs_pred_cunchales_t<-valid_vs_pred_cunchales_t %>% 
  select(-ID)
head(valid_vs_pred_cunchales_t)
unique(valid_vs_pred_cunchales_t$id)



valid_vs_pred_cunchales<-rbind(valid_vs_pred_cunchales_c,valid_vs_pred_cunchales_b,
                               valid_vs_pred_cunchales_t)



validation_dataframe<-rbind(valid_vs_pred_2023,valid_vs_pred_2024,
                            valid_vs_pred_cunchales)
conf_matrix <- confusionMatrix(factor(validation_dataframe$last),factor(validation_dataframe$id),
                               positive="1")
conf_matrix

precision <- conf_matrix$byClass["Pos Pred Value"]
recall <- conf_matrix$byClass["Sensitivity"]

F1 <- 2 * (precision * recall) / (precision + recall)
F1
24341/(13992+24341)
38286/(38286+7020)
24341/(7020+24341)
38286/(38286+13992)

#FIGURE CONFUSION MATRIX

mat <- conf_matrix$table
mat

conf_df <- as.data.frame(mat)



conf_df
conf_df$Prediction=factor(conf_df$Prediction,
                          labels=c("Non-Goose barnacle", "Goose barnacle"))

conf_df$Reference=factor(conf_df$Reference,
                          labels=c("Non-Goose barnacle", "Goose barnacle"))

labels <- data.frame( x = c(2, 1,2,1),
  y = c(2.6, 2.6,2.75,2.75),
  label = c("Sensitivity = 0.64", "Specificity = 0.85",
            "PPV = 0.78", "NPV = 0.73"))


binary_plot<-ggplot(conf_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") + geom_label(aes(label = Freq),
                                          size = 4,
                                          fill = "white",
                                          color = "black") +
  scale_fill_viridis_c(name="Pixels",option="G") +
  labs(x = "Truth",y = "Prediction") +  theme_minimal()+
  theme(axis.text = element_text(size=12,color="black"),
        axis.title = element_text(size=12,color="black"),
        legend.text = element_text(size=12,color="black"),
        axis.text.y = element_text(margin = margin(r = 0)))+
  geom_label(data = labels,aes(x = x, y = y, label = label),
             inherit.aes = FALSE,fill = "white",label.size = 0.3)+
  coord_cartesian(clip = "off") +
    scale_y_discrete(expand = expansion(add = c(0.1, .85))) +
  theme(panel.grid = element_blank())

binary_plot
ggsave("Figure_6.svg",plot=binary_plot,device= "svg",path = "FIGURES/",
       width=6.2,height=3.8,units="in")
