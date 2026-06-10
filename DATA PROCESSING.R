#MULTISPECTRAL LIDAR
#DATA PROCESSING
##MARTA ROMÁN

rm(list = ls())
Sys.setenv(LANG = "en")
library(terra)
library(tidyverse)
library(tidyterra)
library(sf)
future::plan()
setwd("")


##2023 RAPA----
#LOAD MULTISPECTRAL AND DEMS 
multi_23<-rast( "Vigo_Site1_DualMX_32629_corrected.tif")
multi_23$Vigo_Site1_DualMX_32629_corrected_1
plotRGB(multi_23, r = 6, g = 3, b = 1,stretch = "hist")
plot(multi_23)
multi_23
multi_23[multi_23 == 65535] <- NA
plot(multi_23)
crs(multi_23,proj=TRUE)

##DEM
dem_23 <- rast( "Vigo_Site1_DEM_Lidar_32629_7mm.tif")
windows();plot(dem_23)
dem_23
crs(dem_23,proj=TRUE)

terra::hist(dem_23)

na_mask <- is.na(dem_23)
plot(na_mask, main = "Píxeles NA en el DEM")

#RESAMPLE 2023 DEM TO MULTISPECTRAL
dem_2023_resampled <- resample(dem_23, multi_23)
plot(dem_2023_resampled)

dem_2023_mask <- mask(dem_2023_resampled, multi_23[[1]])
plot(dem_2023_mask)

dem_2023_mask
multi_23



mask_both <- !is.na(dem_2023_mask) & !is.na(multi_23[[1]]) 


plot(mask_both)


dem_valid   <- mask(dem_2023_mask, mask_both, maskvalue = FALSE)
multi_valid <- mask(multi_23, mask_both, maskvalue = FALSE)

plot(dem_valid)
plot(multi_valid)

writeRaster(dem_valid,"OUTPUT/DEM_2023_OK.tiff",overwrite=TRUE)
writeRaster(multi_valid,"OUTPUT/MULTI_2023_OK.tiff",overwrite=TRUE)

##2024 RAPA----
##MULTISPECTRAL
multi_24<-rast( "Vigo_1_2024_MULTI.tif")
multi_24$Vigo_1_2024_MULTI_1
plotRGB(multi_24, r = 6, g = 3, b = 1,stretch = "hist")
multi_24

##DEM
dem_24 <- rast( "Vigo_1_2024_DEM.tif")
windows();plot(dem_24)
dem_24
crs(dem_24,proj=TRUE)
windows();plot(dem_24)

terra::hist(dem_24)

##PROJECT TO 32629
dem_24_prj<-project(dem_24,"EPSG:32629")
dem_24_prj
plot(dem_24_prj)


multi_24_prj<-project(multi_24,"EPSG:32629")
plotRGB(multi_24_prj, r = 6, g = 3, b = 1,stretch = "hist")

#RESAMPLE DEM 2024 TO MULTI 2024 

dem_24_resampled<-resample(dem_24_prj,multi_24_prj)
dem_24_resampled
plot(dem_24_resampled)
dem_2024_mask <- mask(dem_24_resampled, multi_24_prj[[1]])
plot(dem_2024_mask)



# #REMOVE WATER

#keep areas with pixels in both 
mask_both <- !is.na(dem_2024_mask) & !is.na(multi_24_prj[[1]]) 

plot(mask_both)


dem_2024_mask   <- mask(dem_2024_mask, mask_both, maskvalue = FALSE)
multi_valid <- mask(multi_24_prj, mask_both, maskvalue = FALSE)

windows();plot(dem_2024_mask)
windows();plot(multi_valid)


writeRaster(multi_24_prj,"OUTPUT/MULTI_24_OK.tiff",overwrite=TRUE)

##CORRECT DEM 2024 ELEVATION
# KEEP AREAS WITHOUT NA IN BOTH
dem_valid<-rast("OUTPUT/DEM_2023_OK.tiff")

crs(dem_2024_mask) <- crs(dem_valid)

dem_2024_resampled <- resample(dem_2024_mask, dem_valid, method = "bilinear") 

mask <- !is.na(dem_valid) & !is.na(dem_2024_resampled)
plot(mask)

dem_23_masked_ok <- mask(dem_valid, mask, maskvalue = FALSE)
windows();plot(dem_23_masked_ok)

dem_24_masked_ok <- mask(dem_2024_resampled, mask, maskvalue = FALSE)
plot(dem_24_masked_ok)

#difference
diff <- dem_24_masked_ok - dem_23_masked_ok


plot(diff)
diff
summary(diff)
hist(diff)
boxplot(diff)

writeRaster(diff,"OUTPUT/DEM_DIFF_24_23.tiff",overwrite=TRUE)

#calculate the mode
vals <- values(diff)
mode_value <- as.numeric(names(sort(table(vals), decreasing = TRUE)[1]))
mode_value

#SUBSTRACT
dem_24_corrected<-dem_24_masked_ok-mode_value
dem_24_corrected
windows();plot(dem_24_corrected)
windows();plot(dem_23_masked_ok)

summary(dem_24_corrected)
summary(dem_23_masked_ok)

hist(dem_23_masked_ok)
hist(dem_24_corrected)
plot(dem_24_corrected)


writeRaster(dem_24_corrected,"OUTPUT/DEM_2024_OK.tiff",overwrite=TRUE)

#TALASO (CUNCHALES)-----

multi_talaso_2024<-rast("Vigo_2_2024_MULTI.tif")
plot(multi_talaso_2024)
plotRGB(multi_talaso_2024,r=6,g=4,b=2,stretch="hist")

dem_talaso_2024<-rast("Vigo_2_2024_DEM.tif")
plot(dem_talaso_2024)
summary(dem_talaso_2024)

#PROJECT 2024 TO 32629 

crs(dem_23,proj=TRUE)
crs(dem_talaso_2024,proj=TRUE)

dem_talaso_prj<-project(dem_talaso_2024,"EPSG:32629")
dem_talaso_prj
plot(dem_talaso_prj)


multi_talaso_prj<-project(multi_talaso_2024,"EPSG:32629")
plotRGB(multi_talaso_prj, r = 6, g = 3, b = 1,stretch = "hist")
plot(multi_talaso_prj)


## RESAMPLE DEM TO MULTI
dem_talaso_resampled <- resample(dem_talaso_prj, multi_talaso_prj)
plot(dem_talaso_resampled)

dem_talaso_mask <- mask(dem_talaso_resampled, multi_talaso_prj[[1]])
plot(dem_talaso_mask)

dem_talaso_mask
multi_talaso_prj


writeRaster(multi_talaso_prj,"OUTPUT/MULTI_TALASO_OK.tiff",overwrite=TRUE)



dem_talaso_corrected<-dem_talaso_mask-mode_value
dem_talaso_corrected
plot(dem_talaso_corrected)
summary(dem_talaso_corrected)
summary(dem_23)

hist(dem_talaso_corrected)
writeRaster(dem_talaso_corrected,"OUTPUT/DEM_TALASO_OK.tiff",overwrite=TRUE)


#TOPOGRAPHIC INDEXES-----
# https://zia207.quarto.pub/digital-terrain-analysis.html#terrain-analysis-in-r
# https://rspatial.org/spatial/index.html
# https://datacarpentry.github.io/r-raster-vector-geospatial/01-raster-structure


###2023 RAPACARALLOS----
#resample to multi


#9mm pixel size. resample 

##TRI

#loop for checking resolution
multi_rapa_23 <- rast("OUTPUT/MULTI_2023_OK.tiff")
dem_rapa_2023 <- rast("OUTPUT/DEM_2023_OK.tiff")

 res_list <- c(0.01,0.02, 0.04, 0.05, 0.10, 0.15, 0.20,0.25, 0.5,0.75,1)   

for (res in res_list) {
  
  template <- rast(
    ext(dem_rapa_2023),
    resolution = res,
    crs = crs(dem_rapa_2023)  )
  
  dem_resampled <- resample(dem_rapa_2023, template, method = "bilinear")
  
  TRI_res <- terrain(dem_resampled, v = "TRI")
  
  windows()
  
  plot(
    TRI_res,
    main = paste0("TRI - Resolución: ", res, " m")  )
}

#20 CM
multi_rapa_23 <- rast("OUTPUT/MULTI_2023_OK.tiff")
dem_rapa_2023 <- rast("OUTPUT/DEM_2023_OK.tiff")

na_mask <- is.na(dem_rapa_2023)
plot(na_mask, main = "Píxeles NA en el DEM")


new_res <- 0.2
template <- rast(
  ext(dem_rapa_2023),
  resolution = new_res,
  crs = crs(dem_rapa_2023))

dem_rapa_2023 <- resample(dem_rapa_2023, template, method = "bilinear")
dem_rapa_2023





TRI_rapa_23<- terrain(dem_rapa_2023, v="TRI")
TRI_rapa_23
windows();plot(TRI_rapa_23)
TRI_rapa_23<-resample(TRI_rapa_23,multi_rapa_23)
TRI_rapa_23
writeRaster(TRI_rapa_23, "OUTPUT/TRI_rapa_23.tif", overwrite=TRUE)


#TPI
#loop for checking resolution

multi_rapa_23 <- rast("OUTPUT/MULTI_2023_OK.tiff")
dem_rapa_2023 <- rast("OUTPUT/DEM_2023_OK.tiff")

res_list <- c(0.01, 0.05, 0.10, 0.15, 0.20,0.25,0.3,0.4, 0.5,0.75,1)   

for (res in res_list) {
  
  template <- rast(
    ext(dem_rapa_2023),
    resolution = res,
    crs = crs(dem_rapa_2023)
  )
  
  dem_resampled <- resample(dem_rapa_2023, template, method = "bilinear")
  
  TRI_res <- terrain(dem_resampled, v = "TRI")
  
  windows()
  
  plot(
    TRI_res,
    main = paste0("TRI - Resolución: ", res, " m")
  )
}



# Create a template raster with desired resolution
new_res <- 0.5
template <- rast(
  ext(dem_rapa_2023),
  resolution = new_res,
  crs = crs(dem_rapa_2023))

dem_rapa_2023 <- resample(dem_rapa_2023, template, method = "bilinear")

TPI_rapa_23<- terrain(dem_rapa_2023, v="TPI")
TPI_rapa_23
windows();plot(TPI_rapa_23)
TPI_rapa_23<-resample(TPI_rapa_23,multi_rapa_23)
TPI_rapa_23
writeRaster(TPI_rapa_23, "OUTPUT/TPI_rapa_23.tif", overwrite=TRUE)


#slope

multi_rapa_23 <- rast("OUTPUT/MULTI_2023_OK.tiff")
dem_rapa_2023 <- rast("OUTPUT/DEM_2023_OK.tiff")
 res_list <- c(0.01, 0.05, 0.10, 0.15, 0.20,0.25,0.3,0.4, 0.5,0.75,1)   

for (res in res_list) {
  
  template <- rast(
    ext(dem_rapa_2023),
    resolution = res,
    crs = crs(dem_rapa_2023)
  )
  
  dem_resampled <- resample(dem_rapa_2023, template, method = "bilinear")
  
  slope_res <- terrain(dem_resampled, v="slope",unit='degrees')
  
  windows()
  
  plot(
    slope_res,
    main = paste0("slope - Resolución: ", res, " m")
  )
}

 new_res <- 0.20
 template <- rast(
   ext(dem_rapa_2023),
   resolution = new_res,
   crs = crs(dem_rapa_2023))
 
 dem_rapa_2023 <- resample(dem_rapa_2023, template, method = "bilinear")



slope_rapa_23  <- terra::terrain(dem_rapa_2023,v="slope",unit='degrees')
windows();plot(slope_rapa_23)
slope_rapa_23<-resample(slope_rapa_23,multi_rapa_23)
slope_rapa_23
writeRaster(slope_rapa_23, "OUTPUT/slope_rapa_23.tif", overwrite = TRUE)

#aspect 
multi_rapa_23 <- rast("OUTPUT/MULTI_2023_OK.tiff")
dem_rapa_2023 <- rast("OUTPUT/DEM_2023_OK.tiff")
res_list <- c(0.01, 0.05, 0.10, 0.15, 0.20,0.25,0.3,0.4, 0.5,0.75,1)   

for (res in res_list) {
  
  template <- rast(
    ext(dem_rapa_2023),
    resolution = res,
    crs = crs(dem_rapa_2023)
  )
  
  dem_resampled <- resample(dem_rapa_2023, template, method = "bilinear")
  
  aspect_res <- terrain(dem_resampled, v="aspect",unit='radians')
  aspect_res<-cos(aspect_res)
   windows()
  
  plot(    aspect_res,
    main = paste0("aspect - Resolución: ", res, " m")  )
}


new_res <- 0.20
template <- rast(
  ext(dem_rapa_2023),
  resolution = new_res,
  crs = crs(dem_rapa_2023))

dem_rapa_2023 <- resample(dem_rapa_2023, template, method = "bilinear")

aspect_rapa_23  <- terra::terrain(dem_rapa_2023,v="aspect",unit='radians')
windows();plot(aspect_rapa_23)
aspect_rapa_23<-cos(aspect_rapa_23)
windows();plot(aspect_rapa_23)
aspect_rapa_23<-resample(aspect_rapa_23,multi_rapa_23)
aspect_rapa_23
writeRaster(aspect_rapa_23, "OUTPUT/aspect_rapa_2023.tif", overwrite = TRUE)


###2024 RAPACARALLOS----
multi_rapa_24<-rast("OUTPUT/MULTI_24_OK.tiff")
dem_rapa_2024<-rast("OUTPUT/DEM_2024_OK.tiff")
plot(dem_rapa_2024)
dem_rapa_2024

##TRI


new_res <- 0.2
template <- rast(
  ext(dem_rapa_2024),
  resolution = new_res,
  crs = crs(dem_rapa_2024))

dem_rapa_2024 <- resample(dem_rapa_2024, template, method = "bilinear")
plot(dem_rapa_2024)
dem_rapa_2024

TRI_rapa_24<- terrain(dem_rapa_2024, v="TRI")
windows();plot(TRI_rapa_24)
TRI_rapa_24
TRI_rapa_24<-resample(TRI_rapa_24,multi_rapa_24)
TRI_rapa_24
writeRaster(TRI_rapa_24, "OUTPUT/TRI_rapa_24.tif", overwrite=TRUE)

#TPI
multi_rapa_24<-rast("OUTPUT/MULTI_24_OK.tiff")
dem_rapa_2024<-rast("OUTPUT/DEM_2024_OK.tiff")
plot(dem_rapa_2024)
dem_rapa_2024

 

new_res <-  0.5
template <- rast(
  ext(dem_rapa_2024),
  resolution = new_res,
  crs = crs(dem_rapa_2024))

dem_rapa_2024 <- resample(dem_rapa_2024, template, method = "bilinear")
plot(dem_rapa_2024)
dem_rapa_2024

TPI_rapa_24<- terrain(dem_rapa_2024, v="TPI")
windows();plot(TPI_rapa_24)
TPI_rapa_24
TPI_rapa_24<-resample(TPI_rapa_24,multi_rapa_24)
TPI_rapa_24
writeRaster(TPI_rapa_24, "OUTPUT/TPI_rapa_24.tif", overwrite=TRUE)



#slope
multi_rapa_24<-rast("OUTPUT/MULTI_24_OK.tiff")
dem_rapa_2024<-rast("OUTPUT/DEM_2024_OK.tiff")
plot(dem_rapa_2024)
dem_rapa_2024

new_res <-  0.2
template <- rast(  ext(dem_rapa_2024),
  resolution = new_res,
  crs = crs(dem_rapa_2024))

dem_rapa_2024 <- resample(dem_rapa_2024, template, method = "bilinear")
plot(dem_rapa_2024)
dem_rapa_2024


slope_rapa_24  <- terra::terrain(dem_rapa_2024,v="slope",unit='degrees')
windows();plot(slope_rapa_24)
slope_rapa_24
slope_rapa_24<-resample(slope_rapa_24,multi_rapa_24)
slope_rapa_24
writeRaster(slope_rapa_24, "OUTPUT/slope_rapa_24.tif", overwrite = TRUE)


#aspect (northness)
dem_rapa_2024<-rast("OUTPUT/DEM_2024_OK.tiff")
new_res <-  0.2
template <- rast(
  ext(dem_rapa_2024),
  resolution = new_res,
  crs = crs(dem_rapa_2024))

dem_rapa_2024 <- resample(dem_rapa_2024, template, method = "bilinear")
plot(dem_rapa_2024)
dem_rapa_2024

aspect_rapa_24  <- terra::terrain(dem_rapa_2024,v="aspect",unit='radians')
aspect_rapa_24<-cos(aspect_rapa_24)
aspect_rapa_24
aspect_rapa_24<-resample(aspect_rapa_24,multi_rapa_24)
windows();plot(aspect_rapa_24)

writeRaster(aspect_rapa_24, "OUTPUT/aspect_rapa_24.tif", overwrite = TRUE)

###2024 TALASO (CUNCHALES)----
multi_talaso_24<-rast("OUTPUT/MULTI_TALASO_OK.tiff")
dem_talaso_2024<-rast("OUTPUT/DEM_TALASO_OK.tiff")
plot(dem_talaso_2024)
dem_talaso_2024

#TRI
res_list <- c(0.01, 0.05, 0.10, 0.15, 0.20,0.25,0.3,0.4, 0.5,0.75,1)   

for (res in res_list) {
  
  template <- rast(
    ext(dem_talaso_2024),
    resolution = res,
    crs = crs(dem_talaso_2024)
  )
  
  dem_resampled <- resample(dem_talaso_2024, template, method = "bilinear")
  
  TRI_res <- terrain(dem_resampled, v="TRI")
  windows()
  
  plot(
    TRI_res,
    main = paste0("TRI - Resolución: ", res, " m")
  )
}

new_res <- 0.2
template <- rast(
  ext(dem_talaso_2024),
  resolution = new_res,
  crs = crs(dem_talaso_2024))

dem_talaso_2024 <- resample(dem_talaso_2024, template, method = "bilinear")
plot(dem_talaso_2024)
dem_talaso_2024

TRI_talaso_24<- terrain(dem_talaso_2024, v="TRI")
windows();plot(TRI_talaso_24)
TRI_talaso_24<-resample(TRI_talaso_24,multi_talaso_24)
TRI_talaso_24
writeRaster(TRI_talaso_24, "OUTPUT/TRI_talaso_24.tif", overwrite=TRUE)

#TPI

multi_talaso_24<-rast("OUTPUT/MULTI_TALASO_OK.tiff")
dem_talaso_2024<-rast("OUTPUT/DEM_TALASO_OK.tiff")


res_list <- c(0.01, 0.05, 0.10, 0.15, 0.20,0.25,0.3,0.4, 0.5,0.75,1)   

for (res in res_list) {
  
  template <- rast(
    ext(dem_talaso_2024),
    resolution = res,
    crs = crs(dem_talaso_2024)  )
  
  dem_resampled <- resample(dem_talaso_2024, template, method = "bilinear")
  
  TPI_res <- terrain(dem_resampled, v="TPI")
  windows()
  
  plot(
    TPI_res,
    main = paste0("TPI - Resolución: ", res, " m")  )
}

new_res <- 0.5
template <- rast(
  ext(dem_talaso_2024),
  resolution = new_res,
  crs = crs(dem_talaso_2024))

dem_talaso_2024 <- resample(dem_talaso_2024, template, method = "bilinear")
plot(dem_talaso_2024)
dem_talaso_2024

TPI_talaso_24<- terrain(dem_talaso_2024, v="TPI")
windows();plot(TPI_talaso_24)
TPI_talaso_24<-resample(TPI_talaso_24,multi_talaso_24)
TPI_talaso_24
writeRaster(TPI_talaso_24, "OUTPUT/TPI_talaso_24.tif", overwrite=TRUE)



#slope
multi_talaso_24<-rast("OUTPUT/MULTI_TALASO_OK.tiff")
dem_talaso_2024<-rast("OUTPUT/DEM_TALASO_OK.tiff")

res_list <- c(0.01, 0.05, 0.10, 0.15, 0.20,0.25,0.3,0.4, 0.5,0.75,1)   

for (res in res_list) {
  
  template <- rast(
    ext(dem_talaso_2024),
    resolution = res,
    crs = crs(dem_talaso_2024)
  )
  
  dem_resampled <- resample(dem_talaso_2024, template, method = "bilinear")
  
 slope_res <- terrain(dem_resampled, v="slope",unit="degrees")
  windows()
  
  plot(    slope_res,
    main = paste0("slope - Resolución: ", res, " m")  )
}


new_res <- 0.2
template <- rast(
  ext(dem_talaso_2024),
  resolution = new_res,
  crs = crs(dem_talaso_2024))

dem_talaso_2024 <- resample(dem_talaso_2024, template, method = "bilinear")
plot(dem_talaso_2024)
dem_talaso_2024

slope_talaso_24  <- terra::terrain(dem_talaso_2024,v="slope",unit='degrees')
windows();plot(slope_talaso_24)
slope_talaso_24<-resample(slope_talaso_24,multi_talaso_24)
slope_talaso_24
writeRaster(slope_talaso_24, "OUTPUT/slope_talaso_24.tif", overwrite = TRUE)



#aspect (northness)

multi_talaso_24<-rast("OUTPUT/MULTI_TALASO_OK.tiff")
dem_talaso_2024<-rast("OUTPUT/DEM_TALASO_OK.tiff")

res_list <- c(0.01, 0.05, 0.10, 0.15, 0.20,0.25,0.3,0.4, 0.5,0.75,1)   

for (res in res_list) {
  
  template <- rast(    ext(dem_talaso_2024),    resolution = res,    crs = crs(dem_talaso_2024)  )
  
  dem_resampled <- resample(dem_talaso_2024, template, method = "bilinear")
  
  aspect_res <- terrain(dem_resampled, v="aspect",unit="radians")
  aspect_res<-cos(aspect_res)
  windows()
  
  plot(
    aspect_res,
    main = paste0("northness - Resolución: ", res, " m")
  )
}


new_res <- 0.2
template <- rast(
  ext(dem_talaso_2024),
  resolution = new_res,
  crs = crs(dem_talaso_2024))

dem_talaso_2024 <- resample(dem_talaso_2024, template, method = "bilinear")
plot(dem_talaso_2024)
dem_talaso_2024



aspect_talaso_24  <- terra::terrain(dem_talaso_2024,v="aspect",unit='radians')
windows();plot(aspect_talaso_24)
aspect_talaso_24<-cos(aspect_talaso_24)
windows();plot(aspect_talaso_24)
aspect_talaso_24<-resample(aspect_talaso_24,multi_talaso_24)
aspect_talaso_24
writeRaster(aspect_talaso_24, "OUTPUT/aspect_talaso_24.tif", overwrite = TRUE)


#APPLY WATER MASK WITH LOW ELEVATION TO REMOVE WATER-------


##RAPA 23
#load dsm and multi
rapa_23_multi<-rast("OUTPUT/MULTI_2023_OK.tiff")
plot(rapa_23_multi)
plotRGB(rapa_23_multi,r=6,g=4,b=2,stretch="hist")


rapa_23_dem<-rast("OUTPUT/DEM_2023_OK.tiff")
plot(rapa_23_dem)

red_23<-multi_23$Vigo_Site1_DualMX_32629_corrected_5
red_23<-rapa_23_multi$Vigo_Site1_DualMX_32629_corrected_5
nir_23<-rapa_23_multi$Vigo_Site1_DualMX_32629_corrected_10
NDVI_2023<-(nir_23-red_23)/(nir_23+red_23)
plot(NDVI_2023)
water_mask<-rapa_23_dem<(-3)&NDVI_2023<0
plot(water_mask)

multi_rapa23_nw<-mask(rapa_23_multi,water_mask, maskvalue = TRUE)
plotRGB(multi_rapa23_nw,r=6,g=4,b=2,stretch="hist")
writeRaster(multi_rapa23_nw,filename = "OUTPUT/MULTI_RAPA23_NW.tiff",overwrite=TRUE)

slope_rapa_23<-rast("OUTPUT/slope_rapa_23.tif")
plot(slope_rapa_23)
slope_rapa23_nw<-mask(slope_rapa_23,water_mask, maskvalue = TRUE)
plot(slope_rapa23_nw)
writeRaster(slope_rapa23_nw,filename = "OUTPUT/slope_rapa23_nw.tiff",overwrite=TRUE)

aspect_rapa_23<-rast("OUTPUT/aspect_rapa_2023.tif")
plot(aspect_rapa_23)
aspect_rapa_23_nw<-mask(aspect_rapa_23,water_mask, maskvalue = TRUE)
plot(aspect_rapa_23_nw)
writeRaster(aspect_rapa_23_nw,filename = "OUTPUT/aspect_rapa_23_nw.tiff",overwrite=TRUE)

TRI_rapa_23<-rast("OUTPUT/TRI_rapa_23.tif")
plot(TRI_rapa_23)
TRI_rapa_23_nw<-mask(TRI_rapa_23,water_mask, maskvalue = TRUE)
plot(TRI_rapa_23_nw)
writeRaster(TRI_rapa_23_nw,filename = "OUTPUT/TRI_rapa_23_nw.tiff",overwrite=TRUE)

TPI_rapa_23<-rast("OUTPUT/TPI_rapa_23.tif")
plot(TPI_rapa_23)
TPI_rapa_23_nw<-mask(TPI_rapa_23,water_mask, maskvalue = TRUE)
plot(TPI_rapa_23_nw)
writeRaster(TPI_rapa_23_nw,filename = "OUTPUT/TPI_rapa_23_nw.tiff",overwrite=TRUE)


## RAPA 24

#load dsm and multi
rapa_24_multi<-rast("OUTPUT/MULTI_24_OK.tiff")
plot(rapa_24_multi)
windows();plotRGB(rapa_24_multi,r=6,g=4,b=2,stretch="hist")

rapa_24_dem<-rast("OUTPUT/DEM_2024_OK.tiff")
plot(rapa_24_dem)
rapa_24_dem<-resample(rapa_24_dem,rapa_24_multi)

red_24<-rapa_24_multi$Vigo_1_2024_MULTI_5
nir_24<-rapa_24_multi$Vigo_1_2024_MULTI_10
NDVI_2024<-(nir_24-red_24)/(nir_24+red_24)
plot(NDVI_2024)
water_mask<-rapa_24_dem<(-3)&NDVI_2024<0
plot(water_mask)


multi_rapa24_nw<-mask(rapa_24_multi,water_mask, maskvalue = TRUE)
multi_rapa24_nw[multi_rapa24_nw == 0] <- NA
windows();plotRGB(multi_rapa24_nw,r=6,g=4,b=2,stretch="hist")

writeRaster(multi_rapa24_nw,filename = "OUTPUT/MULTI_RAPA24_NW.tiff",overwrite=TRUE)
multi_rapa24_nw<-rast("OUTPUT/MULTI_RAPA24_NW.tiff")
slope_rapa_24<-rast("OUTPUT/slope_rapa_24.tif")
plot(slope_rapa_24)
slope_rapa_24_nw<-mask(slope_rapa_24,water_mask, maskvalue = TRUE)
plot(slope_rapa_24_nw)
slope_rapa_24_nw<-resample(slope_rapa_24_nw,multi_rapa24_nw)
writeRaster(slope_rapa_24_nw,filename = "OUTPUT/slope_rapa_24_nw.tiff",overwrite=TRUE)

aspect_rapa_24<-rast("OUTPUT/aspect_rapa_24.tif")
plot(aspect_rapa_24)
aspect_rapa_24_nw<-mask(aspect_rapa_24,water_mask, maskvalue = TRUE)
plot(aspect_rapa_24_nw)
aspect_rapa_24_nw<-resample(aspect_rapa_24_nw,multi_rapa24_nw)
writeRaster(aspect_rapa_24_nw,filename = "OUTPUT/aspect_rapa_24_nw.tiff",overwrite=TRUE)

TRI_rapa_24<-rast("OUTPUT/TRI_rapa_24.tif")
plot(TRI_rapa_24)
TRI_rapa_24_nw<-mask(TRI_rapa_24,water_mask, maskvalue = TRUE)
plot(TRI_rapa_24_nw)
TRI_rapa_24_nw<-resample(TRI_rapa_24_nw,multi_rapa24_nw)
writeRaster(TRI_rapa_24_nw,filename = "OUTPUT/TRI_rapa_24_nw.tiff",overwrite=TRUE)

TPI_rapa_24<-rast("OUTPUT/TPI_rapa_24.tif")
plot(TPI_rapa_24)
TPI_rapa_24_nw<-mask(TPI_rapa_24,water_mask, maskvalue = TRUE)
plot(TPI_rapa_24_nw)
TPI_rapa_24_nw<-resample(TPI_rapa_24_nw,multi_rapa24_nw)
writeRaster(TPI_rapa_24_nw,filename = "OUTPUT/TPI_rapa_24_nw.tiff",overwrite=TRUE)

 
##TALASO (CUNCHALES)

#load dsm and multi
talaso_24_multi<-rast("OUTPUT/MULTI_TALASO_OK.tiff")
plot(talaso_24_multi)
windows();plotRGB(talaso_24_multi,r=6,g=4,b=2,stretch="hist")

talaso_24_dem<-rast("OUTPUT/DEM_TALASO_OK.tiff")
plot(talaso_24_dem)

red_ta<-talaso_24_multi$Vigo_2_2024_MULTI_5
nir_ta<-talaso_24_multi$Vigo_2_2024_MULTI_10
NDVI_ta<-(nir_ta-red_ta)/(nir_ta+red_ta)
plot(NDVI_ta)
water_mask<-talaso_24_dem<(-4)&NDVI_ta<0
plot(water_mask)



multi_talaso_nw<-mask(talaso_24_multi,water_mask, maskvalue = TRUE)
multi_talaso_nw[multi_talaso_nw == 0] <- NA
windows();plotRGB(multi_talaso_nw,r=6,g=4,b=2,stretch="hist")

writeRaster(multi_talaso_nw,filename = "OUTPUT/MULTI_TALASO_NW.tiff",overwrite=TRUE)

slope_talaso<-rast("OUTPUT/slope_talaso_24.tif")
plot(slope_talaso)
slope_talaso_nw<-mask(slope_talaso,water_mask, maskvalue = TRUE)
plot(slope_talaso_nw)
writeRaster(slope_talaso_nw,filename = "OUTPUT/slope_talaso_nw.tiff",overwrite=TRUE)

aspect_talaso<-rast("OUTPUT/aspect_talaso_24.tif")
plot(aspect_talaso)
aspect_talaso_nw<-mask(aspect_talaso,water_mask, maskvalue = TRUE)
plot(aspect_talaso_nw)
writeRaster(aspect_talaso_nw,filename = "OUTPUT/aspect_talaso_nw.tiff",overwrite=TRUE)

TRI_talaso<-rast("OUTPUT/TRI_talaso_24.tif")
plot(TRI_talaso)
TRI_talaso_nw<-mask(TRI_talaso,water_mask, maskvalue = TRUE)
plot(TRI_talaso_nw)
writeRaster(TRI_talaso_nw,filename = "OUTPUT/TRI_talaso_nw.tiff",overwrite=TRUE)

TPI_talaso<-rast("OUTPUT/TPI_talaso_24.tif")
plot(TPI_talaso)
TPI_talaso_nw<-mask(TPI_talaso,water_mask, maskvalue = TRUE)
plot(TPI_talaso_nw)
writeRaster(TPI_talaso_nw,filename = "OUTPUT/TPI_talaso_nw.tiff",overwrite=TRUE)




#EXTRACT FEATURES FROM LABELS----
class_id<-seq(1,9)
class_name<-c("other_barnacles","mussel_spat","adult_mussels","goose_barnacle","red_algae","brown_algae","green_algae","bare_rock","water")
class_table<-tibble(class_id,class_name)

###2023 RAPA----
#LOAD LABELS


library(sf)
labels_23<-st_read("TRAIN_TEST_2023.shp")
labels_23
labels_23<-labels_23 %>% 
  dplyr::rename(class_id=id)
labels_23<-labels_23%>%
  left_join(class_table,by=join_by(class_id)) %>% 
  # filter(!class_id=="10") %>%
  dplyr::select(-class_id,-area) %>% 
  dplyr::rename(class=class_name) 
labels_23
table(labels_23$class)# 
plot(labels_23)
# labels$id<-as.character(labels$id)

labels_23$ID<-paste0("rapa23_",1:( nrow(labels_23)))

labels_23_vect <- terra::vect(as(labels_23, "Spatial"))
labels_23_vect


####EXTRACT INFO FROM LABELS----
# dem_rapa_2023<-rast("OUTPUT/DEM_2023_OK.tiff")
TRI_rapa_23_nw<-rast("OUTPUT/TRI_rapa_23_nw.tiff")
TPI_rapa_23_nw<-rast("OUTPUT/TPI_rapa_23_nw.tiff")

slope_rapa_23_nw<-rast("OUTPUT/slope_rapa23_nw.tiff")
aspect_rapa_23_nw<-rast("OUTPUT/aspect_rapa_23_nw.tiff")
multi_rapa_23_nw<-rast("OUTPUT/MULTI_RAPA23_nw.tiff")

comb_rgb_topo_23<-c(multi_rapa_23_nw,slope_rapa_23_nw,aspect_rapa_23_nw,TRI_rapa_23_nw,TPI_rapa_23_nw) 
plot(comb_rgb_topo_23)
comb_rgb_topo_23

labels_ok_23 <- terra::extract(comb_rgb_topo_23, labels_23_vect)
length(unique(labels_ok_23$ID))
unique(labels_ok_23$ID)

labels_ok_23$ID<-paste0("rapa23_",labels_ok_23$ID)

str(labels_ok_23)
labels_ok_23<-labels_ok_23 %>% 
  left_join(labels_23,by=join_by(ID))
str(labels_ok_23)
labels_ok_23<-labels_ok_23 %>% 
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
length(which(is.na(labels_ok_23)))
table(labels_ok_23$class)

#VIEW SPECTRA PER CLASS

bands.drone<-data.frame(band=c("b1","b2","b3","b4","b5","b6","b7","b8","b9","b10"),
                        wavelength=c("Coastal.blue","Blue.475","Green.531","Green.560","Red.650",
                                     "Red.668","RedEdge.705","RedEdge.717","RedEdge.740","NIR"),
                        center=c(444,475, 531,560,650,
                                 668,705,717,740,842), 
                        width=c(28,32,14,27,16,
                                14,10,12,18,57))

labels_long<-labels_ok_23 %>% 
  dplyr::select(-slope,-aspect,-TRI,-geometry,-TPI) %>% 
  pivot_longer(-c(PixelID,ID,class),names_to = "band",values_to = "reflectance") %>% 
  left_join(bands.drone,by="band")

table(labels_long$class)


which(is.na(labels_long))

labels_long %>% 
  group_by(class,center) %>% 
  dplyr::filter(!class=="water") %>%
  reframe(ref_mean=mean(reflectance),
          ref_sd=sd(reflectance)) %>% 
  ggplot(aes(x=center,y=ref_mean,colour = class,fill=class))+
  geom_line(alpha=.2)+
  geom_ribbon(aes(ymax = ref_mean+ref_sd,ymin=ref_mean-ref_sd),alpha=.2)+
  theme_classic()

# STANDARDIZE

(ri-min)/(max-min)

length(unique(labels_long$PixelID))



labels_long_std<-labels_long %>%
  group_by(PixelID) %>% 
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

#transform into wide format and join with the rest of bands

labels_23_features<-labels_ok_23 %>% 
  dplyr::select(PixelID,ID,slope,aspect,TRI,TPI,class,geometry,
                b1,b2,b3,b4,b5,b6,b7,b8,b9,b10)
labels_23_features
labels_ok_23_std<-labels_long_std%>%
  pivot_wider(id_cols=PixelID,names_from = band, values_from = std.ref) %>% 
  left_join(labels_23_features)
View(labels_ok_23_std)

save(labels_ok_23_std,file="OUTPUT/labels_2023_std_ref.RData")

####load and plot labels------
load(file="OUTPUT/labels_2023_std_ref.RData")

#VIEW SPECTRA PER CLASS

bands.drone<-data.frame(band=c("b1_std","b2_std","b3_std","b4_std","b5_std","b6_std","b7_std","b8_std","b9_std","b10_std"),
                        wavelength=c("Coastal.blue","Blue.475","green.531","Green.560","Red.650",
                                     "Red.668","RedEdge.705","RedEdge.717","RedEdge.740","NIR"),
                        center=c(444,475, 531,560,650,
                                 668,705,717,740,842), 
                        width=c(28,32,14,27,16,
                                14,10,12,18,57))

labels_long_std<-labels_ok_23_std %>% 
  dplyr::select(-slope,-aspect,-TRI,-geometry) %>% 
  pivot_longer(-c(PixelID,ID,class),names_to = "band",values_to = "reflectance") %>% 
  dplyr::filter(band=="b1_std"|band=="b2_std"|band=="b3_std"|band=="b4_std"|band=="b5_std"|
                  band=="b6_std"|band=="b7_std"|band=="b8_std"|band=="b9_std"|band=="b10_std") %>% 
  left_join(bands.drone,by="band")

table(labels_long_std$class)
labels_long<-labels_long_std 

table(labels_long_std$class)
which(is.na(labels_long_std$class))
windows();labels_long_std %>%   dplyr::filter(!class=="water") %>% 
  dplyr::filter(class=="goose_barnacle"|class=="other_barnacles") %>% 
    group_by(class,center) %>% 
  reframe(ref_mean=mean(reflectance),
          ref_sd=sd(reflectance)) %>% 
  ggplot(aes(x=center,y=ref_mean,colour = class,fill=class))+
  geom_line(alpha=.2)+
  geom_ribbon(aes(ymax = ref_mean+ref_sd,ymin=ref_mean-ref_sd),alpha=.2)+
  theme_classic()+ggtitle("rapa 23")
  

###RAPA 2024-----
# dem_rapa_2024_nw<-rast("OUTPUT/DEM_2024_nw.tiff")
TRI_rapa_24_nw<-rast("OUTPUT/TRI_rapa_24_nw.tiff")
TPI_rapa_24_nw<-rast("OUTPUT/TPI_rapa_24_nw.tiff")

slope_rapa_24_nw<-rast("OUTPUT/slope_rapa_24_nw.tiff")
aspect_rapa_24_nw<-rast("OUTPUT/aspect_rapa_24_nw.tiff")
multi_rapa_24_nw<-rast("OUTPUT/MULTI_RAPA24_nw.tiff")

comb_rgb_topo_24<-c(multi_rapa_24_nw,slope_rapa_24_nw,aspect_rapa_24_nw,TRI_rapa_24_nw,TPI_rapa_24_nw) 
plot(comb_rgb_topo_24)
comb_rgb_topo_24

#LOAD LABELS AND EXTRACT
labels_24<-st_read("TRAIN_TEST_2024.shp")
labels_24
labels_24<-labels_24 %>% 
  dplyr::rename(class_id=id)
labels_24<-labels_24%>%
  left_join(class_table,by=join_by(class_id)) %>% 
  # filter(!class_id=="10") %>%
  dplyr::select(-class_id) %>% 
  dplyr::rename(class=class_name) 
labels_24
table(labels_24$class)# 
plot(labels_24)
# labels$id<-as.character(labels$id)

labels_24$ID<-paste0("rapa24_",1:( nrow(labels_24)))

labels_24_vect <- terra::vect(as(labels_24, "Spatial"))
labels_24_vect


####EXTRACT INFO FROM LABELS----

labels_ok_24 <- terra::extract(comb_rgb_topo_24, labels_24_vect)
length(unique(labels_ok_24$ID))
unique(labels_ok_24$ID)

labels_ok_24$ID<-paste0("rapa24_",labels_ok_24$ID)

str(labels_ok_24)
labels_ok_24<-labels_ok_24 %>% 
  left_join(labels_24,by=join_by(ID))
str(labels_ok_24)
labels_ok_24<-labels_ok_24 %>% 
  rownames_to_column(var="PixelID") %>% 
  mutate(PixelID = paste0("rapa24_", PixelID))%>%
  dplyr::rename(b1=Vigo_1_2024_MULTI_1 ,
                b2=Vigo_1_2024_MULTI_2 ,
                b3=Vigo_1_2024_MULTI_3 ,
                b4=Vigo_1_2024_MULTI_4 ,
                b5=Vigo_1_2024_MULTI_5 ,
                b6=Vigo_1_2024_MULTI_6 ,
                b7=Vigo_1_2024_MULTI_7 ,
                b8=Vigo_1_2024_MULTI_8 ,
                b9=Vigo_1_2024_MULTI_9 ,
                b10=Vigo_1_2024_MULTI_10     ) %>% 
  na.omit()
table(labels_ok_24$class)
which(is.na(labels_ok_24))

#VIEW SPECTRA PER CLASS

bands.drone<-data.frame(band=c("b1","b2","b3","b4","b5","b6","b7","b8","b9","b10"),
                        wavelength=c("Coastal.blue","Blue.475","Green.531","Green.560","Red.650",
                                     "Red.668","RedEdge.705","RedEdge.717","RedEdge.740","NIR"),
                        center=c(444,475, 531,560,650,
                                 668,705,717,740,842), 
                        width=c(28,32,14,27,16,
                                14,10,12,18,57))

labels_long<-labels_ok_24 %>% 
  dplyr::select(-slope,-aspect,-TRI,-TPI,-geometry) %>% 
  pivot_longer(-c(PixelID,ID,class),names_to = "band",values_to = "reflectance") %>% 
  left_join(bands.drone,by="band")

table(labels_long$class)

which(is.na(labels_long))

labels_long %>% 
  group_by(class,center) %>% 
  dplyr::filter(!class=="water") %>% 
  reframe(ref_mean=mean(reflectance),
          ref_sd=sd(reflectance)) %>% 
  ggplot(aes(x=center,y=ref_mean,colour = class,fill=class))+
  geom_line(alpha=.2)+
  geom_ribbon(aes(ymax = ref_mean+ref_sd,ymin=ref_mean-ref_sd),alpha=.2)+
  theme_classic()




# STANDARDIZE


#(ri-min)/(max-min)

length(unique(labels_long$PixelID))



labels_long_std<-labels_long %>%
  group_by(PixelID) %>% 
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
View(labels_long_std)
which(is.na(labels_long_std))

#transform into wide format and join with the rest of bands

labels_24_features<-labels_ok_24 %>% 
  dplyr::select(PixelID,ID,slope,aspect,TRI,TPI,class,geometry,
                b1,b2,b3,b4,b5,b6,b7,b8,b9,b10)
labels_24_features
labels_ok_24_std<-labels_long_std%>%
  pivot_wider(id_cols=PixelID,names_from = band, values_from = std.ref) %>% 
  left_join(labels_24_features)
View(labels_ok_24_std)

save(labels_ok_24_std,file="OUTPUT/labels_2024_std_ref.RData")

####load and plot labels------
load(file="OUTPUT/labels_2024_std_ref.RData")

#VIEW SPECTRA PER CLASS

bands.drone<-data.frame(band=c("b1_std","b2_std","b3_std","b4_std","b5_std","b6_std","b7_std","b8_std","b9_std","b10_std"),
                        wavelength=c("Coastal.blue","Blue.475","green.531","Green.560","Red.650",
                                     "Red.668","RedEdge.705","RedEdge.717","RedEdge.740","NIR"),
                        center=c(444,475, 531,560,650,
                                 668,705,717,740,842), 
                        width=c(28,32,14,27,16,
                                14,10,12,18,57))

labels_long_std<-labels_ok_24_std %>% 
  dplyr::select(-slope,-aspect,-TRI,-geometry) %>% 
  pivot_longer(-c(PixelID,ID,class),names_to = "band",values_to = "reflectance") %>% 
  dplyr::filter(band=="b1_std"|band=="b2_std"|band=="b3_std"|band=="b4_std"|band=="b5_std"|
                  band=="b6_std"|band=="b7_std"|band=="b8_std"|band=="b9_std"|band=="b10_std") %>% 
  left_join(bands.drone,by="band")

table(labels_long_std$class)
labels_long<-labels_long_std 

table(labels_long_std$class)
which(is.na(labels_long_std$class))
windows();labels_long_std %>%  
# dplyr::filter(!class=="water") %>% 
  dplyr::filter(class=="goose_barnacle"|class=="other_barnacles") %>% 
  group_by(class,center) %>% 
  reframe(ref_mean=mean(reflectance),
          ref_sd=sd(reflectance)) %>% 
  ggplot(aes(x=center,y=ref_mean,colour = class,fill=class))+
  geom_line(alpha=.2)+
  geom_ribbon(aes(ymax = ref_mean+ref_sd,ymin=ref_mean-ref_sd),alpha=.2)+
  theme_classic()+ggtitle("rapa 24")
  



###TALASO 2024 (CUNCHALES)------

# dem_talaso_2024<-rast("OUTPUT/DEM_TALASO_OK.tiff")
TRI_talaso_24_nw<-rast("OUTPUT/TRI_talaso_nw.tiff")
TPI_talaso_24_nw<-rast("OUTPUT/TPI_talaso_nw.tiff")
slope_talaso_24_nw<-rast("OUTPUT/slope_talaso_nw.tiff")
aspect_talaso_24_nw<-rast("OUTPUT/aspect_talaso_nw.tiff")
multi_talaso_24_nw<-rast("OUTPUT/MULTI_TALASO_nw.tiff")

comb_rgb_topo_ta<-c(multi_talaso_24_nw,TRI_talaso_24_nw,TPI_talaso_24_nw,slope_talaso_24_nw,aspect_talaso_24_nw) 
plot(comb_rgb_topo_ta)
comb_rgb_topo_ta

#LOAD LABELS AND EXTRACT
labels_ta<-st_read("DATA/LABELLED POLYGONS/TRAIN_TEST_2024_TALASO.shp")
labels_ta
labels_ta<-labels_ta %>% 
  dplyr::rename(class_id=id)
labels_ta<-labels_ta%>%
  left_join(class_table,by=join_by(class_id)) %>% 
  # filter(!class_id=="10") %>%
  dplyr::select(-class_id) %>% 
  dplyr::rename(class=class_name) 
labels_ta
table(labels_ta$class)# 
plot(labels_ta)
 
labels_ta$ID<-paste0("ta24_",1:( nrow(labels_ta)))

labels_ta_vect <- terra::vect(as(labels_ta, "Spatial"))
labels_ta_vect


####EXTRACT INFO FROM LABELS----

labels_ok_ta <- terra::extract(comb_rgb_topo_ta, labels_ta_vect)
length(unique(labels_ok_ta$ID))
unique(labels_ok_ta$ID)

labels_ok_ta$ID<-paste0("ta24_",labels_ok_ta$ID)

str(labels_ok_ta)
labels_ok_ta<-labels_ok_ta %>% 
  left_join(labels_ta,by=join_by(ID))
str(labels_ok_ta)
labels_ok_ta<-labels_ok_ta %>% 
  rownames_to_column(var="PixelID") %>% 
  mutate(PixelID = paste0("ta24_", PixelID))%>%
  dplyr::rename(b1=Vigo_2_2024_MULTI_1 ,
                b2=Vigo_2_2024_MULTI_2 ,
                b3=Vigo_2_2024_MULTI_3 ,
                b4=Vigo_2_2024_MULTI_4 ,
                b5=Vigo_2_2024_MULTI_5 ,
                b6=Vigo_2_2024_MULTI_6 ,
                b7=Vigo_2_2024_MULTI_7 ,
                b8=Vigo_2_2024_MULTI_8 ,
                b9=Vigo_2_2024_MULTI_9 ,
                b10=Vigo_2_2024_MULTI_10   ) %>% 
  na.omit()
table(labels_ok_ta$class)


#VIEW SPECTRA PER CLASS

bands.drone<-data.frame(band=c("b1","b2","b3","b4","b5","b6","b7","b8","b9","b10"),
                        wavelength=c("Coastal.blue","Blue.475","Green.531","Green.560","Red.650",
                                     "Red.668","RedEdge.705","RedEdge.717","RedEdge.740","NIR"),
                        center=c(444,475, 531,560,650,
                                 668,705,717,740,842), 
                        width=c(28,32,14,27,16,
                                14,10,12,18,57))

labels_long<-labels_ok_ta %>% 
  dplyr::select(-slope,-aspect,-TRI,-TPI,-geometry) %>% 
  pivot_longer(-c(PixelID,ID,class),names_to = "band",values_to = "reflectance") %>% 
  left_join(bands.drone,by="band")

table(labels_long$class)

which(is.na(labels_long))

labels_long %>% 
  group_by(class,center) %>% 
  reframe(ref_mean=mean(reflectance),
          ref_sd=sd(reflectance)) %>% 
  ggplot(aes(x=center,y=ref_mean,colour = class,fill=class))+
  geom_line(alpha=.2)+
  geom_ribbon(aes(ymax = ref_mean+ref_sd,ymin=ref_mean-ref_sd),alpha=.2)+
  theme_classic()




# STANDARDIZE


#(ri-min)/(max-min)

length(unique(labels_long$PixelID))



labels_long_std<-labels_long %>%
  group_by(PixelID) %>% 
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
View(labels_long_std)
which(is.na(labels_long_std))




#transform into wide format and join with the rest of bands

labels_ta_features<-labels_ok_ta %>% 
  dplyr::select(PixelID,ID,slope,aspect,TRI,TPI,class,geometry,
                b1,b2,b3,b4,b5,b6,b7,b8,b9,b10)
labels_ta_features
labels_ta_std<-labels_long_std%>%
  pivot_wider(id_cols=PixelID,names_from = band, values_from = std.ref) %>% 
  left_join(labels_ta_features)
View(labels_ta_std)

save(labels_ta_std,file="OUTPUT/labels_ta_std_ref.RData")

####load and plot labels------
load(file="OUTPUT/labels_ta_std_ref.RData")

#VIEW SPECTRA PER CLASS

bands.drone<-data.frame(band=c("b1_std","b2_std","b3_std","b4_std","b5_std","b6_std","b7_std","b8_std","b9_std","b10_std"),
                        wavelength=c("Coastal.blue","Blue.475","green.531","Green.560","Red.650",
                                     "Red.668","RedEdge.705","RedEdge.717","RedEdge.740","NIR"),
                        center=c(444,475, 531,560,650,
                                 668,705,717,740,842), 
                        width=c(28,32,14,27,16,
                                14,10,12,18,57))

labels_long_std<-labels_ta_std %>% 
  dplyr::select(-slope,-aspect,-TRI,-geometry) %>% 
  pivot_longer(-c(PixelID,ID,class),names_to = "band",values_to = "reflectance") %>% 
  dplyr::filter(band=="b1_std"|band=="b2_std"|band=="b3_std"|band=="b4_std"|band=="b5_std"|
                  band=="b6_std"|band=="b7_std"|band=="b8_std"|band=="b9_std"|band=="b10_std") %>% 
  left_join(bands.drone,by="band")

table(labels_long_std$class)

table(labels_long_std$class)
which(is.na(labels_long_std$class))
windows();labels_long_std %>%  
  # dplyr::filter(!class=="water") %>% 
  dplyr::filter(class=="goose_barnacle"|class=="other_barnacles") %>% 
  
  group_by(class,center) %>% 
  reframe(ref_mean=mean(reflectance),
          ref_sd=sd(reflectance)) %>% 
  ggplot(aes(x=center,y=ref_mean,colour = class,fill=class))+
  geom_line(alpha=.2)+
  geom_ribbon(aes(ymax = ref_mean+ref_sd,ymin=ref_mean-ref_sd),alpha=.2)+
  theme_classic()+ggtitle("talaso")



##JOIN 3 SITES-----

LABELS<-rbind(labels_ok_23_std,labels_ok_24_std,labels_ta_std)
save(LABELS,file="OUTPUT/LABELS.RData")
load("OUTPUT/LABELS.RData")


##REFLECTANCE PLOT WITH COLOURS AND ALL SITES------

bands.drone<-data.frame(band=c("b1_std","b2_std","b3_std","b4_std","b5_std","b6_std","b7_std","b8_std","b9_std","b10_std"),
                        band_raw=c("b1","b2","b3","b4","b5","b6","b7","b8","b9","b10"),
                        wavelength=c("Coastal.blue","Blue.475","Green.531","Green.560","Red.650",
                                     "Red.668","RedEdge.705","RedEdge.717","RedEdge.740","NIR"),
                        center=c(444,475, 531,560,650,
                                 668,705,717,740,842), 
                        width=c(28,32,14,27,16,
                                14,10,12,18,57))

LABELS_LONG<-LABELS %>% 
  dplyr::select(-slope,-aspect,-TRI,-geometry) %>% 
  pivot_longer(-c(PixelID,ID,class),names_to = "band",values_to = "reflectance") %>% 
  dplyr::filter(band=="b1_std"|band=="b2_std"|band=="b3_std"|band=="b4_std"|band=="b5_std"|
                  band=="b6_std"|band=="b7_std"|band=="b8_std"|band=="b9_std"|band=="b10_std") %>% 
  left_join(bands.drone,by="band")

table(LABELS_LONG$class)

which(is.na(LABELS_LONG$class))

LABELS_LONG %>%  
  dplyr::filter(!class=="water") %>%
  group_by(class,center) %>% 
  reframe(ref_mean=mean(reflectance),
          ref_sd=sd(reflectance)) %>% 
  ggplot(aes(x=center,y=ref_mean,colour = class,fill=class))+
  geom_line(alpha=.2)+
  geom_ribbon(aes(ymax = ref_mean+ref_sd,ymin=ref_mean-ref_sd),alpha=.2)+
  theme_classic()




LABELS_LONG$class<-factor(LABELS_LONG$class,
                          levels=c("goose_barnacle","other_barnacles","adult_mussels","mussel_spat",
                                   "red_algae","brown_algae","green_algae","bare_rock","water"))
table(LABELS_LONG$class)

spectra_plot<-LABELS_LONG %>%   
  dplyr::filter(!class=="water") %>% 
  group_by(class,center) %>% 
  reframe(ref_mean=mean(reflectance),
          ref_sd=sd(reflectance)) %>% 
  ggplot(aes(x=center,y=ref_mean,colour = class,fill=class))+
  geom_line()+
  geom_ribbon(aes(ymax = ref_mean+ref_sd,ymin=ref_mean-ref_sd),alpha=.4)+
  theme_classic()+
  scale_colour_manual(values = c("orange","pink","blue","black","red","goldenrod4","green","grey"),name="Class",
                      labels=c("Goose barnacle","Other barnacles","Adult mussels","Mussel spat",
                               "Red algae","Brown algae","Green algae","Bare rock"))+
  scale_fill_manual(values = c("orange","pink","blue","black","red","goldenrod4","green","grey"),name="Class",
                    labels=c("Goose barnacle","Other barnacles","Adult mussels","Mussel spat",
                             "Red algae","Brown algae","Green algae","Bare rock"))+
  xlab("Wavelength")+ylab("Standardised reflectance")+
  theme(axis.text = element_text(size=16),axis.title = element_text(size=16,colour = "black"),
        legend.text = element_text(size=16,colour = "black"),legend.title = element_text(size=16))+
  ggtitle("A)")

spectra_plot


ggsave("SPECTRA PLOT.svg",plot=spectra_plot,device="svg", path="FIGURES",width=8.5,height=5,units="in")



#better separability between goose and other barnacles at 600-665 nm, calculate index


#raw ref of only barnacles with band ranges


#raw reflectances
LABELS_LONG<-LABELS %>%
  dplyr::select(-slope,-aspect,-TRI,-geometry) %>%
  pivot_longer(-c(PixelID,ID,class),names_to = "band",values_to = "reflectance") %>%
  dplyr::filter(band=="b1"|band=="b2"|band=="b3"|band=="b4"|band=="b5"|
                  band=="b6"|band=="b7"|band=="b8"|band=="b9"|band=="b10") %>%
  rename(band_raw=band) %>%
  left_join(bands.drone,by="band_raw")



bands.drone <- bands.drone %>%
  mutate(xmin = center - width/2,
         xmax = center + width/2)





windows();
barnacles_plot<-LABELS_LONG %>%   
  dplyr::filter(!class=="water") %>% 
  filter(class=="goose_barnacle"|class=="other_barnacles") %>% 
  group_by(class,center) %>% 
  reframe(ref_mean=mean(reflectance),
          ref_sd=sd(reflectance)) %>% 
  ggplot(aes(x=center,y=ref_mean,colour = class,fill=class))+
  geom_line(linewidth=2)+
  geom_ribbon(aes(ymax = ref_mean+ref_sd,ymin=ref_mean-ref_sd),alpha=.4)+
  geom_rect(data = bands.drone,
            aes(xmin = xmin, xmax = xmax,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
 
  theme_classic()+
  scale_colour_manual(values = c("orange","pink"),name="Class",
                      labels=c("Goose barnacle","Other barnacles"))+
  scale_fill_manual(values = c("orange","pink"),name="Class",
                    labels=c("Goose barnacle","Other barnacles"))+
  xlab("Wavelength")+ylab("Raw reflectance")+
  theme(axis.text = element_text(size=16),axis.title = element_text(size=16,colour = "black"),
        legend.text = element_text(size=16,colour = "black"),legend.title = element_text(size=16))+ggtitle("B)")

barnacles_plot
library(ggpubr)
FIG_3<-ggarrange(spectra_plot,barnacles_plot,nrow=2,align='hv',common.legend = TRUE)

FIG_3

ggsave("FIGURE 3.svg",plot=FIG_3,device="svg", path="FIGURES",width=8.5,height=11,units="in")


#AREA UNDERTHE SPECTRa
LABELS$AUS <- ((560-531)*((LABELS$b3 + LABELS$b4)/2) + 
                 (650-560)*((LABELS$b4 + LABELS$b5)/2) + 
                 (668-650)*((LABELS$b5 + LABELS$b6)/2))

LABELS %>%  
  # dplyr::filter(!class=="water") %>% 
  filter(class=="goose_barnacle"|class=="other_barnacles") %>% 
  ggplot(aes(x=class,y=AUS,colour = class))+
  geom_boxplot()+
  geom_point()




#INDEX
# RED SLOPE INDEX (B5-B6)/(B5+B6)
LABELS$b4_b6<-(LABELS$b4-LABELS$b6)/(LABELS$b4+LABELS$b6)


LABELS %>%  
  filter(class=="goose_barnacle"|class=="other_barnacles") %>% 
  ggplot(aes(x=class,y=b4_b6,colour = class))+
  geom_boxplot()+
  geom_point()



LABELS$mean_reflectance_std <- rowMeans(LABELS[, c("b1_std","b2_std","b3_std","b4_std","b5_std",
                                                   "b6_std","b7_std","b8_std","b9_std","b10_std")])


LABELS %>%  
  filter(class=="goose_barnacle"|class=="other_barnacles") %>% 
  ggplot(aes(x=class,y=mean_reflectance_std,colour = class))+
  geom_boxplot()+
  geom_point()


