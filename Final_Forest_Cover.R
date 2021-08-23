library(rgdal)
library(raster)
library(sf)
library(sp)
library(dismo)
library(landscapemetrics)
library(dplyr)
library(stringr)

## behrmann projection
behr <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m"

#### PERU ####
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Mammal_BioFrag_Data")

## Load Data
plots_bats_peru <- read.csv("PID0114_Bats_Amazonia_Peru_wet/PID0114_Plot.csv")

## Change lat and long to coords
plots_bats_peru <- st_as_sf(plots_bats_peru, coords=c('Longitude', 'Latitude'))
st_crs(plots_bats_peru) <- 4326

## Transform to projected coordinate system so in metres
plots_bats_peru <- st_transform(plots_bats_peru, crs = behr)

## Load raster and reproject
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Forest_Cover/Peru")
str_name <-'Peru_Polygon.tif' 
peruraster <- raster(str_name)
peruraster <- projectRaster(peruraster, crs = behr)
plot(peruraster)
## add points
plot(st_geometry(plots_bats_peru), add=T, col="black", pch=4, cex=0.6)

## how to plot buffer now projection is in m, 900m is buffer
for (i in plots_bats_peru) {
  sites_buffer <- st_buffer(plots_bats_peru$geometry, 900)
}
sites_buffer <- st_as_sf(sites_buffer)
plot(st_geometry(sites_buffer), add=T, col="red")

### using landscape metrics package to get percentage forest cover >70%
peruraster[peruraster < 70] <- 0
peruraster[peruraster >= 70] <- 1 # transforms data to binary where 1=forest and 0=non forest

## frag metrics calculation of forest cover
perufrag_metrics_900 <- sample_lsm(peruraster, plots_bats_peru,plot_id = plots_bats_peru$Sites, shape = "circle", size = 900, what = "lsm_c_pland")
peruonly_forest900 <- subset(perufrag_metrics_900, class==1)

## write to csv
write.csv(peruonly_forest900, "C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Forest_Cover/Peru/Peru_Bat_Forest_Cover_900m.csv", row.names = FALSE)


#### COSTA RICA ####
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Mammal_BioFrag_Data")

## Load Data
plots_bats_cr <- read.csv("PID0116_Bats_Costa_Rica_wet/PID0116_Plot.csv")

## Change lat and long to coords
plots_bats_cr <- st_as_sf(plots_bats_cr, coords=c('Long', 'Lat'))
st_crs(plots_bats_cr) <- 4326

## Transform to projected coordinate system so in metres
plots_bats_cr <- st_transform(plots_bats_cr, crs = behr)

## Load raster and reproject
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Forest_Cover/Costa_Rica")
str_name <-'Costa_Rica_Polygon_2005.tif' 
crraster <- raster(str_name)
crraster <- projectRaster(crraster, crs = behr)
plot(crraster)
## add points
plot(st_geometry(plots_bats_cr), add=T, col="black", pch=4, cex=0.6)

## how to plot buffer now projection is in m, 900m is buffer
sites_buffer <- st_buffer(plots_bats_cr, 900)
sites_buffer <- st_as_sf(sites_buffer)
plot(st_geometry(sites_buffer), add=T, col="red")

### using landscape metrics package to get percentage forest cover >70%
crraster[crraster < 70] <- 0
crraster[crraster >= 70] <- 1 # transforms data to binary where 1=forest and 0=non forest

## frag metrics calculation of forest cover
crfrag_metrics_900 <- sample_lsm(crraster, plots_bats_cr,plot_id = plots_bats_cr$Plot, shape = "circle", size = 900, what = "lsm_c_pland")
cronly_forest900 <- subset(crfrag_metrics_900, class==1)

## write to csv
write.csv(cronly_forest900, "C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Forest_Cover/Costa_Rica/Costa_Rica_Bat_Forest_Cover_900m.csv", row.names = FALSE)


#### BRAZIL ####
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Mammal_BioFrag_Data")

## Load Data
plots_bats_jari <- read.csv("PID0058_Jari_Bats/PID0058_Plot.csv")

## Change lat and long to coords
plots_bats_jari <- st_as_sf(plots_bats_jari, coords=c('Longitude', 'Latitude'))
st_crs(plots_bats_jari) <- 4326

## Transform to projected coordinate system so in metres
plots_bats_jari <- st_transform(plots_bats_jari, crs = behr)

## Load raster and reproject
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Forest_Cover/Brazil")
str_name <-'overall_brazil_map.tif' 
brazraster <- raster(str_name)
brazraster <- projectRaster(brazraster, crs = behr)
plot(brazraster)
## add points
plot(st_geometry(plots_bats_jari), add=T, col="black", pch=4, cex=0.6)

## how to plot buffer now projection is in m, 900m is buffer
sites_buffer <- st_buffer(plots_bats_jari, 900)
sites_buffer <- st_as_sf(sites_buffer)
plot(st_geometry(sites_buffer), add=T, col="red")

### using landscape metrics package to get percentage forest cover >70%
brazraster[brazraster < 70] <- 0
brazraster[brazraster >= 70] <- 1 # transforms data to binary where 1=forest and 0=non forest

## frag metrics calculation of forest cover
brazfrag_metrics_900 <- sample_lsm(brazraster, plots_bats_jari,plot_id = plots_bats_jari$Plot, shape = "circle", size = 900, what = "lsm_c_pland")

### need to add the missing data to 900, as plots where forest = 0 have been removed, so need to add them back in
which(brazfrag_metrics_900$value == 100)
brazfrag_metrics_900$plot_id[c(5,6)]
plot_id <- c("14", "52")
value <- c(0,0)
missingdata <- data.frame(plot_id,value)
missingdata$layer <- 1
missingdata$level <- "class"
missingdata$id <- NA
missingdata$metric <- "pland"
missingdata$value <- 0
missingdata$class <- 1
missingdata$percentage_inside <- 99.93661
brazonly_forest900 <- subset(brazfrag_metrics_900, class==1)
brazonly_forest900 <- rbind(missingdata,brazonly_forest900)

## write to csv
write.csv(brazonly_forest900, "C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Forest_Cover/Brazil/Brazil_Bat_Forest_Cover_900m.csv", row.names = FALSE)


#### MEXICO ####
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Mammal_BioFrag_Data")

## Load Data
plots_bats_mex <- read.csv("PID0093_Mexico_Bats/PID0093_Plot.csv")

## Change lat and long to coords
plots_bats_mex <- st_as_sf(plots_bats_mex, coords=c('Lon', 'Lat'))
st_crs(plots_bats_mex) <- 4326

## Transform to projected coordinate system so in metres
plots_bats_mex <- st_transform(plots_bats_mex, crs = behr)

## Load raster and reproject
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Forest_Cover/Mexico")
str_name <-'Mexico_Polygon.tif' 
mexraster <- raster(str_name)
mexraster <- projectRaster(mexraster, crs = behr)
plot(mexraster)
## add points
plot(st_geometry(plots_bats_mex), add=T, col="black", pch=4, cex=0.6)

## how to plot buffer now projection is in m, 900m is buffer
sites_buffer <- st_buffer(plots_bats_mex, 900)
sites_buffer <- st_as_sf(sites_buffer)
plot(st_geometry(sites_buffer), add=T, col="red")

### using landscape metrics package to get percentage forest cover >70%
mexraster[mexraster < 70] <- 0
mexraster[mexraster >= 70] <- 1 # transforms data to binary where 1=forest and 0=non forest

## frag metrics calculation of forest cover
bmfrag_metrics_900 <- sample_lsm(mexraster, plots_bats_mex,plot_id = plots_bats_mex$Plot, shape = "circle", size = 900, what = "lsm_c_pland")
bmonly_forest900 <- subset(bmfrag_metrics_900, class==1)

## write to csv
write.csv(bmonly_forest900, "C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Forest_Cover/Mexico/Mexico_Bat_Forest_Cover_900m.csv", row.names = FALSE)



## can use same buffer for all mammal groups

#### MEXICO ####
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Mammal_BioFrag_Data")

## Load Data
plots_mam_mex <- read.csv("PID0092_Mexico_Mammals/PID0092_Plot.csv")

## Change lat and long to coords
plots_mam_mex <- st_as_sf(plots_mam_mex, coords=c('Longitude', 'Latitude'))
st_crs(plots_mam_mex) <- 4326

## Transform to projected coordinate system so in metres
plots_mam_mex <- st_transform(plots_mam_mex, crs = behr)

## Load raster and reproject
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Forest_Cover/Mexico")
str_name <-'Mexico_Polygon.tif' 
mexraster <- raster(str_name)
mexraster <- projectRaster(mexraster, crs = behr)
plot(mexraster)
## add points
plot(st_geometry(plots_mam_mex), add=T, col="black", pch=4, cex=0.6)

## how to plot buffer now projection is in m, 900m is buffer
sites_buffer <- st_buffer(plots_mam_mex, 900)
sites_buffer <- st_as_sf(sites_buffer)
plot(st_geometry(sites_buffer), add=T, col="red")

### using landscape metrics package to get percentage forest cover >70%
mexraster[mexraster < 70] <- 0
mexraster[mexraster >= 70] <- 1 # transforms data to binary where 1=forest and 0=non forest

## frag metrics calculation of forest cover
mmfrag_metrics_900 <- sample_lsm(mexraster, plots_mam_mex,plot_id = plots_mam_mex$Site, shape = "circle", size = 900, what = "lsm_c_pland")
mmonly_forest900 <- subset(mmfrag_metrics_900, class==1)

## write to csv
write.csv(mmonly_forest900, "C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Forest_Cover/Mexico/Mexico_Mammal_Forest_Cover_900m.csv", row.names = FALSE)



#### SAO PAULO ####
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Mammal_BioFrag_Data")

## Load Data
plots_bats_saop <- read.csv("PID0222_Sao_Paulo_Bats/PID0222_Plot.csv")

## Change lat and long to coords
plots_bats_saop <- st_as_sf(plots_bats_saop, coords=c('Longitude', 'Latitude'))
st_crs(plots_bats_saop) <- 4326

## Transform to projected coordinate system so in metres
plots_bats_saop <- st_transform(plots_bats_saop, crs = behr)

## Load raster and reproject
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Forest_Cover/Brazil")
str_name <-'sao_paulo_forest.tif' 
saopraster <- raster(str_name)
saopraster <- projectRaster(saopraster, crs = behr)
plot(saopraster)
## add points
plot(st_geometry(plots_bats_saop), add=T, col="black", pch=4, cex=0.6)

## how to plot buffer now projection is in m, 900m is buffer
sites_buffer <- st_buffer(plots_bats_saop, 900)
sites_buffer <- st_as_sf(sites_buffer)
plot(st_geometry(sites_buffer), add=T, col="red")

### using landscape metrics package to get percentage forest cover >70%
saopraster[saopraster < 70] <- 0
saopraster[saopraster >= 70] <- 1 # transforms data to binary where 1=forest and 0=non forest

## frag metrics calculation of forest cover
sbfrag_metrics_900 <- sample_lsm(saopraster, plots_bats_saop,plot_id = plots_bats_saop$Plot, shape = "circle", size = 900, what = "lsm_c_pland")

## write to csv
write.csv(sbfrag_metrics_900, "C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Forest_Cover/Brazil/Sao_Paulo_Bats_Forest_Cover_900m.csv", row.names = FALSE)


## rbind all ######

allforest <- rbind(mmonly_forest900, bmonly_forest900, brazonly_forest900, peruonly_forest900, cronly_forest900)
var(allforest$value)
hist(allforest$value)
