library(rgdal)
library(raster)
library(sf)
library(sp)
library(dismo)
library(landscapemetrics)
library(dplyr)
library(stringr)

##### ALL MAMMALS #####

## behrmann projection
behr <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m"

## read in combined plot data
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Mammal_BioFrag_Data")
combplot <- read.csv("Combined_Plot_Data.csv")
combplot <- st_as_sf(combplot, coords=c('Longitude', 'Latitude'))
st_crs(combplot) <- 4326
## Transform to projected coordinate system so in metres
combplot <- st_transform(combplot, crs = behr)

## nearest neighbour
all_sites_distances <- st_distance(combplot)
diag(all_sites_distances) <- Inf
nn_dist <- apply(all_sites_distances, 1, min)
min(nn_dist)
median(nn_dist)
max(nn_dist)
mean(nn_dist)
plot(nn_dist)

## buffer to use
round(median(nn_dist))/2





#### LARGE MAMMALS ####

## read in plot data
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Mammal_BioFrag_Data")
largemamplot <- read.csv("PID0092_Mexico_Mammals/PID0092_Plot.csv")
largemamplot <- st_as_sf(largemamplot, coords=c('Longitude', 'Latitude'))
st_crs(largemamplot) <- 4326
## Transform to projected coordinate system so in metres
largemamplot <- st_transform(largemamplot, crs = behr)


#### DAVID'S BIT OF CODE FOR BUFFER SIZES #### 
##install.packages("lwgeom")
##install.packages("RColorBrewer")
library(lwgeom)
library(RColorBrewer)

# get a convex hull for each site to look at buffer distances
sites_multi <- aggregate(largemamplot, list(study=largemamplot$Site), FUN=function(x) x[1])[,1]
sites_hull <- st_convex_hull(sites_multi)

# maximum buffer distance
sites_hull_buffer <- st_buffer(sites_hull, 10000)

plot(st_geometry(sites_hull))
plot(st_geometry(sites_hull_buffer), add=TRUE, border='grey')

# LOOK AT THE MINIMUM DISTANCES BETWEEN SITES
all_sites_distances <- st_distance(largemamplot)
diag(all_sites_distances) <- Inf
nn_dist <- apply(all_sites_distances, 1, min)
min(nn_dist)
median(nn_dist)
max(nn_dist)
mean(nn_dist)
plot(nn_dist)

round(median(nn_dist))/2
## buffer to use is 1700m

#### SMALL MAMMALS ####

plots_mam_braz <- read.csv("PID0069_Jari_SmallMammals/PID0069_Plot.csv")

## Change lat and long to coords
plots_mam_braz <- st_as_sf(plots_mam_braz, coords=c('Longitude', 'Latitude'))
st_crs(plots_mam_braz) <- 4326

## Transform to projected coordinate system so in metres
plots_mam_braz <- st_transform(plots_mam_braz, crs = behr)

# LOOK AT THE MINIMUM DISTANCES BETWEEN SITES
all_sites_distances <- st_distance(plots_mam_braz)
diag(all_sites_distances) <- Inf
nn_dist <- apply(all_sites_distances, 1, min)
min(nn_dist)
median(nn_dist)
max(nn_dist)
mean(nn_dist)
plot(nn_dist)

round(median(nn_dist))/2
## buffer to use is 3100m

plot(brazraster)
plot(st_geometry(plots_mam_braz), add=T, col="black")
sites_buffer <- st_buffer(plots_mam_braz,3100)
plot(st_geometry(sites_buffer), add=T, col="red")
