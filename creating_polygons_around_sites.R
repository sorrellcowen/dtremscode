
### load packages
install.packages('raster') # Core raster GIS data package
install.packages('sf') # Core vector GIS data package
install.packages('sp') # Another core vector GIS package
install.packages('dismo') # Species Distribution Modelling
install.packages('rgdal') # Interface to the Geospatial Data Abstraction Library
install.packages("landscapemetrics")
install.packages("dplyr")
install.packages("stringr")



library(rgdal)
library(raster)
library(sf)
library(sp)
library(dismo)
library(landscapemetrics)
library(dplyr)
library(stringr)
library(rgeos)
library(geosphere)


# Load some (coarse) country background data
setwd("C:/Users/solco/OneDrive/TFE/Week 5 GIS")
sne110 <- st_read('data/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp')


#### BRAZIL SITES ####
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Mammal_BioFrag_Data")
plots_bats_jari <- read.csv("PID0058_Jari_Bats/PID0058_Plot.csv")

plots_bats_jari <- st_as_sf(plots_bats_jari, coords=c('Longitude', 'Latitude'))
st_crs(plots_bats_jari) <- 4326
print(plots_bats_jari)


# Create a modelling extent for plotting and cropping the global data.
model_extent <- extent(c(-54,-52,-2,0))

# Plot site data over map
plot(st_geometry(sne110), xlim=model_extent[1:2], ylim=model_extent[3:4], 
     bg='lightblue', col='ivory')
plot(st_geometry(plots_bats_jari), add=TRUE, col='red', pch=4, cex=0.6)
box()



#### Create a polygon around sites ####


### create polygon around sites with 0.1 buffer
sites_region <- st_as_sfc(st_bbox(plots_bats_jari))
sites_region <- st_buffer(sites_region, 0.1)

### find min and max long and lat to use in google earth engine
print(sites_region)

## plot the polygon onto map
plot(st_geometry(sites_region), add=TRUE, col='red')

## plot the sites back on to make sure they all fall into polygon
plots_bats_jari <- st_as_sf(plots_bats_jari, coords=c('Longitude', 'Latitude'))
st_crs(plots_mammals_mexico) <- 4326
plot(st_geometry(plots_bats_jari), add=TRUE, col='black', pch=4, cex=0.6)




##### MEXICO SITES #####
 

setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Mammal_BioFrag_Data")

plots_mammals_mexico <- read.csv("PID0092_Mexico_Mammals/PID0092_Plot.csv")

plots_mammals_mexico <- st_as_sf(plots_mammals_mexico, coords=c('Longitude', 'Latitude'))
st_crs(plots_mammals_mexico) <- 4326
print(plots_mammals_mexico)


# Create a modelling extent for plotting and cropping the global data.
model_extent <- extent(c(-92,-89,15,17))

# Plot site data over map
plot(st_geometry(sne110), xlim=model_extent[1:2], ylim=model_extent[3:4], 
     bg='lightblue', col='ivory')
plot(st_geometry(plots_mammals_mexico), add=TRUE, col='red', pch=4, cex=0.6)
box()


#### Create a polygon around sites ####


### create polygon around sites with 0.1 buffer
sites_region <- st_as_sfc(st_bbox(plots_mammals_mexico))
sites_region <- st_buffer(sites_region, 0.1)

### find min and max long and lat to use in google earth engine
print(sites_region)

## plot the polygon onto map
plot(st_geometry(sites_region), add=TRUE, col='red')

## plot the sites back on to make sure they all fall into polygon
plots_mammals_mexico <- st_as_sf(plots_mammals_mexico, coords=c('Longitude', 'Latitude'))
st_crs(plots_mammals_mexico) <- 4326
plot(st_geometry(plots_mammals_mexico), add=TRUE, col='black', pch=4, cex=0.6)




##### COSTA RICA SITES #####

setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Mammal_BioFrag_Data")

plots_bats_cr <- read.csv("PID0116_Bats_Costa_Rica_wet/PID0116_Plot.csv")

plots_bats_cr <- st_as_sf(plots_bats_cr, coords=c('Long', 'Lat'))
st_crs(plots_bats_cr) <- 4326
print(plots_bats_cr)


# Create a modelling extent for plotting and cropping the global data.
model_extent <- extent(c(-85,-83,9,11))

# Plot site data over map
plot(st_geometry(sne110), xlim=model_extent[1:2], ylim=model_extent[3:4], 
     bg='lightblue', col='ivory')
plot(st_geometry(plots_bats_cr), add=TRUE, col='red', pch=4, cex=0.6)
box()


#### Create a polygon around sites ####


### create polygon around sites with 0.1 buffer
sites_region <- st_as_sfc(st_bbox(plots_bats_cr))
sites_region <- st_buffer(sites_region, 0.1)

### find min and max long and lat to use in google earth engine
print(sites_region)

## plot the polygon onto map
plot(st_geometry(sites_region), add=TRUE, col='red')

## plot the sites back on to make sure they all fall into polygon
plot(st_geometry(plots_bats_cr), add=TRUE, col='black', pch=4, cex=0.6)




##### PERU SITES #####

setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Mammal_BioFrag_Data")

plots_bats_peru <- read.csv("PID0114_Bats_Amazonia_Peru_wet/PID0114_Plot.csv")

plots_bats_peru <- st_as_sf(plots_bats_peru, coords=c('Longitude', 'Latitude'))
st_crs(plots_bats_peru) <- 4326
print(plots_bats_peru)


# Create a modelling extent for plotting and cropping the global data.
model_extent <- extent(c(-74,-72,-5,-3))

# Plot site data over map
plot(st_geometry(sne110), xlim=model_extent[1:2], ylim=model_extent[3:4], 
     bg='lightblue', col='ivory')
plot(st_geometry(plots_bats_peru), add=TRUE, col='red', pch=4, cex=0.6)
box()


#### Create a polygon around sites ####


### create polygon around sites with 0.1 buffer
sites_region <- st_as_sfc(st_bbox(plots_bats_peru))
sites_region <- st_buffer(sites_region, 0.1)

### find min and max long and lat to use in google earth engine
print(sites_region)

## plot the polygon onto map
plot(st_geometry(sites_region), add=TRUE, col='red')

## plot the sites back on to make sure they all fall into polygon
plot(st_geometry(plots_bats_peru), add=TRUE, col='black', pch=4, cex=0.6)




##### SAO PAULO SITES #####

setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Mammal_BioFrag_Data")

plots_bats_saop <- read.csv("PID0222_Sao_Paulo_Bats/PID0222_Plot.csv")

plots_bats_saop <- st_as_sf(plots_bats_saop, coords=c('Longitude', 'Latitude'))
st_crs(plots_bats_saop) <- 4326
print(plots_bats_saop)


# Create a modelling extent for plotting and cropping the global data.
model_extent <- extent(c(-50,-45,-24,-19))

# Plot site data over map
plot(st_geometry(sne110), xlim=model_extent[1:2], ylim=model_extent[3:4], 
     bg='lightblue', col='ivory')
plot(st_geometry(plots_bats_saop), add=TRUE, col='red', pch=4, cex=0.6)
box()


#### Create a polygon around sites ####


### create polygon around sites with 0.1 buffer
sites_region <- st_as_sfc(st_bbox(plots_bats_saop))
sites_region <- st_buffer(sites_region, 11000)


### find min and max long and lat to use in google earth engine
print(sites_region)

## plot the polygon onto map
plot(st_geometry(sites_region), add=TRUE, col='red')

## plot the sites back on to make sure they all fall into polygon
plot(st_geometry(plots_bats_saop), add=TRUE, col='black', pch=4, cex=0.6)


