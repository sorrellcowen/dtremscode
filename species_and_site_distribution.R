#### final project work ##

#### my own go at modelling species distribution ####

install.packages('raster') # Core raster GIS data package
install.packages('sf') # Core vector GIS data package
install.packages('sp') # Another core vector GIS package
install.packages('dismo') # Species Distribution Modelling
install.packages('rgdal') # Interface to the Geospatial Data Abstraction Library

library(rgdal)
library(raster)
library(sf)
library(sp)
library(dismo)

# Load some (coarse) country background data
setwd("C:/Users/solco/OneDrive/TFE/Week 5 GIS")
sne110 <- st_read('data/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp')

# Load in mammal shape file
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/IUCN Mammal Range Data")
mammalshape <- st_read("MAMMALS_TERRESTRIAL_ONLY.shp")

#### plot POLYGONS of Desmodus rotundus species ####
desmodus <- subset(mammalshape, binomial=="Desmodus rotundus")

#### see where desmodus are in the world
plot(st_geometry(sne110))
plot(st_geometry(desmodus), add=TRUE, col='red', pch=4, cex=0.6)

## change model extent for map ###
par(mfrow=c(1,1))
model_extent <- extent(c(-120,-20,-35,30))
# Plot the species data over a basemap
plot(st_geometry(sne110), xlim=model_extent[1:2], ylim=model_extent[3:4], 
     bg='lightblue', col='ivory')
plot(st_geometry(desmodus), add=TRUE, col='red', pch=3, cex=0.6)
box()
## we have a distribution map for the Common Vampire Bat!!! YAY

##### lay sites over the top ####
# mexico
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Mammal_BioFrag_Data")
plots_mammals_mexico <- read.csv("PID0092_Mexico_Mammals/PID0092_Plot.csv")
plots_mammals_mexico <- st_as_sf(plots_mammals_mexico, coords=c('Longitude', 'Latitude'))
st_crs(plots_mammals_mexico) <- 4326
print(plots_mammals_mexico)
plot(st_geometry(plots_mammals_mexico), add=TRUE, col='black', pch=4, cex=0.6)

# brazil
plots_bats_jari <- read.csv("PID0058_Jari_Bats/PID0058_Plot.csv")
plots_bats_jari <- st_as_sf(plots_bats_jari, coords=c('Longitude', 'Latitude'))
st_crs(plots_bats_jari) <- 4326
print(plots_bats_jari)
plot(st_geometry(plots_bats_jari), add=TRUE, col='black', pch=4, cex=0.6)

# costa rica
plots_bats_cr <- read.csv("PID0116_Bats_Costa_Rica_wet/PID0116_Plot.csv")
plots_bats_cr <- st_as_sf(plots_bats_cr, coords=c('Long', 'Lat'))
st_crs(plots_bats_cr) <- 4326
print(plots_bats_cr)
plot(st_geometry(plots_bats_cr), add=TRUE, col='black', pch=4, cex=0.6)

# peru
plots_bats_peru <- read.csv("PID0114_Bats_Amazonia_Peru_wet/PID0114_Plot.csv")
plots_bats_peru <- st_as_sf(plots_bats_peru, coords=c('Longitude', 'Latitude'))
st_crs(plots_bats_peru) <- 4326
print(plots_bats_peru)
plot(st_geometry(plots_bats_peru), add=TRUE, col='black', pch=4, cex=0.6)


#### ARBITEUS SPECIES ####

## plot POLYGONS of Artibeus lituratus species 
artibeus <- subset(mammalshape, binomial=="Artibeus lituratus")

#### see where artibeus are in the world
plot(st_geometry(sne110))
plot(st_geometry(artibeus), add=TRUE, col='green', pch=4, cex=0.6)

## change model extent for map ###
par(mfrow=c(1,1))
model_extent <- extent(c(-120,-20,-35,30))
# Plot the species data over a basemap
plot(st_geometry(sne110), xlim=model_extent[1:2], ylim=model_extent[3:4], 
     bg='lightblue', col='ivory')
plot(st_geometry(artibeus), add=TRUE, col='green', pch=3, cex=0.6)
box()
## we have a distribution map for the Great Fruit Eating Bat!!! YAY


#### CAROLLIA SPECIES ####

## plot POLYGONS of Carollia perspicillata species 
carollia <- subset(mammalshape, binomial=="Carollia perspicillata")

#### see where carollia are in the world
plot(st_geometry(sne110))
plot(st_geometry(carollia), add=TRUE, col='blue', pch=4, cex=0.6)

## change model extent for map ###
par(mfrow=c(1,1))
model_extent <- extent(c(-120,-20,-35,30))
# Plot the species data over a basemap
plot(st_geometry(sne110), xlim=model_extent[1:2], ylim=model_extent[3:4], 
     bg='lightblue', col='ivory')
plot(st_geometry(carollia), add=TRUE, col='blue', pch=3, cex=0.6)
box()
## we have a distribution map for the Seba's short-tailed bat!!! YAY

## check mexico site falls in range for Carollia
## change model extent for map ###
par(mfrow=c(1,1))
model_extent <- extent(c(-93,-91,15,17))

plot(st_geometry(sne110), xlim=model_extent[1:2], ylim=model_extent[3:4], 
     bg='lightblue', col='ivory')
plot(st_geometry(carollia), add=TRUE, col='blue', pch=3, cex=0.6)
plot(st_geometry(plots_mammals_mexico), add=TRUE, col='red', pch=4, cex=0.6)

## they do! thank god




#### CHIRODERMA SPECIES ####

## plot POLYGONS of Chiroderma villosum species 
chiroderma <- subset(mammalshape, binomial=="Chiroderma villosum")

#### see where chiroderma are in the world
plot(st_geometry(sne110))
plot(st_geometry(chiroderma), add=TRUE, col='orange', pch=4, cex=0.6)

## change model extent for map ###
par(mfrow=c(1,1))
model_extent <- extent(c(-120,-20,-35,30))
# Plot the species data over a basemap
plot(st_geometry(sne110), xlim=model_extent[1:2], ylim=model_extent[3:4], 
     bg='lightblue', col='ivory')
plot(st_geometry(chiroderma), add=TRUE, col='orange', pch=3, cex=0.6)
box()
## we have a distribution map for the Hairy big-eyed bat!!! YAY



#### GLOSSOPHAGA  SPECIES ####

## plot POLYGONS of Glossophaga soricina species 
glossophaga <- subset(mammalshape, binomial=="Glossophaga soricina")

#### see where glossophaga are in the world
plot(st_geometry(sne110))
plot(st_geometry(glossophaga), add=TRUE, col='yellow', pch=4, cex=0.6)

## change model extent for map ###
par(mfrow=c(1,1))
model_extent <- extent(c(-120,-20,-35,30))
# Plot the species data over a basemap
plot(st_geometry(sne110), xlim=model_extent[1:2], ylim=model_extent[3:4], 
     bg='lightblue', col='ivory')
plot(st_geometry(glossophaga), add=TRUE, col='yellow', pch=3, cex=0.6)
box()
## we have a distribution map for the Pallas's long-tongued bat!!! YAY



#### PLATYRRHINUS  SPECIES ####

## plot POLYGONS of Platyrrhinus helleri species 
platyrrhinus <- subset(mammalshape, binomial=="Platyrrhinus helleri")

#### see where glossophaga are in the world
plot(st_geometry(sne110))
plot(st_geometry(glossophaga), add=TRUE, col='yellow', pch=4, cex=0.6)

## change model extent for map ###
par(mfrow=c(1,1))
model_extent <- extent(c(-120,-20,-35,30))
# Plot the species data over a basemap
plot(st_geometry(sne110), xlim=model_extent[1:2], ylim=model_extent[3:4], 
     bg='lightblue', col='ivory')
plot(st_geometry(glossophaga), add=TRUE, col='yellow', pch=3, cex=0.6)
box()
## we have a distribution map for the Pallas's long-tongued bat!!! YAY




#### STURNIRA SPECIES ####

## plot POLYGONS of Sturnira lilium species 
sturnira <- subset(mammalshape, binomial=="Sturnira lilium")

#### see where sturnira are in the world
plot(st_geometry(sne110))
plot(st_geometry(sturnira), add=TRUE, col='grey', pch=4, cex=0.6)

## change model extent for map ###
par(mfrow=c(1,1))
model_extent <- extent(c(-120,-20,-35,30))
# Plot the species data over a basemap
plot(st_geometry(sne110), xlim=model_extent[1:2], ylim=model_extent[3:4], 
     bg='lightblue', col='ivory')
plot(st_geometry(sturnira), add=TRUE, col='purple', pch=3, cex=0.6)
box()
## we have a distribution map for the Little yellow-shouldered bat!!!

## CANNOT USE IT HOWEVER AS RANGE DOES NOT ENCOMPASS ANY OF THE SITES IT WAS OBSERVED AT



#### TRACHOPS SPECIES ####

## plot POLYGONS of Trachops cirrhosus species 
trachops <- subset(mammalshape, binomial=="Trachops cirrhosus")

#### see where trachops are in the world
plot(st_geometry(sne110))
plot(st_geometry(trachops), add=TRUE, col='dark blue', pch=4, cex=0.6)

## change model extent for map ###
par(mfrow=c(1,1))
model_extent <- extent(c(-120,-20,-35,30))
# Plot the species data over a basemap
plot(st_geometry(sne110), xlim=model_extent[1:2], ylim=model_extent[3:4], 
     bg='lightblue', col='ivory')
plot(st_geometry(trachops), add=TRUE, col='dark blue', pch=3, cex=0.6)
box()
## we have a distribution map for the Fringe-lipped bat!!!





#### URODERMA SPECIES ####

## plot POLYGONS of Uroderma bilobatum species 
uroderma <- subset(mammalshape, binomial=="Uroderma bilobatum")

#### see where uroderma are in the world
plot(st_geometry(sne110))
plot(st_geometry(uroderma), add=TRUE, col='gold', pch=4, cex=0.6)

## change model extent for map ###
par(mfrow=c(1,1))
model_extent <- extent(c(-120,-20,-35,30))
# Plot the species data over a basemap
plot(st_geometry(sne110), xlim=model_extent[1:2], ylim=model_extent[3:4], 
     bg='lightblue', col='ivory')
plot(st_geometry(uroderma), add=TRUE, col='gold', pch=3, cex=0.6)
box()
## we have a distribution map for the Tent-making bat!!!

### CANNOT USE AS RANGE ONLY CONTAINS 2 OUT OF 4 SITES - WHAT A SHAME


#### MIMON (Gardnerycteris) SPECIES ####

## plot POLYGONS of Gardnerycteris (Mimon) crenulatum species 
mimon <- subset(mammalshape, binomial=="Gardnerycteris crenulatum")

#### see where carollia are in the world
plot(st_geometry(sne110))
plot(st_geometry(mimon), add=TRUE, col='pink', pch=4, cex=0.6)

## change model extent for map ###
par(mfrow=c(1,1))
model_extent <- extent(c(-120,-20,-35,30))
# Plot the species data over a basemap
plot(st_geometry(sne110), xlim=model_extent[1:2], ylim=model_extent[3:4], 
     bg='lightblue', col='ivory')
plot(st_geometry(mimon), add=TRUE, col='pink', pch=3, cex=0.6)
box()
## we have a distribution map for the Striped hairy-nosed bat!!! YAY

## check mexico site falls in range for Mimon
## change model extent for map ###
par(mfrow=c(1,1))
model_extent <- extent(c(-93,-91,15,17))

plot(st_geometry(sne110), xlim=model_extent[1:2], ylim=model_extent[3:4], 
     bg='lightblue', col='ivory')
plot(st_geometry(mimon), add=TRUE, col='pink', pch=3, cex=0.6)
plot(st_geometry(plots_mammals_mexico), add=TRUE, col='red', pch=4, cex=0.6)

## It half does! Try using it where distance to edge is probably zero, keep it in and see what happens


#### ONLY HAVE 8 BATS OVER 4 LOCATIONS ####

#### Can use Artibeus lituratus, Carollia perspicillata, Chiroderma villosum, Desmodus rotundus, Glossophaga soricina,
#### Platyrrhinus helleri, Trachops cirrhosus, Mimon (Gardnerycteris) crenulatum





#### using gbif data to get point distribution ####

# Check the species (Common Vampire Bat) without downloading - this shows the number of records
gbif('Desmodus', 'rotundus', download=FALSE)

# Download the data
locs <- gbif('Desmodus', 'rotundus')
locs <- subset(locs, ! is.na(lat) | ! is.na(lon))
# Convert to an sf object 
locs <- st_as_sf(locs, coords=c('lon', 'lat'))
st_crs(locs) <- 4326

### check basis of records for locatioNs, remove preserved and fossil specimens ###
View(locs)
desmodus_obvs <- subset(locs, locs$basisOfRecord=="HUMAN_OBSERVATION") 

#### see where desmodus are in the world ###
plot(st_geometry(sne110))
plot(st_geometry(desmodus_obvs), add=TRUE, col='red', pch=4, cex=0.6)

## change model extent for map ###
par(mfrow=c(1,1))
model_extent <- extent(c(-120,-20,-35,30))
# Plot the species data over a basemap
plot(st_geometry(sne110), xlim=model_extent[1:2], ylim=model_extent[3:4], 
     bg='lightblue', col='ivory')
plot(st_geometry(desmodus_obvs), add=TRUE, col='purple', pch=3, cex=0.6)
box()


##### getting locations of sampling sites at each location ####

#### MEXICO SITES ####
### trying to plot sites 
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Mammal_BioFrag_Data")

plots_mammals_mexico <- read.csv("PID0092_Mexico_Mammals/PID0092_Plot.csv")

plots_mammals_mexico <- st_as_sf(plots_mammals_mexico, coords=c('Longitude', 'Latitude'))
st_crs(plots_mammals_mexico) <- 4326
print(plots_mammals_mexico)

# Create a modelling extent for plotting and cropping the global data.
model_extent <- extent(c(-93,-91,15,17))

# Plot site data over map
plot(st_geometry(sne110), xlim=model_extent[1:2], ylim=model_extent[3:4], 
     bg='lightblue', col='ivory')
plot(st_geometry(plots_mammals_mexico), add=TRUE, col='red', pch=4, cex=0.6)
box()


#### BRAZIL SITES ####
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Mammal_BioFrag_Data")

plots_bats_jari <- read.csv("PID0058_Jari_Bats/PID0058_Plot.csv")

plots_bats_jari <- st_as_sf(plots_bats_jari, coords=c('Longitude', 'Latitude'))
st_crs(plots_bats_jari) <- 4326
print(plots_bats_jari)

# Create a modelling extent for plotting and cropping the global data.
model_extent <- extent(c(-56,-52,-3,3))

# Plot site data over map
plot(st_geometry(sne110), xlim=model_extent[1:2], ylim=model_extent[3:4], 
     bg='lightblue', col='ivory')
plot(st_geometry(plots_bats_jari), add=TRUE, col='red', pch=4, cex=0.6)
box()


#### COSTA RICA SITES ####
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Mammal_BioFrag_Data")

plots_bats_cr <- read.csv("PID0116_Bats_Costa_Rica_wet/PID0116_Plot.csv")

plots_bats_cr <- st_as_sf(plots_bats_cr, coords=c('Long', 'Lat'))
st_crs(plots_bats_cr) <- 4326
print(plots_bats_cr)

# Create a modelling extent for plotting and cropping the global data.
model_extent <- extent(c(-87,-82,8,12))

# Plot site data over map
plot(st_geometry(sne110), xlim=model_extent[1:2], ylim=model_extent[3:4], 
     bg='lightblue', col='ivory')
plot(st_geometry(plots_bats_cr), add=TRUE, col='red', pch=4, cex=0.6)
box()


#### PERU SITES ####
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Mammal_BioFrag_Data")

plots_bats_peru <- read.csv("PID0114_Bats_Amazonia_Peru_wet/PID0114_Plot.csv")

plots_bats_peru <- st_as_sf(plots_bats_peru, coords=c('Longitude', 'Latitude'))
st_crs(plots_bats_peru) <- 4326
print(plots_bats_peru)

# Create a modelling extent for plotting and cropping the global data.
model_extent <- extent(c(-75,-70,-5,-2))

# Plot site data over map
plot(st_geometry(sne110), xlim=model_extent[1:2], ylim=model_extent[3:4], 
     bg='lightblue', col='ivory')
plot(st_geometry(plots_bats_peru), add=TRUE, col='red', pch=4, cex=0.6)
box()








