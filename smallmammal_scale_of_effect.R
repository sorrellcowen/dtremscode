

########### MAKE SURE TIDYR IS NOT INSTALLED BEFORE EXTRACTING R.VALS #############

library(rgdal)
library(raster)
library(sf)
library(sp)
library(dismo)
library(landscapemetrics)
library(dplyr)
library(stringr)

### small mammal buffer size ####

setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Mammal_BioFrag_Data")
smallmamjari <- read.csv("PID0069_Jari_SmallMammals/PID0069_species_matrix.csv")
smallmamjarimeta <- read.csv("PID0069_Jari_SmallMammals/Species_Meta.csv")
plots_mam_braz <- read.csv("PID0069_Jari_SmallMammals/PID0069_Plot.csv")

## Change lat and long to coords
plots_mam_braz <- st_as_sf(plots_mam_braz, coords=c('Longitude', 'Latitude'))
st_crs(plots_mam_braz) <- 4326

## Transform to projected coordinate system so in metres
plots_mam_braz <- st_transform(plots_mam_braz, crs = 29101)




## Load raster and reproject
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Forest_Cover/Brazil")
str_name <-'overall_brazil_map.tif' 
brazilraster <- raster(str_name)
brazilraster <- projectRaster(brazilraster, crs = 29101)
plot(brazilraster)
## add points
plot(st_geometry(plots_mam_braz), add=T, col="black", pch=4, cex=0.6)

## how to plot buffer now projection is in m, 600m is buffer
for (i in plots_mam_braz) {
  sites_buffer <- st_buffer(plots_mam_braz$geometry, 600)
}

plot(st_geometry(sites_buffer), add=T, col="red")
### 600 is the number we need to figure out,
### have to correlate species richness against buffer size

## calculating species richness per site
smallmamjari[is.na(smallmamjari)] <- 0
mambraznonsite <- smallmamjari[, -1]

## species richness
for (row in 1:nrow(mambraznonsite)) {
  specrich <- rowSums(mambraznonsite > 0)
}

## add the name of site to each value
sites <- smallmamjari[, 1]
specrichmambraz <- do.call(rbind, Map(data.frame, A=sites, B=specrich))


## species accumulation curve with brazil mammal sites
tmambraznonsite <- t(mambraznonsite)
tmambraznonsite <- as.data.frame(tmambraznonsite)
sum(colSums(tmambraznonsite[1:2 , ] > 0) > 0 )
observed.S <- NULL
for(i in 1:nrow(mambraznonsite)){
  observed.S[i] <- sum(colSums(mambraznonsite[1:i , ] > 0) > 0)
}
## species accumulation
observed.S
## species accumulation curve
plot(observed.S, type = 'l', xlab = 'Number of samples',
     ylab = 'Number of species')



#### FOR BRAZIL MAMMAL BUFFER CALC ####

colnames(specrichmambraz) <- c("Plot","SpecRich")
specrichmambraz

## how to plot buffer now projection is in m
for (i in plots_mam_braz) {
  sites_buffer_300 <- st_buffer(plots_mam_braz$geometry, 300)
  sites_buffer_600 <- st_buffer(plots_mam_braz$geometry, 600)
  sites_buffer_900 <- st_buffer(plots_mam_braz$geometry, 900)
  sites_buffer_1200 <- st_buffer(plots_mam_braz$geometry, 1200)
  sites_buffer_1500 <- st_buffer(plots_mam_braz$geometry, 1500)
  sites_buffer_1800 <- st_buffer(plots_mam_braz$geometry, 1800)
}
plot(brazilraster)
print(brazilraster)
brazilraster[brazilraster < 0] <- 0
brazilraster[brazilraster > 100] <- 100

plot(st_geometry(plots_mam_braz), add=TRUE, col="black", pch=4, cex=0.6)

bufferlist <- list(sites_buffer_300,sites_buffer_600,sites_buffer_900,sites_buffer_1200,sites_buffer_1500, sites_buffer_1800)
bufferlist <- c(sites_buffer_300,sites_buffer_600,sites_buffer_900,sites_buffer_1200,sites_buffer_1500, sites_buffer_1800)

## detach tidyr
detach("package:tidyr", unload=TRUE)
for (b in bufferlist) {
  bufferlist <- st_as_sf(bufferlist)
  
  #### extract pixel values for buffer sites
  r.vals <- extract(brazilraster, bufferlist)
  
  #### make list of values into usable dataframe of values
  n.obs <- sapply(r.vals, length)
  seq.max <- seq_len(max(n.obs))
  df <- t(sapply(r.vals, "[", i = seq.max))
  df <- as.data.frame(df)
  
  
  ### change names and transpose the data frame
  ## rownames(df) <- rep(plots_mam_braz$Plot)
  df2 <- t(df)
  df2 <- as.data.frame(df2)
  
  ### figure out means for each site
  forestbuffertogether <- colMeans(df2, na.rm=T)
  forestbuffertogether <- as.data.frame(forestbuffertogether)
  
  ## reorganise dataframe
  forestbuffertogether <- tibble::rownames_to_column(forestbuffertogether, "Sites")
  colnames(forestbuffertogether) <- c("Sites","buffer_forest_all")
  
  ## add site column and buffer column
  Plot <- rep(plots_mam_braz$Plot, 6)
  Buffer <- rep(c(300, 600, 900, 1200, 1500, 1800), each = 15)
  SpeciesRichness <- rep(specrichmambraz$SpecRich, 6)
  forestbuffer <- cbind(forestbuffertogether, Buffer, Plot, SpeciesRichness)
  mambrazforestbuffer <- forestbuffer[, -1]
}

library(tidyr)
mambrazforestbuffer <- spread(mambrazforestbuffer,key=Buffer, value=buffer_forest_all)
summary(mambrazforestbuffer)
specrichmambraz

lm300 <- lm(SpeciesRichness~`300`, data=forestbuffer)
summary(lm300)
lm600 <- lm(SpeciesRichness~`600`, data=forestbuffer)
summary(lm600)
lm900 <- lm(SpeciesRichness~`900`, data=forestbuffer)
summary(lm900)
lm1200 <- lm(SpeciesRichness~`1200`, data=forestbuffer)
summary(lm1200)
lm1500 <- lm(SpeciesRichness~`1500`, data=forestbuffer)
summary(lm1500)
lm1800 <- lm(SpeciesRichness~`1800`, data=forestbuffer)
summary(lm1800)

boofer <- c(300,600,900,1200,1500,1800)
rsquared <- c(0.0241, 0.04165, 0.05775, 0.07561, 0.09018, 0.08778)
plot(boofer, rsquared, type="b")

