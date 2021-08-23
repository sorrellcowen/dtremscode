setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Mammal_BioFrag_Data")


library(smoothr)
library(rmapshaper)
library(rgdal)
library(raster)
library(sf)
library(sp)
library(dismo)
library(rgdal)
library(dplyr)
library(tidyr)
library(ggplot2)
library(units)

## behrmann projection
behr <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m"

## Load in Species Matrix, Plot Data, Mammal continental margins

# species matrix
jarisp <- read.csv("PID0058_Jari_Bats/PID0058_species_matrix.csv")
mamjarisp <- read.csv("PID0069_Jari_SmallMammals/PID0069_species_matrix.csv")
mammexsp <- read.csv("PID0092_Mexico_Mammals/PID0092_species_matrix.csv")
mexsp <- read.csv("PID0093_Mexico_Bats/PID0093_species_matrix.csv")
perusp <- read.csv("PID0143_Peru_species_matrix.csv")
crsp <- read.csv("PID0144_Costa_Rica_species_matrix.csv")
saopsp <- read.csv("PID0222_Sao_Paulo_Bats/PID0222_species_matrix.csv")


# plot
jariplot <- read.csv("PID0058_Jari_Bats/PID0058_Plot.csv")
mamjariplot <- read.csv("PID0069_Jari_SmallMammals/PID0069_Plot.csv")
mammexplot <- read.csv("PID0092_Mexico_Mammals/PID0092_Plot.csv")
mexplot <- read.csv("PID0093_Mexico_Bats/PID0093_Plot.csv")
peruplot <- read.csv("PID0114_Bats_Amazonia_Peru_wet/PID0114_Plot.csv")
crplot <- read.csv("PID0116_Bats_Costa_Rica_wet/PID0116_Plot.csv")
saopplot <- read.csv("PID0222_Sao_Paulo_Bats/PID0222_Plot.csv")

# continental margins
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Mammal_Ranges")
mamrange <- st_read("Mammal_continental_range_margins.shp")
st_crs(mamrange) <- behr
head(mamrange)

# mammal range polygons
mampoly <- st_read("cleaned_ranges.shp")
head(mampoly)
colnames(mampoly)[1] <- "Species"
diff <- setdiff(mampoly$Species, mamrange$SCINAME)
mampoly <- subset(mampoly, Species!="Natalus stramineus")
mampoly <- subset(mampoly, Species!="Pteronotus parnellii")
st_crs(mampoly) <- behr
mampoly <- mampoly[order(mampoly$Species),]
head(mampoly)
tail(mampoly)

### change variable names to match other data sets
mampoly$Species <- gsub(" ", "\\.", mampoly$Species)
summary(mampoly)
mampoly$Species <- tolower(mampoly$Species)

## split polygon ranges for later
mampolysplit <- split(mampoly, mampoly$Species)


## now have to pivot longer the species matrix dataframes
jarisp <- pivot_longer(jarisp, !Plot, names_to = "Species", values_to = "Abundance")
mamjarisp <- pivot_longer(mamjarisp, !Plot, names_to = "Species", values_to = "Abundance")
mammexsp <- pivot_longer(mammexsp, !Plot, names_to = "Species", values_to = "Abundance")
mexsp <- pivot_longer(mexsp, !Plot, names_to = "Species", values_to = "Abundance")
perusp <- pivot_longer(perusp, !Plot, names_to = "Species", values_to = "Abundance")
crsp <- pivot_longer(crsp, !Plot, names_to = "Species", values_to = "Abundance")
saopsp <- pivot_longer(saopsp, !Plot, names_to = "Species", values_to = "Abundance")

## add PID column
jarisp$PID <- "PID0058"
mamjarisp$PID <- "PID0069"
mammexsp$PID <- "PID0092"
mexsp$PID <- "PID0093"
perusp$PID <- "PID0143"
crsp$PID <- "PID0144"
saopsp$PID <- "PID0222"

## rbind these together
totalsp <- rbind(jarisp,mamjarisp,mammexsp,mexsp,perusp,crsp,saopsp)
## create incidence column
totalsp[is.na(totalsp)] <- 0
totalsp$Abundance <- as.numeric(totalsp$Abundance)
totalsp$Incidence <- 0
totalsp$Incidence[totalsp$Abundance >= 1] <- 1

## save file
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK")
write.csv(totalsp, "Mammal_BioFrag_Data/long_version_total_mammal_incidence.csv", row.names = FALSE)

## add location data for each site
summary(crplot)
colnames(mammexplot)[1] <- "Plot"
colnames(peruplot)[1] <- "Plot"
colnames(mexplot)[2:3] <- c("Latitude", "Longitude")
colnames(crplot)[2:3] <- c("Latitude", "Longitude")
summary(saopplot)

## rbind location data (no need to bind mamjariplot as plots are identical to jariplot)
locs <- rbind(jariplot,mammexplot,mexplot,peruplot,crplot,saopplot)

## check for duplicates
which(duplicated(locs$Latitude))
which(duplicated(locs$Longitude))
## is one duplicate, so make sure to keep an eye out

summary(locs)
locs <- locs[,-4]
## merge plot and sp incidence data
spandlocs <- merge(totalsp, locs, by="Plot")

## make coordinates actual coordinates
spandlocs <- st_as_sf(spandlocs, coords=c("Longitude","Latitude"))
st_crs(spandlocs) <- 4326
spandlocs <- st_transform(spandlocs, crs=behr)

## summary of two dataframes needed
summary(mamrange)
summary(spandlocs)

## make variable names the same
mamrange$SCINAME <- gsub(" ", "\\.", mamrange$SCINAME)
summary(mamrange)
mamrange$SCINAME <- tolower(mamrange$SCINAME)

spandlocs$Species <- gsub("_", "\\.", spandlocs$Species)
spandlocs$Species <- tolower(spandlocs$Species)

length(unique(spandlocs$Species))
length(unique(mamrange$SCINAME))

species_dropped <- setdiff(mamrange$SCINAME, spandlocs$Species)
species_dropped

## try to reduce spandlocs dataframe to same species that we have ranges for
spandlocs2 <- spandlocs[spandlocs$Species %in% mamrange$SCINAME,]
length(unique(spandlocs2$Species))

# change name in mamrange to match spandlocs2
colnames(mamrange)[1] <- "Species"

## split spandlocs2 and mamrange by species
spandlocssplit <- split(spandlocs2, spandlocs2$Species)
mamrangesplit <- split(mamrange, mamrange$Species)

# plot(st_geometry(removed$desmodus.rotundus$geometry), col="red")
# plot(st_geometry(mamrangesplit$desmodus.rotundus$geometry, col="blue", add=T))
##### CALCULATING DISTANCE TO RANGE EDGE #####

## got to figure out how to somehow make this into for loop for each species

# example species - Ametrida.centurio
# st_distance(spandlocssplit$ametrida.centurio, mamrangesplit$ametrida.centurio)
# spandlocssplit$ametrida.centurio$Range_Edge_Distance <- st_distance(spandlocssplit$ametrida.centurio, mamrangesplit$ametrida.centurio)
# plot(st_geometry(mamrangesplit$ametrida.centurio$geometry))
# plot(st_geometry(spandlocssplit$ametrida.centurio$geometry), add=T, col="red")

# example species - Desmodus.rotundus
# st_distance(spandlocssplit$desmodus.rotundus, mamrangesplit$desmodus.rotundus)
# spandlocssplit$desmodus.rotundus$Range_Edge_Distance <- st_distance(spandlocssplit$desmodus.rotundus, mamrangesplit$desmodus.rotundus)
# plot(st_geometry(mamrangesplit$desmodus.rotundus$geometry))
# plot(st_geometry(spandlocssplit$desmodus.rotundus$geometry), add=T, col="red")
## seems to be a problem, seems to be some areas of range edge
## within the actual range which is skewing distance measurements

# example species - Vampyrum.spectrum
# st_distance(spandlocssplit$vampyrum.spectrum, inbitsgone$vampyrum.spectrum)
# spandlocssplit$vampyrum.spectrum$Range_Edge_Distance <- st_distance(spandlocssplit$vampyrum.spectrum, mamrangesplit$vampyrum.spectrum)
# plot(st_geometry(mamrangesplit$vampyrum.spectrum$geometry), col="red")
# plot(st_geometry(inbitsgone$vampyrum.spectrum$geometry), col="blue", add=T)
# plot(st_geometry(spandlocssplit$vampyrum.spectrum$geometry), add=T, col="red")
### does brazil site fall outside of range? Yes
## means need to figure out way to make distances negative

## does this work??
#distances = st_distance(spandlocs2, mamrange)
#summary(distances)
#distances <- as.data.frame(distances)
## I mean, get's distance for each species for each site & species, but problem still

### I do not know how to do for loops
## for(i in spandlocssplit) {
  
  ## Just get borders and ranges for the species we are interested in
  ##distances = drop_units(st_distance(i, mamrangesplit))

## }
## doesn't work when both are lists

## use function!!
fun <- function(spandlocssplit, mamrangesplit, mampolysplit) {
  Distance <- drop_units(st_distance(spandlocssplit, mamrangesplit))
  in_Range <- drop_units(st_distance(spandlocssplit, mampolysplit)) # Check if species is in the range
  final_Distances <- Distance * ((in_Range == 0)* 2 - 1)
  
}
tog <- mapply(fun, spandlocssplit, mamrangesplit, mampolysplit, SIMPLIFY = FALSE)
#### think it works?




### rbind the list into one dataframe
tog <- do.call(rbind, tog)
tog <- as.data.frame(tog)
colnames(tog) <- "Range_Edge_Distance"
## is equal length to species and location dataframe- it worked!!

## add distance to range edge to dataframe
spandlocs2 <- spandlocs2[order(spandlocs2$Species),]
spandlocs2$Range_Edge_Distance <- tog$Range_Edge_Distance

summary(spandlocs2)
hist(spandlocs2$Range_Edge_Distance)
## very skewed!! Is this due to weird bits in middle of range? No


summary(spandlocs2)
### save file
write.csv(spandlocs2, "Occupancy_and_Range_Edge_Distance_Long_Format.csv", row.names = FALSE)
st_write(spandlocs2, 'Occupancy_and_Range_Edge_Distance_Long_Format.shp', delete_layer=TRUE)
