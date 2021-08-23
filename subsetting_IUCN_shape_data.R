setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK")

library(rgdal)
library(raster)
library(sf)
library(sp)
library(dismo)
library(rgdal)
library(nngeo)
# Load some (coarse) country background data
setwd("C:/Users/solco/OneDrive/TFE/Week 5 GIS")
sne110 <- st_read('data/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp')

# Load in mammal shape file
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/IUCN Mammal Range Data")
mammalshape <- st_read("MAMMALS_TERRESTRIAL_ONLY.shp")
summary(mammalshape)

## subset dataset to just get bits of interest
mambi <- mammalshape[,c("binomial","order_","family","SHAPE_Leng","SHAPE_Area","geometry")]

## behrmann projection
behr <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m"


## need to subset for mammals of interest, all 150 of them!! Use names from arcGIS/similar_species.R code
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Mammal_BioFrag_Data")

bats_jari <- read.csv("PID0058_Jari_Bats/Species_Meta.csv")
mammals_jari <- read.csv("PID0069_Jari_SmallMammals/Species_Meta.csv")
bats_mexico <- read.csv("PID0093_Mexico_Bats/Species_Meta.csv")
mammals_mexico <- read.csv("PID0092_Mexico_Mammals/Species_Meta.csv")
batsperu <- read.csv("PID0143_Peru_Species_Meta.csv")
batscr <- read.csv("PID0144_CR_Species_Meta.csv")
batssaop <- read.csv("PID0222_Sao_Paulo_Bats/PID0222_Species_Meta.csv")

####### count total species I have
batsbrazil <- bats_jari$Species_IUCN
mambrazil <- mammals_jari$Species_IUCN
batsmex <- bats_mexico$Species_IUCN
mammex <- mammals_mexico$Species_IUCN
batsperu <- batsperu$Species_IUCN
batscr <- batscr$Species_IUCN
batssaop <- batssaop$Species

## make into one list
l <- list(mambrazil=mambrazil,batsbrazil=batsbrazil,batsperu=batsperu,batscr=batscr,batsmex=batsmex,mammex=mammex, batssaop=batssaop)

#turn the lists into data frame with NAs
dfl <-sapply(l, '[', seq(max(sapply(l, length))))

#Counting the number of occurrences of each species 
n_occur <- data.frame(table(dfl))
colnames(n_occur) <- c("Species_IUCN","Frequency")
n_occur
nrow(n_occur)

## some names differ between shape file and n_occur dataset, so have to change
## them before the list of names can be used to subset
mambi$binomial <- gsub("Dermanura anderseni","Artibeus anderseni", mambi$binomial)
mambi$binomial <- gsub("Dermanura cinerea","Artibeus cinereus", mambi$binomial)
mambi$binomial <- gsub("Dermanura gnoma","Artibeus gnomus", mambi$binomial)
mambi$binomial <- gsub("Dermanura phaeotis","Artibeus phaeotis", mambi$binomial)
mambi$binomial <- gsub("Dermanura tolteca","Artibeus toltecus", mambi$binomial)
mambi$binomial <- gsub("Dermanura watsoni","Artibeus watsoni", mambi$binomial)
mambi$binomial <- gsub("Marmosa demerarae","Micoureus demerarae", mambi$binomial)
mambi$binomial <- gsub("Gardnerycteris crenulatum","Mimon crenulatum", mambi$binomial)
mambi$binomial <- gsub("Euryoryzomys emmonsae","Oryzomys emmonsae", mambi$binomial)
mambi$binomial <- gsub("Euryoryzomys macconnelli","Oryzomys macconnelli", mambi$binomial)
mambi$binomial <- gsub("Hylaeamys megacephalus","Oryzomys megacephalus", mambi$binomial)
mambi$binomial <- gsub("Lophostoma schulzi","Tonatia schulzi", mambi$binomial)
mambi$binomial <- gsub("Vampyriscus bidens","Vampyressa bidens", mambi$binomial)
mambi$binomial <- gsub("Vampyriscus brocki","Vampyressa brocki", mambi$binomial)
mambi$binomial <- gsub("Vampyriscus nymphaea","Vampyressa nymphaea", mambi$binomial)


## ones to get rid of from n_occur, either no range or genus level only
n_occur <- subset(n_occur, n_occur$Species_IUCN!="Chironectes minimus")
n_occur <- subset(n_occur, n_occur$Species_IUCN!="Lontra longicaudis")
n_occur <- subset(n_occur, n_occur$Species_IUCN!="Marmosops sp")
n_occur <- subset(n_occur, n_occur$Species_IUCN!="Micoureus sp")
n_occur <- subset(n_occur, n_occur$Species_IUCN!="Neusticomys oyapocki")
n_occur <- subset(n_occur, n_occur$Species_IUCN!="Oryzomys sp")
n_occur <- subset(n_occur, n_occur$Species_IUCN!="Tapirus bairdii")
n_occur <- subset(n_occur, n_occur$Species_IUCN!="Anoura geoffroyi")

length(unique(n_occur$Species_IUCN))
## subset to mammals of interest
mambi2 <- mambi[mambi$binomial %in% n_occur$Species_IUCN,]
head(mambi2)
length(unique(mambi2$binomial))
mambi2 <- mambi2[order(mambi2$binomial),]

diff <- setdiff(n_occur$Species_IUCN,mambi2$binomial)
diff

## transform shape data
mambi2 <- st_transform(mambi2, crs=behr)

## save subsetted ranges
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Mammal_Ranges")
st_write(mambi2, 'mammal_ranges_of_interest.shp', delete_layer=TRUE)

## check if valid
st_is_valid(mambi2)
check <- st_is_valid(mambi2, reason=T)
check

# Condense this to get one feature per species:
# - get a list with of single species sf dataframes
mambi2 <- split(mambi2, mambi2$binomial)

# - apply st_union across those to get a single (multi)polyline per species
mambi2 <- lapply(mambi2, st_union)

# - put them back together into an sf object
mambi2_sfc <- do.call(c, mambi2)
mambi2 <- st_as_sf(data.frame(SCINAME = names(mambi2),
                             geometry = mambi2_sfc))

## set coordinate projection
st_crs(mambi2) <- behr

# save the file
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Mammal_Ranges")
st_write(mambi2, 'cleaned_ranges.shp', delete_layer=TRUE)
