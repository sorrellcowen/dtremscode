setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK")

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

## load in data
bigboi <- read.csv("Occupancy_and_Range_Edge_Distance_Long_Format.csv", header = T)

setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Forest_Cover")
braz <- read.csv("Brazil/Hansen_Brazil_Bat_Forest_Cover_1250m.csv")
peru <- read.csv("Peru/Hansen_Peru_Bat_Forest_Cover_1250m.csv")
cr <- read.csv("Costa_Rica/Hansen_Costa_Rica_Bat_Forest_Cover_1250m.csv")
batmex <- read.csv("Mexico/Hansen_Mexico_Bat_Forest_Cover_1250m.csv")
mammex <- read.csv("Mexico/Hansen_Mexico_Mammal_Forest_Cover_1250m.csv")
saop <- read.csv("Brazil/Hansen_Sao_Paulo_Bats_Forest_Cover_1250m.csv")


## bind forest cover data
forestcover <- rbind(braz,peru,cr,batmex,mammex,saop)
summary(forestcover)
## subset to important parts
subforestcover <- forestcover[,c("plot_id","value")]
## change column names
colnames(subforestcover) <- c("Plot","Forest_Cover_Percentage")

## check for duplicate plots
duplicated(subforestcover$Plot)
## remove duplicates
subforestcover <- subforestcover[-c(60,61,62),]

### merge forest cover data with bigboi dataset 
randf <- merge(bigboi,subforestcover, by="Plot")


range(randf$Range_Edge_Distance)
hist(randf$Range_Edge_Distance)
hist(randf$Forest_Cover_Percentage)

# make distance to range edge in km
randf$Edge_Dist_in_km <- randf$Range_Edge_Distance/1000

randf$posdistancekm <- abs(randf$Edge_Dist_in_km)
randf$sqrtdistancekmpos <- sqrt(randf$posdistancekm)


randf$sqrt_dist_edge <- ifelse(randf$Edge_Dist_in_km < 0, randf$sqrtdistancekmpos * -1, randf$sqrtdistancekmpos * 1)
randf$forest_cover_prop <- randf$Forest_Cover_Percentage/100



## write csv
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK")
write.csv(randf, "Hansen_Range_Edge_Forest_Cover_Incidence.csv", row.names = F)




