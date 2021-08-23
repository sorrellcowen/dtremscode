#### PERU ####

setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Forest_Cover/Peru")

### load tif file and convert to raster

str_name <-'Peru_Polygon.tif' 
peruraster <- raster(str_name)
plot(peruraster)

### look at summary of raster
print(peruraster)

### change projection to match mexico site locations
peruraster <- projectRaster(peruraster, crs = 4326)
peruraster[peruraster < 0] <- 0
peruraster[peruraster > 100] <- 100

## disaggregate data to get values of pixels

# Copy parents
square_disagg <- disaggregate(peruraster, fact=2)
# Interpolate
square_disagg_interp <- disaggregate(peruraster, fact=2, method='bilinear')
plot(square_disagg_interp)

### plot CR points over the top
plot(st_geometry(plots_bats_peru), add=TRUE, col='black', pch=4, cex=0.6)


### investigate values of disaggregated data
print(square_disagg_interp)

cellStats(square_disagg_interp, quantile)
cellStats(square_disagg_interp, mean)
# Which is the highest cell
which.max(square_disagg_interp)

### make loop to calculate individual buffers for each site, in this case we have used 600m as example (0.006 degrees)
for (i in plots_bats_peru) {
  sites_buffer <- st_buffer(plots_bats_peru$geometry, 0.006)
}

## plot buffers onto raster
plot(st_geometry(sites_buffer), add=TRUE, col="red")

### calculate average forest cover for each buffer
sites_buffer <- st_as_sf(sites_buffer)

#### extract pixel values for buffer sites
r.vals <- extract(square_disagg_interp, sites_buffer)
#### make list of values into usable dataframe of values
n.obs <- sapply(r.vals, length)
seq.max <- seq_len(max(n.obs))
df <- t(sapply(r.vals, "[", i = seq.max))
df <- as.data.frame(df)

### change names and transpose the data frame
rownames(df) <- plots_bats_peru$Sites
df2 <- t(df)
df2 <- as.data.frame(df2)

### figure out means for each site
mean_forest_peru_sites <- colMeans(df2, na.rm=T)
mean_forest_peru_sites <- as.data.frame(mean_forest_peru_sites)

## reorganise dataframe
library(dplyr)
mean_forest_peru_sites <- tibble::rownames_to_column(mean_forest_peru_sites, "Sites")
colnames(mean_forest_peru_sites) <- c("Sites","Mean_Forest_Cover")

## save dataframe as csv
write.csv(mean_forest_peru_sites, "C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Forest_Cover/Peru/Peru_Sites_Forest_Cover_0.006degreebuffer.csv", row.names = FALSE)