#### COSTA RICA ####

setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Forest_Cover/Costa_Rica")

### load tif file and convert to raster

str_name<-'Costa_Rica_Polygon.tif' 
cr_raster <- raster(str_name)
plot(cr_raster)

### look at summary of raster
print(cr_raster)

### change projection to match mexico site locations
cr_raster <- projectRaster(cr_raster, crs = 4326)
cr_raster[cr_raster < 0] <- 0

## disaggregate data to get values of pixels

# Copy parents
square_disagg <- disaggregate(cr_raster, fact=2)
# Interpolate
square_disagg_interp <- disaggregate(cr_raster, fact=2, method='bilinear')
plot(square_disagg_interp)

### plot CR points over the top
plot(st_geometry(plots_bats_cr), add=TRUE, col='black', pch=4, cex=0.6)


### investigate values of disaggregated data
print(square_disagg_interp)

cellStats(square_disagg_interp, quantile)
cellStats(square_disagg_interp, mean)
# Which is the highest cell
which.max(square_disagg_interp)

### make loop to calculate individual buffers for each site, in this case we have used 600m as example (0.006 degrees)
for (i in plots_bats_cr) {
  sites_buffer <- st_buffer(plots_bats_cr$geometry, 0.006)
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
rownames(df) <- plots_bats_cr$Plot
df2 <- t(df)
df2 <- as.data.frame(df2)

### figure out means for each site
mean_forest_cr_sites <- colMeans(df2, na.rm=T)
mean_forest_cr_sites <- as.data.frame(mean_forest_cr_sites)