### Mexico ####

setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Forest_Cover/Mexico")

### load tif file and convert to raster

#### TIF FILE COMES FROM INSPECTING GOOGLE EARTH ENGINE IMAGE COLLECTION AND THEN CHOOSING FIRST IMAGE ####

str_name<-'Mexico_Polygon.tif' 
imported_raster <- raster(str_name)
plot(imported_raster)

### look at summary of raster
print(imported_raster)

### change projection to match mexico site locations
mexico_raster <- projectRaster(imported_raster, crs = 4326)
mexico_raster[mexico_raster < 0] <- 0

## disaggregate data to get values of pixels

# Copy parents (method 1)
square_disagg <- disaggregate(mexico_raster, fact=2)
# Interpolate (method 2)
square_disagg_interp <- disaggregate(mexico_raster, fact=2, method='bilinear')

## use interpolate but could also use cop parents method (I think)

### plot and get summary of disaggregated raster
plot(square_disagg_interp)
print(square_disagg_interp)

### plot mexico points over the top
plot(st_geometry(plots_mammals_mexico), add=TRUE, col='black', pch=4, cex=0.6)


### investigate values of disaggregated data
print(square_disagg_interp)

cellStats(square_disagg_interp, quantile)
cellStats(square_disagg_interp, mean)
# Which is the highest cell
which.max(square_disagg_interp)
idx <- which.max(square_disagg_interp)
pos <- xyFromCell(square_disagg_interp,idx)
pos <- as.data.frame(pos)
pos <- st_as_sf(pos, coords = c('x','y'))
plot(st_geometry(pos), add=TRUE, col="black", pch=8, cex=1)


### make loop to calculate individual buffers for each site, in this case we have used 600m as example (0.006 degrees)
for (geometry in plots_mammals_mexico) {
  sites_buffer <- st_buffer(plots_mammals_mexico$geometry, 0.006)
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
rownames(df) <- plots_mammals_mexico$Site
df2 <- t(df)
df2 <- as.data.frame(df2)

### figure out means for each site
mean_forest_mexico_sites <- colMeans(df2, na.rm = TRUE)
mean_forest_mexico_sites <- as.data.frame(mean_forest_mexico_sites)

