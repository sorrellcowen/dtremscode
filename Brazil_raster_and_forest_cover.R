#### BRAZIL ####
#### IS TWO TIF FILES AS POLYGON GOES OVER TWO SATELLITE IMAGES, SO HAVE TO MERGE IMAGES 

### Importing and merging raster files
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Forest_Cover/Brazil")

str_name<-'Brazil_Polygon_1.tif' 
brazilraster1 <- raster(str_name)
plot(brazilraster1)

str_name <- "Brazil_Polygon_2.tif"
brazilraster2 <- raster(str_name)
plot(brazilraster2)

brazilraster <- mosaic(brazilraster1, brazilraster2, fun=max, tolerance=0.05, filename="overall_brazil_map.tif", overwrite=TRUE)

plot(brazilraster)
### look at summary of raster
print(brazilraster)

### change projection to match brazil site locations
brazilraster <- projectRaster(brazilraster, crs = 4326)
brazilraster[brazilraster < 0] <- 0
brazilraster[brazilraster > 100] <- 100

## buffer sites
sites_buffer <- st_buffer(plots_bats_jari, 900)


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
rownames(df) <- plots_bats_jari$Plot
df2 <- t(df)
df2 <- as.data.frame(df2)

### figure out means for each site
mean_forest_brazil_sites <- colMeans(df2, na.rm=T)
mean_forest_brazil_sites <- as.data.frame(mean_forest_brazil_sites)
