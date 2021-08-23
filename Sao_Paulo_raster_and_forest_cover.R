#### SAO PAULO ####
#### IS TWO TIF FILES AS POLYGON GOES OVER TWO SATELLITE IMAGES, SO HAVE TO MERGE IMAGES 

### Importing and merging raster files
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Forest_Cover/Brazil")

str_name<-'Sao_Paulo_Polygon_1.tif' 
saopraster1 <- raster(str_name)
plot(saopraster1)

str_name <- "Sao_Paulo_Polygon_2.tif"
saopraster2 <- raster(str_name)
plot(saopraster2)

saopraster <- mosaic(saopraster1, saopraster2, fun=max, tolerance=0.05, filename="sao_paulo_forest.tif", overwrite=TRUE)

### look at summary of raster
print(saopraster)

### change projection to match brazil site locations
saopraster <- projectRaster(saopraster, crs = 4326)
saopraster[saopraster < 0] <- 0
saopraster[saopraster > 100] <- 100

## bring in site data
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Mammal_BioFrag_Data")
plots_bats_saop <- read.csv("PID0222_Sao_Paulo_Bats/PID0222_Plot.csv")
plots_bats_saop <- st_as_sf(plots_bats_saop, coords=c('Longitude', 'Latitude'))
st_crs(plots_bats_saop) <- 4326

## buffer sites
sites_buffer <- st_buffer(plots_bats_saop, 900)


## plot buffers onto raster
plot(saopraster)
plot(st_geometry(sites_buffer), add=TRUE, col="red")


### calculate average forest cover for each buffer
sites_buffer <- st_as_sf(sites_buffer)

#### extract pixel values for buffer sites
r.vals <- extract(saopraster, sites_buffer)

#### make list of values into usable dataframe of values
n.obs <- sapply(r.vals, length)
seq.max <- seq_len(max(n.obs))
df <- t(sapply(r.vals, "[", i = seq.max))
df <- as.data.frame(df)


### change names and transpose the data frame
rownames(df) <- plots_bats_saop$Plot
df2 <- t(df)
df2 <- as.data.frame(df2)

### figure out means for each site
mean_forest_saop_sites <- colMeans(df2, na.rm=T)
mean_forest_saop_sites <- as.data.frame(mean_forest_saop_sites)

### add plot names
mean_forest_saop_sites <- cbind(rownames(mean_forest_saop_sites), data.frame(mean_forest_saop_sites, row.names=NULL))
colnames(mean_forest_saop_sites) <- c("plot_id","value")

## save forest cover measures

