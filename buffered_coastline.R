library(rgdal)
library(raster)
library(sf)
library(sp)
library(dismo)
library(landscapemetrics)
library(dplyr)
library(stringr)


setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Buffered_Coastline")

## can skip down to after land final bit as shape file has already been made
## skip to line 87 #

## load in data required
baia <- st_read("baia_de_caxiuana.shp")
plot(st_geometry(baia))

## inlet data
inlets <- st_read("Inlets_from_ranges_poly.shp")
plot(st_geometry(inlets))

## continent data
na <- st_read("gshhs_north_america.shp")
sa <- st_read("gshhs_south_america.shp")
plot(st_geometry(na))
plot(st_geometry(sa))

## projection to change measurement to m 
behr <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m"

## change continent projection data
sa <- st_transform(sa, behr)
na <- st_transform(na, behr)
st_is_valid(sa)
st_is_valid(na)

# Those are actually sufficiently high detail to be split at the 
# Panama canal so patch them back together with a polygon covering the 
# narrow part of the panama canal
patch_points <- matrix(c(-79.86, 9.06, -79.73, 9.22, -79.53, 9.07, 
                         -79.62, 8.91, -79.86, 9.06), byrow=TRUE, ncol=2)

patch <- st_sfc(st_polygon(list(patch_points)), crs=st_crs(4326))
patch <- st_transform(patch, behr)

na <- st_union(st_geometry(na), patch)
land <- st_union(st_geometry(na), st_geometry(sa))

st_is_valid(land)
plot(land)

# There are a number of locations along the coast where the ranges follow
# inland waterways that don't feature on the GSHHS coastline. These matter 
# when they extend far enough inland that they aren't covered by a buffer of
# the GSHHS, so end up as tiny fragments of apparently terrestrial range.

## an example of this is Baia de Cauixiana

# Bite the baia from the rest of the coast
baia <- st_transform(baia, behr)
land_final <- st_difference(st_geometry(land), st_geometry(baia))

# plot that to check it
plot(st_geometry(baia), border='red')
plot(st_geometry(land_final), add=TRUE, col='grey')

st_is_valid(land_final)

# There are others that don't appear in GSHHS and presumably come from whatever
# coastline source was used to clip the ranges. These sections of range have been
# extracted from a representative species range (Jacana jacana), converted to clipping
# polygons and mildly edited to conform to the existing coastline without creating
# extra mess.

inlets <- st_transform(inlets, behr)
inlets <- st_union(inlets)

land_final <- st_difference(st_geometry(land_final), st_geometry(inlets))

st_is_valid(land_final)

st_write(land_final, 'new_world_continental_coastline.shp', delete_layer=TRUE)
plot(st_geometry(land_final))

## read in file
land_final <- st_read('new_world_continental_coastline.shp')
land_final <- st_transform(land_final, behr)

# BUFFER COASTLINE

# - A negative number shrinks the polygon, buffering inwards. -
# - We're using 13 km to avoid retaining land in the in seaward margin 
#   of Lagoa do Peixe. 
# - Takes a little while to run, so store it. 
# - Note that if the buffer is greater than about 15km, the Isthmus of Panama
#   splits, so you could buffer N and S separately.

### trying to split and join back together to avoid mass processing power
na_buff <- st_buffer(na, -13000)
st_write(na_buff, 'north_america_continental_coastline_buffered.shp', delete_layer=TRUE)
sa_buff <- st_buffer(sa, -13000)
st_write(sa_buff, 'south_america_continental_coastline_buffered.shp', delete_layer=TRUE)
all_buff <- st_union(st_geometry(na), st_geometry(sa))
plot(st_geometry(patch), border="red")
plot(st_geometry(all_buff), add=T, border="black")

### new files form above have been made so just load them in and save time
na_buff <- st_read("north_america_continental_coastline_buffered.shp")
sa_buff <- st_read("south_america_continental_coastline_buffered.shp")
plot(st_geometry(na_buff))


land_buff <- st_buffer(land_final, -13000)

st_write(land_buff, 'new_world_continental_coastline_buffered.shp', delete_layer=TRUE)

# Plot the area around Lagoa do Peixe to check for sliver polygons
lagoa_coords <- matrix(c(-52.5, -52.5, -50, -50, -52.5, -32.5, 
                         -30, -30, -32.5, -32.5), ncol=2)
lagoa <- st_sfc(st_polygon(list(lagoa_coords), crs=4326))
plot(st_transform(lagoa, saaeac))
plot(land_final, add=TRUE, border='red', col=NA)
plot(land_buff, add=TRUE, border=NA, col='grey')


