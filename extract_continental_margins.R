library(sf)
library(units)

setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK")

## behrmann projection
behr <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m"


# load the cleaned species ranges in WGS84 and transform
mammals <- st_read('Mammal_Ranges/cleaned_ranges.shp')


## ---------------------------------------------------------
## D) EXTRACT THE CONTINENTAL MARGIN OF SPECIES RANGES
##    SUPERSET OF THE SPECIES FROM THE OCCUPANCY DATA
## ---------------------------------------------------------

# There is an awful lot of irrelevant range data in the cleaned
# ranges: global distributions of species that occur in the ACF
# and ranges of maritime species that occur along the coast. 
# This operation removes that - we only need terrestrial margins
# for the calculations

# convert polygon ranges to lines - we don't want to create closed
# polygons of range, just get the margins as lines
mammals_mln <- st_cast(mammals, 'MULTILINESTRING', do_split=FALSE)

# get continental margins (about 10 minutes on a macbook pro)
land_buff <- st_read('Buffered_Coastline/new_world_continental_coastline_buffered.shp')
land_final <- st_read('Buffered_Coastline/new_world_continental_coastline.shp')

all(st_is_valid(mammals_mln))
continental_margin <- st_intersection(mammals_mln, st_geometry(land_buff))

# how many species do we lose completely? 2 have been lost
## why?
## Natalus Stramineus only exists in a small range of islands north of Brazil- can ignore as range is tiny
## Pteronotus parnellii also occurs only on islands in the Caribbean
species_dropped <- setdiff(mammals$SCINAME, continental_margin$SCINAME)
species_dropped

# Of those missing species, show ranges
## Natalus stramineus
plot(st_geometry(mammals[mammals$SCINAME == 'Natalus stramineus',]), col=NA, border='red')
plot(land_buff, add=TRUE, col='grey', border=NA)
plot(land_final, border='black', col=NA, add=TRUE)

## Pteronotus parnellii
plot(st_geometry(mammals[mammals$SCINAME == 'Pteronotus parnellii',]), col=NA, border='red')
plot(land_buff, add=TRUE, col='grey', border=NA)
plot(land_final, border='black', col=NA, add=TRUE)

# write that data out to file
st_write(continental_margin, 'Mammal_Ranges/Mammal_continental_range_margins.shp', delete_layer=TRUE)

## just to check missing species from original collected n_occur (n_occur comes from subsetting_IUCN_shape_data.R)
missing_species <- setdiff(n_occur$Species_IUCN, continental_margin$SCINAME)

plot(st_geometry(mammals[mammals$SCINAME == 'Vampyrum spectrum',]), col=NA, border='red')
plot(land_buff, add=TRUE, col='grey', border=NA)
plot(land_final, border='black', col=NA, add=TRUE)

plot(st_geometry(continental_margin[continental_margin$SCINAME == 'Vampyrum spectrum',]), col="red")
tail(continental_margin)
