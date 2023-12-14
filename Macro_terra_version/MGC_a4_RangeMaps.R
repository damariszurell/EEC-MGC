# library(raster)
# library(sf)

library(terra)


rm(list = ls())

##########################
# 1.1 IUCN range maps ####
##########################

# Load the shapefile
# old: 
# (shrew <- raster::shapefile('data/IUCN_Sorex_alpinus.shp'))

(shrew <- terra::vect('data/IUCN_Sorex_alpinus.shp'))

#-------------------------------------------------------------------------------

# Plot the Central Europe
library(maps)
map('world',xlim = c(5,30), ylim = c(40,55))

# Overlay the range of the Alpine Shrew
plot(shrew, col = 'red', add = TRUE)

#-------------------------------------------------------------------------------
# Read shapefile for all mammals using the raster package:
# old:
# mammals <- raster::shapefile('data/MAMMTERR.shp')
# raster::head(mammals)

mammals <- terra::vect('data/MAMMTERR.shp')

#-------------------------------------------------------------------------------

mammals

#-------------------------------------------------------------------------------

# Inspect attribute table
terra::head(mammals)

#-------------------------------------------------------------------------------

# Show all entries for the species 'Lynx lynx'
# old:
# raster::subset(mammals_old@data, BINOMIAL=='Lynx lynx')

terra::values(terra::subset(mammals, mammals$BINOMIAL == "Lynx lynx"))
# !!!
# just using terra::subset doesn't return the entries
# !!!


# Show all entries for the species with the word 'Lynx' in their name
# old:
# grep('Lynx', mammals_old@data$BINOMIAL)
# mammals_old@data[grep('Lynx',mammals_old@data$BINOMIAL),]


grep('Lynx', mammals$BINOMIAL)
mammals[grep('Lynx', mammals$BINOMIAL),] # !!! output differs from raster output

#-------------------------------------------------------------------------------

# Range map of the Eurasian lynx
# old:
# lynx_lynx_old <- mammals_old[mammals_old@data$BINOMIAL=='Lynx lynx',]

lynx_lynx <- mammals[mammals$BINOMIAL=='Lynx lynx',]


# Map the range
library(maps)
map('world')
terra::plot(lynx_lynx, col='red', add=T)

#-------------------------------------------------------------------------------
##########################
# 1.2 BIEN range maps ####
##########################

library(BIEN) # depends on raster

# old:
# (monkey_puzzle <- BIEN_ranges_load_species('Araucaria_araucana'))

# IMPORTANT: BIEN loads the data in as SpatialPolygonsDataFrame. We just change it right away.
# This might cause problems in once sp is retired

(monkey_puzzle <- terra::vect(BIEN_ranges_load_species('Araucaria_araucana')))
# there will be a warning message about the crs (always appears when directly
# transforming a SpatialPolygonsDataFrame with vect())

# this solves the "issue":
crs(monkey_puzzle) <- '+proj=longlat +datum=WGS84 +no_defs'

#-------------------------------------------------------------------------------

# Map
map('world', xlim = c(-180,-20), ylim = c(-60,80))
terra::plot(monkey_puzzle, col = 'red', add = TRUE)

#-------------------------------------------------------------------------------

######################################
# 2.1 Range size and range centre ####
######################################

# Range area of alpine shrew in square meters:
# old: 
# area(shrew)

terra::expanse(shrew) # m²

#-------------------------------------------------------------------------------

# Range area of alpine shrew in square kilometers:
# old:
# area(shrew)/1000000

terra::expanse(shrew)/1000000 # km²

#-------------------------------------------------------------------------------

# Range area of monkey puzzle in square meters:
# area(monkey_puzzle)

terra::expanse(monkey_puzzle)

#-------------------------------------------------------------------------------

library(rgeos) # depends on sp and will be retired by the end of 2023

# range centroid:
# old
# gCentroid(shrew)

terra::centroids(shrew)

#-------------------------------------------------------------------------------

# Map the species range and add the centroid to the map
map('world', xlim = c(5,30), ylim = c(40,55))
plot(shrew, col = 'red', add = TRUE)
plot(terra::centroids(shrew), col = 'blue', add = TRUE, lwd = 3)

#-------------------------------------------------------------------------------

# Let's crop the range polygons to South America
# old
# monkey_puzzle_SAm <- gIntersection(monkey_puzzle, as(extent(-85, -30, -55, 5),"SpatialPolygons"))
# 
# # Map the range and range centroid
# map('world',xlim = c(-100,-10),ylim = c(-60,15))
# plot(monkey_puzzle_SAm, col='red',add=T)
# plot(gCentroid(monkey_puzzle_SAm), col='blue',add=T,lwd=3)


monkey_puzzle_SAm <- terra::intersect(monkey_puzzle, ext(-85, -30, -55, 5))

map('world',xlim = c(-100,-10),ylim = c(-60,15))
plot(monkey_puzzle_SAm, col = 'red', add = TRUE)
plot(terra::centroids(monkey_puzzle_SAm), col = 'blue', add = T, lwd = 3)

#-------------------------------------------------------------------------------

#################################
# 2.2 Rasterising range maps ####
#################################
# TODO: doesn't work that way with terra

# By default, terra() will create a 1° resolution map in the *WGS 84* coordinate system (lon/lat).
(r_1deg <- terra::rast())

#-------------------------------------------------------------------------------

(shrew_1deg <- terra::rasterize(shrew, r_1deg))

#-------------------------------------------------------------------------------

map('world',xlim = c(5,30), ylim = c(40,55))
plot(shrew, col = 'red', add = TRUE)
plot(shrew_1deg, add = TRUE, alpha = 0.6, legend = FALSE)

#-------------------------------------------------------------------------------

(r_2deg <- terra::rast(res=2))

# Rasterize the eurasian lynx data
lynx_lynx_2deg <- terra::rasterize(lynx_lynx, r_2deg)

# Map the occupied grid cells
map('world')
plot(lynx_lynx_2deg, add = TRUE, legend = FALSE)


#!!!
# NOTE: I think the next code block about the values can be skipped
#!!! 

#-------------------------------------------------------------------------------

###########################################
# 2.2.2 Rasterising range maps with letsR #
###########################################

library(letsR) # requires raster
library(raster)

# raster version for monkey puzzle (lets.presab doesn't work with terra objects)
(monkey_puzzle_raster <- BIEN_ranges_load_species('Araucaria_araucana'))

# The lets.presab() function expects specific column names in the Polygons data frame
colnames(monkey_puzzle_raster@data) <- "binomial"

# We set the resolution to 1 degree (the default) and restrict the spatial extent to South America
r_monkey_puzzle <- lets.presab(monkey_puzzle_raster, resol = 1, xmn = -100, xmx = -10, ymn = -57, ymx = 15)

# Map the range and range centroid
map('world',xlim = c(-100,-10),ylim = c(-60,15))
plot(monkey_puzzle_SAm,col = 'blue', add = TRUE)
plot(r_monkey_puzzle, add = TRUE, alpha = 0.6, legend = FALSE)

#-------------------------------------------------------------------------------

####################################################################
# 2.2.3 Bulk-rasterising multiple species range maps with letsR ####
####################################################################

# !!!
# NOTE: this will have to be done with raster again
# !!!
# Extract the available Pinus species names
(pinus_names <- BIEN_ranges_genus("Pinus",match_names_only = T)[,1])

#-------------------------------------------------------------------------------

# Download the range maps for all Pinus species
pinus <- BIEN_ranges_load_species(pinus_names)

# Format the column names and rasterise
colnames(pinus@data) <- "binomial"
r_pinus <- lets.presab(pinus, resol = 1, xmn = -170, xmx = -10, ymn = -57, ymx = 60)

# Plot species richness
plot(r_pinus)

#-------------------------------------------------------------------------------

# prepare raster version of the mammals shapefile
mammals_raster <- raster::shapefile('data/MAMMTERR.shp')

# Subset the SpatialPolygonsDataFrame
artibeus_spp <- mammals_raster[grep('Artibeus',mammals_raster@data$BINOMIAL),]

# Rasterize the ranges using the letsR package
r_artibeus_spp <- lets.presab(artibeus_spp, resol = 2, 
                              presence = 1, origin = 1, seasonal = 1)

#-------------------------------------------------------------------------------

# Map the species richness
plot(r_artibeus_spp)

#-------------------------------------------------------------------------------

# Map single species - here, just the first two
par(mfrow=c(1,2))
plot(r_artibeus_spp, name = "Artibeus amplus")
plot(r_artibeus_spp, name = "Artibeus anderseni")

#-------------------------------------------------------------------------------

# Look at structure of the object and at the presence-absence matrix
str(r_artibeus_spp, 1)

#-------------------------------------------------------------------------------

head(r_artibeus_spp$Presence_and_Absence_Matrix)