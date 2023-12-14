library(sf)
library(terra)

# old:
# library(raster)
# library(sp)
# library(dismo)

rm(list = ls())

###############

# 2.1 Spatial
# We set a seed for the random number generator, so that we all obtain the same results
set.seed(12345)

coords <- cbind(
  x <- rnorm(10, sd=2),
  y <- rnorm(10, sd=2)
)

str(coords)
#-------------------------------------------------------------------------------

plot(coords)

#-------------------------------------------------------------------------------

# # Convert into SpatialPoints
# old:
# sp <- sp::SpatialPoints(coords)

# ISSUE: you can't use coords directly with sf. 
# terra::vect works fine if you just want a print summary similar to raster (no information on the bounding box)
# for a similar output to showDefault(sp): convert sp to sn sf object using st_as_sf

# new version
sp <- terra::vect(coords)

# Check out the object
class(sp)


# OPTIONAL ########
# sp <- st_as_sf(sp)
# class(sp)
# showDefault(sp) # This holds way less information than an SpatialPoints object from the sp package
##################

#-------------------------------------------------------------------------------

# old
# sp <- SpatialPoints(coords, proj4string=CRS('+proj=longlat +datum=WGS84'))
# sp

# terra version
sp <- terra::vect(coords, crs = '+proj=longlat +datum=WGS84')
sp

#-------------------------------------------------------------------------------

# Create attribute table
(data <- data.frame(ID=1:10,species=sample(c('beech','oak','birch'),10,replace=T)))

#-------------------------------------------------------------------------------

# old
# # Create SpatialPointsDataFrame
# (spdf <- SpatialPointsDataFrame(sp, data))

# for terra, you can simply add the additional information to the existing Spatial Vector
sp_trees <- sp
(terra:: values(sp_trees) <- data)


# alternative: combine everything right away
# (sp_trees2 <- terra::vect(coords, crs = '+proj=longlat +datum=WGS84', atts = data))
  

########################################
# 2.2 SpatialLines and SpatialPolygons #
########################################

# Create SpatialLines through all oak trees in the data

#old:
# lns  <- raster::spLines(subset(spdf,species=='oak'), crs=CRS('+proj=longlat +datum=WGS84'))
# pols <- raster::spPolygons(subset(spdf,species=='birch'), crs=CRS('+proj=longlat +datum=WGS84'))

# prepare subsets for the two tree species 
oak   <- terra::subset(sp_trees, sp_trees$species == 'oak')
birch <- terra::subset( sp_trees, sp_trees$species == 'birch')

# sp_oak with the geometry 'points' can be plotted as lines as it is, but sp_birch needs to be modified before we can plot it as polygon:
# get the coordinates for the birch trees and transform them to a SpatialVector (type = polygons)
birch_coords <- terra::crds(birch)
birch <- terra::vect(birch_coords, type = "polygons")

plot(sp, axes = TRUE, lwd = 2)
terra::lines(oak, col = 'red', lwd = 2)
terra::polys(birch, border = 'blue', col = 'yellow', lwd = 2, alpha = 1)

#####################################
# 2.3 Reading vector data from file #
#####################################
# old:
#(shrew <- raster::shapefile('data/IUCN_Sorex_alpinus.shp'))

# terra:
(shrew <- terra::vect('data/IUCN_Sorex_alpinus.shp'))

#-------------------------------------------------------------------------------

# Plot Central Europe
library(maps)
map('world', xlim = c(5,30), ylim = c(40,55))

# overlay the range of the Alpine Shrew
plot(shrew, col = "red", add = TRUE)

# or:
# terra::polys(shrew, col = "red")
#-------------------------------------------------------------------------------

######################
# 3 Raster data in R #
######################

# old:
# (r1 <- raster(ncol=10, nrow=10, xmx=-80, xmn=-150, ymn=20, ymx=60))

(r1 <- terra::rast(ncol = 10, nrow = 10, xmax = -80, xmin = -150, ymax = 60, ymin = 20 ))

#-------------------------------------------------------------------------------
# old:
# summary(raster::values(r1))
# raster::values(r1) <- rnorm(ncell(r1))


summary(terra::values(r1))

terra::values(r1) <- rnorm(ncell(r1))

# plot the raster
plot(r1)

#-------------------------------------------------------------------------------

# create another raster layer and assign values
r2 <- r1

# old:
# raster::values(r1) <- rnorm(ncell(r1))
terra::values(r2) <- 1:ncell(r2)

# Stack the raster layers
# old:
# (r <- stack(r1,r2))
(r <- c(r1, r2)) # c() as alternative to stack()

#-------------------------------------------------------------------------------

plot(r)

#-------------------------------------------------------------------------------

###########################
# 3.1 Read in raster data #
###########################

# old:
# (temp <- raster('data/UK_temp.tif'))
(temp <- terra::rast('data/UK_temp.tif'))

# ------------------------------------------------------------------------------

plot(temp)

#-------------------------------------------------------------------------------

# old:
# (bioclim <- stack('data/UK_bioclim.grd'))
(bioclim <- terra::rast('data/UK_bioclim.grd')) # as alternative to stack()

#-------------------------------------------------------------------------------

plot(bioclim)

#-------------------------------------------------------------------------------

############################
# 3.2 Download raster data #
############################

# old:
# (clim <- getData("worldclim", var="bio", res=10, download=T, path="data"))

library(geodata) 

(clim <- geodata::worldclim_global(var = 'bio', res = 10, download = T, path = 'data')) # alternative to getData()

plot(clim)

##############################
# 3.3 Manipulate raster data #
##############################

# old: (examples)
# temp_eur <- crop(clim[[1]], c(-15,45,35,72))
# temp_eur_onedeg <- aggregate(temp_eur, 6)


# Crop the temperature layer (bio1) to roughly European extent
temp_eur <- terra::crop(clim[[1]], c(-15,45,35,72))

# Aggregate to one-degree and two-degree resolution
temp_eur_onedeg <- terra::aggregate(temp_eur, 6)
temp_eur_twodeg <- terra::aggregate(temp_eur, 12)

par(mfrow=c(1,3))
plot(temp_eur)
plot(temp_eur_onedeg)
plot(temp_eur_twodeg)

###########################
# 3.4 Extract raster data #
###########################

# Generate random locations
# old:
# library(dismo)
# lonlat <- randomPoints(temp_eur, 10)

lonlat <- terra::spatSample(temp_eur, size = 10, method = 'random', replace = FALSE, xy = TRUE, values = FALSE) # xy to return coordinates

# dev.off() to reset par()

# Map temperature and locations
plot(temp_eur)
points(lonlat, cex=2, pch=19)

#------------------------------------------------------------------------------- 

# Extract temperature values at these locations
# old:
#raster::extract(temp_eur, lonlat)

terra::extract(temp_eur, lonlat)
