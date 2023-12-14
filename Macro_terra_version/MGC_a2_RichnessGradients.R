# library(raster)
library(terra)

rm(list = ls())

# Read in the distribution dataset:
bird_dist <- read.table('data/UK_BBatlas_2008.csv',header=T, sep=',', stringsAsFactors = F)

str(bird_dist, list.len = 10)

#-------------------------------------------------------------------------------

############################
# 1.1 Distribution data ####
############################

library(terra)

# The coordinate reference system for the UK National Grid is:
proj_UK <- '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs'

# old:
# r_ptarmigan <- rasterFromXYZ(bird_dist[,c('EASTING','NORTHING','Lagopus_lagopus')], crs=proj_UK)

# terra version 
r_ptarmigan <- terra::rast(bird_dist[,c('EASTING','NORTHING','Lagopus_lagopus')], crs=proj_UK, type = "xyz")


# terra plot will look different than the raster version 
plot(r_ptarmigan)

#-------------------------------------------------------------------------------

####################################
# 1.2 Species richness patterns ####
####################################

richness <- data.frame(bird_dist[,2:3], richness = rowSums(bird_dist[,-c(1:3)]))
head(richness)

# old:
# r_richness <- rasterFromXYZ(richness, crs=proj_UK)

# terra version
r_richness <- terra::rast(richness, crs = proj_UK, type = "xyz")

plot(r_richness)

#-------------------------------------------------------------------------------

########################################################
# 1.3 Task 2: Latitudinal species richness gradient ####
########################################################

library(ggplot2)
ggplot(data = richness, mapping = aes(x = NORTHING, y = richness)) + 
  geom_point() + 
  geom_smooth(method="loess") + 
  xlab('Latitude (UK NORTHING)') + 
  ylab('Species richness')

#-------------------------------------------------------------------------------

#######################################
# 1.4 Species-energy relationships ####
#######################################

# old:
# clim <- stack('data/UK_bioclim.grd')

clim <- terra::rast('data/UK_bioclim.grd')

plot(clim)

# TODO: !!! it appears that one of the code boxes in the practical is missing (the one where richness2 is created)

# !!! my version to create richness2
richness2 <- cbind(richness, terra::extract(clim, richness[,1:2]))


# save(richness2,file='data/UK_bird_richness_clim.RData')

#-------------------------------------------------------------------------------

##################################
# 2 -  More richness patterns ####
##################################

# Read in the distribution dataset:
bird_dist2 <- read.table('data/UK_BBatlas_1968_2008.csv',header=T, sep=',', stringsAsFactors = F)

# Look at data, the first three columns contain coordinate information, the 4th column contains the time periods:
head(bird_dist2[,1:20])

#-------------------------------------------------------------------------------

# Species names are contained in the remaining columns:
spp <- base::names(bird_dist2)[-c(1:4)]
spp_names <- sub('_',' ',spp)

#-------------------------------------------------------------------------------

# Which time periods are contained in the data?
unique(bird_dist2$period)

#-------------------------------------------------------------------------------

# Extract species occurrences per time period and compute species richness:
birds_68 <- base::subset(bird_dist2, period == '1968-72')
birds_68$richness68 <- rowSums(birds_68[,spp])

# old:
# plot(rasterFromXYZ(birds_68[,c('EASTING','NORTHING','richness68')]))
plot(terra::rast(birds_68[,c('EASTING','NORTHING','richness68')], type = "xyz"))
