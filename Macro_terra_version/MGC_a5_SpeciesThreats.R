library(terra)

rm(list = ls())

# Read in the distribution dataset:
bird_dist <- read.table('data/UK_BBatlas_2008.csv', header = TRUE, sep = ',', stringsAsFactors = FALSE)

# Species names are contained in the columns:
spp <- names(bird_dist)[-c(1:3)]

# For later usage, we need to remove the underscore in the names:
spp <- sub('_',' ',spp)

redlist_status <- read.table('data/UK_birds_redlist_status.csv', header=T, sep=',')

#-------------------------------------------------------------------------------

# !!!
# I skipped the first section where raster was not used


##############################################
# 1.2 Mapping hotspots of threatened species #
##############################################

library(raster)

# !!!
# NOTE: raster required here to be able to use spplot
# !!!

# Identify all vulnerable species 
vu_spp <- subset(redlist_status,category=='VU')$scientific_name
# We have to make sure that species names are written in the same way in the redlist and distribution data
vu_spp <- sub(' ','_', intersect(vu_spp,spp))

# Identify all least concern species
lc_spp <- subset(redlist_status,category=='LC')$scientific_name
lc_spp <- sub(' ','_', intersect(lc_spp,spp))

# Now, we extract the distribution data for the VU and LC species groups, make rasters, stack these and plot
spplot(stack(
  rasterFromXYZ(data.frame(bird_dist[,2:3],log_VU=log(rowSums(bird_dist[,vu_spp])))),
  rasterFromXYZ(data.frame(bird_dist[,2:3],log_LC=log(rowSums(bird_dist[,lc_spp]))))), 
  main='log( species richness)')

#-------------------------------------------------------------------------------

#########################
# 2 Red list threats ####
#########################

redlist_threats <- read.table('data/UK_birds_redlist_threats.csv', header = TRUE, sep = ',')

#-------------------------------------------------------------------------------

redlist_threats[sample(nrow(redlist_threats),10),-c(1:2)]

#-------------------------------------------------------------------------------

table(redlist_threats$timing)

#-------------------------------------------------------------------------------

######################################
# 2.1 Mapping hotspots of threats ####
######################################

# Which ongoing threats are the most common ?
sort(table(subset(redlist_threats, species %in% spp & timing=='Ongoing')$title), decreasing=T)[1:10]

#-------------------------------------------------------------------------------

# Identify the species experiencing threats from hunting
spp_threat1 <- sub(' ','_',subset(redlist_threats,title=="Hunting & trapping terrestrial animals" & species %in% spp)$species)
# Identify the species experiencing threats from industry and military
spp_threat2 <- sub(' ','_',subset(redlist_threats,title=="Industrial & military effluents" & species %in% spp)$species)

# Map species experiencing threats from hunting
plot(terra::rast(data.frame(bird_dist[,2:3],rowSums(bird_dist[,spp_threat1])), type = 'xyz'), main="Hunting & trapping terrestrial animals")

#-------------------------------------------------------------------------------

# species experiencing threats from industry and military
plot(terra::rast(data.frame(bird_dist[,2:3],rowSums(bird_dist[,spp_threat2])), type = 'xyz'), main="Industrial & military effluents")

