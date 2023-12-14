library(terra)

rm(list = ls())

# Read in the distribution dataset:
bird_dist <- read.table('data/UK_BBatlas_1968_2008.csv',header=T, sep=',', stringsAsFactors = F)

# Look at data, the first three columns contain coordinate information, the 4th column contains the time periods:
head(bird_dist[,1:20])

#-------------------------------------------------------------------------------

# Species names are contained in the remaining columns:
spp <- names(bird_dist)[-c(1:4)]
spp_names <- sub('_',' ',spp)

#-------------------------------------------------------------------------------

###############################
# Species richness changes ####
###############################

# Extract species occurrences per time period and compute species richness:
birds_68 <- subset(bird_dist,period=='1968-72')
birds_68$richness68 <- rowSums(birds_68[,spp])

# old:
# plot(rasterFromXYZ(birds_68[,c('EASTING','NORTHING','richness68')]))

plot(terra::rast(birds_68[, c('EASTING', 'NORTHING', 'richness68')], type = 'xyz'))

#-------------------------------------------------------------------------------

# Calculate species richness
bird_dist$richness <- rowSums(bird_dist[,spp])


library(raster)

# !!!
# NOTE: this has to be done with spplot/raster to achieve the desired plot. 
# !!!
# Map species richness for different time periods
spplot(stack(
  rasterFromXYZ(subset(bird_dist,period=='1968-72')[,c('EASTING','NORTHING','richness')]),
  rasterFromXYZ(subset(bird_dist,period=='1988-91')[,c('EASTING','NORTHING','richness')]),
  rasterFromXYZ(subset(bird_dist,period=='2008-11')[,c('EASTING','NORTHING','richness')])),
  names.attr = c('1968-72','1988-91','2008-11'), main='Species richness'
)

#-------------------------------------------------------------------------------

#########################################
# 2 Mapping species richness changes ####
#########################################

# Define axis for colour breaks
brks_diff <- seq(-50,50,by = 5)

# Define colour palette
cols.diff = colorRampPalette(
  rev(c('#650A13','#b2182b','#d6604d','#f4a582','grey90','#92c5de','#4393c3','#2166ac','#0B2F52')))(21) # red-blue diverging

# Calculate the difference in species richness between 1968 and 2011
# old
# change_richness <- rasterFromXYZ(subset(bird_dist,period=='2008-11')[,c('EASTING','NORTHING','richness')]) -
#   rasterFromXYZ(subset(bird_dist,period=='1968-72')[,c('EASTING','NORTHING','richness')])

change_richness <- terra::rast(subset(bird_dist,period=='2008-11')[,c('EASTING','NORTHING','richness')]) -
  terra::rast(subset(bird_dist,period=='1968-72')[,c('EASTING','NORTHING','richness')])

# !!!
# NOTE: original code in the practical defines the object brks_diff but then uses brks.diff in the plot function
# !!!
# Plot the species richness change between 1968 and 2011
plot(change_richness, axes=F,  col=cols.diff, lab.breaks=brks_diff, zlim=c(-50,50), 
     main='Species richness change \n1968 - 2011')

#-------------------------------------------------------------------------------

##########################################################
# 2.1 Mapping taxon-specific species richness changes ####
##########################################################

# !!!
# TODO: remove one of the two code boxes where redlist_status is defined.
# !!!
redlist_status <- read.table('data/UK_birds_redlist_status.csv', header=T, sep=',')

#-------------------------------------------------------------------------------

# Restrict red list table to those species that are found in UK:
redlist_status <- subset(redlist_status, scientific_name %in% spp_names)

# List number of bird species in different orders:
table(redlist_status$order)

#-------------------------------------------------------------------------------

# List number of bird species in different families:
table(redlist_status$family)

#-------------------------------------------------------------------------------

# List bird species in order ACCIPITRIFORMES:
subset(redlist_status,order=='ACCIPITRIFORMES')$scientific_name

#-------------------------------------------------------------------------------

# Extract species belonging to the order ACCIPITRIFORMES:
accipiter_spp <- subset(redlist_status,order=='ACCIPITRIFORMES')$scientific_name
accipiter_spp <- sub(' ','_', accipiter_spp)

# Compute ACCIPITRIFORMES species richness
birds_68$sr_accipiter <- rowSums(birds_68[,accipiter_spp])

# Map ACCIPITRIFORMES species richness
# old
# plot(rasterFromXYZ(birds_68[,c('EASTING','NORTHING','sr_accipiter')]), main="Richness ACCIPITRIFORMES")

plot(terra::rast(birds_68[,c('EASTING','NORTHING','sr_accipiter')], type = 'xyz'), main = 'Richness ACCIPITRIFORMES')

#-------------------------------------------------------------------------------

#############################################
# 3.1. Quantifying species range changes ####
#############################################

# no changes required in this section

#-------------------------------------------------------------------------------

########################################
# 3.2 Mapping species range changes ####
########################################

# !!!
# NOTE: raster is required here to be able to use spplot
# !!!

# ranges sedge warbler
sedge_68 <- rasterFromXYZ(subset(bird_dist,period=='1968-72')[,c('EASTING','NORTHING','Acrocephalus_schoenobaenus')])
sedge_88 <- rasterFromXYZ(subset(bird_dist,period=='1988-91')[,c('EASTING','NORTHING','Acrocephalus_schoenobaenus')])
sedge_08 <- rasterFromXYZ(subset(bird_dist,period=='2008-11')[,c('EASTING','NORTHING','Acrocephalus_schoenobaenus')])

# Plot the range in different time periods
spplot(stack(sedge_68, sedge_88, sedge_08))

#-------------------------------------------------------------------------------

# !!!
# keep on working with the sedge objects that were created using raster
# !!!

# Compute differences. To distinguish clearly where cells are colonised or decolonised, we assign occurrences in 1968 a value=1 and occurrence in 2008 a value=2:
dummy <- sedge_08
values(dummy)[values(dummy)>0 & !is.na(values(dummy))] <- 2

# Sum up the layers. Values=1 indicate presence in 1968, values=2 presence in 2008, values=3 presence in both periods
diff <- sedge_68+dummy
diff_f <- as.factor(diff)
rat <- levels(diff_f)[[1]]
rat[["diff"]] <- c("Absent", "Extinction", "Colonisation", "Stable")
levels(diff_f) <- rat

library(rasterVis)
custom.pal <- c("grey75", "coral", "cyan3","grey25")
levelplot(diff_f, margin=F, scales=list(draw=FALSE),col.regions=custom.pal,main='Sedge warbler range change (1968-2011)')

