# library(raster)
library(terra)

rm(list = ls())

#######################################
# 1 Data on breeding bird richness ####
#######################################

# We load the R object from last session (using the function load()) and assign it to a new object name (using the function get())
UK_birdrichness_clim <- get(load('data/UK_bird_richness_clim.RData'))

# Inspect the data
summary(UK_birdrichness_clim)

#-------------------------------------------------------------------------------

UK_birdrichness_clim <- na.omit(UK_birdrichness_clim)

#-------------------------------------------------------------------------------

# map species richness

# old:
# plot(rasterFromXYZ(UK_birdrichness_clim[,c('EASTING','NORTHING','richness')]))

plot(terra::rast(UK_birdrichness_clim[,c('EASTING', 'NORTHING', 'richness')], type = 'xyz'))

#-------------------------------------------------------------------------------
# NO CHANGES MADE TO THIS SECTION

library(ggplot2)
library(ggpubr)

# Plot species-energy relationships
p1 <- ggplot(data = UK_birdrichness_clim, mapping = aes(x = bio10/10, y = richness)) + 
  geom_point() + 
  geom_smooth(method="loess") + 
  xlab('Summer temperature [°C]') + 
  ylab('Species richness')

p2 <- ggplot(data = UK_birdrichness_clim, mapping = aes(x = bio4, y = richness)) + 
  geom_point() + 
  geom_smooth(method="loess") + 
  xlab('Temperature seasonality') + 
  ylab('Species richness')

p3 <- ggplot(data = UK_birdrichness_clim, mapping = aes(x = bio12, y = richness)) + 
  geom_point() + 
  geom_smooth(method="loess") + 
  xlab('Annual precipitation [mm]') + 
  ylab('Species richness')

ggarrange(p1, p2, p3, ncol = 3)

#-------------------------------------------------------------------------------

# section without raster functions. Did it just to create objects that were required later on ----

# We first fit a GLM for the bio10 variable assuming a linear relationship:
m1 <- glm(richness ~ bio10, family="poisson", data= UK_birdrichness_clim)

# We can get a summary of the model:
summary(m1) 
# Fit a quadratic relationship with bio10:
summary( glm(richness ~ bio10 + I(bio10^2), family="poisson", data= UK_birdrichness_clim))
# Or use the poly() function:
summary( glm(richness ~ poly(bio10,2) , family="poisson", data= UK_birdrichness_clim) )
# Fit two linear variables:
summary( glm(richness ~ bio10 + bio12, family="poisson", data= UK_birdrichness_clim) )
# Fit two variables with linear and quadratic terms:
summary( glm(richness ~ poly(bio10,2) + poly(bio12,2), family="poisson", data= UK_birdrichness_clim))

library(corrplot)

# We first estimate a correlation matrix from the predictors. 
# We use Spearman rank correlation coefficient, as we do not know 
# whether all variables are normally distributed.
cor_mat <- cor(UK_birdrichness_clim[,-c(1:3)], method='spearman')

# We can visualise this correlation matrix. For better visibility, 
# we plot the correlation coefficients as percentages.
corrplot.mixed(cor_mat, tl.pos='lt', tl.cex=0.6, number.cex=0.5, addCoefasPercent=T)

library(devtools)
devtools::install_git("https://gitup.uni-potsdam.de/macroecology/mecofun.git")

library(mecofun)

# Run select07()
var_sel <- select07(X=UK_birdrichness_clim[,-c(1:3)], 
                    y=UK_birdrichness_clim$richness, 
                    threshold=0.7, family='poisson')

# Check out the structure of the resulting object:
str(var_sel)

# We extract the names of the weakly correlated predictors ordered by the univariate variable importance in terms of AIC:
(pred_sel <- var_sel$pred_sel)
# How many data points do we have?
nrow(UK_birdrichness_clim)

# Fit the full model:
m_full <- glm( richness ~ poly(bio5,2) + poly(bio15,2) + poly(bio9,2) + poly(bio6,2) + poly(bio8,2) + poly(bio3,2), 
               family='poisson', data=UK_birdrichness_clim)

# Inspect the model:
summary(m_full)

# Explained deviance:
1 - deviance(m_full)/m_full$null.deviance
m_step <- step(m_full)

# Dispersion factor
deviance(m_full)/m_full$df.residual

# Fit the full model with nbinom family:
library(MASS)
m_full2 <- glm.nb( richness ~ poly(bio5,2) + poly(bio15,2) + poly(bio9,2) + poly(bio6,2) + poly(bio8,2) + poly(bio3,2), 
                   data=UK_birdrichness_clim)

# Inspect the model:
summary(m_full2)
m_step2 <- step(m_full2)

# Names of predictors
my_preds <- c('bio5','bio15','bio9','bio6','bio3')

par(mfrow=c(2,3))
partial_response(m_step2, predictors = UK_birdrichness_clim[,my_preds],ylim=c(0,100), ylab='Species richness')

plot(UK_birdrichness_clim$richness, m_step2$fitted.values, pch=19, col='grey55',xlab='Observed richness', ylab='Predicted richness', xlim=c(0,100), ylim=c(0,100))
abline(0,1, lty='dashed',lwd=2)
abline(lm(m_step2$fitted.values ~ UK_birdrichness_clim$richness), col='darkgreen', lwd=2)

# We create 5 folds for our data
folds <- dismo::kfold(UK_birdrichness_clim, k = 5)

# Make cross-validated predictions
preds_cv <- crossvalSDM(m_step2, traindat = UK_birdrichness_clim, colname_species = 'richness', colname_pred = my_preds, kfold=folds)

# Calculate C-index
library(Hmisc)
# On training data
rcorr.cens(m_step2$fitted.values, UK_birdrichness_clim$richness)[1]

# On cross-validated data
rcorr.cens(preds_cv, UK_birdrichness_clim$richness)[1]


#-------------------------------------------------------------------------------

############################
# 2.9 Model predictions ####
############################

# Load climate data
# old:
# clim <- stack('data/UK_bioclim.grd')

clim <- terra::rast('data/UK_bioclim.grd')

#-------------------------------------------------------------------------------

# Prepare climate data frames
# old:
# clim_df <- data.frame(rasterToPoints(clim))
clim_df <- terra::as.data.frame(clim, xy = TRUE)

# Make predictions
# Make continuous predictions:
# old: 
# clim_df$pred_richness <- predict(m_step2, newdata= clim_df, type="response")

clim_df$pred_richness <- terra::predict(m_step2, newdata = clim_df, type="response") # same as for the raster version
head(clim_df)

#-------------------------------------------------------------------------------

# Make raster of predictions to current environment:
# old:
# r_pred_richness <- rasterFromXYZ(clim_df[,c('x','y','pred_richness')])

r_pred_richness <- terra::rast(clim_df[,c('x','y','pred_richness')], type = 'xyz')
plot(r_pred_richness, axes = FALSE, main = 'Predicted species richness') 
