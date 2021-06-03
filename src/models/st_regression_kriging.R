###
###  st_regression_features.R
###
###  Applies Spatio-Temporal Regression Kriging on the regression features.
###
###  Input: Regression features & prediction map (output from st_regression_features.R).
###  Output: Interpolated maps for different moments in time.
###

rm(list=ls())
library(raster)
library(gstat)
library(sp)
library(sf)

utrecht <- st_read("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/external/WijkBuurtkaart_2020_v1/gem_utrecht.shp")



# Load the Regression Features
d <- read.csv('C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/processed/regression_features/features1500.csv')
coordinates(d) <- ~x+y
projection(d) <- projection(utrecht)

# Set factors
d$road <- as.factor(d$road)
d$rail <- as.factor(d$rail)
d$HH <- as.factor(d$HH)
d$DD <- as.factor(d$DD)

# Train the model
lm <- lm(pm2_5_mean ~ address_density + pop_density + road + rail + easting + northing + DD + HH + FH + `T` + P + U, data=d, na.action="na.exclude")
summary(lm)

# Generate predictions
lm_pred <- predict.lm(lm, se.fit=T)

# Create the prediction dataframe
pred_df <- data.frame("date"=d$date, "x"=d@coords[,1], "y"=d@coords[,2], "pm2_5"=d$pm2_5_mean, "pred"=lm_pred[["fit"]], "se.fit"=lm_pred[["se.fit"]])
pred_df$res <- pred_df$pm2_5 - pred_df$pred
pred_df$date <- as.POSIXct(pred_df$date)
coordinates(pred_df) <- ~x+y
projection(pred_df) <- projection(utrecht)

# Create Spatio-Temporal Dataframe of Residuals
time_list <- (pred_df$date) 
res_df <- data.frame("res"=pred_df$res)
vms <- SpatialPoints(pred_df@coords,crs(pred_df)) 

stdf <- STIDF(sp=vms, time=time_list, data=res_df)

# Create Empirical ST Variogram
vv=variogram(res~1, data=stdf, tunit="hours", tlags=0:6)

# waaaaaaa
plot(vv)
plot(vv, map=F)





# metric
metricVgm <- vgmST("metric", joint=vgm(psill=100, model="Sph", range=2000, nugget=0), stAni=25) 
set.seed(seed=123)
fitmetricVgm <- fit.StVariogram(vv, metricVgm)
attr(fitmetricVgm, "optim")$value # MSE
attr(fitmetricVgm, "optim")$convergence # convergence code (should be 0)
plot(vv, fitmetricVgm, wireframe=T)
fitmetricVgm

# separable 
sepVgm <- vgmST("separable", 
                space=vgm(psill=90, model="Sph", range=2000, nugget=0),
                time=vgm(psill=30, model="Sph", range=12, nugget=0),
                sill=100)
set.seed(seed=123)
fitsepVgm <- fit.StVariogram(vv, sepVgm, maxit=10000, st.Ani=25, control=list(parscale=c(100,1,10,1,100)), # use parscale to set similar step lengths for optimization of different units in space and time
                             lower=c(10,0,0.1,0,0.1),
                             upper=c(2000,100,24,100,100)) # check parameters parscale/lower/upper
attr(fitsepVgm, "optim")$value # MSE
attr(fitsepVgm, "optim")$convergence # convergence code (should be 0)
plot(vv, fitsepVgm, wireframe=T, zlab="gamma")
plot(vv, list(fitmetricVgm, fitsepVgm), all=T, wireframe=T)
fitsepVgm

# sum-metric
sumVgm <- vgmST("sumMetric", space=vgm(psill=90, model="Sph", range=2000, nugget=0),
                time=vgm(psill=30, model="Sph", range=12, nugget=0),
                joint=vgm(psill=100, model="Sph", range=2000, nugget=0), stAni=25)
set.seed(seed=123)
fitsumVgm <- fit.StVariogram(vv, sumVgm, maxit=10000, control=list(parscale=c(1,100,1, # use parscale to set similar step lengths for optimization of different units in space and time
                                                                              1,0.5,1,
                                                                              1,100,1,
                                                                              1)),
                             lower=c(sill.s=0,range.s=10,nugget.s=0,
                                     sill.t=0,range.t=0.1,nugget.t=0,
                                     sill.st=0,range.st=10,nugget.st=0,
                                     anis=0),
                             upper=c(sill.s=200,range.s=5000,nugget.s=200,
                                     sill.t=100,range.t=24,nugget.t=100,
                                     sill.st=200,range.st=5000,nugget.st=200,
                                     anis=100))
attr(fitsumVgm, "optim")$value # MSE
attr(fitsumVgm, "optim")$convergence # convergence code (should be 0)
plot(vv, fitsumVgm, wireframe=T, zlab="gamma")
plot(vv, list(fitmetricVgm, fitsepVgm, fitsumVgm), all=T, wireframe=T)
plot(vv, list(fitmetricVgm, fitsepVgm, fitsumVgm), all=T, wireframe=F, map=F)
plot(vv, list(fitmetricVgm, fitsepVgm, fitsumVgm), all=T, wireframe=T, diff=T) # differences between sample and fitted variogram
fitsumVgm 

plot(vv, wireframe=T, main="Sample variogram")
plot(vv, fitsumVgm, wireframe=T, zlab="gamma", main="Sum-metric variogram model")
plot(vv, list(fitsumVgm, fitmetricVgm, fitsepVgm), all=T, wireframe=T, 
     scales=list(arrows=F, z=list(distance=5)), 
     xlab=list("h (m)", rot=30), ylab=list("u (hours)", rot=-35), zlab=expression(~gamma)) 

extractPar(fitsumVgm)