###
###  external_cv.R
###
###  Applies External Cross-Validation on the ST Regression Kriging Models
###
###  Input: Snuffelfiets Regression Features (output from st_regression_features.R) +  Reference Station Regression Features (https://data.rivm.nl/data/luchtmeetnet/).
###  Output: Calculated statistics.
###

rm(list=ls())
gc()
library(raster)
library(gstat)
library(sp)
library(sf)
library(spacetime)
library(stars)
library(data.table)
library(tidyverse)



externalCV <- function(res){
  # Opening the Reference Stations Regression Features:
  refr <- read.csv('~/GitHub/ADS-Snuffelfiets-Thesis/data/processed/regression_features/reference_features100.csv')

  # Transform Reference Statoins to 'stars' object 
  refr_loc <- refr[c("X","Y","val")]
  sf_data <- st_as_sf(refr_loc, coords = c("X", "Y"), crs = 28992)
  
  pm_stars = refr %>% st_as_sf(coords = c('X','Y'))
  
  xy_ = st_coordinates(unique(pm_stars))
  pm_stars = pm_stars[order(xy_[,"X"], xy_[,"Y"]),]
  
  geom = st_sfc(unique(pm_stars$geometry))
  
  time = as.POSIXct(unique(pm_stars$date))
  time = time[order(time)]
  
  cast = dcast(pm_stars, as.character(geometry) ~ date, value.var = "val", fun.aggregate = max)  
  mat = as.matrix(cast[,-1])
  
  dims = st_dimensions(space = geom,time = as.POSIXct(time))
  
  pm_stars = st_as_stars(val = mat,dim = dims)
  
  
  # Convert Reference Stations Regression Features for subsequent LM-predictions:
  refr$date = as.POSIXct(refr$date)
  refr = refr[order(refr[,"date"], refr[,"X"], refr[,"Y"]),]
  coordinates(refr) <- ~X+Y
  utrecht <- st_read("~/GitHub/ADS-Snuffelfiets-Thesis/data/external/WijkBuurtkaart_2020_v1/gem_utrecht.shp")
  projection(refr) <- projection(utrecht)
  
  refr$road <- as.factor(refr$road)
  refr$rail <- as.factor(refr$rail)
  refr$HH <- as.factor(refr$HH)
  refr$wind_dir <- as.factor(refr$wind_dir)
  

  ####################################################
  # Spatio-Temporal Regression Kriging with Snuffelfiets data on the Reference Station Locations:

  # Load the Snuffelfiets Regression Features:
  filename <- paste("~/GitHub/ADS-Snuffelfiets-Thesis/data/processed/regression_features/features", res, ".csv", sep="")
  d <- read.csv(filename)
  d$date = as.POSIXct(d$date)
  d = d[order(d[,"date"], d[,"X"], d[,"Y"]),]
  coordinates(d) <- ~X+Y
  projection(d) <- projection(utrecht)
  
  d$road <- as.factor(d$road)
  d$rail <- as.factor(d$rail)
  d$HH <- as.factor(d$HH)
  d$wind_dir <- as.factor(d$wind_dir)
  
  ####################################################
  # Linear Model:
  
  # Train the model on SF data
  lm <- lm(pm2_5_mean ~ address_density + pop_density  + road + rail + wind_dir + HH + wind_speed + humidity , data=d, na.action="na.exclude")
  summary(lm)
  
  # Generate SF predictions
  lm_pred <- predict.lm(lm, se.fit=T)
  
  # Create the SF prediction-residuals dataframe
  pred_df <- data.frame("x"=d@coords[,1], "y"=d@coords[,2], "date"=d$date, "pm2_5"=d$pm2_5_mean, "pred"=lm_pred[["fit"]], "se.fit"=lm_pred[["se.fit"]])
  pred_df$res <- pred_df$pm2_5 - pred_df$pred
  
  
  # Convert SF residuals to 'stars' object
  sf_data <- st_as_sf(pred_df, coords = c("x", "y"), crs = 28992)
  
  pred_stars = sf_data %>% st_as_sf(coords = c('X','Y'))
  
  xy_ = st_coordinates(unique(pred_stars))
  pred_stars = pred_stars[order(xy_[,"X"], xy_[,"Y"]),]
  
  geom = st_sfc(unique(pred_stars$geometry))
  
  time = as.POSIXct(unique(pred_stars$date))
  time = time[order(time)]
  
  cast = dcast(pred_stars, as.character(geometry) ~ date, value.var = 'res', fun.aggregate = max)  
  mat = as.matrix(cast[,-1])
  
  dims = st_dimensions(space = geom,time = as.POSIXct(time))
  
  pred_stars = st_as_stars(res = mat,dim = dims)
  
  
  ####################################################
  # Variogram:
  
  # Creating the Empirical ST Variogram on the SF Residuals
  vv=variogramST(res~1, pred_stars, cressie=F, boundaries=c(500,1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500), tlags = 0:6)
  
  # Fitting the ST Variogram model
  metric <- vgmST("metric", joint=vgm(psill=25, model="Exp", range=2000, nugget=0), stAni=1200) 
  set.seed(seed=123)
  fitmetric <- fit.StVariogram(vv, metric)
  
  ####################################################
  # Residual Kriging on the Reference Stations Locations:
  
  gc()
  
  st_r_kriging = krigeST(res~1,data=pred_stars['res'],
                         newdata=pm_stars['val'], nmax=50, stAni=fitmetric$stAni/3600,
                         modelList = fitmetric, computeVar=T)
  
  
  ####################################################
  # LM predictions on the Reference Stations Locations:
  
  predictors <- refr[c("address_density", "pop_density", "road", "rail", "wind_dir", "HH", "wind_speed", "humidity")]
  
  full_pred = predict.lm(lm, predictors)
  full_pred <- data.frame("date"=refr@data[,"date"], "x"=coordinates(refr)[,"X"], "y"=coordinates(refr)[,"Y"], "lm_pred"=full_pred)
  
  # Convert LM Predictions to "stars" object:
  sf_data <- st_as_sf(full_pred, coords = c("x", "y"), crs = 28992)
  
  full_pred_stars = sf_data %>% st_as_sf(coords = c('X','Y'))
  
  xy_ = st_coordinates(unique(full_pred_stars))
  full_pred_stars = full_pred_stars[order(xy_[,"X"], xy_[,"Y"]),]
  
  geom = st_sfc(unique(full_pred_stars$geometry))
  
  time = as.POSIXct(unique(full_pred_stars$date))
  time = time[order(time)]
  
  cast = dcast(full_pred_stars, as.character(geometry) ~ date, value.var = 'lm_pred', fun.aggregate = max)  
  mat = as.matrix(cast[,-1])
  
  dims = st_dimensions(space = geom,time = as.POSIXct(time))
  
  full_pred_stars = st_as_stars(lm_pred = mat,dim = dims)
  
  gc()
  
  
  ####################################################
  # Add LM Predictions to the Kriged Residuals:
  
  
  total_list = list()
  for (i in 1:length(st_dimensions(st_r_kriging)$time$values)){
    coords <- st_coordinates(st_dimensions(st_r_kriging)$sfc$values)
    total <- data.frame("date"= st_dimensions(st_r_kriging)$time$values[i], "x"=coords[,"X"], "y"=coords[,"Y"], "lm_pred"=full_pred_stars$lm_pred[,i], "krige_pred"=st_r_kriging$var1.pred[,i], "var"=st_r_kriging$var1.var[,i])
    total$sum <- total$lm_pred + total$krige_pred
    total_list[[i]] <- total
  }
  total <- dplyr::bind_rows(total_list)
  total$date <- as.character(total$date)
  
  
  ####################################################
  # Compare ST.Reg.Kriging predictions with Reference Station measurements:
  
  # Add Reference Station measurements from https://data.rivm.nl/data/luchtmeetnet/:
  rivm_pm25 <- read.csv('~/GitHub/ADS-Snuffelfiets-Thesis/data/external/2020_PM25.csv', skip=7, sep=";")
  rivm_pm25 <- rivm_pm25[,c(5,26,28)] 
  rivm_pm25$date <- as.POSIXct(rivm_pm25$Einddatumtijd, format="%Y%m%d %H:%M")
  rivm_pm25$date <- as.character(rivm_pm25$date)
  
  rivm_pm25 <- rivm_pm25 %>%
    pivot_longer(c("NL10636", "NL10643"), names_to = "location", values_to = "PM2.5")
  
  rivm_pm25 <- data.frame(rivm_pm25[,c(2,3,4)])
  
  
  rivm_pm25$x[rivm_pm25$location == "NL10636"] <- coords[1,"X"]
  rivm_pm25$x[rivm_pm25$location == "NL10643"] <- coords[2,"X"]
  
  full <- merge(total, rivm_pm25, by=c("date","x"))
  
  # Reference Data contains negative and NA values -> remove:
  full <- full[!is.na(full$PM2.5),]
  full <- full[(full$PM2.5>0),]
  
  
  # Calculate statistics (adopted from demo(stkrige-crossvalidation)):
  crossStat <- function(pred=full$sum, res=full$PM2.5, digits=NA) {
    
    diff <- pred - res
    variance1 <- var(pred)
    variance2 <- var(res)
    cov12 <- cov(pred, res)
    mu1 <- mean(pred)
    mu2 <- mean(res)
    RMSE <- sqrt(mean(diff^2))
    MAE <- mean(abs(diff))
    ME <- mean(diff)
    COR <- cor(pred, res)
    MSE <- mean(diff^2)
    CONCOR <- (2*cov12)/(variance1+variance2+(mu1-mu2)^2)
    stats <- c(RMSE, MAE, ME, COR, MSE, CONCOR)
    names(stats) <- c("RMSE", "MAE", "ME", "COR", "MSE", "CONCOR")
    
    if(is.na(digits))
      return(stats)
    else
      return(round(stats, digits))
  }
  
  stats <- data.frame(as.list(crossStat(digits=5)))
  
  
  ####################################################
  # Save the results as CSV:
  
  filename = paste("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/output/external_cv/external_cv", res, ".csv", sep="")
  write.csv(stats, file=filename, row.names=FALSE)
  
}

externalCV(100)
