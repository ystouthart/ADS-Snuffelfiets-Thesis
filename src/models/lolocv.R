###
###  st_reg_lolocv.R
###
###  Applies Leave-One-Location-Out Cross Validation to assess the Kriging model's internal accuracy.
###
###  Input: Regression features & prediction map (output from st_regression_features.R).
###  Output: Model performance statistics.
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




lolocv <- function(res){
  # Load the Regression Features:
  d <- read.csv(paste('C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/processed/regression_features/features', res, '.csv', sep=""))
  d$date = as.POSIXct(d$date)
  d = d[order(d[,"date"], d[,"X"], d[,"Y"]),]
  coordinates(d) <- ~X+Y
  utrecht <- st_read("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/external/WijkBuurtkaart_2020_v1/gem_utrecht.shp")
  projection(d) <- projection(utrecht)
  
  # Set factors
  d$road <- as.factor(d$road)
  d$rail <- as.factor(d$rail)
  d$HH <- as.factor(d$HH)
  d$wind_dir <- as.factor(d$wind_dir)
  d$pop <- d$pop/1000
  d$address <- d$address/1000
  
  ####################################################
  # Linear Model:
  
  # Train the model
  lm <- lm(pm2_5_mean ~ address_density + pop_density  + road + rail + wind_dir + HH + wind_speed + humidity , data=d, na.action="na.exclude")
  summary(lm)
  
  # Generate predictions
  lm_pred <- predict.lm(lm, se.fit=T)
  
  # Create the prediction dataframe
  pred_df <- data.frame("x"=d@coords[,1], "y"=d@coords[,2], "date"=d$date, "pm2_5"=d$pm2_5_mean, "pred"=lm_pred[["fit"]], "se.fit"=lm_pred[["se.fit"]])
  pred_df$res <- pred_df$pm2_5 - pred_df$pred  
  
  
  # Converting pred_df to 'stars' object
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
  
  # Creating the Empirical ST Variogram
  vv= variogramST(res~1, pred_stars, cressie=F, boundaries=c(500,1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500), tlags = 0:6)
  
  
  # Fitting the ST Variogram model
  # metric
  metric <- vgmST("metric", joint=vgm(psill=25, model="Exp", range=2000, nugget=0), stAni=1200) 
  
  set.seed(seed=123)
  fitmetric <- fit.StVariogram(vv, metric)
  
  
  #####################################
  # LOLOCV:
  
  # LOLO Predictions:
  locations = st_as_stars(value = matrix(1, length(st_dimensions(pred_stars)$space$values), length(st_dimensions(pred_stars)$time$values)), dim = dims)
  
  pred_matrix <- matrix(NA, length(st_dimensions(locations)$space$values),length(st_dimensions(locations)$time$values))
  
  lloc <- length(st_dimensions(locations)$space$values)
  
  for (loc in 1:lloc){
    print(loc/lloc)
    lolocv_krige = krigeST(res~1, data=pred_stars["res", -loc],
                           newdata=locations["value", loc], nmax=50, 
                           stAni=fitmetric$stAni/3600, modelList = fitmetric)
    pred_matrix[loc,] <- lolocv_krige$var1.pred
  }
  
  
  
  #####################################
  # Original values:
  
  res_matrix <- matrix(NA, length(st_dimensions(locations)$space$values),length(st_dimensions(locations)$time$values))
  
  for (loc in 1:length(st_dimensions(pred_stars)$space$values)){
    res_matrix[loc,] <- unname(pred_stars$res[loc,])
  }
  
  
  #####################################
  # Calculating performance statistics:
  
  
  # crossStat function adopted from demo(stkrige-crossvalidation):
  crossStat <- function(pred=mat3[][!is.na(res_matrix)], res=res_matrix[!is.na(res_matrix)], digits=NA) {
    
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
  
  
  #####################################
  # Save as CSV:
  
  filename = paste("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/output/lolocv/par_lolocv_", res, ".csv", sep="")
  write.csv(stats, file=filename, row.names=FALSE)
  
}

