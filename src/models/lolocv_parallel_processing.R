###
###  st_reg_lolocv.R
###
###  Applies Leave-One-Location-Out Cross Validation with Parallel Processing to assess ST-Reg.-Kriging model's internal accuracy.
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
library(parallel)
library(foreach)
library(bigstatsr)

res <- 125


lolocvParallel <- function(res){
  # Load the Regression Features:
  d <- read.csv(paste('~/GitHub/ADS-Snuffelfiets-Thesis/data/processed/regression_features/features', res, '.csv', sep=""))
  d$date = as.POSIXct(d$date)
  d = d[order(d[,"date"], d[,"X"], d[,"Y"]),]
  coordinates(d) <- ~X+Y
  utrecht <- st_read("~/GitHub/ADS-Snuffelfiets-Thesis/data/external/WijkBuurtkaart_2020_v1/gem_utrecht.shp")
  projection(d) <- projection(utrecht)
  
  # Set factors
  d$road <- as.factor(d$road)
  d$rail <- as.factor(d$rail)
  d$HH <- as.factor(d$HH)
  d$wind_dir <- as.factor(d$wind_dir)
  
  print("check load")
  
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
  
  
  print("check lm")
  
  ####################################################
  # Variogram:
  
  # Creating the Empirical ST Variogram
  vv= variogramST(res~1, pred_stars, cressie=F, boundaries=c(500,1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500), tlags = 0:6)
  
  
  # Fitting the ST Variogram model
  # metric
  metric <- vgmST("metric", joint=vgm(psill=25, model="Exp", range=2000, nugget=0), stAni=1200) 
  
  set.seed(seed=123)
  fitmetric <- fit.StVariogram(vv, metric)
  
  print("check vgm")
  
  #####################################
  # LOLOCV:
  
  # LOLO Predictions:
  locations = st_as_stars(value = matrix(1, length(st_dimensions(pred_stars)$space$values), length(st_dimensions(pred_stars)$time$values)), dim = dims)
  
  pred_matrix <- matrix(NA, length(st_dimensions(locations)$space$values),length(st_dimensions(locations)$time$values))
  
  lloc <- length(st_dimensions(locations)$space$values)
  
  
  # Parallel Processing (https://privefl.github.io/blog/a-guide-to-parallelism-in-r/):
  mat <- FBM(length(st_dimensions(locations)$space$values),length(st_dimensions(locations)$time$values))
  
  cl <- parallel::makeCluster(7)
  
  doParallel::registerDoParallel(cl)
  
  parts <- split(x = 1:lloc, f = 1:7)
  
  clusterExport(cl = cl, varlist = c("pred_stars", "locations", "fitmetric", "parts", "pred_matrix"), envir = .GlobalEnv)
  clusterEvalQ(cl = cl, expr = c(library('sp'), library('gstat'), library('stars')))
  
  print("check parallel set")
  print(Sys.time())
  
  tmp3 <- foreach(j = 1:7, .combine = 'c') %:%
    foreach(i = parts[[j]], .combine = 'c') %dopar% {
      mat[i,] <- krigeST(res~1, data=pred_stars["res", -i],
                         newdata=locations["value", i], nmax=50, 
                         stAni=fitmetric$stAni/3600, modelList = fitmetric)$var1.pred[1,]
      NULL
    }
  
  
  parallel::stopCluster(cl)
  
  print("check parallel completed")
  print(Sys.time())
  
  #####################################
  
  # Original values:
  res_matrix <- matrix(NA, length(st_dimensions(locations)$space$values),length(st_dimensions(locations)$time$values))
  
  for (loc in 1:length(st_dimensions(pred_stars)$space$values)){
    res_matrix[loc,] <- unname(pred_stars$res[loc,])
  }
  
  
  
  #####################################
  # Calculating performance statistics:
  
  
  
  # crossStat function adopted from demo(stkrige-crossvalidation)
  crossStat <- function(pred=mat[][!is.na(res_matrix)], res=res_matrix[!is.na(res_matrix)], digits=NA) {
    
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

    stats <- c(RMSE, MAE, ME, COR, MSE)
    names(stats) <- c("RMSE", "MAE", "ME", "COR", "MSE")
    
    if(is.na(digits))
      return(stats)
    else
      return(round(stats, digits))
  }
  
  stats <- data.frame(as.list(crossStat(digits=5)))
  
  filename = paste("~/GitHub/ADS-Snuffelfiets-Thesis/output/lolocv/parallel_lolocv_", res, ".csv", sep="")
  write.csv(stats, file=filename, row.names=FALSE)
  
}


lolocvParallel(125)


