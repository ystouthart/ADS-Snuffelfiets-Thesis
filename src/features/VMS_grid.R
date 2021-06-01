rm(list=ls())
library(raster)
library(gstat)
library(sp)
library(sf)


vmsGrid <- function(d, res){
  #
  # Takes in the Snuffelfiets SPDF, and returns a SPDF with pm2.5 values aggregated on a grid with resolution = res.
  # d = SPDF, res = raster resolution
  #
  r <- raster(extent(utrecht), resolution=c(res), crs=crs(utrecht))
  r_pm25_med <- rasterize(d, r, field = "pm2_5", median)
  r_pm25_mean <- rasterize(d, r, field = "pm2_5", mean)
  r_count <- rasterize(d, r, field = "pm2_5", 'count')
  r_unique <- rasterize(d, r, field = "sensor", fun=function(x, ...) {length(unique(na.omit(x)))})
  r_se <- rasterize(d, r, field = "pm2_5", fun=function(x, ...) {sd(x)/sqrt(length(x))})
  r_sd <- rasterize(d, r, field = "pm2_5", sd)
  
  r <- stack(r_pm25_med, r_pm25_mean, r_count, r_unique, r_se, r_sd)

  spdf = as.data.frame(r,xy=TRUE)
  spdf <- setNames(spdf, c("x", "y", "pm2_5_med", "pm2_5_mean", "count", "unique", "se", "sd"))
  
  filename = paste("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/vms_grid/total/grid_vms", res, ".csv", sep='')
  write.csv(spdf, file=filename, row.names=FALSE)
  
  print(paste("done: vms ", res, " total", sep=""))
}


vmsGridDaily <- function(d, res){
  #
  # Takes in the full Snuffelfiets SPDF, and saves a SPDF with aggregated median pm2.5 values per day in .csv.
  # d = SPDF, res = raster resolution
  #
  d$recording_time <- as.POSIXct(d$recording_time,format="%Y-%m-%d %H:%M:%S")
  d$recording_timeD <- as.POSIXct(trunc(d$recording_time, units = "days"),format="%Y-%m-%d %H:%M:%S")  
  uniq <- unique(unlist(d$recording_timeD))
  
  for (i in 1:length(uniq)){
    data_1d <- subset(d, recording_timeD == uniq[i])
    r <- raster(extent(utrecht), resolution=c(res), crs=crs(utrecht))
    r_pm25_med <- rasterize(data_1d, r, field = "pm2_5", median)
    r_pm25_mean <- rasterize(data_1d, r, field = "pm2_5", mean)
    r_count <- rasterize(data_1d, r, field = "pm2_5", 'count')
    r_unique <- rasterize(data_1d, r, field = "sensor", fun=function(x, ...) {length(unique(na.omit(x)))})
    r_se <- rasterize(data_1d, r, field = "pm2_5", fun=function(x, ...) {sd(x)/sqrt(length(x))})
    r_sd <- rasterize(data_1d, r, field = "pm2_5", sd)
    
    r <- stack(r_pm25_med, r_pm25_mean, r_count, r_unique, r_se, r_sd)
    
    spdf = as.data.frame(r,xy=TRUE)
    spdf <- setNames(spdf, c("x", "y", "pm2_5_med", "pm2_5_mean", "count", "unique", "se", "sd"))
    spdf$date <- uniq[i]
    
    filename = paste("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/vms_grid/daily/", res, "/grid_vms", res, "_", format(uniq[i], "%m%d"), ".csv", sep='')
    write.csv(spdf, file=filename, row.names=FALSE)
    
    print(paste("done: vms ", res, " ", uniq[i], " ", i, "/", length(uniq), sep="")) 
  }
} 


vmsGridHourly <- function(d, res){
  #
  # Takes in the full Snuffelfiets SPDF, and saves a SPDF with aggregated median pm2.5 values per hour in .csv.
  # d = SPDF, res = raster resolution
  #
  d$recording_time <- as.POSIXct(d$recording_time,format="%Y-%m-%d %H:%M:%S")
  d$recording_timeH <- as.POSIXct(trunc(d$recording_time, units = "hours"),format="%Y-%m-%d %H:%M:%S")  
  uniq <- unique(unlist(d$recording_timeH))
  
  for (i in 1:length(uniq)){
    data_1h <- subset(d, recording_timeH == uniq[i])
    r <- raster(extent(utrecht), resolution=c(res), crs=crs(utrecht))
    r_pm25_med <- rasterize(data_1h, r, field = "pm2_5", median)
    r_pm25_mean <- rasterize(data_1h, r, field = "pm2_5", mean)
    r_count <- rasterize(data_1h, r, field = "pm2_5", 'count')
    r_unique <- rasterize(data_1h, r, field = "sensor", fun=function(x, ...) {length(unique(na.omit(x)))})
    r_se <- rasterize(data_1h, r, field = "pm2_5", fun=function(x, ...) {sd(x)/sqrt(length(x))})
    r_sd <- rasterize(data_1h, r, field = "pm2_5", sd)
    
    r <- stack(r_pm25_med, r_pm25_mean, r_count, r_unique, r_se, r_sd)
    
    spdf = as.data.frame(r,xy=TRUE)
    spdf <- setNames(spdf, c("x", "y", "pm2_5_med", "pm2_5_mean", "count", "unique", "se", "sd"))
    spdf$date <- uniq[i]
    
    filename = paste("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/vms_grid/hourly/", res, "/grid_vms", res, "_", format(uniq[i], "%m%d-%H"), ".csv", sep='')
    write.csv(spdf, file=filename, row.names=FALSE)
    
    print(paste("done: vms ", res, " ", uniq[i], " ", i, "/", length(uniq), sep="")) 
  }
} 




# Read the City of Utrecht polygon (CBS, 2020)
utrecht <- st_read(
  "C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/external/WijkBuurtkaart_2020_v1/gem_utrecht.shp")

# Read the SF data for week 2 and 3, year 2020, during weekdays and daytime, and transform to EPSG:28992.
data <- read.csv("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/raw/city_jan_2020/data_selection_bbox.csv") 
coordinates(data) <- ~lon+lat
proj4string(data) <- CRS(st_crs(4326)$wkt)
data <- spTransform(data, crs(utrecht))


# Create VMS Grids
# Total
vmsGrid(data, 1500)
vmsGrid(data, 1000)
vmsGrid(data, 500)
vmsGrid(data, 250)
vmsGrid(data, 125)
vmsGrid(data, 100)
vmsGrid(data, 50)
vmsGrid(data, 25)
# Daily
vmsGridDaily(data, 1500)
vmsGridDaily(data, 1000)
vmsGridDaily(data, 500)
vmsGridDaily(data, 250)
vmsGridDaily(data, 125)
vmsGridDaily(data, 100)
vmsGridDaily(data, 50)
vmsGridDaily(data, 25)
# Hourly
vmsGridHourly(data, 1500)
vmsGridHourly(data, 1000)
vmsGridHourly(data, 500)
vmsGridHourly(data, 250)
vmsGridHourly(data, 125)
vmsGridHourly(data, 100)
vmsGridHourly(data, 50)
vmsGridHourly(data, 25)
