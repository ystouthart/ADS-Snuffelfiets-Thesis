rm(list=ls())
library(raster)
library(gstat)
library(sp)
library(sf)


vmsGrid <- function(d, res){
  #
  # Takes in the Snuffelfiets SPDF, and returns a SPDF with aggregated median pm2.5 values.
  # d = SPDF, res = raster resolution
  #
  r <- raster(extent(utrecht), resolution=c(res), crs=st_crs(28992)$wkt)
  r_pm25 <- rasterize(d, r, field = "pm2_5", median)
  r_count <- rasterize(d, r, field = "pm2_5", 'count')
  r_unique <- rasterize(d, r, field = "pm2_5", fun=function(x, ...) {length(unique(na.omit(x)))})
  
  r <- stack(r_pm25, r_count, r_unique)

  spdf = as.data.frame(r,xy=TRUE)
  spdf <- setNames(spdf, c("x","y","pm2_5", "count", "unique"))
  coordinates(spdf)=~x+y
  
  export <- as.data.frame(spdf)
  
  filename = paste("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/vms_grid/grid_vms", res, ".csv", sep='')
  write.csv(export, file=filename, row.names=FALSE)
  
  print(paste("done: vms ", res, " daily", sep=""))
}


vmsGridDaily <- function(d, res){
  #
  # Takes in the full Snuffelfiets SPDF, and saves a SPDF with aggregated median pm2.5 values per day in .csv.
  # d = SPDF, res = raster resolution
  #
  data$recording_time <- as.POSIXct(data$recording_time,format="%Y-%m-%d %H:%M:%S")
  data$recording_timeD <- as.POSIXct(trunc(data$recording_time, units = "days"),format="%Y-%m-%d %H:%M:%S")  
  uniq <- unique(unlist(data$recording_timeD))
  
  for (i in 1:length(uniq)){
    data_1h <- subset(data, recording_timeD == uniq[i])
    r <- raster(extent(utrecht), resolution=c(res), crs=st_crs(28992)$wkt)
    
    r_pm25 <- rasterize(data_1h, r, field = "pm2_5", median)
    r_count <- rasterize(data_1h, r, field = "pm2_5", 'count')
    r_unique <- rasterize(data_1h, r, field = "pm2_5", fun=function(x, ...) {length(unique(na.omit(x)))})
    
    r <- stack(r_pm25, r_count, r_unique)
    
    spdf = as.data.frame(r,xy=TRUE)
    spdf <- setNames(spdf, c("x","y","pm2_5", "count", "unique"))
    coordinates(spdf)=~x+y
    
    export <- as.data.frame(spdf)
    
    filename = paste("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/vms_grid/daily/", res, "/grid_vms", res, "_", format(uniq[i], "%m%d"), ".csv", sep='')
    write.csv(export, file=filename, row.names=FALSE)
    
    print(paste("done: vms ", res, " ", uniq[i], " ", i, "/", length(uniq), sep="")) 
  }
} 


vmsGridHourly <- function(d, res){
  #
  # Takes in the full Snuffelfiets SPDF, and saves a SPDF with aggregated median pm2.5 values per hour in .csv.
  # d = SPDF, res = raster resolution
  #
  data$recording_time <- as.POSIXct(data$recording_time,format="%Y-%m-%d %H:%M:%S")
  data$recording_timeH <- as.POSIXct(trunc(data$recording_time, units = "hours"),format="%Y-%m-%d %H:%M:%S")  
  uniq <- unique(unlist(data$recording_timeH))
  
  for (i in 1:length(uniq)){
    data_1h <- subset(data, recording_timeH == uniq[i])
    r <- raster(extent(utrecht), resolution=c(res), crs=st_crs(28992)$wkt)
    
    r_pm25 <- rasterize(data_1h, r, field = "pm2_5", median)
    r_count <- rasterize(data_1h, r, field = "pm2_5", 'count')
    r_unique <- rasterize(data_1h, r, field = "pm2_5", fun=function(x, ...) {length(unique(na.omit(x)))})
    
    r <- stack(r_pm25, r_count, r_unique)
    
    spdf = as.data.frame(r,xy=TRUE)
    spdf <- setNames(spdf, c("x","y","pm2_5", "count", "unique"))
    coordinates(spdf)=~x+y
    
    export <- as.data.frame(spdf)
    
    filename = paste("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/vms_grid/hourly/", res, "/grid_vms", res, "_", format(uniq[i], "%m%d-%H"), ".csv", sep='')
    write.csv(export, file=filename, row.names=FALSE)
    
    print(paste("done: vms ", res, " ", uniq[i], " ", i, "/", length(uniq), sep="")) 
  }
} 




# Read the City of Utrecht polygon (CBS, 2020)
utrecht <- st_read(
  "C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/external/WijkBuurtkaart_2020_v1/gem_utrecht.shp")

# Read the SF data for week 2 and 3, year 2020, during weekdays and daytime, and transform to SPDF.
data <- read.csv("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/external/city_jan_2020/week2_3_weekdays_dt.csv") 
coordinates(data) <- ~lon+lat
proj4string(data) <- CRS(st_crs(4326)$wkt)
data <- spTransform(data, CRS(st_crs(28992)$wkt))


# Create VMS Grids

# Total
vmsGrid(data, 1500)
vmsGrid(data, 1000)
vmsGrid(data, 500)
vmsGrid(data, 250)
vmsGrid(data, 125)
# Daily
vmsGridDaily(data, 1500)
vmsGridDaily(data, 1000)
vmsGridDaily(data, 500)
vmsGridDaily(data, 250)
vmsGridDaily(data, 125)
# Hourly
vmsGridHourly(data, 1500)
vmsGridHourly(data, 1000)
vmsGridHourly(data, 500)
vmsGridHourly(data, 250)
vmsGridHourly(data, 125)


