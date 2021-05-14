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
  
  return(spdf)
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
vms1500 <- vmsGrid(data, 1500)
vms1000 <- vmsGrid(data, 1000)
vms500 <- vmsGrid(data, 500)
vms250 <- vmsGrid(data, 250)
vms125 <- vmsGrid(data, 125)



# Save VMS grids as .csv.
filename = "C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/vms_grid/grid_"

export <- as.data.frame(vms1500)
write.csv(export, file=paste(filename, "vms1500.csv", sep=""), row.names=FALSE)

export <- as.data.frame(vms1000)
write.csv(export, file=paste(filename, "vms1000.csv", sep=""), row.names=FALSE)

export <- as.data.frame(vms500)
write.csv(export, file=paste(filename, "vms500.csv", sep=""), row.names=FALSE)

export <- as.data.frame(vms250)
write.csv(export, file=paste(filename, "vms250.csv", sep=""), row.names=FALSE)

export <- as.data.frame(vms125)
write.csv(export, file=paste(filename, "vms125.csv", sep=""), row.names=FALSE)
