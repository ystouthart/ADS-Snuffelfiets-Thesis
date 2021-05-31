rm(list=ls())
library(raster)
library(gstat)
library(sp)
library(sf)



# Read the City of Utrecht polygon (CBS, 2020)
utrecht <- st_read(
  "C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/external/WijkBuurtkaart_2020_v1/gem_utrecht.shp")


uniKrig <- function(d, res) {
  #
  # Takes in the VMS Grid data and returns Universal Kriging Raster
  # d = VMS Grid in .csv, res = resolution (= set equal to input data)
  # Load the VMS grid data as a SpatialPoints DF
  coordinates(d) <- ~x+y
  proj4string(d) <- CRS(st_crs(28992)$wkt)
  d
  data_na = is.na(d$pm2_5)
  
  # Create a sample variogram
  spdf.vgm <- variogram(pm2_5 ~ 1, d[!data_na,])
  
  # Fit a variogram model
  vgm.mod <- fit.variogram(spdf.vgm, vgm(c("Exp", "Mat", "Sph")), fit.kappa = T)
  plot(spdf.vgm, vgm.mod)
  print(vgm.mod)
  
  # Apply Universal Kriging on the empty raster cells.
  uni_krig <- krige(pm2_5 ~ x+y, d[!data_na,], d, vgm.mod, nmax=20)
  spplot(uni_krig, c("var1.pred")) 
  
  
  # Rasterize the Kriging map - Important: set the same resolution as the input VMS grid data.
  r <- raster(extent(utrecht), resolution=c(res), crs=st_crs(28992)$wkt)
  krig.rst <- rasterize(uni_krig, r, "var1.pred")
  
  return(krig.rst)
}

ordKrig <- function(d, res) {
  #
  # Takes in the VMS Grid data and returns Ordinary Kriging Raster
  # d = VMS Grid in .csv, res = resolution (= set equal to input data)
  # Load the VMS grid data as a SpatialPoints DF
  coordinates(d) <- ~x+y
  proj4string(d) <- CRS(st_crs(28992)$wkt)
  
  data_na = is.na(d$pm2_5)
  
  # Create a sample variogram
  spdf.vgm <- variogram(pm2_5 ~ 1, d[!data_na,])
  
  # Fit a variogram model
  vgm.mod <- fit.variogram(spdf.vgm, vgm(c("Exp", "Mat", "Sph")), fit.kappa = T)
  plot(spdf.vgm, vgm.mod)
  print(vgm.mod)
  
  # Apply Universal Kriging on the empty raster cells.
  uni_krig <- krige(pm2_5 ~ 1, d[!data_na,], d, vgm.mod)
  spplot(uni_krig, c("var1.pred")) 
  
  
  # Rasterize the Kriging map - Important: set the same resolution as the input VMS grid data.
  r <- raster(extent(utrecht), resolution=c(res), crs=st_crs(28992)$wkt)
  krig.rst <- rasterize(uni_krig, r, "var1.pred")
  
  return(krig.rst)
}


data <- read.csv("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/vms_grid/total/grid_vms1500.csv") 
vms1500 <- ordKrig(data, 1500)
writeRaster(vms1500,'C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/output/universal_kriging/krige1500.tif',options=c('TFW=YES'))

data <- read.csv("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/vms_grid/total/grid_vms1000.csv") 
vms1000 <- ordKrig(data, 1000)
writeRaster(vms1000,'C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/output/universal_kriging/krige1000.tif',options=c('TFW=YES'))

data <- read.csv("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/vms_grid/total/grid_vms500.csv") 
vms500 <- ordKrig(data, 500)
writeRaster(vms500,'C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/output/universal_kriging/krige500.tif',options=c('TFW=YES'),overwrite=T)

data <- read.csv("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/vms_grid/total/grid_vms250.csv") 
vms250 <- ordKrig(data, 250)
writeRaster(vms250,'C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/output/universal_kriging/krige250.tif',options=c('TFW=YES'))

data <- read.csv("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/vms_grid/total/grid_vms125.csv") 
vms125 <- ordKrig(data, 125)
writeRaster(vms125,'C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/output/universal_kriging/krige125.tif',options=c('TFW=YES'))


# Plot the result
plot(vms1500)
plot(utrecht$geometry, add=TRUE)
plot(vms1000)
plot(utrecht$geometry, add=TRUE)
plot(vms500)
plot(utrecht$geometry, add=TRUE)
plot(vms250)
plot(utrecht$geometry, add=TRUE)
plot(vms125)
plot(utrecht$geometry, add=TRUE)


plot(data$x, data$pm2_5)

coordinates(data) <- ~x+y
proj4string(data) <- CRS(st_crs(28992)$wkt)
data_na = is.na(data$pm2_5)

cv <-automap::autoKrige.cv(pm2_5~1,input_data=data[!data_na,])
ak<-automap::autoKrige(pm2_5~1, input_data=data[!data_na,], new_data=data)
ak
plot(ak)
spplot(data)
plot(ak$krige_output)


spdf.vgm <- variogram(pm2_5 ~ 1, data[!data_na,])
vgm.mod <- fit.variogram(spdf.vgm, vgm(c("Exp", "Mat", "Sph")), fit.kappa = T)
print(vgm.mod)
plot(spdf.vgm, vgm.mod)
uni_krig <- krige(pm2_5 ~ 1, data[!data_na,], data, vgm.mod)
spplot(uni_krig, c("var1.pred"))


####### High Resolution #######
data <- read.csv("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/vms_grid/total/grid_vms100.csv") 
vms100 <- ordKrig(data, 100)

data <- read.csv("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/vms_grid/total/grid_vms50.csv") 
vms50 <- ordKrig(data, 50)

data <- read.csv("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/vms_grid/total/grid_vms25.csv") 
vms25 <- ordKrig(data, 25)


plot(vms100)
plot(utrecht$geometry, add=TRUE)

plot(vms50)
plot(utrecht$geometry, add=TRUE)

plot(vms25)
plot(utrecht$geometry, add=TRUE)


##################
###### TEST ######
data <- read.csv("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/vms_grid/total/grid_vms500.csv") 
coordinates(data) <- ~x+y
proj4string(data) <- CRS(st_crs(28992)$wkt)

empty_500 <- read.csv("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/r2.csv") 
coordinates(empty_500) <- ~x+y
proj4string(empty_500) <- CRS(st_crs(28992)$wkt)


data_na = is.na(data$pm2_5)

# Create a sample variogram

spdf.vgm <- variogram(pm2_5 ~ 1, data[!data_na,])

# Fit a variogram model
vgm.mod <- fit.variogram(spdf.vgm, vgm(c("Exp", "Mat", "Sph")), fit.kappa = T)
plot(spdf.vgm, vgm.mod)
print(vgm.mod)

# Apply Universal Kriging on the empty raster cells.
uni_krig <- krige(pm2_5 ~ 1, data[!data_na,], empty_500, vgm.mod)
spplot(uni_krig, c("var1.pred")) 

spplot(data)
spplot(empty_500)
plot(empty_500)
