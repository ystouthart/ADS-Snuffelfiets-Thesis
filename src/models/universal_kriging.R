rm(list=ls())
library(raster)
library(gstat)
library(sp)
library(sf)


uniKrig <- function(d, res) {
  #
  # Takes in the VMS Grid data and returns Universal Kriging Raster
  # d = VMS Grid in .csv, res = resolution (= set equal to input data)
  #
  
  # Load the VMS grid data as a SpatialPoints DF
  coordinates(data) <- ~x+y
  proj4string(data) <- CRS(st_crs(28992)$wkt)
  
  data_na = is.na(data$pm2_5)
  
  # Create a sample variogram
  spdf.vgm <- variogram(pm2_5 ~ 1, data[!data_na,])
  
  # Fit a variogram model
  vgm.mod <- fit.variogram(spdf.vgm, vgm(c("Exp", "Mat", "Sph")), fit.kappa = TRUE)
  plot(spdf.vgm, vgm.mod)
  print(vgm.mod)
  
  # Apply Universal Kriging on the empty raster cells.
  uni_krig <- krige(pm2_5 ~ x+y, data[!data_na,], data, vgm.mod, nmax=20)
  spplot(uni_krig, c("var1.pred")) 
  
  
  # Rasterize the Kriging map - Important: set the same resolution as the input VMS grid data.
  r <- raster(extent(utrecht), resolution=c(res), crs=st_crs(28992)$wkt)
  krig.rst <- rasterize(uni_krig, r, "var1.pred")
  
  return(krig.rst)
}


# Read the City of Utrecht polygon (CBS, 2020)
utrecht <- st_read(
  "C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/external/WijkBuurtkaart_2020_v1/gem_utrecht.shp")


data <- read.csv("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/vms_grid/grid_vms1500.csv") 
vms1500 <- uniKrig(data, 1500)
writeRaster(vms1500,'C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/output/universal_kriging/krige1500.tif',options=c('TFW=YES'))

data <- read.csv("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/vms_grid/grid_vms1000.csv") 
vms1000 <- uniKrig(data, 1000)
writeRaster(vms1000,'C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/output/universal_kriging/krige1000.tif',options=c('TFW=YES'))

data <- read.csv("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/vms_grid/grid_vms500.csv") 
vms500 <- uniKrig(data, 500)
writeRaster(vms500,'C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/output/universal_kriging/krige500.tif',options=c('TFW=YES'))

data <- read.csv("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/vms_grid/grid_vms250.csv") 
vms250 <- uniKrig(data, 250)
writeRaster(vms250,'C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/output/universal_kriging/krige250.tif',options=c('TFW=YES'))

data <- read.csv("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/vms_grid/grid_vms125.csv") 
vms125 <- uniKrig(data, 125)
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



