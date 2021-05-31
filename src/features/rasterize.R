library(sf)
library(rgdal)
library(raster)

# Read the City of Utrecht polygon (CBS, 2020)
utrecht <- st_read(
  "C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/external/WijkBuurtkaart_2020_v1/gem_utrecht.shp")

d <- read.csv("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/raw/city_jan_2020/seperate_weeks/resource_2020_01_06_2020_01_13.csv") # Read Snuffelfiets data
coordinates(d) <- ~lon+lat # Convert SF data to Spatial Points
projection(d) <- projection(utrecht)


r <- raster(extent(utrecht), resolution=c(500), crs=projection(utrecht)) # Create a raster with the extend of the Shapefile


x1 <- rasterize(d, r, field = "pm2_5", mean) # Fill the raster with the Snuffelfiets points and take the mean of each cell
x2 <- rasterize(d, r, field = "pm2_5", 'count')  # Fill the raster with the Snuffelfiets points and take the count of each cell



# Save raster as TIF:
r <- stack(x1, x2) # combine the two rasters as seperate layers
wr <- writeRaster(r, filename="../../data/external/raster3.TIF", overwrite=TRUE) # save raster


# Save raster as CSV:
spdf = as.data.frame(r,xy=TRUE)
spdf <- setNames(spdf, c("x","y","pm2_5_mean", "pm2_5_count"))
write.csv(spdf, file="../../data/external/raster3.csv", row.names=FALSE)


plot(utrecht$geometry)

r <- raster((utrecht), resolution=c(500), crs=projection(utrecht)) # Create a raster with the extend of the Shapefile
x1 <- rasterize(d, r, field = "pm2_5", mean) # Fill the raster with the Snuffelfiets points and take the mean of each cell

plot(x1)




library(tidyverse)
library(stars)
crs = 28992
crs
de <- st_transform(st_as_sf(utrecht), crs=crs(utrecht))
st_bbox(de) %>%
  st_as_stars(dx = 500) %>%
  st_set_crs(crs) %>%
  st_crop(de) -> grd
grd

plot(grd)
crs(grd)
wr <- writeRaster(st_as_sf(grd), filename="../../data/external/grd.TIF", overwrite=TRUE)
st_as_sf(grd)



r <- raster(extent(utrecht), resolution=c(500), crs=projection(utrecht), vals=0) # Create a raster with the extend of the Shapefile
r2 <- raster::mask(r,utrecht)


plot(r)
plot(r2)

wr <- writeRaster(r2, filename="C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/r2.TIF", overwrite=TRUE)
# Save raster as CSV:
spdf = as.data.frame(r2,xy=TRUE)
spdf <- setNames(spdf, c("x","y","fill"))
write.csv(spdf, file="C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/r2.csv", row.names=FALSE)



##################
###### TEST ######
data <- read.csv("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/vms_grid/total/grid_vms500.csv") 
coordinates(data) <- ~x+y
proj4string(data) <- CRS(st_crs(28992)$wkt)

coordinates(spdf) <- ~x+y
proj4string(spdf) <- CRS(st_crs(28992)$wkt)


# Create a sample variogram

spdf.vgm <- variogram(pm2_5 ~ 1, data[!is.na(data$pm2_5),])

# Fit a variogram model
vgm.mod <- fit.variogram(spdf.vgm, vgm(c("Exp", "Mat", "Sph")), fit.kappa = T)
plot(spdf.vgm, vgm.mod)
print(vgm.mod)

# Apply Universal Kriging on the empty raster cells.
uni_krig <- krige(pm2_5 ~ 1, data[!is.na(data$pm2_5),], spdf[!is.na(spdf$fill),], vgm.mod)
spplot(uni_krig, c("var1.pred")) 


r <- raster(extent(utrecht), resolution=c(500), crs=st_crs(28992)$wkt)
krig.rst <- rasterize(uni_krig, r, "var1.pred")

plot(krig.rst)

write.csv(data[!is.na(data$pm2_5),], file="C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/r2data.csv", row.names=FALSE)

writeRaster(krig.rst,'C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/krige500.tif',options=c('TFW=YES'),overwrite=T)



########

ak<-automap::autoKrige(pm2_5~1, input_data=data[!is.na(data$pm2_5),], new_data=spdf[!is.na(spdf$fill),])

plot(ak)
spplot(ak$krige_output,c("var1.pred"))



ak<-automap::autoKrige(pm2_5~1, input_data=data[!is.na(data$pm2_5),], new_data=data)

plot(ak)
spplot(ak$krige_output,c("var1.pred"))

