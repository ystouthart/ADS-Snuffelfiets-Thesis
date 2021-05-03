library(sf)
library(rgdal)
library(raster)

utrecht <- st_read("C:/Users/Klant/Downloads/Utrecht.shp")

d <- read.csv("../../data/external/resource_2021_03_29_2021_04_05.csv") %>%
  st_as_sf(coords = c("lon", "lat"), crs=4326) %>%
  st_transform(crs=28992)

r <- raster(extent(utrecht), resolution=c(25), crs=st_crs(28992)$wkt) #modified

x1 <- rasterize(d, r, field = "pm2_5", mean)
x2 <- rasterize(d, r, field = "pm2_5", 'count')  # deze toevoegen als laag (+ evt. nog "unieke" fietsen)

x1 <- addLayer(x1,x2)
#names(x1) <- c('pm2_5', 'count')

rf <- writeRaster(x1, filename="../../data/external/raster3.TIF", overwrite=TRUE)



