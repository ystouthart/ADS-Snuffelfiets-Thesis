library(raster)
library(sf)
library(rgdal)
library(sp)
library(automap)
library(tidyverse)
library(cluster)

utrecht <- st_read(
  "C:/Users/Klant/Downloads/Utrecht.shp")


d <- read.csv("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/external/city_jan_2020/january_2020_city.csv") %>%
  st_as_sf(coords = c("lon", "lat"), crs=4326) %>%
  st_transform(crs=28992)


r <- raster(extent(utrecht), resolution=c(500), crs=st_crs(28992)$wkt) #modified


rast <- rasterize(d, r, field = "pm2_5", mean)

spdf <- rasterToPolygons(rast)

centroids <- getSpPPolygonsLabptSlots(spdf)

spdf$x <- centroids[,1]
spdf$y <- centroids[,2]

df <- as.data.frame(spdf)

k1 <- kmeans(df, centers = 111, nstart = 25)
df$clusters <- k1$cluster

write.csv(x=df, file="111data.csv")
write.csv(x=k1$centers, file="111centers.csv")



df <- as.data.frame(spdf)

k2 <- kmeans(df, centers = 10, nstart = 25)
df$clusters <- k2$cluster

write.csv(x=df, file="10data.csv")
write.csv(x=k2$centers, file="10centers.csv")

