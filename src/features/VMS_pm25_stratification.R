library(raster)
library(sf)
library(sp)
library(tidyverse)
library(cluster)

# Read the City of Utrecht polygon (CBS, 2020)
utrecht <- st_read(
  "C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/external/WijkBuurtkaart_2020_v1/gem_utrecht.shp")

# Read the SF data for January 2020
d <- read.csv("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/external/city_jan_2020/city_jan_2020.csv") %>%
  st_as_sf(coords = c("lon", "lat"), crs=4326) %>%
  st_transform(crs=28992)


# Create a raster with the extend of the City of Utrecht, resolution 500m
r <- raster(extent(utrecht), resolution=c(500), crs=st_crs(28992)$wkt)

# Fill the raster with the Snuffelfiets data of January, take the median value per cell.
rast <- rasterize(d, r, field = "pm2_5", median)

# Polygonize raster, add centroids
spdf <- rasterToPolygons(rast)
centroids <- getSpPPolygonsLabptSlots(spdf)

spdf$x <- centroids[,1]
spdf$y <- centroids[,2]

# Convert spatial-DF to regular DF, apply k-means (k=111)
df <- as.data.frame(spdf)

k1 <- kmeans(df, centers = 111, nstart = 25)
df$clusters <- k1$cluster

# Write results to .csv
write.csv(x=df, file="C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/pm25_stratified/111data.csv")
write.csv(x=k1$centers, file="C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/pm25_stratified/111centers.csv")



# Convert spatial-DF to regular DF, apply k-means (k=10)
df <- as.data.frame(spdf)

k2 <- kmeans(df, centers = 10, nstart = 25)
df$clusters <- k2$cluster

# Write results to .csv
write.csv(x=df, file="C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/pm25_stratified/10data.csv")
write.csv(x=k2$centers, file="C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/pm25_stratified/10centers.csv")

