rm(list=ls())
library(raster)
library(gstat)
library(sp)
library(sf)

##############################################################################################################
# regression_features.R: Combines the aggregated Snuffelfiets measurements (dependent variable) with the independent features.
# The independent features are both Spatial (e.g. distance to roads) and Temporal (e.g. humidity, time).
#
# Input: Aggregated & rasterized Snuffelfiets data (output from VMS_grid.R).
# Output: A single CSV with all features needed for Spatio Temporal Regression Kriging.
##############################################################################################################


# Important: Set the resolution of the aggregated input data.
res <- 500

# Open the City of Utrecht polygon (CBS, 2020).
utrecht <- st_read("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/external/WijkBuurtkaart_2020_v1/gem_utrecht.shp")

########

# Create the prediction grid
pred_grid <- raster(extent(utrecht), resolution=c(res), crs=projection(utrecht)) 
sp_pred_grid <- SpatialPoints(pred_grid)
proj4string(sp_pred_grid) <- proj4string(pred_grid)

########

# Create the spatial regression predictors:

r <- raster(extent(utrecht), resolution=c(res), crs=projection(utrecht)) 

# Address density and population density

neighborhoods <- st_read("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/external/WijkBuurtkaart_2020_v1/buurt_2020_v1.shp")
address_density <- rasterize(neighborhoods, r, field = "OAD", mean)
values(address_density)[values(address_density)<0] <- NA

pop_density <- rasterize(neighborhoods, r, field = "BEV_DICHTH", mean)
values(pop_density)[values(pop_density)<0] <- NA

# Headings
headings <- as.data.frame(coordinates(r))
headings$east <- coordinates(r)[,1]
headings$north <- coordinates(r)[,2]
coordinates(headings) <- ~x+y
easting <- rasterize(headings, r, field="east")
northing <- rasterize(headings, r, field="north")

# Proximity to main roads (A/N-type)
roads <- shapefile("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/external/01-12-2020/NWB-Light/nwb-light.shp")
roads_r <- rasterize(roads, r, field="COUNT")
values(roads_r)[values(roads_r)>0] <- 1
close_to_road <- (distance(roads_r)<=500) # Label all area's closer than 500m to main roads

########

# Create the temporal regression predictors:

# Open the KNMI data (KNMI, 2021):
knmi <- read.csv('C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/external/uurgeg_260_2011-2020/uurgeg_260_2011-2020.txt', skip=31, strip.white=T)
knmi <- knmi[,c(2,3,4,5,8,15,18)] 

# Create Datetime column for matching
knmi$date <- as.POSIXct(paste(knmi$YYYYMMDD, knmi$HH, sep=" "), format="%Y%m%d %H")

# Change KNMI wind direction from degrees to factors (cardinal directions)
degree_to_dir <- function(deg){
  if
  deg <- floor((deg/45) + 0.5)
  dirs <- c("N","NE","E", "SE","S","SW","W","NW")
  return(dirs[(deg%%8)+1])
}
