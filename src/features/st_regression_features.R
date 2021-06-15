###
###  regression_features.R
###
###  Combines the aggregated Snuffelfiets measurements (dependent variable) with the independent features.
###  The independent features are both Spatial (e.g. distance to roads) and Temporal (e.g. humidity, time).
###
###  Input: Aggregated & rasterized Snuffelfiets data (output from VMS_grid.R).
###  Output: A single CSV with all features needed for Spatio Temporal Regression Kriging.
###

rm(list=ls())
library(raster)
library(gstat)
library(sp)
library(sf)
library(osmenrich)


# Important: Set the resolution of the aggregated input data.
res <- 1000

# Open the City of Utrecht polygon (CBS, 2020).
utrecht <- st_read("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/external/WijkBuurtkaart_2020_v1/gem_utrecht.shp")


###################################################################


# Create the spatial regression predictors:

# Address density and population density
r <- raster(extent(utrecht), resolution=c(res), crs=projection(utrecht)) 
neighborhoods <- st_read("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/external/WijkBuurtkaart_2020_v1/buurt_2020_v1.shp")

address_density <- rasterize(neighborhoods, r, field = "OAD", mean)
values(address_density)[values(address_density)<0] <- 0

pop_density <- rasterize(neighborhoods, r, field = "BEV_DICHTH", mean)
values(pop_density)[values(pop_density)<0] <- 0

# Proximity to main roads (A/N-type)
roads <- raster("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/external/road_distance_utrecht.TIF")
values(roads)[values(roads)<=150] <- 1
values(roads)[values(roads)>150] <- 0 # Label all area's closer than 100m to main roads

# Proximity to rail roads
rail <- raster("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/external/rail_distance_utrecht.TIF")
values(rail)[values(rail)<=150] <- 1
values(rail)[values(rail)>150] <- 0


###################################################################


# Create the temporal regression predictors:

# Open the KNMI data (KNMI, 2021):
knmi <- read.csv('C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/external/uurgeg_260_2011-2020/uurgeg_260_2011-2020.txt', skip=31, strip.white=T)
knmi <- knmi[,c(2,3,4,5,18)] 

# Create Datetime column for matching
knmi$date <- as.POSIXct(paste(knmi$YYYYMMDD, knmi$HH, sep=" "), format="%Y%m%d %H")
knmi$date <- as.character(knmi$date)
knmi <- knmi[,-1] 

# Change KNMI wind direction from degrees to factors (cardinal directions)
degree_to_dir <- function(deg){
  deg <- floor((deg/45) + 0.5)
  dirs <- c("N","NE","E", "SE","S","SW","W","NW")
  return(dirs[(deg%%8)+1])
}

knmi$DD[!(knmi$DD==990|knmi$DD==00)] <- degree_to_dir(knmi$DD[!(knmi$DD==990|knmi$DD==00)])
knmi$DD[knmi$DD==990] <- "VAR" # variable
knmi$DD[knmi$DD==0] <- "NO" # no wind

colnames(knmi) <- c("YYYYMMDD", "HH", "wind_dir", "wind_speed", "humidity")

###################################################################


# Combine dependents and independents:

# Load the aggregated Snuffelfiets measurements (same resolution as res!!!)
filename <- paste("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/processed/vms_grid/hourly/", res, "/f_full_grid_vms", res, ".csv", sep="")
data <- read.csv(filename)
coordinates(data) <- ~x+y
projection(data) <- projection(utrecht) # set the projection equal

# Add the Spatial predictors
data$pop_density <- extract(pop_density, data)
data$address_density <- extract(address_density, data)
data$road <- extract(roads, data)
data$rail <- extract(rail, data)

# Add the Temporal predictors
data <- merge(data, knmi, by="date")


###################################################################


# Using 'osmenrich' package for adding more spatial features from OpenStreetMap:


sf_data <- st_as_sf(data.frame(round(coordinates(r),1)), coords = c("x", "y"), crs = 28992)

sf_data_ts <- sf_data %>%
  enrich_osm(
    name = "traffic_signals",
    key = "highway",
    value = "traffic_signals",
    r = (res/2)
  )

st_crs(sf_data_ts) <- st_crs(data)
data <- st_as_sf(data, crs=28992)

spat_join <- st_join(data, sf_data_ts)


###################################################################
###################################################################


# Save as Regression Features CSV file
filename = paste("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/processed/regression_features/features", res, ".csv", sep='')
st_write(spat_join, filename, layer_options = "GEOMETRY=AS_XY", append=FALSE)

