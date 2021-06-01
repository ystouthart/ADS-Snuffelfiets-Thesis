rm(list=ls())
library(raster)
library(gstat)
library(sp)
library(sf)

##############################################################################################################
#  regression_features.R: Combines the aggregated Snuffelfiets measurements (dependent variable) with the independent features.
#  The independent features are both Spatial (e.g. distance to roads) and Temporal (e.g. humidity, time).
#
#  Input: Aggregated & rasterized Snuffelfiets data (output from VMS_grid.R).
#  Output: A single CSV with all features needed for Spatio Temporal Regression Kriging.
##############################################################################################################


# Important: Set the resolution of the aggregated input data.
res <- 100

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
values(address_density)[values(address_density)<0] <- 0

pop_density <- rasterize(neighborhoods, r, field = "BEV_DICHTH", mean)
values(pop_density)[values(pop_density)<0] <- 0

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

# Proximity to rail roads
rail <- raster("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/external/rail_utrecht.TIF")
values(rail)[values(rail)>0] <- 1
values(rail)[values(rail)==0] <- NA
close_to_rail <- (distance(rail)<=250)



########



# Create the temporal regression predictors:

# Open the KNMI data (KNMI, 2021):
knmi <- read.csv('C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/external/uurgeg_260_2011-2020/uurgeg_260_2011-2020.txt', skip=31, strip.white=T)
knmi <- knmi[,c(2,3,4,5,8,15,18)] 

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
knmi$DD[knmi$DD==990] <- "VAR"
knmi$DD[knmi$DD==0] <- "NO"



########


# Combine dependents and independents:

# Load the aggregated Snuffelfiets measurements (same resolution as res!!!)
data <- read.csv('C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/vms_grid/hourly/100/full_grid_vms100.csv')
coordinates(data) <- ~x+y
projection(data) <- projection(utrecht) # set the projection equal

# Add the Spatial predictors
data$pop_density <- extract(pop_density, data)
data$address_density <- extract(address_density, data)
data$easting <- extract(easting, data)
data$northing <- extract(northing, data)
data$road <- extract(close_to_road, data)
data$road <- as.factor(data$road)
data$rail <- extract(close_to_rail, data)
data$rail <- as.factor(data$rail)

# Add the Temporal predictors
data <- merge(data, knmi, by="date")


########


# Save as CSV file
filename = paste("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/interim/regression_features/features", res, ".csv", sep='')
write.csv(data, file=filename, row.names=FALSE)

