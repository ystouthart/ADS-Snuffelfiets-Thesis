#
# retrieval_prep_city_january.py
#
# Opens and preprocesses the Snuffelfiets data within the city of Utrecht for January 2020.
#

# Import packages
import pandas as pd 
import numpy as np 
import geopandas as gpd

from tqdm import tqdm
import glob


# Create temporary storage.
store = []


# Open City of Utrecht polygon.
utrecht = gpd.read_file("../../data/external/WijkBuurtkaart_2020_v1/gem_utrecht.shp")
utrecht = utrecht.to_crs("EPSG:28992")

# Create a bounding box around the City of Utrecht
bbox = gpd.GeoDataFrame(gpd.GeoSeries(utrecht.envelope), columns=['geometry'])


# Opening all raw CSV files.
path = "../../data/raw/city_jan_2020/raw" # use your path
all_files = glob.glob(path + "/*.csv")


# Download each CSV file, pre-process, and save.
for csv in tqdm(all_files): 
    df = pd.read_csv(csv, usecols=['sensor', 'lon', 'lat', 'recording_time', 'pm2_5'])
    df['recording_time'] = pd.to_datetime(df['recording_time'], format="%Y-%m-%d %H:%M:%S")


    # Remove all measurements with pm2.5 >150 ug/m3.
    df = df[~(df["pm2_5"]>150)]


    # Set all measurements with pm2.5 <1 ug/m3 to 1.0.
    df.loc[df["pm2_5"]<1, "pm2_5"] = 1.0


    # Remove all measurements with avg. speed >45 km/h (=12.5 m/s)
    geo_df = gpd.GeoDataFrame(df, geometry=gpd.points_from_xy(df['lon'],df['lat'], crs="EPSG:4326"))
    geo_df = geo_df.sort_values(by=['sensor', 'recording_time'])
    geo_df = geo_df.to_crs("EPSG:28992") # from WGS84 Geographic to Amersfoort / RD New Projected

    geo_df['distance'] = geo_df.distance(geo_df.shift(1)) # distance between two consecutive measurements
    geo_df['delta_time'] = geo_df['recording_time'] - geo_df['recording_time'].shift(1) # delta_time between two consecutive measurements
    geo_df['delta_time'] = geo_df['delta_time'].dt.seconds # convert to seconds
    geo_df['avg_speed_ms'] = geo_df['distance'] / geo_df['delta_time'] # avg(v)=x/t

    geo_df = geo_df[~(geo_df['avg_speed_ms']>12.5)]


    # Remove all measurements where the estimated speed between two measurements is below 5-7.5 km/h (~2.0 m/s).
    geo_df = geo_df[~(geo_df["avg_speed_ms"]<2.0)]


    # Remove all measurements outside of Utrecht City.
    geo_df = gpd.sjoin(geo_df, bbox, how="inner", op='within')


    # Remove redundant columns.
    geo_df.drop(labels=["geometry", 'index_right'], inplace=True, axis=1)    
    

    # Keep all rows between 01-01-2020 and 31-01-2020.
    geo_df = geo_df.loc[(geo_df['recording_time'] >= "2020-01-01") & (geo_df['recording_time'] < "2020-02-01")]


    # Add CSV to temporary storage.
    store.append(geo_df)


# Merge temporary storage.
df = pd.concat(store)
df.reset_index(drop=True, inplace=True)


# Save merged dataframe as .csv.
filename = "../../data/raw/city_jan_2020/full_city_jan_2020_bbox.csv"
df.to_csv(filename, index=False)
print("CSV saved")
