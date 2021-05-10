# Import packages
import pandas as pd 
import numpy as np 
import geopandas as gpd

from tqdm import tqdm
import requests
import io
import json
import re

# Open Utrecht City polygon.
utrecht = gpd.read_file("../../data/external/WijkBuurtkaart_2020_v1/gem_utrecht.shp")
utrecht = utrecht.to_crs("EPSG:28992")


# Retrieving all .csv-file locations.
# url for RIVM corrected data: https://ckan.dataplatform.nl/api/3/action/package_show?id=3660d2e1-84ee-46bf-a7b6-7e9ac1fcaf3a&include_tracking=true
url = 'https://ckan.dataplatform.nl/api/3/action/package_show?id=9cc4de28-6d03-4b59-8c66-085b3e8b3956&include_tracking=true'
r = requests.get(url)
data = r.json()

csv_urls = [x['url'] for x in data['result']['resources']]
jan_2020 = [url for url in csv_urls if re.match(".+2020_01.+", url)]
#test_csv_urls = csv_urls[:3]


# Download each .csv-file, pre-process, and save.
for csv in tqdm(jan_2020): 
    response = requests.get(csv)
    file_object = io.StringIO(response.content.decode('utf-8'))
    df = pd.read_csv(file_object, usecols=['sensor', 'air_quality_observed_id', 'lon', 'lat',
     'recording_time', 'trip_sequence', 'humidity', 'pm2_5', 'pressure', 'temperature'])
    df['recording_time'] = pd.to_datetime(df['recording_time'], format="%Y-%m-%d %H:%M:%S")


    # Remove all measurements with pm2.5 >150 ug/m3.
    df = df[~(df["pm2_5"]>150)]


    # Set all measurements with pm2.5 <1 ug/m3 to 1.
    df.loc[df["pm2_5"]<1, "pm2_5"] = 1


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


    # Remove all measurements outside of Utrecht Province.
    geo_df = gpd.sjoin(geo_df, utrecht, how="inner", op='within')


    # Save as .csv-file
    geo_df.drop(labels=["geometry",'GM_CODE', 'JRSTATCODE', 'GM_NAAM', 'H2O', 'OAD', 'STED', 'BEV_DICHTH',
       'AANT_INW', 'AANT_MAN', 'AANT_VROUW', 'P_00_14_JR', 'P_15_24_JR',
       'P_25_44_JR', 'P_45_64_JR', 'P_65_EO_JR', 'P_ONGEHUWD', 'P_GEHUWD',
       'P_GESCHEID', 'P_VERWEDUW', 'AANTAL_HH', 'P_EENP_HH', 'P_HH_Z_K',
       'P_HH_M_K', 'GEM_HH_GR', 'P_WEST_AL', 'P_N_W_AL', 'P_MAROKKO',
       'P_ANT_ARU', 'P_SURINAM', 'P_TURKIJE', 'P_OVER_NW', 'OPP_TOT',
       'OPP_LAND', 'OPP_WATER', 'Shape_Leng', 'Shape_Area', 'index_right'], inplace=True, axis=1)    
    geo_df.reset_index(drop=True, inplace=True)
    filename = "../../data/external/city_jan_2020/" + csv.split('/')[-1]
    geo_df.to_csv(filename, index=False)
