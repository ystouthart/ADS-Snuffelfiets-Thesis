### Create Virtual Measurement Stations on Neighborhood-level.

import pandas as pd
import numpy as np
import geopandas as gpd
from tqdm import tqdm
import glob
import os


neighborhoods = gpd.read_file("../../data/external/WijkBuurtkaart_2020_v1/buurt_2020_v1.shp")
neighborhoods.to_crs("EPSG:28992", inplace=True)
utrecht_neigh = neighborhoods[['BU_NAAM','GM_NAAM','BEV_DICHTH','AANT_INW', 'AANT_MAN', 'AANT_VROUW','AANTAL_HH','OPP_TOT', 
'OPP_LAND', 'OPP_WATER','geometry']][neighborhoods["GM_NAAM"]=="Utrecht"]

# Add a Virtual Measurement Station in every neighborhood.
utrecht_neigh["virtual_measurement_station"] = utrecht_neigh.representative_point()


def VMS_neighborhood_hourly(geo_df):
    geo_df = gpd.sjoin(geo_df, utrecht_neigh, how="inner", op='within')

    geo_df['recording_time'] = pd.to_datetime(df['recording_time'], format="%Y-%m-%d %H:%M:%S")
    geo_df['date'] = geo_df['recording_time'].dt.date
    geo_df['hour'] = geo_df['recording_time'].dt.hour

    median_pm25 = geo_df[["BU_NAAM", "date", "hour", "pm2_5"]].groupby(["BU_NAAM", "date", "hour"]).median().reset_index()
    median_pm25["datetime"] = pd.to_datetime(median_pm25["date"]) + median_pm25["hour"].astype('timedelta64[h]')

    export = median_pm25[["BU_NAAM", "datetime", "pm2_5"]].merge(utrecht_neigh[["BU_NAAM", "virtual_measurement_station"]], on="BU_NAAM")
    export['x'] = export.virtual_measurement_station.apply(lambda p: p.x)
    export['y'] = export.virtual_measurement_station.apply(lambda p: p.y)

    return export


def VMS_neighborhood_daily(geo_df):
    geo_df = gpd.sjoin(geo_df, utrecht_neigh, how="inner", op='within')

    geo_df['recording_time'] = pd.to_datetime(df['recording_time'], format="%Y-%m-%d %H:%M:%S")
    geo_df['date'] = geo_df['recording_time'].dt.date
    geo_df['hour'] = geo_df['recording_time'].dt.hour

    median_pm25 = geo_df[["BU_NAAM", "date", "hour", "pm2_5"]].groupby(["BU_NAAM", "date"]).median().reset_index()
    median_pm25["datetime"] = pd.to_datetime(median_pm25["date"]) + median_pm25["hour"].astype('timedelta64[h]')

    export = median_pm25[["BU_NAAM", "datetime", "pm2_5"]].merge(utrecht_neigh[["BU_NAAM", "virtual_measurement_station"]], on="BU_NAAM")
    export['x'] = export.virtual_measurement_station.apply(lambda p: p.x)
    export['y'] = export.virtual_measurement_station.apply(lambda p: p.y)

    return export


os.chdir("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/external/city")
csvs = glob.glob('*.{}'.format("csv"))


for csv in tqdm(csvs):
    file_dir1 = "../../interim/neighborhood/hourly_" + csv 
    file_dir2 = "../../interim/neighborhood/daily_" + csv

    df = pd.read_csv(csv)

    if len(df) > 1:
        geo_df = gpd.GeoDataFrame(df, geometry=gpd.points_from_xy(df['lon'],df['lat'], crs="EPSG:4326"))
        geo_df = geo_df.to_crs("EPSG:28992")

        VMS_neighborhood_hourly(geo_df).to_csv(file_dir1,index=False)
        VMS_neighborhood_daily(geo_df).to_csv(file_dir2,index=False)