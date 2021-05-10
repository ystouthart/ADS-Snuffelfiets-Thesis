### Create Virtual Measurement Stations on District-level.

import pandas as pd
import numpy as np
import geopandas as gpd
from tqdm import tqdm
import glob
import os


districts = gpd.read_file("../../data/external/WijkBuurtkaart_2020_v1/wijk_2020_v1.shp")
districts.to_crs("EPSG:28992", inplace=True)
utrecht_dist = districts[['WK_NAAM','GM_NAAM','BEV_DICHTH','AANT_INW', 'AANT_MAN', 'AANT_VROUW','AANTAL_HH',
'OPP_TOT', 'OPP_LAND', 'OPP_WATER','geometry']][districts["GM_NAAM"]=="Utrecht"]

# Add a Virtual Measurement Station in every district.
utrecht_dist["virtual_measurement_station"] = utrecht_dist.representative_point()
 

def VMS_districts_hourly(geo_df):
    geo_df = gpd.sjoin(geo_df, utrecht_dist, how="inner", op='within')

    geo_df['recording_time'] = pd.to_datetime(df['recording_time'], format="%Y-%m-%d %H:%M:%S")
    geo_df['date'] = geo_df['recording_time'].dt.date
    geo_df['hour'] = geo_df['recording_time'].dt.hour

    median_pm25 = geo_df[["WK_NAAM", "date", "hour", "pm2_5"]].groupby(["WK_NAAM", "date", "hour"]).median().reset_index()
    median_pm25["datetime"] = pd.to_datetime(median_pm25["date"]) + median_pm25["hour"].astype('timedelta64[h]')

    export = median_pm25[["WK_NAAM", "datetime", "pm2_5"]].merge(utrecht_dist[["WK_NAAM", "virtual_measurement_station"]], on="WK_NAAM")
    export['x'] = export.virtual_measurement_station.apply(lambda p: p.x)
    export['y'] = export.virtual_measurement_station.apply(lambda p: p.y)

    return export


def VMS_districts_daily(geo_df):
    geo_df = gpd.sjoin(geo_df, utrecht_dist, how="inner", op='within')

    geo_df['recording_time'] = pd.to_datetime(df['recording_time'], format="%Y-%m-%d %H:%M:%S")
    geo_df['date'] = geo_df['recording_time'].dt.date
    geo_df['hour'] = geo_df['recording_time'].dt.hour

    median_pm25 = geo_df[["WK_NAAM", "date", "hour", "pm2_5"]].groupby(["WK_NAAM", "date"]).median().reset_index()
    median_pm25["datetime"] = pd.to_datetime(median_pm25["date"]) + median_pm25["hour"].astype('timedelta64[h]')

    export = median_pm25[["WK_NAAM", "datetime", "pm2_5"]].merge(utrecht_dist[["WK_NAAM", "virtual_measurement_station"]], on="WK_NAAM")
    export['x'] = export.virtual_measurement_station.apply(lambda p: p.x)
    export['y'] = export.virtual_measurement_station.apply(lambda p: p.y)

    return export


os.chdir("C:/Users/Klant/Documents/GitHub/ADS-Snuffelfiets-Thesis/data/external/city")
csv_s = glob.glob('*.{}'.format("csv"))


for csv in tqdm(csv_s):
    file_dir1 = "../../interim/district/hourly_" + csv 
    file_dir2 = "../../interim/district/daily_" + csv

    df = pd.read_csv(csv)

    if len(df) > 1:
        geo_df = gpd.GeoDataFrame(df, geometry=gpd.points_from_xy(df['lon'],df['lat'], crs="EPSG:4326"))
        geo_df = geo_df.to_crs("EPSG:28992")

        VMS_districts_hourly(geo_df).to_csv(file_dir1,index=False)
        VMS_districts_daily(geo_df).to_csv(file_dir2,index=False)