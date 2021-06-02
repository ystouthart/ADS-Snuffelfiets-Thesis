###
### select_period.py
### 
### Creates a subset of the full Snuffelfiets data for further analysis
###


import pandas as pd
import numpy as np
import geopandas as gpd


df = pd.read_csv("../../data/raw/full_sf_bbox.csv")
df['recording_time'] = pd.to_datetime(df['recording_time'], format="%Y-%m-%d %H:%M:%S")

df["week"] = df["recording_time"].dt.isocalendar().week
df["weekday"] = df["recording_time"].dt.dayofweek
df['hour'] = df['recording_time'].dt.hour

df = df.loc[((df["week"] == 2) | (df["week"] == 3))]
df = df.loc[((df["weekday"] >= 0) & (df["weekday"] <= 4))]
df = df.loc[~((df["hour"]>0) & (df["hour"]<6))]

df.to_csv("../../data/raw/city_jan_2020/data_selection_bbox.csv",index=False)
print("done")