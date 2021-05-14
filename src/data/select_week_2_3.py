###
### Keep data of week 2 and 3 (2020-01-06 - 2020-01-20) during weekdays and daytime (7:00 - 00:00)
### TODO: Merge with retrieval_prep_city_january.py
###

import pandas as pd
import numpy as np
import geopandas as gpd


df = pd.read_csv("../../data/raw/city_jan_2020/city_jan_2020.csv")
df['recording_time'] = pd.to_datetime(df['recording_time'], format="%Y-%m-%d %H:%M:%S")

df["week"] = df["recording_time"].dt.isocalendar().week
df["weekday"] = df["recording_time"].dt.dayofweek
df['hour'] = df['recording_time'].dt.hour

df = df.loc[((df["week"] == 2) | (df["week"] == 3))]
df = df.loc[((df["weekday"] >= 1) & (df["weekday"] <= 5))]
df = df.loc[~((df["hour"]>0) & (df["hour"]<6))]

df.to_csv("../../data/raw/city_jan_2020/week2_3_weekdays_dt.csv",index=False)
print("done")