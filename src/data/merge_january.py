# Import packages
import pandas as pd 
from tqdm import tqdm
import glob


path = "../../data/external/city_jan_2020/" # use your path
all_files = glob.glob(path + "/*.csv")

data = []

for filename in tqdm(all_files):
    df = pd.read_csv(filename, index_col=None, header=0)
    data.append(df)

frame = pd.concat(data, axis=0, ignore_index=True)

frame.reset_index(drop=True, inplace=True)

frame = frame.loc[(frame['recording_time'] >= "2020-01-01") & (frame['recording_time'] <= "2020-01-31")]

filename = "../../data/external/city_jan_2020/january_2020_city.csv"
frame.to_csv(filename, index=False)