"""
Author: wolfno
Creation Date: 19 Feb 2025
Title: "Aggregating UK Car Sales Data"

Multiple CSV files are loaded and aggregated into one single
pandas DataFrame. 

Data is downloaded from Kaggle:
https://www.kaggle.com/datasets/adityadesai13/used-car-dataset-ford-and-mercedes
"""

import os
import pandas as pd
from pathlib import Path

"""
Data Loading:
The following code assumes the CSV files are stored in a separate folder
named "Data" that is stored in the current working directory
"""

car_brands = ["audi", "bmw", "cclass", "focus", "ford", "hyundi", "merc",
              "skoda", "toyota", "vauxhall", "vw"]
df_cars = pd.DataFrame()
current_wdir = os.getcwd()
for brand in car_brands:
    path_to_file = current_wdir + "/Data/" + brand + ".csv"
    current_car_dataset = pd.read_csv(path_to_file)
    current_car_dataset["brand"] = brand
    df_cars = pd.concat([df_cars, current_car_dataset], axis=0)
    current_car_dataset = pd.DataFrame()

#Reordering columns so that brand is first
column_order = list(df_cars.columns)
column_order.insert(0, "brand")
column_order.pop()
df_cars = df_cars[column_order]

#Exporting one grand file in the working directory
df_cars.to_csv("all_cars.csv", index=False)
export_path = Path(os.getcwd() + "/all_cars.csv")
if export_path.exists():
    print("File was exported successfully.")

