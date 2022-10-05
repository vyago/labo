import lightgbm as lgb
import shap
import os
import pandas as pd

os.chdir( "C:\\Users\\vyago\\Desktop\\Maestría Ciencias de Datos\\07-DMEYF" )

dataset = pd.read_csv("./datasets/competencia2_2022.csv.gz")

marzo = dataset.loc[dataset["foto_mes"]==202103]

marzo = dataset.loc[dataset["foto_mes"]==202105]

