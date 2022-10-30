import pandas as pd
import tensorflow as tf
from tensorflow.keras import layers, losses
from tensorflow.keras.models import Model
import os 
import matplotlib.pyplot as plt
import numpy as np
from sklearn.preprocessing import MinMaxScaler

from os import system
system("cls")


os.chdir( "C:\\Users\\vyago\\Desktop\\Maestría Ciencias de Datos\\07-DMEYF" )

dataset = pd.read_csv("./datasets/competencia2_2022.csv.gz", low_memory=False)




#--------------------------------------------------------------
#Divido en entrenamiento y test
train = dataset.loc[dataset["foto_mes"].isin([202102,202103])]
#train = dataset.loc[dataset["foto_mes"] in [202102,202101,202007,202008,202103, 202104]]
#test = dataset.loc[dataset["foto_mes"]==202105]
test = dataset[dataset["foto_mes"]==202103]

# ELIMINO VALORES NULOS
train=train.fillna(train.mean()) # RELLENO CON LA MEDIA DE CADA COLUMNA LOS VALORES NULOS, LO NECESITA LA RED
test = test.fillna(test.mean())

drop = ["foto_mes","numero_de_cliente","clase_ternaria"] #COLUMNAS A ELIMINAR

train = train[train.columns.drop( drop)] 
test = test[test.columns.drop(drop)]

#ESCALO VARIABLES POR MÍNIMO Y MÁXIMO

scaler = MinMaxScaler()
scaler = scaler.fit(train) # OBTENGO VARIABLES DE ESCALADO EN TRAIN

train = scaler.transform(train) # LAS APLICO EN TRAIN
test = scaler.transform(test)  # LAS APLICO EN TEST
variables = train.shape[1]  # OBTENGO VARIABLES TOTALES 

train = tf.cast(train, tf.float32)
test = tf.cast(test, tf.float32)


# GENERO AUTOENCODER
class Autoencoder(Model):
  def __init__(self,variables):
    super(Autoencoder, self).__init__()
    self.encoder = tf.keras.Sequential([
      layers.Flatten(),
      layers.Dense(variables,activation="relu"),
      layers.Dense(int(variables/2), activation="relu"),
      layers.Dense(int(variables/4), activation="relu"),
      layers.Dense(100, activation="relu")])

    self.decoder = tf.keras.Sequential([
      layers.Dense(int(variables/4), activation="relu"),
      layers.Dense(int(variables/3), activation="relu"),
      layers.Dense(int(variables/2),activation="relu"),
      layers.Dense(variables, activation="sigmoid")])

  def call(self, x):
    encoded = self.encoder(x)
    decoded = self.decoder(encoded)
    return decoded

  def reductor(self,x):
      encoded = self.encoder(x)
      return encoded

autoencoder=Autoencoder(variables)

autoencoder.compile(optimizer='adam', loss=losses.MeanSquaredError())

history = autoencoder.fit(x=train, y=train, 
            epochs=1, 
            batch_size=1048,
            validation_data=(test, test),
            shuffle=True)


features_nuevas = autoencoder.reductor(dataset)

df = pd.concat([dataset,features_nuevas],axis=1)

df.to_csv("./datasets/test_autoencoder_2022.csv")







