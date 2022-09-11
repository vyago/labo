## Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

## Cargo librerias 

require(data.table)
require(rpart)
require("dplyr")


# Poner la carpeta de la materia de SU computadora local
setwd("C:/Users/vyago/Desktop/Maestría Ciencias de Datos/07-DMEYF")
# Poner sus semillas
semillas <- c(444457,444583,444697,444743,444817)

# Cargamos el dataset
dataset <- fread("./datasets/competencia1_2022.csv")




train <- dataset[foto_mes == 202101]
dapply <- dataset[foto_mes == 202103]

## Clases BAJAS+1 y BAJA+2 combinadas
train[, clase_binaria := ifelse(
                            clase_ternaria == "CONTINUA",
                                "noevento",
                                "evento"
                            )]
train[,clase_ternaria:=NULL]





#Calculo de la ganancia
calcular_ganancia <- function(modelo, test) {
    pred_testing <- predict(modelo, test, type = "prob")
    return(sum(
        (pred_testing[, "evento"] >= 1/20) * ifelse(test$clase_binaria == "evento",
                                         78000, -2000) / 0.3
    ))
}

#Modelo base

modelo_arbol <- function(ds,formula,semilla) {
    set.seed(semilla)
    in_training <- caret::createDataPartition(ds$clase_binaria,
                     p = 0.70, list = FALSE)
    dtrain  <-  ds[in_training, ]
    dtest   <-  ds[-in_training, ]

    modelo <- rpart("clase_binaria ~.",
        data=dtrain,
        xval= 0,
        cp= -0.1297,
        minsplit=  171,   
        minbucket=  20,   
        maxdepth=     5 )

    gan <- calcular_ganancia(modelo, dtest)
    return (gan)
}

ganancia <- c()

for (s in semillas){

    gan_mod <-  modelo_arbol(train,formula,s)
    ganancia <- c(ganancia,gan_mod)
}
ganancia_final <- mean(ganancia)

