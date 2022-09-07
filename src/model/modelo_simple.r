#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")
require("dplyr")

#cargo el dataset
setwd("C:/Users/vyago/Desktop/Maestría Ciencias de Datos/07-DMEYF")
dataset  <- fread( "./datasets/competencia1_2022.csv")

#Semillas
semillas <- c(444457,444583,444697,444743,444817)

#split 
dtrain <- dataset[ foto_mes==202101 ]
dapply <- dataset[ foto_mes==202103 ]

# Clases BAJAS+1 y BAJA+2 combinadas
dtrain<-dtrain[, clase_binaria2 := ifelse(
                            clase_ternaria == "CONTINUA",
                                "noevento",
                                "evento"
                            )]
dtrain<-dtrain[,clase_ternaria:=NULL]

#Calculo de ganancia

# Función de ganancia normalizada al valor de partición
calcular_ganancia <- function(modelo, test) {
    pred_testing <- predict(modelo, test, type = "prob")
    sum(
        (pred_testing[, "evento"] >= 0.06) * ifelse(test$clase_binaria == "evento",
                                         78000, -2000) / 0.3
    )
}


#Modelo para analizar importancia de variables
modelo <- rpart(
    formula= "clase_binaria2 ~ .",
    data= dtrain,
    model= TRUE,
    xval= 0,
    cp= -1,
    minsplit= 3, # dejo que crezca y corte todo lo que quiera
    minbucket= 3,
    maxdepth= 13 )


var_impo <- modelo$variable.importance

# variables nulas
#dtrain[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:2]

var_impo <- names(var_impo[var_impo>1.5])

relacion <- var_impo[1:10]


#Evaluación del modelo
ganancia <- c()
for (s in semillas){
    in_training <- caret::createDataPartition(dtrain$clase_binaria2, p = 0.70,
        list = FALSE)
    train  <-  dtrain[in_training, ]
    test   <-  dtrain[-in_training, ]

        nuevas <- c()
    for (var1 in relacion) {
        for (var2 in relacion) {
            if (var1 != var2) {
                nueva <- paste(var1, var2, sep = "___")
                train[, (nueva) := get(var1) / get(var2)]
                test[, (nueva) := get(var1) / get(var2)]
            nuevas <- c(nuevas, nueva)
        }
    }
}
variables_modelo <- c(var_impo,nuevas)

campos <- paste(variables_modelo, collapse = " + ")
formula <- paste0( "clase_binaria2 ~", campos )


modelo <- rpart(
    formula,
    data= dtrain,
    model= TRUE,
    xval= 0,
    cp= -1,
    minsplit= 100, # dejo que crezca y corte todo lo que quiera
    minbucket= 3,
    maxdepth= 13 )

gan <- calcular_ganancia(modelo,test)
ganancia <- c(ganancia,gan)

}

ganancia_prom <- mean(ganancia)

var_impo2 <- modelo$variable.importance