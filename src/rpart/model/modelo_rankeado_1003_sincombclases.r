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
                            clase_ternaria == "BAJA+2",
                                "evento",
                                "noevento"
                            )]
train[,clase_ternaria:=NULL]


##Rankeo de variables en pesos

variables_iniciales <- names(train)
var_pesos <- variables_iniciales[variables_iniciales %like% "^m"] 
seleccion_variables <- variables_iniciales[!variables_iniciales  %in% var_pesos]

prefix <- "r_"
r_var_pesos <- c()
for (var in var_pesos) {
    train[, (paste(prefix, var, sep = "")) := ntile(get(var), 20)]
    dapply[, (paste(prefix, var, sep = "")) := ntile(get(var), 20)]
    r_var_pesos <- c(r_var_pesos,paste(prefix, var, sep = ""))
}

##Features con ranking
train[,campo1:=as.integer(r_mcaja_ahorro<3 & r_mtarjeta_visa_consumo<5 & r_mprestamos_personales <17)]
train[,campo2:=as.integer(r_mcaja_ahorro<3 & r_mtarjeta_visa_consumo<5 & r_mprestamos_personales >=17)]
train[,campo3:=as.integer(r_mcaja_ahorro<3 & r_mtarjeta_visa_consumo>=5 & Visa_msaldototal <7919.8)]
train[,campo4:=as.integer(r_mcaja_ahorro<3 & r_mtarjeta_visa_consumo>=5 & Visa_msaldototal >=7919.8)]
train[,campo5:=as.integer(r_mcaja_ahorro>=3 & r_mtarjeta_visa_consumo>=5 & cpayroll_trx <1)]
train[,campo6:=as.integer(r_mcaja_ahorro>=3 & r_mtarjeta_visa_consumo>=5 & cpayroll_trx >=1)]
train[,campo7:=as.integer(r_mcaja_ahorro>=3 & r_mtarjeta_visa_consumo>=5 & r_mpasivos_margen <6)]
train[,campo8:=as.integer(r_mcaja_ahorro>=3 & r_mtarjeta_visa_consumo>=5 & r_mpasivos_margen >=6)]

dapply[,campo1:=as.integer(r_mcaja_ahorro<3 & r_mtarjeta_visa_consumo<5 & r_mprestamos_personales <17)]
dapply[,campo2:=as.integer(r_mcaja_ahorro<3 & r_mtarjeta_visa_consumo<5 & r_mprestamos_personales >=17)]
dapply[,campo3:=as.integer(r_mcaja_ahorro<3 & r_mtarjeta_visa_consumo>=5 & Visa_msaldototal <7919.8)]
dapply[,campo4:=as.integer(r_mcaja_ahorro<3 & r_mtarjeta_visa_consumo>=5 & Visa_msaldototal >=7919.8)]
dapply[,campo5:=as.integer(r_mcaja_ahorro>=3 & r_mtarjeta_visa_consumo>=5 & cpayroll_trx <1)]
dapply[,campo6:=as.integer(r_mcaja_ahorro>=3 & r_mtarjeta_visa_consumo>=5 & cpayroll_trx >=1)]
dapply[,campo7:=as.integer(r_mcaja_ahorro>=3 & r_mtarjeta_visa_consumo>=5 & r_mpasivos_margen <6)]
dapply[,campo8:=as.integer(r_mcaja_ahorro>=3 & r_mtarjeta_visa_consumo>=5 & r_mpasivos_margen >=6)]

var_nuevas <- c("campo1","campo2","campo3","campo4","campo5","campo6","campo7","campo8")
variables_modelo <- c(r_var_pesos,seleccion_variables,var_nuevas)
campos <- paste(variables_modelo, collapse = " + ")
formula <- paste0( "clase_binaria ~", campos )




# corrijo manualmente el drifting de  Visa_fultimo_cierre
 dapply[ Visa_fultimo_cierre== 1, Visa_fultimo_cierre :=  4 ]
 dapply[ Visa_fultimo_cierre== 7, Visa_fultimo_cierre := 11 ]
 dapply[ Visa_fultimo_cierre==21, Visa_fultimo_cierre := 25 ]
 dapply[ Visa_fultimo_cierre==14, Visa_fultimo_cierre := 18 ]
 dapply[ Visa_fultimo_cierre==28, Visa_fultimo_cierre := 32 ]
 dapply[ Visa_fultimo_cierre==35, Visa_fultimo_cierre := 39 ]
 dapply[ Visa_fultimo_cierre> 39, Visa_fultimo_cierre := Visa_fultimo_cierre + 4 ]

# corrijo manualmente el drifting de  Visa_fultimo_cierre
 dapply[ Master_fultimo_cierre== 1, Master_fultimo_cierre :=  4 ]
 dapply[ Master_fultimo_cierre== 7, Master_fultimo_cierre := 11 ]
 dapply[ Master_fultimo_cierre==21, Master_fultimo_cierre := 25 ]
 dapply[ Master_fultimo_cierre==14, Master_fultimo_cierre := 18 ]
 dapply[ Master_fultimo_cierre==28, Master_fultimo_cierre := 32 ]
 dapply[ Master_fultimo_cierre==35, Master_fultimo_cierre := 39 ]
 dapply[ Master_fultimo_cierre> 39, Master_fultimo_cierre := Master_fultimo_cierre + 4 ]


## Variables con más del 60% de valores nulos

nulos <- c("Master_Finiciomora", "Master_mconsumospesos", "Master_mconsumosdolares", 
"Master_madelantopesos", "Master_madelantodolares", "Master_mpagospesos", 
"Master_mpagosdolares", "Master_mconsumototal", "Master_cconsumos", 
"Master_cadelantosefectivo", "Visa_Finiciomora")









#Calculo de la ganancia
calcular_ganancia <- function(modelo, test) {
    pred_testing <- predict(modelo, test, type = "prob")
    return(sum(
        (pred_testing[, "evento"] >= 1/40) * ifelse(test$clase_binaria == "evento",
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

    modelo <- rpart(formula,
        data=train,
        xval= 0,
        cp= -0.56,
        minsplit=  580,   
        minbucket=  161,   
        maxdepth=     16 )

    gan <- calcular_ganancia(modelo, dtest)
    return (gan)
}

ganancia <- c()
for (s in semillas){

    gan_mod <-  modelo_arbol(train,formula,s)
    ganancia <- c(ganancia,gan_mod)
}
ganancia_final <- mean(ganancia)

modelo <- rpart(formula,
        data=train,
        xval= 0,
        cp= -0.56,
        minsplit=  580,   
        minbucket=  161,   
        maxdepth=     16 )

#aplico el modelo a los datos nuevos
prediccion  <- predict( object= modelo,
                        newdata= dapply,
                        type = "prob")

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[ , prob_baja2 := prediccion[, "evento"] ]

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/40
dapply[ , Predicted := as.numeric( prob_baja2 > 1/40) ]

View(dapply[,.N,by=Predicted])
#genero el archivo para Kaggle
#primero creo la carpeta donde va el experimento
#dir.create( "./exp/" )
dir.create( "./exp/KA1003" )

fwrite( dapply[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA1003/K1002_001.csv",
        sep=  "," )

a<-modelo$variable.importance