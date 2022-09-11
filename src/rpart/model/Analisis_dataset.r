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

dataset[,campo1:=as.integer(mcaja_ahorro<259.94 & mtarjeta_visa_consumo<857.2 & mprestamos_personales <14858)]
dataset[,campo2:=as.integer(mcaja_ahorro<259.94 & mtarjeta_visa_consumo<857.2 & mprestamos_personales >=14858)]
dataset[,campo3:=as.integer(mcaja_ahorro<259.94 & mtarjeta_visa_consumo >= 857.2 & Visa_msaldototal <7919.8)]
dataset[,campo4:=as.integer(mcaja_ahorro<259.94 & mtarjeta_visa_consumo >= 857.2 & Visa_msaldototal >=7919.8)]
dataset[,campo5:=as.integer(mcaja_ahorro>= 259.94 & mtarjeta_visa_consumo<2003.7 & cpayroll_trx<1)]
dataset[,campo6:=as.integer(mcaja_ahorro>= 259.94 & mtarjeta_visa_consumo<2003.7 & cpayroll_trx>=1)]
dataset[,campo7:=as.integer(mcaja_ahorro>= 259.94 & mtarjeta_visa_consumo>=2003.7 & mpasivos_margen<231.5)]
dataset[,campo8:=as.integer(mcaja_ahorro>= 259.94 & mtarjeta_visa_consumo>=2003.7 & mpasivos_margen>=231.5)]



train <- dataset[foto_mes == 202101]
dapply <- dataset[foto_mes == 202103]

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

# Clases BAJAS+1 y BAJA+2 combinadas
train[, clase_binaria := ifelse(
                            clase_ternaria == "CONTINUA",
                                "noevento",
                                "evento"
                            )]
train[,.N,by=clase_binaria]

resumen <- summary(train)

#Análisis de valores nulos
train_sc <- train[,-c("clase_ternaria","clase_binaria")]
na<- apply(X = is.na(train_sc), MARGIN = 2, FUN = mean)

na_orden<-sort(na)

na_test<- apply(X = is.na(dapply), MARGIN = 2, FUN = mean)

na_test_orden<-sort(na)

#na_test_names <- names(na_test[na_test>0.59])