## Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

## Cargo librerias 

require(data.table)
require(rpart)
require("dplyr")
require("rpart.plot")


# Poner la carpeta de la materia de SU computadora local
setwd("C:/Users/vyago/Desktop/Maestría Ciencias de Datos/07-DMEYF")
# Poner sus semillas
semillas <- c(444457,444583,444697,444743,444817)

# Cargamos el dataset
dataset <- fread("./datasets/competencia1_2022.csv")



train <- dataset[foto_mes == 202101]
dapply <- dataset[foto_mes == 202103]

# Clases BAJAS+1 y BAJA+2 combinadas
train[, clase_binaria := ifelse(
                            clase_ternaria == "CONTINUA",
                                "noevento",
                                "evento"
                            )]

train[,clase_ternaria:=NULL]

variables_iniciales <- names(train)
var_pesos <- variables_iniciales[variables_iniciales %like% "^m"] 
seleccion_variables <- variables_iniciales[!variables_iniciales  %in% var_pesos]

prefix <- "r_"
r_var_pesos <- c()
for (var in var_pesos) {
    train[, (paste(prefix, var, sep = "")) := ntile(get(var), 20)]
    
    r_var_pesos <- c(r_var_pesos,paste(prefix, var, sep = ""))
}

variables_modelo <- c(r_var_pesos,seleccion_variables)
variables_modelo <- variables_modelo[which(variables_modelo !="ctrx_quarter")]
campos <- paste(variables_modelo, collapse = " + ")
formula <- paste0( "clase_binaria ~", campos )



modelo <- rpart(formula,
        data=train,
        xval= 0,
        cp= -0.54,
        #minsplit=  1073,   
        minbucket=  1000,   
        maxdepth=     3 )

importancia<-modelo$variable.importance
pdf(file = "./exp/FE1001/FE_LEAKLEARNER_3.pdf", width=28, height=4)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()


##Features con ranking
train[,campo1:=as.integer(rmcaja_ahorro<3 & r_mtarjeta_visa_consumo<5 & r_mprestamos_personales <17)]
train[,campo2:=as.integer(rmcaja_ahorro<3 & r_mtarjeta_visa_consumo<5 & r_mprestamos_personales >=17)]
train[,campo3:=as.integer(rmcaja_ahorro<3 & r_mtarjeta_visa_consumo>=5 & Visa_msaldototal <7919.8)]
train[,campo4:=as.integer(rmcaja_ahorro<3 & r_mtarjeta_visa_consumo>=5 & Visa_msaldototal >=7919.8)]
train[,campo5:=as.integer(rmcaja_ahorro>=3 & r_mtarjeta_visa_consumo>=5 & cpayroll_trx <1)]
train[,campo6:=as.integer(rmcaja_ahorro>=3 & r_mtarjeta_visa_consumo>=5 & cpayroll_trx >=1)]
train[,campo7:=as.integer(rmcaja_ahorro>=3 & r_mtarjeta_visa_consumo>=5 & r_mpasivos_margen <6)]
train[,campo8:=as.integer(rmcaja_ahorro>=3 & r_mtarjeta_visa_consumo>=5 & r_mpasivos_margen >=6)]


##Features sin ranking
#train[,campo1:=as.integer(mcaja_ahorro<259.94 & mtarjeta_visa_consumo<857.2 & mprestamos_personales <14858)]
#train[,campo2:=as.integer(mcaja_ahorro<259.94 & mtarjeta_visa_consumo<857.2 & mprestamos_personales >=14858)]
#train[,campo3:=as.integer(mcaja_ahorro<259.94 & mtarjeta_visa_consumo >= 857.2 & Visa_msaldototal <7919.8)]
#train[,campo4:=as.integer(mcaja_ahorro<259.94 & mtarjeta_visa_consumo >= 857.2 & Visa_msaldototal >=7919.8)]
#train[,campo5:=as.integer(mcaja_ahorro>= 259.94 & mtarjeta_visa_consumo<2003.7 & cpayroll_trx<1)]
#train[,campo6:=as.integer(mcaja_ahorro>= 259.94 & mtarjeta_visa_consumo<2003.7 & cpayroll_trx>=1)]
#train[,campo7:=as.integer(mcaja_ahorro>= 259.94 & mtarjeta_visa_consumo>=2003.7 & mpasivos_margen<231.5)]
#train[,campo8:=as.integer(mcaja_ahorro>= 259.94 & mtarjeta_visa_consumo>=2003.7 & mpasivos_margen>=231.5)]
#train[,campo9:=as.integer(active_quarter=1 & mcaja_ahorro<1325.9 & mtarjeta_visa_consumo<875.62 & mprestamos_personales <14199)]




