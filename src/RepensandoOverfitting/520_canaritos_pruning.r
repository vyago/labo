#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")
require("rpart.plot")
require("dplyr")

setwd("." )  #establezco la carpeta donde voy a trabajar
#cargo el dataset
setwd("C:/Users/vyago/Desktop/Maestría Ciencias de Datos/07-DMEYF")
dataset  <- fread( "./datasets/competencia1_2022.csv")

#uso esta semilla para los canaritos
set.seed(102191)

#agrego 30 canaritos
for( i in 1:30 ) dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset)) ]

dtrain <- dataset[ foto_mes==202101 ]
dapply <- dataset[ foto_mes==202103 ]


#Aplico feature engineering

# Clases BAJAS+1 y BAJA+2 combinadas
dtrain[, clase_binaria2 := ifelse(
                            clase_ternaria == "CONTINUA",
                                "noevento",
                                "evento"
                            )]
dtrain[,clase_ternaria:=NULL]

var_pesos <- variables_modelo[variables_modelo %like% "^m"] 


prefix <- "r_"
r_var_pesos <- c()
for (var in var_pesos) {
    dtrain[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
    dapply[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
    r_var_pesos <- c(r_var_pesos,paste(prefix, var, sep = ""))
}

dtrain <- dtrain[,!..var_pesos]


#Primero  veo como quedan mis arboles
modelo_original <- rpart(
    formula= "clase_binaria2 ~ . -numero_de_cliente -ctrx_quarter",
    data= dtrain,
    model= TRUE,
    xval= 0,
    cp= -1,
    minsplit= 2, # dejo que crezca y corte todo lo que quiera
    minbucket= 1,
    maxdepth= 30 )

#hago el pruning de los canaritos
#haciendo un hackeo a la estructura  modelo_original$frame
# -666 es un valor arbritrariamente negativo que jamas es generado por rpart
modelo_original$frame[ modelo_original$frame$var %like% "canarito", "complexity"] <- -666
modelo_pruned  <- prune(  modelo_original, -666 )

prediccion  <- predict( modelo_pruned, dapply, type = "prob")[,"evento"]

entrega  <-  as.data.table( list( "numero_de_cliente"= dapply$numero_de_cliente,
                                  "Predicted"= as.integer(  prediccion > 0.050 ) ) )

fwrite( entrega, paste0( "./exp/stopping_at_canaritos.csv"), sep="," )
#pdf(file = "./work/stopping_at_canaritos.pdf", width=28, height=4)
#prp(modelo_pruned, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
#dev.off()
 View(modelo_original$variable.importance)

variables_modelo <- names(modelo_original$variable.importance)

