#
# EXPERIMENTO DE FEATURE ENGINEERING

## Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

## Cargo librerias 

require(data.table)
require(rpart)
require("dplyr")


# Poner la carpeta de la materia de SU computadora local
setwd("C:/YAGO/Maestría Ciencias de Datos/07-DMEYF")
# Poner sus semillas
semillas <- c(444457,444583,444697,444743,444817)

# Cargamos el dataset
dataset <- fread("./datasets/competencia1_2022.csv")

# Creamos una clase binaria
dataset[, clase_binaria := ifelse(
                            clase_ternaria == "BAJA+2",
                                "evento",
                                "noevento"
                            )]

dataset[,clase_ternaria:=NULL]

# Particionamos

in_training <- caret::createDataPartition(dataset$clase_binaria,
                     p = 0.70, list = FALSE)
dtrain  <-  dataset[in_training, ]
dtest   <-  dataset[-in_training, ]

# Función de ganancia normalizada al valor de partición
calcular_ganancia <- function(modelo, test) {
    pred_testing <- predict(modelo, test, type = "prob")
    sum(
        (pred_testing[, "evento"] >= 0.025) * ifelse(test$clase_binaria == "evento",
                                         78000, -2000) / 0.3
    )
}


## ---------------------------
## Step 2: Importancia de variables
## ---------------------------

# Antes de empezar vamos a ver la importancia de variables
modelo <- rpart(clase_binaria ~ .,
                data = dtrain,
                xval = 0,
                cp = -1,
                minsplit = 20,
                minbucket = 10,
                maxdepth = 10)

calcular_ganancia(modelo, dtest)

print(modelo$variable.importance)
variables <- names(modelo$variable.importance)[1:55]

#FEATURE ENGINEERING

##RANKEO DE VARIABLES PESIFICADAS

#Todas las variables que comienzan con m minúscula son montos en pesos

var_pesos <- variables[variables %like% "^m"] 


prefix <- "r_"
r_var_pesos <- c()
for (var in var_pesos) {
    dtrain[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
    dtest[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
    r_var_pesos <- c(r_var_pesos,paste(prefix, var, sep = ""))
}


#Selección de variables para entrenar
seleccion_variables <- variables[!variables  %in% var_pesos]
variables_modelo <- c(r_var_pesos,seleccion_variables)

campos <- paste(variables_modelo, collapse = " + ")
formula <- paste0( "clase_binaria ~ ", campos )

#Modelo
parametros <- param_basicos  <- list("cp"=-0.3,  #complejidad minima
                        "minsplit"=  80,  #minima cantidad de registros en un nodo para hacer el split
                        "minbucket"=  1,  #minima cantidad de registros en una hoja
                        "maxdepth"=  10 )  #máxima profundidad de un árbol
modelo <- rpart(formula,
                    data = dtrain,
                    xval = 0,
                    control=parametros)

    

gan <- calcular_ganancia(modelo,dtest)
nombres <- names(modelo$variable.importance)

experimento <- data.table(variables=nombres,ganancia=modelo$variable.importance)


#nombre_experimento <- paste0(unlist(parametros,recursive=FALSE),sep=",",colllapse="_")

#fwrite( dapply[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
#        file= "./exp/KA2002/KA2002_002.csv", 
#        sep= "," )