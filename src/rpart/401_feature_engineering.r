#
# EXPERIMENTO DE FEATURE ENGINEERING

## Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

## Cargo librerias 

require(data.table)
require(rpart)
require("dplyr")


require("data.table")
require("rlist")

require("rpart")
require("parallel")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")




# Poner la carpeta de la materia de SU computadora local
setwd("C:/Users/vyago/Desktop/Maestría Ciencias de Datos/07-DMEYF")
# Poner sus semillas
semillas <- c(444457,444583,444697,444743,444817)

# Cargamos el dataset
dataset <- fread("./datasets/competencia1_2022.csv")

# Creamos una clase binaria
dataset[, clase_binaria:= ifelse(
                            clase_ternaria == "BAJA+2",
                                "evento",
                                "noevento"
                            )]

dataset[,clase_ternaria:=NULL]

# Particionamos

set.seed(semillas[1])

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
variables_iniciales <- names(modelo$variable.importance)[1:55]


## ---------------------------
## Step 3: Ingeniería de Atributos
## ---------------------------

##RANKEO DE VARIABLES PESIFICADAS

#Todas las variables que comienzan con m minúscula son montos en pesos

var_pesos <- variables_iniciales[variables_iniciales %like% "^m"] 


prefix <- "r_"
r_var_pesos <- c()
for (var in var_pesos) {
    dtrain[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
    dtest[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
    r_var_pesos <- c(r_var_pesos,paste(prefix, var, sep = ""))
}


#Selección de variables para entrenar
seleccion_variables <- variables_iniciales[!variables_iniciales  %in% var_pesos]
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
#variables_mod <- c(variables_modelo,"clase_binaria")
#df <- rbind(dtrain,dtest)
#dataset_fe <- df[,..variables_mod]
#fwrite(dataset_fe,file="C:/Users/vyago/Desktop/Maestría Ciencias de Datos/07-DMEYF/exp/FE/001_FE_RANKING.csv")


## ---------------------------
## Step 4: Optimización Bayesiana
## ---------------------------




# Seteamos nuestra primera semilla
semillas <- c(444457,444583,444697,444743,444817)

set.seed(semillas[1])



# Armamos una función para modelar con el fin de simplificar el código futuro
modelo_rpart <- function(train, test, cp =  -0.3, ms = 20, mb = 1, md = 10,formula) {
    modelo <- rpart(formula, data = train,
                    xval = 0,
                    cp = cp,
                    minsplit = ms,
                    minbucket = mb,
                    maxdepth = md)

    test_prediccion <- predict(modelo, test, type = "prob")
    ganancia <- ganancia(test_prediccion[, "evento"], test$clase_binaria)
    return (ganancia)
}


ganancia <- function(probabilidades, clase) {
  return(sum(
    (probabilidades >= 0.025) * ifelse(clase == "evento", 78000, -2000))
  )
}


# Una función auxiliar para los experimentos
experimento_rpart <- function(ds, semillas, cp = 0, ms = 20, mb = 1, md = 10) {
  ganancia <- c()
  for (s in semillas) {
    set.seed(s)
    in_training <- caret::createDataPartition(ds$clase_binaria, p = 0.70,
        list = FALSE)
    train  <-  ds[in_training, ]
    test   <-  ds[-in_training, ]

    variables_iniciales <- names(ds)
    var_pesos <- variables_iniciales[variables_iniciales %like% "^m"] 

    prefix <- "r_"
    r_var_pesos <- c()
    for (var in var_pesos) {
        train[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
        test[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
        r_var_pesos <- c(r_var_pesos,paste(prefix, var, sep = ""))
    }


    #Selección de variables para entrenar
    seleccion_variables <- variables_iniciales[!variables_iniciales  %in% var_pesos]
    variables_modelo <- c(r_var_pesos,seleccion_variables)

    campos <- paste(variables_modelo, collapse = " + ")
    formula <- paste0( "clase_binaria ~ ", campos )



    r <- modelo_rpart(train, test,formula,
                    cp = cp, ms = ms, mb = mb, md = md)
    ganancia <- c(ganancia, r)
  }
  mean(ganancia)
}





set.seed(semillas[1])
obj_fun_md_ms <- function(x) {
  experimento_rpart(dataset, semillas
            , md = x$maxdepth
            , ms = x$minsplit,
            mb = floor(x$minbucket*x$minsplit))
}

obj_fun <- makeSingleObjectiveFunction(
  minimize = FALSE,
  fn = obj_fun_md_ms,
  par.set = makeParamSet(
    makeIntegerParam("maxdepth",  lower = 4L, upper = 30L),
    makeIntegerParam("minsplit",  lower = 1L, upper = 300L),
    makeNumericParam("minbucket",  lower = 0L, upper = 1L)
    # makeNumericParam <- para parámetros continuos
  ),
  noisy = TRUE,
  has.simple.signature = FALSE
)

ctrl <- makeMBOControl()
ctrl <- setMBOControlTermination(ctrl, iters = 2L)
ctrl <- setMBOControlInfill(
  ctrl,
  crit = makeMBOInfillCritEI(),
  opt = "focussearch",
)

lrn <- makeMBOLearner(ctrl, obj_fun)

surr_km <- makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

run_md_ms <- mbo(obj_fun, learner = surr_km, control = ctrl, )

print(run_md_ms)
