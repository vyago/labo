##
## Sobre como cocinar Pizzas
##
## ---------------------------
## Step 1: Cargando los datos y las librerías
## ---------------------------
##
## Success is a lousy teacher. It seduces smart people into thinking they can't 
## lose.
## --- Bill Gates

# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
#install.packages("lubridate")
#install.packages("lhs")
#install.packages("DiceKriging")
#install.packages("mlrMBO")
require("data.table")
require("rpart")
require("ROCR")
require("ggplot2")
require("lubridate")
require("lhs")
require("DiceKriging")
require("mlrMBO")

# Poner la carpeta de la materia de SU computadora local
setwd("C:/YAGO/Maestría Ciencias de Datos/07-DMEYF")
# Poner sus semillas
semillas <- c(444457,444583,444697,444743,444817)

# Cargamos el dataset
dataset <- fread("./datasets/competencia1_2022.csv")

# Nos quedamos solo con el 202101
dataset <- dataset[foto_mes == 202101]
# Creamos una clase binaria
dataset[, clase_binaria := ifelse(
                            clase_ternaria == "BAJA+2",
                                "evento",
                                "noevento"
                            )]
# Borramos el target viejo
dataset[, clase_ternaria := NULL]

# Seteamos nuestra primera semilla
set.seed(semillas[1])



# Armamos una función para modelar con el fin de simplificar el código futuro
modelo_rpart <- function(train, test, cp =  -0.3, ms = 20, mb = 1, md = 10) {
    modelo <- rpart(clase_binaria ~ ., data = train,
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
    r <- modelo_rpart(train, test, 
                    cp = cp, ms = ms, mb = mb, md = md)
    ganancia <- c(ganancia, r)
  }
  mean(ganancia)
}




## ---------------------------
## Step 10: Buscando con una Opt. Bayesiana para 2 parámetros
## ---------------------------

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
ctrl <- setMBOControlTermination(ctrl, iters = 120L)
ctrl <- setMBOControlInfill(
  ctrl,
  crit = makeMBOInfillCritEI(),
  opt = "focussearch",
)

lrn <- makeMBOLearner(ctrl, obj_fun)

surr_km <- makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

run_md_ms <- mbo(obj_fun, learner = surr_km, control = ctrl, )
print(run_md_ms)

## TAREA:
## Agregue todos los parámetros que considere. Una vez que tenga sus mejores
## parámetros, haga una copia del script rpart/z101_PrimerModelo.R, cambie los
## parámetros dentro del script, ejecutelo y suba a Kaggle su modelo.