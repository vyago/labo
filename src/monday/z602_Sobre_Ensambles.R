##
## Sobre Ensambles
##
## ---------------------------
## Step 1: Conceptos
## ---------------------------
##
##  Hay quienes pasan por el bosque y sólo ven leña para el fuego
## --- León Tolstoi

## Preguntas
## - ¿Qué es un ensamble de modelos?
## - ¿Cómo tienen que ser los modelos dentro de un ensamble?
## - ¿Qué técnicas conoce para ensamblar modelos?
## - ¿Por qué funcionan mejor los ensambles?


# Los ensambles pueden partir de modelos ya desarrollados, o de modelos que se
# creen especialmente para ser ensamblados.

# Sobre los segundos, veremos son los llamados Bagging (bootstrap aggregating).
# - Hacer **N** nuevos conjunto de entrenamiento usando boostraping, o sea,
#   reemplazar nuestro dataset por elementos aleatorios con reemplazo.
# - Para este cada nuevo dataset obtener un modelo.
# - Promediar (o votar) las salidas de los modelos.

# El espíritu detrás de este algoritmo, puede entenderse en que cada modelo es
# una especialista de sólo una parte, y la suma de muchos especialistas
# consiguen un buen modelo.

# El algoritmo de **bagging** más conocido es el **random forest**.

## ---------------------------
## Step 2: Primer RF
## ---------------------------

# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")
require("ranger")
require("randomForest")
require("lightgbm")

# Poner la carpeta de la materia de SU computadora local
setwd("/home/aleb/dmeyf2022")
# Poner sus semillas
semillas <- c(17, 19, 23, 29, 31)

# Cargamos el dataset y nos quedamos solo con el 202101
dataset <- fread("./datasets/competencia1_2022.csv")
enero <- dataset[foto_mes == 202101]
rm(dataset)

# Importante que la clase sea factor
enero[, clase_binaria1 := factor(ifelse(
                            clase_ternaria == "BAJA+2",
                                "evento",
                                "noevento"
                            ))]
enero$clase_ternaria <- NULL
in_training <- caret::createDataPartition(enero$clase_binaria1,
                     p = 0.70, list = FALSE)

dtrain  <-  enero[in_training, ]
dtest   <-  enero[-in_training, ]

# ranger no soporta, como lo hacen otras librerías, los missing values
dtrain <-  na.roughfix(dtrain)
dtest <-  na.roughfix(dtest)

# Cantidad de variables que abren por cada hoja
n_variables <- round(sqrt(dim(dtrain)[2] - 1))

t0 <- Sys.time()
modelo_rf_1 <- ranger(clase_binaria1 ~ ., data = dtrain,
                  probability = TRUE,
                  num.trees = 100,
                  min.node.size=10,
                  mtry = n_variables,
                  splitrule = "gini",
                  sample.fraction = 0.66,
                  importance = "impurity",
                  verbose = TRUE)
t1 <- Sys.time()
as.numeric(t1 - t0, units = "secs")

## ---------------------------
## Step 3: Midiendo el primero RF
## ---------------------------

pred_train <- predict(modelo_rf_1, dtrain)
pred_test <- predict(modelo_rf_1, dtest)

# Ganancia en dtrain
print(sum((pred_train$predictions[, "evento"] >= 0.025) * ifelse(
                    dtrain$clase_binaria1 == "evento",
                    78000, -2000) / 0.7))
# Ganancia en dtest
print(sum((pred_test$predictions[, "evento"] >= 0.025) * ifelse(
                    dtest$clase_binaria1 == "evento",
                    78000, -2000) / 0.3))

## Preguntas
## - ¿Qué paso en `train`?
## - ¿Se veía esa diferencia en los árboles?

## ---------------------------
## Step 4: Importancia de variables
## ---------------------------

importancia <- as.data.table(modelo_rf_1$variable.importance,
                    keep.rownames = TRUE)
colnames(importancia) <- c("variable", "importancia")
setorder(importancia, -importancia)
importancia

## Preguntas
## - ¿Qué significa que una variable sea más importante que otra?
## - ¿Qué significa que una variable tenga 0 importancia?
## - ¿Con el **RF** es suficiente como para descartarlas?
## - ¿Qué una variable tenga algo de importancia es suficiente como para
## - entender que da valor?

## ---------------------------
## Step 5: Un experimento con pollitos
## ---------------------------

dtrain$pollito <- runif(nrow(dtrain))

modelo_rf_2 <- ranger(clase_binaria1 ~ ., data = dtrain,
                  probability = TRUE,
                  num.trees = 150,
                  min.node.size = 10,
                  mtry = n_variables,
                  splitrule = "gini",
                  importance = "impurity",
                  verbose = TRUE)

importancia2 <- as.data.table(modelo_rf_2$variable.importance,
                    keep.rownames = TRUE)
colnames(importancia2) <- c("variable", "importancia")
setorder(importancia2, -importancia)
importancia2

## Active learning o a llorar a la iglesia.

## ---------------------------
## Step 6: Boosting, la navaja suiza de los modelos - Conceptos
## ---------------------------

# Estos se construyen de forma serial.
# Primero se parte de un modelo (que puede ser un valor constante) y se
# complementa con un modelo que busca mejorar al anterior.

# Hay dos algoritmos muy conocidos de este tipo:

# **Adaboost**: Que cada nuevo modelo va mejorando a los anteriores poniendo un
# peso mayor en los casos donde la clasificación es incorrecta

# **Gradient Boosting**: Que cada nuevo modelo va mejorando los anteriores,
# tratando de corregir los residuos, buscando estos últimos con el gradiente
# de una función de perdida.

# Este último se empezó a hacer muy popular por la excelente pieza de tecnología
# que es su implementación **xgboost**, superado luego por el LightGBM.

## ---------------------------
## Step 7: LightGBM
## ---------------------------

# Cargamos todo para tener un código limpio
dataset <- fread("./datasets/competencia1_2022.csv")
enero <- dataset[foto_mes == 202101]
marzo <- dataset[foto_mes == 202103]
kaggle <- fread("./datasets/kaggle_competencia1_realidad.csv")
marzo <- marzo[kaggle, on = .(numero_de_cliente)]
rm(dataset)

clase_binaria <- ifelse(enero$clase_ternaria == "BAJA+2", 1, 0)
enero$clase_ternaria <- NULL

dtrain  <- lgb.Dataset(data = data.matrix(enero), label = clase_binaria)

ganancia_lgb <- function(probs, datos) {
  return(list("name" = "ganancia",
                "value" =  sum( (probs > 0.025) *
                    ifelse(getinfo(datos, "label") == 1, 78000, -2000)) / 0.2,
                "higher_better" = TRUE))
}

set.seed(semillas[1])
model_lgbm_cv <- lgb.cv(data = dtrain,
         eval = ganancia_lgb,
         stratified = TRUE,
         nfold = 5,
         param = list( objective= "binary",
                       max_bin = 15,
                       min_data_in_leaf = 4000,
                       learning_rate = 0.05
                       )
      )

model_lgbm_cv$best_iter
unlist(model_lgbm_cv$record_evals$valid$ganancia$eval)[ model_lgbm_cv$best_iter]

model_lgm <- lightgbm(data = ds_train,
            nrounds = model_lgbm_cv$best_iter,
            params = list(objective = "binary",
                            max_bin = 15,
                            min_data_in_leaf = 4000,
                            learning_rate = 0.05),
             verbose = -1)


lgb.importance(model_lgm, percentage = TRUE)

## ---------------------------
## Step 8: En kaggle
## ---------------------------

marzo$pred_marzo <- predict(model_lgm, data.matrix(marzo[,1:154]))

# TOTAL
sum((marzo$pred_marzo > 0.025) *
            ifelse(marzo$Predicted == 1, 78000, -2000))

# Público
sum((marzo$pred_marzo > 0.025 & marzo$Usage == "Public") *
                    ifelse(marzo$Predicted == 1, 78000, -2000)) / 0.3

# Privado
sum((marzo$pred_marzo > 0.025 & marzo$Usage == "Private") *
                    ifelse(marzo$Predicted == 1, 78000, -2000)) / 0.7

## Bienvenido al mundo de los ensambles
