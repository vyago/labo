##
## Sobre el Oro
##
## ---------------------------
## Step 1: Armando un modelo para usar.
## ---------------------------
##
## All that gliters is not gold
## --- William Shakespeare

# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")

# Poner la carpeta de la materia de SU computadora local
setwd("/home/aleb/dmeyf2022")
# Poner sus semillas
semillas <- c(17, 19, 23, 29, 31)

# Cargamos el dataset y nos quedamos solo con el 202101
dataset <- fread("./datasets/competencia1_2022.csv")
enero <- dataset[foto_mes == 202101]
marzo <- dataset[foto_mes == 202103]

# Borramos el dataset para liberar memoria.
rm(dataset)

# Cargamos el privado de Kaggle
kaggle <- fread("./datasets/kaggle_competencia1_realidad.csv")

# Armamos diferentes clases binarias:
# Sólo es evento las clase BAJA+2
enero[, clase_binaria1 := ifelse(
                            clase_ternaria == "BAJA+2",
                                "evento",
                                "noevento"
                            )]

# Entrenamos en Enero para ver como funciona nuestro modelo en Marzo.
parametros <- list(cp = -1, minsplit = 1073, minbucket = 278, maxdepth = 9)
modelo <- rpart(clase_binaria1 ~ . - clase_ternaria,
                data = enero,
                xval = 0,
                control = parametros)

## ---------------------------
## Step 2: Aplicando ese modelo a los datos de Kaggle
## ---------------------------

# Predigo la probabilidad de marzo.
marzo$pred_marzo <- predict(modelo, marzo, type = "prob")[, "evento"]

# Determino a quienes voy a estimular usando un el punto de corte 0.025
marzo[, envio := ifelse(pred_marzo > 0.025, 1, 0)]

# Junto las variables de kaggle a marzo
marzo <- marzo[kaggle, on = .(numero_de_cliente)]

# Calculamos las ganancias en el total, en el público y en el privado

# Marzo entero
marzo[, sum(ifelse(envio == 1,
                ifelse(Predicted == 1, 78000, -2000)
            , 0))]

# Solo público, con correción del 0.3
marzo[, sum(ifelse(envio == 1 & Usage == "Public",
                ifelse(Predicted == 1, 78000, -2000)
            , 0))] / 0.3

# Solo privado, con correción del 0.7
marzo[, sum(ifelse(envio == 1 & Usage == "Private",
                ifelse(Predicted == 1, 78000, -2000)
            , 0))] / 0.7

## Pregunta
## ¿Esperaba esas diferencias entre leaderboards?

## ---------------------------
## Step 3: Creando 100 leaderboards
## ---------------------------

leaderboad <- data.table()
set.seed(semillas[1])
for (i in 1:100) {
  split <- caret::createDataPartition(marzo$Predicted, p = 0.70, list = FALSE)
  privado <- sum((marzo$pred_marzo[split] > 0.025) *
                    ifelse(marzo$Predicted[split] == 1, 78000, -2000)) / 0.7
  publico <- sum((marzo$pred_marzo[-split] > 0.025) *
                    ifelse(marzo$Predicted[-split] == 1, 78000, -2000)) / 0.3
  leaderboad <- rbindlist(list(leaderboad,
                data.table(privado = privado, publico = publico)))
}

leaderboad$r_privado <- frank(leaderboad$privado)
leaderboad$r_publico <- frank(leaderboad$publico)

leaderboad

## Preguntas
## ¿Qué conclusiones saca al ver los valores?
## - Respecto al valor real
## - Respecto a la relación entre el **público** y el **privado**

## ---------------------------
## Step 4: Graficando leaderboads
## ---------------------------

df <- melt(leaderboad, measure.vars =  c("privado", "publico"))
ggplot(df, aes(x = value, color = variable)) + geom_density()

## Observaciones?

## ---------------------------
## Step 5: Compitiendo entre dos modelos
## ---------------------------

# Sumamos un modelo básico

parametros2 <- list(cp = -1, minsplit = 2, minbucket = 1, maxdepth = 5)
modelo2 <- rpart(clase_binaria1 ~ . - clase_ternaria,
                data = enero,
                xval = 0,
                control = parametros2)

marzo$pred2_marzo <- predict(modelo2, marzo, type = "prob")[, "evento"]

# Marzo entero
marzo[, sum(ifelse(pred2_marzo >= 0.025,
                ifelse(Predicted == 1, 78000, -2000)
            , 0))]

# Solo público, con correción del 0.3
marzo[, sum(ifelse(pred2_marzo >= 0.025 & Usage == "Public",
                ifelse(Predicted == 1, 78000, -2000)
            , 0))] / 0.3

# Solo privado, con correción del 0.7
marzo[, sum(ifelse(pred2_marzo >= 0.025 & Usage == "Private",
                ifelse(Predicted == 1, 78000, -2000)
            , 0))] / 0.7


## Preguntas
## Abriendo la caja de pandora, ¿Cúal de los dos modelos era mejor?

## ---------------------------
## Step 6: Compitiendo entre dos modelos, ahora en los leaderboards
## ---------------------------

leaderboad2 <- data.table()
set.seed(semillas[1])
for (i in 1:100) {
  split <- caret::createDataPartition(marzo$Predicted, p = 0.70, list = FALSE)
  privado <- sum((marzo$pred_marzo[split] > 0.025) *
                    ifelse(marzo$Predicted[split] == 1, 78000, -2000)) / 0.7
  publico <- sum((marzo$pred_marzo[-split] > 0.025) *
                    ifelse(marzo$Predicted[-split] == 1, 78000, -2000)) / 0.3
  privado2 <- sum((marzo$pred2_marzo[split] > 0.025) *
                    ifelse(marzo$Predicted[split] == 1, 78000, -2000)) / 0.7
  publico2 <- sum((marzo$pred2_marzo[-split] > 0.025) *
                    ifelse(marzo$Predicted[-split] == 1, 78000, -2000)) / 0.3

  leaderboad2 <- rbindlist(list(leaderboad2,
                data.table(privado = privado,
                           publico = publico,
                           privado2 = privado2,
                           publico2 = publico2)))
}

leaderboad2


## Preguntas
## Viendo la tabla anterior, ¿En cuántos leaderboard hubiera elegido el modelo
## correcto usando el público?

## ---------------------------
## Step 7: Compitiendo entre dos modelos, las curvas!
## ---------------------------

df2 <- melt(leaderboad2, measure.vars =  c("publico", "publico2"))
ggplot(df2, aes(x = value, color = variable)) + geom_density()

df3 <- melt(leaderboad2, measure.vars =  c("privado", "privado2"))
ggplot(df3, aes(x = value, color = variable)) + geom_density()

## Active learning ... entender que pasa.

