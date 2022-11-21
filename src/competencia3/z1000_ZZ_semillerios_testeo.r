#Necesita para correr en Google Cloud
# 128 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

require("lightgbm")

#Parametros del script
PARAM  <- list()
PARAM$experimento  <- "ZZ100042"
PARAM$exp_input  <- "HT94143"

PARAM$modelos  <- 1   #posición de los parámetros del modelo a elegir
PARAM$cantidad_modelos <- 60 #cantidad de modelos que quiero usar para el semillerio
# FIN Parametros del script

ksemilla  <- 102191

#------------------------------------------------------------------------------
options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

base_dir <- "C:/Users/vyago/Desktop/Maestría Ciencias de Datos/07-DMEYF/"

#creo la carpeta donde va el experimento
dir.create( paste0( base_dir, "exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( base_dir, "exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

#leo la salida de la optimizaciob bayesiana
arch_log  <- paste0( base_dir, "exp/", PARAM$exp_input, "/BO_log.txt" )
tb_log  <- fread( arch_log )
setorder( tb_log, -ganancia )

#leo el nombre del expermento de la Training Strategy
#arch_TS  <- paste0( base_dir, "exp/", PARAM$exp_input, "/TrainingStrategy.txt" )
TS  <- "TS1"

#leo el dataset donde voy a entrenar el modelo final
arch_dataset  <- paste0( base_dir, "exp/", TS, "/dataset_train_final.csv.gz" )
dataset  <- fread( arch_dataset )

#leo el dataset donde voy a aplicar el modelo final
arch_future  <- paste0( base_dir, "exp/", TS, "/dataset_future.csv.gz" )
dfuture <- fread( arch_future )


#defino la clase binaria
dataset[ , clase01 := ifelse( clase_ternaria %in% c("BAJA+1","BAJA+2"), 1, 0 )  ]

campos_buenos  <- setdiff( colnames(dataset), c( "clase_ternaria", "clase01") )

#genero las semillas 
generador_semillas <- function(cantidad) {
  semillas <- c()

  for (num in 1:cantidad) {
    semillas <- c(semillas,1+num+1234*num)
  }
  return (semillas)
}

semillas <- generador_semillas(PARAM$cantidad_modelos)

#genero un modelo para cada uno de las modelos_qty MEJORES iteraciones de la Bayesian Optimization

ruta_predicciones  <- paste0( base_dir, "exp/", PARAM$experimento, "/pred_ensemble.csv" )

#si tengo predicciones hechas, retoma la corrida desde el último modelo
if (file.exists(ruta_predicciones)) {
  alm_predicciones <- fread(ruta_predicciones)
  nombre_ult_modelo <- names(alm_predicciones)[ncol(alm_predicciones)]
  ult_modelo <- as.integer(unlist(strsplit(nombre_ult_modelo,split="-"))[2]) + 1
}else{
  ult_modelo <- 1
}

for( i in  ult_modelo:PARAM$cantidad_modelos )
{


  parametros  <- as.list( copy( tb_log[ PARAM$modelos ] ) )
  iteracion_bayesiana  <- parametros$iteracion_bayesiana
  if( i<=2){
        fwrite(parametros,
          file= "parametros.csv",
          sep= "\t" )
  }


  #creo CADA VEZ el dataset de lightgbm
  dtrain  <- lgb.Dataset( data=    data.matrix( dataset[ , campos_buenos, with=FALSE] ),
                          label=   dataset[ , clase01],
                          weight=  dataset[ , ifelse( clase_ternaria %in% c("BAJA+2"), 1.0000001, 1.0)],
                          free_raw_data= FALSE
                        )

  ganancia  <- parametros$ganancia

  #elimino los parametros que no son de lightgbm
  parametros$experimento  <- NULL
  parametros$cols         <- NULL
  parametros$rows         <- NULL
  parametros$fecha        <- NULL
  parametros$prob_corte   <- NULL
  parametros$estimulos    <- NULL
  parametros$ganancia     <- NULL
  parametros$iteracion_bayesiana  <- NULL

  #Utilizo la semilla definida en este script
  parametros$seed  <- semillas[i]
  
  #genero el modelo entrenando en los datos finales
  set.seed( parametros$seed )
  modelo_final  <- lightgbm( data= dtrain,
                             param=  parametros,
                             verbose= -100 )


  #genero la prediccion, Scoring
  prediccion  <- predict( modelo_final,
                          data.matrix( dfuture[ , campos_buenos, with=FALSE ] ) )

  if (!file.exists(ruta_predicciones)){

    tb_prediccion  <- dfuture[  , list( numero_de_cliente, foto_mes ) ]
    seed_prediccion <- quote(paste0("mod-",i))
    tb_prediccion[ , eval(seed_prediccion) := prediccion ]


    nom_pred  <- "pred_ensemble.csv"  

    fwrite( tb_prediccion,
          file= nom_pred,
          sep= "\t" )
  }else{
    alm_predicciones <- fread(ruta_predicciones)
    seed_prediccion <- quote(paste0("mod-",i))
    alm_predicciones[,eval(seed_prediccion) := prediccion]
    nom_pred  <- "pred_ensemble.csv"  

    fwrite(alm_predicciones,
          file= nom_pred,
          sep= "\t" )
  }



  #borro y limpio la memoria para la vuelta siguiente del for
  try(rm( tb_prediccion ))
  rm( modelo_final)
  rm( parametros )
  rm( dtrain )
  try(rm(alm_predicciones))
  gc()
}

