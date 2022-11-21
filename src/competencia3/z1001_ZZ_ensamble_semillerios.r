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
PARAM$experimento  <- "ZE100141"
PARAM$exp_input  <- "ZZ100041"

PARAM$ensemble  <- 'mediana'  # acá puede ir mediana o rankeo
# FIN Parametros del script


#------------------------------------------------------------------------------
options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

base_dir <- "~/buckets/b1/"

#creo la carpeta donde va el experimento
dir.create( paste0( base_dir, "exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( base_dir, "exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

#leo el dataset con las predicciones generadas

arch_pred <- paste0(base_dir,"exp/",PARAM$exp_input,"/pred_ensemble.csv")
predicciones <- fread(arch_pred)

ensamblado <- function (tipo){
  if(tipo=="mediana"){
    predicciones[,prob:= apply(predicciones, 1, median)]
  }
  if(tipo="rankeo"){
    modelos <- names(predicciones)[-c(1:2)]
    predicciones[,(modelos):=lapply(.SD,function(x){rank(x,ties.method = "random")}),.SD=modelos]
    predicciones[,prob:=prob:= apply(predicciones, 1, median)]

  }
}





  #genero los archivos para Kaggle
  cortes  <- seq( from=  10000,
                  to=   16000,
                  by=     500 )


  setorder( predicciones, -prob )

  for( corte in cortes )
  {
    predicciones[  , Predicted := 0L ]
    predicciones[ 1:corte, Predicted := 1L ]

    nom_submit  <- paste0( PARAM$experimento, 
                           "_",
                           sprintf( "%05d", corte ),
                           ".csv" )

    fwrite(  predicciones[ , list( numero_de_cliente, Predicted ) ],
             file= nom_submit,
             sep= "," )

  }


  #borro y limpio la memoria para la vuelta siguiente del for
  rm( predicciones )
  rm( tb_importancia )
  rm( modelo_final)
  rm( parametros )
  rm( dtrain )
  gc()
}
