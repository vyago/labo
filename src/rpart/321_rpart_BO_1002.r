#Optimizacion Bayesiana de hiperparametros de  rpart

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")

require("rpart")
require("parallel")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")


#Defino la  Optimizacion Bayesiana

kBO_iter  <- 50   #cantidad de iteraciones de la Optimizacion Bayesiana

hs  <- makeParamSet(
          makeNumericParam("cp"       , lower= -1   , upper=    0.1),
          makeIntegerParam("minsplit" , lower=  1L  , upper= 800L),  #la letra L al final significa ENTERO
          makeIntegerParam("minbucket", lower=  1L  , upper= 400L),
          makeIntegerParam("maxdepth" , lower=  3L  , upper=   20L),
          forbidden = quote( minbucket > 0.5*minsplit ) )             # minbuket NO PUEDE ser mayor que la mitad de minsplit

ksemilla_azar  <- 102191   #cambiar por la primer semilla

#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./work/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0( folder, substitute( reg), ext )

  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )

    cat( linea, file=archivo )
  }

  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )

  cat( linea, file=archivo, append=TRUE )  #grabo al archivo

  if( verbose )  cat( linea )   #imprimo por pantalla
}
#------------------------------------------------------------------------------
#particionar agrega una columna llamada fold a un dataset que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30 
# particionar( data=dataset, division=c(1,1,1,1,1), agrupa=clase_ternaria, seed=semilla)   divide el dataset en 5 particiones

particionar  <- function( data, division, agrupa="", campo="fold", start=1, seed=NA )
{
  if( !is.na( seed)  )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x ) }, division, seq( from=start, length.out=length(division) )  ) )

  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
           by= agrupa ]
}
#------------------------------------------------------------------------------
#fold_test  tiene el numero de fold que voy a usar para testear, entreno en el resto de los folds
#param tiene los hiperparametros del arbol

ArbolSimple  <- function( fold_test, data, param )
{





  #genero el modelo
  modelo  <- rpart("clase_binaria ~ .", 
                   data= data[ fold != fold_test, ],  #entreno en todo MENOS el fold_test que uso para testing
                   xval= 0,
                   control= param )

  #aplico el modelo a los datos de testing
  prediccion  <- predict( modelo, 
                          data[ fold==fold_test, ],  #aplico el modelo sobre los datos de testing
                          type= "prob")   #quiero que me devuelva probabilidades

  prob_baja2  <- prediccion[, "evento"]  #esta es la probabilidad de baja

  #calculo la ganancia
  ganancia_testing  <- data[ fold==fold_test ][ prob_baja2 > 1/20,  
                                                sum( ifelse( clase_binaria=="evento", 78000, -2000 ) )] 

  return( ganancia_testing )  #esta es la ganancia sobre el fold de testing, NO esta normalizada
}
#------------------------------------------------------------------------------

ArbolesCrossValidation  <- function( data, param, qfolds, pagrupa, semilla )
{
  divi  <- rep( 1, qfolds )  # generalmente  c(1, 1, 1, 1, 1 )  cinco unos

  particionar( data, divi, seed=semilla, agrupa=pagrupa )  #particiono en dataset en folds

  ganancias  <- mcmapply( ArbolSimple, 
                          seq(qfolds), # 1 2 3 4 5
                          MoreArgs= list( data, param), 
                          SIMPLIFY= FALSE,
                          mc.cores= 1 )   #se puede subir a qfolds si posee Linux o Mac OS

  data[ , fold := NULL ]

  #devuelvo la primer ganancia y el promedio
  ganancia_promedio  <- mean( unlist( ganancias ) )   #promedio las ganancias
  ganancia_promedio_normalizada  <- ganancia_promedio * qfolds  #aqui normalizo la ganancia
  
  return( ganancia_promedio_normalizada )
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros, lamentablemente se pasan como variables globales

EstimarGanancia  <- function( x )
{
   GLOBAL_iteracion  <<-  GLOBAL_iteracion + 1

   xval_folds  <- 5
   ganancia  <- ArbolesCrossValidation( dataset,
                                        param= x, #los hiperparametros del arbol
                                        qfolds= xval_folds,  #la cantidad de folds
                                        pagrupa= "clase_binaria",
                                        semilla= ksemilla_azar )

   #logueo 
   xx  <- x
   xx$xval_folds  <-  xval_folds
   xx$ganancia  <- ganancia
   xx$iteracion <- GLOBAL_iteracion
   loguear( xx,  arch= archivo_log )

   return( ganancia )
}
#------------------------------------------------------------------------------
#Aqui empieza el programa

setwd("C:/Users/vyago/Desktop/Maestría Ciencias de Datos/07-DMEYF")

#cargo el dataset
dataset  <- fread("./datasets/competencia1_2022.csv")   #donde entreno
dataset <- dataset[foto_mes==202101]

## Clases BAJAS+1 y BAJA+2 combinadas
dataset[, clase_binaria := ifelse(
                            clase_ternaria == "BAJA+2",
                                "evento",
                                "noevento"
                            )]
dataset[,clase_ternaria:=NULL]

## Ranking de variables pesificadas
variables_iniciales <- names(dataset)
var_pesos <- variables_iniciales[variables_iniciales %like% "^m"] 
seleccion_variables <- variables_iniciales[!variables_iniciales  %in% var_pesos]

prefix <- "r_"
r_var_pesos <- c()
for (var in var_pesos) {
    dataset[, (paste(prefix, var, sep = "")) := ntile(get(var), 20)]
    
    r_var_pesos <- c(r_var_pesos,paste(prefix, var, sep = ""))
}

variables_modelo <- c(r_var_pesos,seleccion_variables)

dataset <- dataset[,..variables_modelo]

##Features con ranking
dataset[,campo1:=as.integer(r_mcaja_ahorro<3 & r_mtarjeta_visa_consumo<5 & r_mprestamos_personales <17)]
dataset[,campo2:=as.integer(r_mcaja_ahorro<3 & r_mtarjeta_visa_consumo<5 & r_mprestamos_personales >=17)]
dataset[,campo3:=as.integer(r_mcaja_ahorro<3 & r_mtarjeta_visa_consumo>=5 & Visa_msaldototal <7919.8)]
dataset[,campo4:=as.integer(r_mcaja_ahorro<3 & r_mtarjeta_visa_consumo>=5 & Visa_msaldototal >=7919.8)]
dataset[,campo5:=as.integer(r_mcaja_ahorro>=3 & r_mtarjeta_visa_consumo>=5 & cpayroll_trx <1)]
dataset[,campo6:=as.integer(r_mcaja_ahorro>=3 & r_mtarjeta_visa_consumo>=5 & cpayroll_trx >=1)]
dataset[,campo7:=as.integer(r_mcaja_ahorro>=3 & r_mtarjeta_visa_consumo>=5 & r_mpasivos_margen <6)]
dataset[,campo8:=as.integer(r_mcaja_ahorro>=3 & r_mtarjeta_visa_consumo>=5 & r_mpasivos_margen >=6)]





#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/HT1003/", showWarnings = FALSE )
setwd("./exp/HT1003/")   #Establezco el Working Directory DEL EXPERIMENTO


archivo_log  <- "HT1003.txt"
archivo_BO   <- "HT1003.RDATA"

#leo si ya existe el log, para retomar en caso que se se corte el programa
GLOBAL_iteracion  <- 0

if( file.exists(archivo_log) )
{
 tabla_log  <- fread( archivo_log )
 GLOBAL_iteracion  <- nrow( tabla_log )
}



#Aqui comienza la configuracion de la Bayesian Optimization

funcion_optimizar  <- EstimarGanancia

configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
              fn=       funcion_optimizar,
              minimize= FALSE,   #estoy Maximizando la ganancia
              noisy=    TRUE,
              par.set=  hs,
              has.simple.signature = FALSE
             )

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= archivo_BO)
ctrl  <- setMBOControlTermination(ctrl, iters= kBO_iter )
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI())

surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if( !file.exists( archivo_BO ) ) {

  run  <- mbo( fun=     obj.fun, 
               learner= surr.km,
               control= ctrl)

} else  run  <- mboContinue( archivo_BO )   #retomo en caso que ya exista

