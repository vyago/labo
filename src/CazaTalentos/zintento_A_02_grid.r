#Intento de Solucion del desafio  15k
#que NO logra solucionarlo, una que falta una idea fundamental, una chispa, un Momento Eureka
#pero crea estructura sobre la cual trabajar


#limpio la memoria
rm( list=ls() )
gc()

require("data.table")

ftirar  <- function( prob, qty )
{
  return(  sum( runif(qty) < prob ) )
}


#variables globales que usan las funciones gimnasio_xxxx
GLOBAL_jugadores  <- c()
GLOBAL_tiros_total  <- 0

#Crea el juego
#a cada jugador se le pone un numero de 1 a 100 en la espalda
#debajo de ese numero esta el indice_de_enceste  que NO puede ser visto por el cazatalentos
gimnasio_init  <- function() 
{
  GLOBAL_jugadores  <<- sample( c( (501:599 ) / 1000 , 0.7 ) )
  GLOBAL_tiros_total  <<- 0
  el_mejor <<- which.max(GLOBAL_jugadores)
}


#se le pasa un vector con los IDs de los jugadores y la cantidad de tiros a realizar
#devuelve en un vector cuantos aciertos tuvo cada jugador
gimnasio_tirar  <- function(  pids,  pcantidad )
{
  
  GLOBAL_tiros_total  <<-  GLOBAL_tiros_total + length( pids )*pcantidad
  res  <- mapply(  ftirar, GLOBAL_jugadores[pids], pcantidad )

  return( res )
}


#El cazatalentos decide a que jugador llevarse
#devuelve la cantidad de tiros libres y si le acerto al verdadero_mejor o no
gimnasio_veredicto  <- function( pid )
{
  return( list("tiros_total"= GLOBAL_tiros_total, 
               "acierto"=     as.integer( GLOBAL_jugadores[pid]==0.7) ))
}
#------------------------------------------------------------------------------

Estrategia_A  <- function(q1,q2,q3,q4,t1=50,t2=55,t3=65,t4=85)
{
  #Estrategia
  #En la primer ronda se hace tirar 90 tiros libres a cada uno de los 100 jugadores ( se gastan 9000 tiros )
  #Se eligen a la mejor mitad de primer ronda( se descarta a la otra mitad)
  #En la segunda ronda, a la mejor mitad de la primera se los hace tirar 400 tiros a cada uno
  #Se elige el mejor jugador de la segunda ronda

  gimnasio_init()

  #Esta el la planilla del cazatalentos
  #el id es el numero que tiene en la espalda cada jugador
  planilla_cazatalentos  <- data.table( "id"= 1:100 )

  
  #Ronda 1  ------------------------------------------------------
  #tiran los 100 jugadores es decir 1:100   90  tiros libres cada uno
  ids_juegan1  <- 1:100   #los jugadores que participan en la ronda,

  planilla_cazatalentos[ ids_juegan1,  tiros1 := t1 ]  #registro en la planilla que tiran 90 tiros

  #Hago que tiren
  resultado1  <- gimnasio_tirar( ids_juegan1, t1)
  planilla_cazatalentos[ ids_juegan1,  aciertos1 := resultado1 ]  #registro en la planilla

  #Ronda 2 -------------------------------------------------------
  #A la mitad mejor la hago tirar 400 tiros cada uno
  #La mediana siempre parte a un conjunto en dos partes de igual cantidad
  quantil1  <- planilla_cazatalentos[ ids_juegan1, quantile(aciertos1, probs = q1,names=FALSE) ]
  ids_juegan2  <- planilla_cazatalentos[ ids_juegan1 ][ aciertos1 > quantil1, id ]

  planilla_cazatalentos[ ids_juegan2,  tiros2 := t2 ]  #registro en la planilla que tiran 400 tiros
  resultado2  <- gimnasio_tirar( ids_juegan2, t2)
  planilla_cazatalentos[ ids_juegan2,  aciertos2 := resultado2 ]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan2,  aciertos_totales_2 := aciertos1 + aciertos2]
  #Ronda 3
  quantil2  <- planilla_cazatalentos[ ids_juegan2, quantile(aciertos_totales_2, probs = q2,names=FALSE) ]
  ids_juegan3  <- planilla_cazatalentos[ ids_juegan2 ][ aciertos_totales_2 > quantil2, id ]

  planilla_cazatalentos[ ids_juegan3,  tiros3 := t3 ]  #registro en la planilla que tiran 400 tiros
  resultado3  <- gimnasio_tirar( ids_juegan3, t3)
  planilla_cazatalentos[ ids_juegan3,  aciertos3 := resultado3 ]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan1,  aciertos_totales_3 := aciertos1 + aciertos2 + aciertos3]


  #Ronda 4
  quantil3  <- planilla_cazatalentos[ ids_juegan3, quantile(aciertos_totales_3, probs = q3,names=FALSE) ]
  ids_juegan4  <- planilla_cazatalentos[ ids_juegan3 ][ aciertos_totales_3 >= quantil3, id ]

  planilla_cazatalentos[ ids_juegan4,  tiros4 := t4 ]  #registro en la planilla que tiran 400 tiros
  resultado4  <- gimnasio_tirar( ids_juegan4, t4)
  planilla_cazatalentos[ ids_juegan4,  aciertos4 := resultado4 ]  #registro en la planilla.
  planilla_cazatalentos[ ids_juegan4,  aciertos_totales_4 := aciertos1+aciertos2+aciertos3+aciertos4]

    #Ronda 5
  quantil4  <- planilla_cazatalentos[ ids_juegan4, quantile(aciertos_totales_4, probs = q4,names=FALSE) ]
  ids_juegan5  <- planilla_cazatalentos[ ids_juegan4 ][ aciertos_totales_4 >= quantil4, id ]
  tiros <- floor((14000-GLOBAL_tiros_total)/length(ids_juegan5))
  if (tiros<0)
  {
    tiros=0
  }
  planilla_cazatalentos[ ids_juegan5,  tiros5 := tiros ]  #registro en la planilla que tiran 400 tiros
  resultado5  <- gimnasio_tirar( ids_juegan5, tiros)
  planilla_cazatalentos[ ids_juegan5,  aciertos5 := resultado5 ]  #registro en la planilla.
  planilla_cazatalentos[ ids_juegan5,  aciertos_totales_5 := aciertos1+aciertos2+aciertos3+aciertos4+aciertos5]


  #Epilogo
  #El cazatalentos toma una decision, elige al que mas aciertos tuvo en la ronda2
  pos_mejor <-  planilla_cazatalentos[ , which.max(aciertos_totales_5) ]
  id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]

  #Finalmente, la hora de la verdadero_mejor
  #Termino el juego
  veredicto  <- gimnasio_veredicto( id_mejor )

  return( veredicto )
}
#------------------------------------------------------------------------------

#Aqui hago la Estimacion Montecarlo del porcentaje de aciertos que tiene la estrategia A

set.seed( 121124256 )  #debe ir una sola vez, ANTES de los experimentos

tabla_veredictos  <- data.table(  tiros_total=integer(),  acierto=integer() )

#for (q1 in seq(0.20,0.30,0.02)){
 # for (q2 in seq(0.26,0.36,0.02)){
   # for (q3 in seq(0.48,0.80,0.02)){
    #    for (q4 in seq(0.60,0.90,0.05)){
           # for (t1 in seq(40,70,5)){
            #    for(t2 in seq(40,70,5)){
                    for (t3 in seq(40,90,5)){
                        for (t4 in seq(40,90,5)){
for( experimento  in  1:100 )
{
  if( experimento %% 1000 == 0 )  cat( experimento, " ")  #desprolijo, pero es para saber por donde voy

  veredicto  <- Estrategia_A(0.3,0.35,0.6,0.8,50,55,t3,t4)
  
  tabla_veredictos  <- rbind( tabla_veredictos, veredicto )
}

cat("\n")

tiros_total  <-  tabla_veredictos[  , max( tiros_total) ]
tasa_eleccion_correcta  <-  tabla_veredictos[  , mean( acierto) ]
cat( tasa_eleccion_correcta, " ")
cat( c(t3,t4), " ")


tiros_total
tasa_eleccion_correcta
                            
 #                       }
  #                  }
                }
           }
 #       }
  #  }
  #}
#}



#Esta estrategia elije al verdadero_mejor el 99% de las veces
#pero lamentablemente necesita de un total de 36600   tiros libres

