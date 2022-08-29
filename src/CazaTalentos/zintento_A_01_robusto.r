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
  GLOBAL_jugadores  <<-  sample( c( (501:599 ) / 1000 , 0.7 ) )
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

genera_ranking <- function (paciertos) 
{   posiciones <- integer(length(paciertos))
    orden <- order(paciertos,decreasing=TRUE)
    posicion = 1
    for (j in 1:length(orden)) {
        
        posiciones[orden[j]] = posicion
        posicion = posicion + 1

    }
  return  (posiciones)

}
#------------------------------------------------------------------------------

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

  planilla_cazatalentos[ ids_juegan1,  tiros1 := 70 ]  #registro en la planilla que tiran 90 tiros
  #Hago que tiren
  resultado1  <- gimnasio_tirar( ids_juegan1, 70)
  planilla_cazatalentos[ ids_juegan1,  aciertos1 := resultado1 ]  #registro en la planilla
  orden <- genera_ranking(resultado1)
  planilla_cazatalentos[ ids_juegan1,  orden1:= orden]

  #Ronda 2 -------------------------------------------------------
  #A la mitad mejor la hago tirar 400 tiros cada uno
  #La mediana siempre parte a un conjunto en dos partes de igual cantidad
  quantil1  <- planilla_cazatalentos[ ids_juegan1, quantile(aciertos1, probs = 0.40,names=FALSE) ]
  ids_juegan2  <- planilla_cazatalentos[ ids_juegan1 ][ aciertos1 > quantil1, id ]

  planilla_cazatalentos[ ids_juegan2,  tiros2 := 30 ]  #registro en la planilla que tiran 400 tiros
  resultado2  <- gimnasio_tirar( ids_juegan2, 40)
  planilla_cazatalentos[ ids_juegan2,  aciertos2 := resultado2 ]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan2,  aciertos_totales_2 := aciertos1 + aciertos2]
  orden <- genera_ranking(resultado2)
  planilla_cazatalentos[ ids_juegan2,  orden2:= orden]
  #Ronda 3
  quantil2  <- planilla_cazatalentos[ ids_juegan2, quantile(aciertos_totales_2, probs = 0.5,names=FALSE) ]
  ids_juegan3  <- planilla_cazatalentos[ ids_juegan2 ][ aciertos_totales_2 > quantil2, id ]

  planilla_cazatalentos[ ids_juegan3,  tiros3 := 50 ]  #registro en la planilla que tiran 400 tiros
  resultado3  <- gimnasio_tirar( ids_juegan3, 50)
  planilla_cazatalentos[ ids_juegan3,  aciertos3 := resultado3 ]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan1,  aciertos_totales_3 := aciertos1 + aciertos2 + aciertos3]
  orden <- genera_ranking(resultado3)
  planilla_cazatalentos[ ids_juegan3,  orden3:= orden]


  #Ronda 4
  quantil3  <- planilla_cazatalentos[ ids_juegan3, quantile(aciertos_totales_3, probs = 0.5,names=FALSE) ]
  ids_juegan4  <- planilla_cazatalentos[ ids_juegan3 ][ aciertos_totales_3 >= quantil3, id ]

  planilla_cazatalentos[ ids_juegan4,  tiros4 := 70 ]  #registro en la planilla que tiran 400 tiros
  resultado4  <- gimnasio_tirar( ids_juegan4, 70)
  planilla_cazatalentos[ ids_juegan4,  aciertos4 := resultado4 ]  #registro en la planilla.
  planilla_cazatalentos[ ids_juegan4,  aciertos_totales_4 := aciertos1+aciertos2+aciertos3+aciertos4]
  orden <- genera_ranking(resultado4)
  planilla_cazatalentos[ ids_juegan4,  orden4:= orden]

  
  #Ronda 5
  quantil4  <- planilla_cazatalentos[ ids_juegan4, quantile(aciertos_totales_4, probs = 0.7,names=FALSE) ]
  ids_juegan5  <- planilla_cazatalentos[ ids_juegan4 ][ aciertos_totales_4 >= quantil4, id ]

  planilla_cazatalentos[ ids_juegan5,  tiros5 := 300 ]  #registro en la planilla que tiran 400 tiros
  resultado5  <- gimnasio_tirar( ids_juegan5, 300)
  planilla_cazatalentos[ ids_juegan5,  aciertos5 := resultado5 ]  #registro en la planilla.
  planilla_cazatalentos[ ids_juegan5,  aciertos_totales_5 := aciertos1+aciertos2+aciertos3+aciertos4+aciertos5]
  orden <- genera_ranking(resultado5)
  planilla_cazatalentos[ ids_juegan5,  orden5:= orden]
  planilla_cazatalentos[ids_juegan5, ranking_promedio := (orden1*0.2+orden2*0.3+orden3*0.4+orden4*0.5+orden5)/5]

  #Epilogo
  #El cazatalentos toma una decision, elige al que mas aciertos tuvo en la ronda2
  pos_mejor <-  planilla_cazatalentos[, which.min(ranking_promedio) ]
  id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]

  #Finalmente, la hora de la verdadero_mejor
  #Termino el juego
  veredicto  <- gimnasio_veredicto( id_mejor )
  
  return( veredicto )
  View(veredicto)

#El veredicto da que la estrategia seguida por el cazatalentos fue exitosa para este caso
#Le acerto al verdadero_mejor

#En el siguiente script veremos de hacer una Estimacion Montecarlo
#De 10000 veces que el entrenador sigue esta estrategia, cuantas realmente le acierta
