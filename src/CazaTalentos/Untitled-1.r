a  <- c(1,3,2,4,6)

#a <- order(a,decreasing=TRUE)

#View(a)


genera_ranking <- function (paciertos) 
{   posiciones <- integer(length(paciertos))
    orden <- order(paciertos,decreasing=TRUE)
    posicion = 1
    #View(orden)
    for (j in orden) {
        
        posiciones[j] = posicion
        posicion = posicion + 1

    }
  return  (posiciones)

}

#View(genera_ranking(a))

#View(seq(from=10,to=1))

a<- seq(1,10,2)

View(a)