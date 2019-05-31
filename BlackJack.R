#Black Jack
Mazo<-expand.grid(c("D","T","P","E"),c(1:13))
Mazo<-paste0(Mazo[[1]],Mazo[[2]])

#Black Jack
Barajar<-function(Mazo){
  #set.seed(as.numeric(Sys.time()))
  Mazo[sample(1:length(Mazo),replace = F)]
}

Barajar(Mazo)


BlackJack<-function(apuestas=c(4,1,6),
                    estrategias=c("Pedir<15","Plantrse>18","Plantarse>19")){
  
  No_jugadores<-length(apuestas)
  Juego<-Barajar(Mazo)
  
  Valor<-function(carta){
    valor<-as.numeric(substr(carta,2,nchar(carta)))
    if(valor>10){return(10)}
    if(valor==1){return(c(1,11))}
    return(valor)
  }
  library(purrr)
  Valores<-purrr::map(Juego,Valor)
  
  Inicio<-list()
  for(i in 1:(No_jugadores+1)){
    Cartas<-Valores[(i*2-1):((i*2))]
    #Si hay un As, tengo 2 resultados
    if(max(map_int(Cartas,length))>1){
      if(sum(map_int(Cartas,length))==4){#Si tengo 2 A's, hay 3 resultados no perdedoes
        Inicio[[i]]<-c(1,2,11)
      }else{
        Inicio[[i]]<-Cartas[[1]]+Cartas[[2]]
      }
    }else{
      Inicio[[i]]<-Cartas[[1]]+Cartas[[2]]
    }
    
  }
  #Ya que se definieron las primeras apuestas, se procede con las estrategias
  
  
  
}


