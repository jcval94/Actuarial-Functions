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
                    estrategias=c(15,18,19)){
  #ñadimos la estrategia de la casa
  estrategias<-c(estrategias,18)

  No_jugadores<-length(apuestas)
  Juego<-Barajar(Mazo)

  Valor<-function(carta){
    valor<-as.numeric(substr(carta,2,nchar(carta)))
    if(valor>10){return(10)}
    if(valor==1){return(c(1,11))}
    return(valor)
  }

  win<-function(Ls){
    if(21 %in% Ls){return(T)}
    else{return(F)}
  }

  library(purrr)
  Valores<-purrr::map(Juego,Valor)

  Inicio<-ganancia<-sumas<-list()
  for(i in 1:(No_jugadores+1)){
    Cartas<-Valores[(i*2-1):((i*2))]
    #Si hay un As, tengo 2 resultados
    if(max(map_int(Cartas,length))>1){
      if(sum(map_int(Cartas,length))==4){#Si tengo 2 A's, hay 2 resultados no perdedoes
        Inicio[[i]]<-c(2,12)
      }else{
        Inicio[[i]]<-Cartas[[1]]+Cartas[[2]]
      }
    }else{
      Inicio[[i]]<-Cartas[[1]]+Cartas[[2]]
    }

  }
  Inicio
  #Ya que se definieron las primeras apuestas, se procede con las estrategias
  #Cartas usadas
  C_U<-(No_jugadores+1)*2
  fase<-function(estrategia,valores){
    Cartas_tomadas<-list()
    n<-0
    while(any(valores<estrategia) & !(21 %in% valores)){#seguir pidiendo
      n<-n+1
      Cartas_tomadas[[n]]<-Valores[[C_U]]
      valores<-valores+Valores[[C_U]]
      C_U<-C_U+1
    }
    return(list(valores,C_U,Cartas_tomadas))
  }

  for(i in 1:(No_jugadores+1)){

    FSS<-fase(estrategia = estrategias[i] ,
              valores = Inicio[[i]])
    sumas[[i]]<-FSS[[1]]
    C_U<-FSS[[2]]
    Avance[[i]]<-FSS[[3]]
  }
  #Veamos los resultados
  Ganadores_inicio<-map_lgl(Inicio,win)
  Ganadores<-map_lgl(sumas,win)
  #Si gana la casa, todos pierden o empatan
  if(Ganadores[length(Ganadores)]){
    Ganancia<--(!Ganadores)*c(apuestas,0)
    if(Ganadores_inicio[length(Ganadores)]){
      Ganancia<--(!Ganadores_inicio)*c(apuestas,0)
    }
  }else{
    CRUPIER_Suma<-sumas[[length(Ganadores)]]<22
    #Si el croupier pierde se les paga a aquellos que tengan un número < 22
    if(!any(CRUPIER_Suma)){
      Pagar<-map_lgl(sumas,~any(.x<22))
      Ganancia<-c(apuestas,0)*Pagar-c(apuestas,0)*!Pagar
    }else{#Si no, se le paga a aquellos que tengan > al croupier
      #Tomamos el máximo de los valoes menores a 22
      CS<-sumas[[length(Ganadores)]]
      VC<-max(CS[CS<22])
      Pagar<-map_lgl(sumas,~any(.x>VC & .x<22))
      Ganancia<-c(apuestas,0)*Pagar-c(apuestas,0)*!Pagar
    }
  }
  Ganancia<-Ganancia*ifelse(Ganadores_inicio,1.5,1)
  return(list(Ganancia[1:No_jugadores],Inicio,sumas,Avance))
}
set.seed(12)
Res<-BlackJack(apuestas = c(1,1,1,1),estrategias = c(18,18,18,18))
Res2<-BlackJack(apuestas = c(1,1,1,1),estrategias = c(18,18,18,18))
Res3<-BlackJack(apuestas = c(1,1,1,1),estrategias = c(18,18,18,18))

#Simularemos 1000 partidas con 3 jugadores de 1 apuesta

Res<-map(1:1000,~BlackJack(apuestas = c(1,1,1))[[1]])

#Ganancia del casino
G_Casino<-as.numeric(map(Res,~-sum(.x)))

summary(G_Casino)
sum(G_Casino)
