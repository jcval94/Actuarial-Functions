#Vencer al azar p1.
ruleta<-function(apuestas=1,posiciones=3){
  
  if(any(posiciones>37)){warning("Must be < 38");return(NULL)}
  
  if(length(apuestas)!=length(posiciones)){
    apuestas<-rep(apuestas,length(posiciones))
  }
  rnd        <- floor(runif(1,0,1)*37)+1
  vectorTF   <- rnd == posiciones
  if(rnd==1){
    return(0)
  }
  if(any(vectorTF)){
    return(apuestas[vectorTF]*36)
  }else{
    return(0)
  }
}

#Hagamos 1000 apuestas de 1$ a la mitad de la ruleta
Resultados<-c()
Inversion<-c()
for(i in 1:1000){
  apuestas<-1
  posiciones = c(1:18)
  Inversion[i]<-apuestas*length(posiciones)
  Resultados[i]<-ruleta(apuestas,posiciones)
}

Ganancia<-sum(Resultados)-sum(Inversion)
print(Ganancia)

########################################################################
#Es claro que siempre pierdo, qué forma hay de ganar?
#Cada vez que pierdo duplico *mi apuesta *el número de casillas

Resultados<-c()
Inversion<-c()
Apuestas<-c()
apuestas<-1
for(i in 1:1000){
  
  posiciones = c(1:18)
  
  Inversion[i]<-apuestas*length(posiciones)
  Resultados[i]<-ruleta(apuestas,posiciones)
  if(Resultados[i]==0){
    apuestas<-apuestas*2
  }else{
    apuestas<-1
  }
  Apuestas[i]<-apuestas
}

Ganancia<-sum(Resultados-Inversion)
print(Ganancia)

#Veamos algunos resultados:
summary(Resultados)
summary(Inversion)


