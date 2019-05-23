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
