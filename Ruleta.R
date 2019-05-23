
##No basta con duplicar, deben elegirse las posiciones correctas o volverás a perder

Resultados<-c()
Inversion<-c()
Apuestas<-c()
apuestas<-1
for(i in 1:1000){
  
  posiciones = c(1:31)
  
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

#Previo a la generalización del problema, funcionalicemos el proceso con sus resultados

Ruleta<-function(Juegos_jugados = 1000,posiciones = 18,
                 apuesta_inicial = 1,
                 fn_fracaso=function(x){x*2}){
  
  posiciones<-1:posiciones
  
  Resultados<-c()
  Inversion<-c()
  Apuestas<-c()
  apuestas<-apuesta_inicial
  for(i in 1:Juegos_jugados){
    
    Inversion[i]<-apuestas*length(posiciones)
    Resultados[i]<-ruleta(apuestas,posiciones)
    if(Resultados[i]==0){
      #Lo redondearemos siempre a 2 decimales
      apuestas<-round(fn_fracaso(apuestas),2)
    }else{
      apuestas<-apuesta_inicial
    }
    Apuestas[i]<-apuestas
  }
  
  Ganancia<-(Resultados-Inversion)
  
  cum_G<-cumsum(Ganancia)
  
  list(Ganancia=list(Ganancia,c(summary(Ganancia),sum(Ganancia))),
       Apuestas=list(Apuestas,c(summary(Apuestas),sum(Apuestas))),
       Inversion=list(Inversion,c(summary(Inversion),sum(Inversion))),
       Ganancia_Acum=list(cum_G,c(summary(cum_G),tail(cum_G,1))),
       Resultados=Resultados)
  
}

#Probemos con 5 centavos
#Simulemos 100 juegos
set.seed(12345)
Rul<-Ruleta(Juegos_jugados = 100,apuesta_inicial = .05)

#Ganancia
Rul[[1]][[2]]
#Máx. inversion
Rul[[3]][[2]]
#Max. inversión acumulada
Rul[[4]][[2]]

plot(Rul[[4]][[1]])
########################################
##############Escenarios################
########################################
#Vamos a 300 apuestas con posiciones de la 1 a la 22
set.seed(12345)
Rul<-Ruleta(Juegos_jugados = 300,apuesta_inicial = .05,posiciones = c(22))

#Ganancia
Rul[[1]][[2]]
#Máx. inversion
Rul[[3]][[2]]
#Max. inversión acumulada
Rul[[4]][[2]]

plot(Rul[[4]][[1]])

##################################
#Vamos a cambiar la forma de juego
Rul<-Ruleta(Juegos_jugados = 100,apuesta_inicial = .05,
            posiciones = c(2),fn_fracaso = function(x){if(x>1){x+2}else{x*2}})

#Ganancia
Rul[[1]][[2]]
#Máx. inversion
Rul[[3]][[2]]
#Max. inversión acumulada
Rul[[4]][[2]]

plot(Rul[[4]][[1]])

##################################
#60 juegos apostando la mitad pero multiplicando por 1.5 cada vez que perdemos
#Encontrando una fn óptima
Rul<-Ruleta(Juegos_jugados = 60,apuesta_inicial = .05,
            posiciones = c(5),fn_fracaso = function(x){x*1.5})
#Ganancia
Rul[[1]][[2]][7]
#Máx. inversion
Rul[[3]][[2]][6]
#Max. inversión acumulada
Rul[[4]][[2]]

plot(Rul[[4]][[1]])


##################################
#Nueva forma:
Rul<-Ruleta(Juegos_jugados = 45,apuesta_inicial = .1,
            posiciones = c(20),fn_fracaso = function(x){x*1.5})
#Ganancia
Rul[[1]][[2]][7]
#Máx. inversion
Rul[[3]][[2]][6]
#Max. inversión acumulada
Rul[[4]][[2]]

plot(Rul[[4]][[1]])

