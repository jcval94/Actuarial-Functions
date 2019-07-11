plt<-plot(1:100,rnorm(100),col="skyblue3")
#1 negro
#2 rojo
#3 verde
#4 azul
#5 cyan
#6 magenta
#7 amarisho
#8 gris
#9 negro otra vez
#  colors() para mostrar el nombre del color

par(mfrow=c(1,2)) #mfcol para escribirlo en columnas, sirve para ordenarlas 
#en el orden que se deseé, por filas (row) co columnas(col)
plot(1:100,rnorm(100),col="skyblue3")
plot(1:100,rnorm(100),col="peru")
#par (mfcol=c(1,2))
a<-matrix(c(1,1,2,3))
a
a<-matrix(c(1,1,2,3),2,2)
a
a<-matrix(c(1,1,2,3),2,2,byrow=T)#by row es para columnas
a
layout(a)
layout.show()

plot(1:100,rnorm(100),col="skyblue3",main="Azul")
plot(1:100,rnorm(100),col="green",main="Verde")
plot(1:100,rnorm(100),col="red",main="Rojo")
plot(1:100,rnorm(100),col="yellow",main="Amarillo pollito")

hist(1:100,rnorm(100),col="skyblue3")


help("poisson") #muestra ayuda con distribuciones
??poisson

#ddist da la fn de densidad
#pdist da la fn de dist
#qdist da el cuantil de p bajo dist
b<-dpois(5,10) #= la probabilida de que (X=5) con X distribuido como Poisson(10)
b
a<-ppois(5,10) #= la probabilida de que (X<=5) con X distribuido como Poisson(10)
a
qpois(a,10) #funciona como una inversa al d pois

set.seed(24)
rpois(1,10)
mean(rpois(100,10)) ##100 poisones con media 10
var(rpois(1000,10)) ##la semilla se corre con cada enter
summary(rpois(1000,10))

ppois(12,10)
ppois(13,10)
ppois(12,10)
ppois(11,10)
win.graph()
plot(0:21,dpois(0:21,10),type="l",ylim=c(0,.15),
     xlab="Valores de X",ylab="Probabilidades",
     main="Densidad Pisson(10)",col="peru",lwd="2")
#type es para cambiar por puntos o lineas, lwd es para el grueso de linea
lines(0:21,dpois(0:21,7),col=2,lwd=2)
lines(0:21,dpois(0:21,14),col=5,lwd=2)



hist(rpois(5000,10))
#hist muestra una serie de puntos bien distribuidos conforme a la dis deseada

set.seed(50)
histograma<-hist(rpois(5000,10))
class(histograma)
mode(histograma)#lo que en el fondo es
histograma
histograma$counts
sum(histograma$counts)

hist(rpois(5000,10),breaks = 50)

#seq(0,20,length=21)

plot(1:21,dbinom(seq(0,20,length=21),25,.4),type='l',
     main="Binomial distribution changing n & p",
     col="green",lwd=2,ylab="",xlab="")
lines(1:21,dbinom(seq(0,20,length=21),50,.2),col="green2",lwd=2)
lines(1:21,dbinom(seq(0,20,length=21),100,.1),col="green3",lwd=2)
lines(1:21,dbinom(seq(0,20,length=21),400,.025),col="green4",lwd=2)
legend("topright",c("n=25,p=.4","n=50,p=.1","n=100,p=.025"),
       col=c("green","green2","green3","green4"),lwd=2)

#?actuar-package (muestra lo que hay en el paquete)

#l es el número de v.a's que quiero generar
#rpois(l,lambda)
#y así para lo demás

dat<-cbind.data.frame(dat,Clust=(Kmedias_compp$cluster))
Kmedias_compp$cluster
