## Indicamos el directorio en el que vamos a trabajar

## setwd (set work direction)  cambia el directio al indicado
setwd("C:/Users/cody8/Desktop/Documentos/Escuela/9no/Teoria")

## Los insumos para el c�lculo del BEL para el modelo estaturio son:

# Prima emitida de los �ltimos 8 a�os.
# Tri�ngulo de Siniestros por a�o p�liza y retraso


PE<-read.table("Insumos/Ejemplo/PE_GM_ej.csv",sep=",",header=T)
TS<-read.table("Insumos/Ejemplo/TS_GM_ej.csv",sep=",",header=T)

# La funci�n read.table guarda un arreglo como un objeto de clase data.frame
# Los data frame son matrices arreglados como tablas, cada columna de la matriz 
# corresponde a una variable y las variables estan asociadas por rengl�n
class(PE)
# PE[1] Corresponde al a�o
PE[1]
# PE[2] a la prima
PE[2]
#Tambien se puede llamar la variable por su nombre
PE$Prima..emitida
# En el primer caso, la varible se obtiene como data.frame, y en el segundo, como vector
class(PE[2])
class(PE$Prima..emitida)
# Creamos una Tabla con los factores de siniestralidad, 
fsin<-TS[,2:9]/PE[,2]
class(fsin)
# Cada fila de siniestros esta dividida por la prima correspondiente
# R aplica la misma divisi�n entrada por entrada para cada columna

## Completar el tri�ngulo con Muestreo Simple (Bootstrap)

fsinc<-cbind(fsin[,1],fsin[,2],fsin[,3],fsin[,4],fsin[,5],fsin[,6],fsin[,7],fsin[,8])
class(fsinc)
fsin[1:8]
class(fsin[,1:8])
# "fsinc" es una matriz a diferencia de tomar los datos fsin que se conserva como 
# data frame

# Para completar el tri�ngulo haremos muestreo de los factores
# Ejemplo de Muestreo: Indicamos una secuencia
1:10
# Si queremos un n�mero entre 1:10 usamos sample y pedimos una observaci�n
sample(1:10,1)

# Ahora veremos la importancia de utilizar la clase adecuada de los objetos en R
# Hagamos una muestra sobre fsin
sample(fsin,1)
# Como fsin es un data.frame tiene como elementos las variables de la tabla por lo que al
# ejecutar una muestra de tama�o 1 nos devuelve toda la variable
sample(fsinc,1)
# fsinc es de clase matriz por lo que al muestrear sobre este objeto si obtenemos 1 factor
# Completemos la matriz fsinc con muestraz de los factores de la columna
fsinc

i<-2   #Indice, corresponde a la primer columna que hay que completar
for(i in 2:length(fsinc[1,]))    # Con length(fsinc[1,]) obtenenmos el n�mero de col de fsinc
{
  j<-2
  for(j in 2:i)
  {
    fsinc[(8-(i-j)),i]<-sample(fsinc[(1:(8-(i-1))),i],1)
  }      
}
#Borramos los �ndices
remove(i,j)

# Para obtener los montos de Siniestros basta multiplicar la matriz por las primas

fsinc*PE$Prima..emitida
sin<-fsinc*PE$Prima..emitida
# Para validar que el triangulo superior se manntiene igual restamos las matrices y sumamos las diferencias
sum(sin-TS[,2:9])
# El argumento na.rm=T permite realizar la operaci�n ignorando los "NA"
sum(sin-TS[,2:9],na.rm=T)

# Creemos un par de vectores para guardar los factores de Siniestralidad tanto de RRC como de INBR

frrc<-vector(mode="numeric")
fsonr<-vector(mode="numeric")
frrc

# Ambos se encuentran vacios

i<-1
for(i in 1:length(sin[,1]))
{
  fsonr[i]<-sum(sin[i,(2:length(sin[1]))]/PE$Prima..emitida[i])
  frrc[i]<-sum(sin[i,])/PE$Prima..emitida[i]
}
remove(i)
# La media de n-vectores simulados corresponder�a a los �ndices de Siniestralidad �ltima de 
# cada reserva.
# Para la simulaci�n que hicimos los �ndices ser�an

mean(frrc)    #La funci�n mean regresa el promedio del argumento que se alimenta
mean(fsonr)

### En r se pueden guardar procedimientos en funciones propias para solo mandar llamar despues

## Ejemplo promedio ##

# Argumentos: a corresponde a un conjunto de valores sobre los cuales se desea obtener el promedio

promedio<-function(a)   # Se declara la funci�n y sus argumentos
{
p<-sum(a)/length(a)
return(p)               # Se indica el resultado que debe devolver la funci�n 
}

# Validamos que la funci�n que creamos (promedio) es igual a la funci�n precargada mean
promedio(1:10)
mean(1:10)
remove(promedio)
###################################################################################################

## Integremos el proceso de muestro simple en una funci�n ##

# los insumos que requerimos son la Prima emitida y el triangulo de siniestros

## setwd (set work direction)  cambia el directio al indicado
setwd("C:/Users/h_barragan/Documents/Curso R/")

## Los insumos para el c�lculo del BEL para el modelo estaturio son:

# Prima emitida de los �ltimos 8 a�os.
# Tri�ngulo de Siniestros por a�o p�liza y retraso

PE<-read.table("Insumos/Ejemplo/PE_GM_ej.csv",sep=",",header=T)
TS<-read.table("Insumos/Ejemplo/TS_GM_ej.csv",sep=",",header=T)

PEMI<-PE$Prima..emitida
SIN<-TS[,2:9]

Ind.Sin<-function(PEMI,SIN)
{
  fsin<-SIN/PEMI
  fsinc<-cbind(fsin[,1],fsin[,2],fsin[,3],fsin[,4],fsin[,5],fsin[,6],fsin[,7],fsin[,8])
  i<-2   
  for(i in 2:length(fsinc[1,]))   
  {
    j<-2
    for(j in 2:i)
    {
      fsinc[(8-(i-j)),i]<-sample(fsinc[(1:(8-(i-1))),i],1)
    }      
  }
   remove(i,j)
  sin<-fsinc*PEMI
  frrc<-vector(mode="numeric")
  fsonr<-vector(mode="numeric")
  fdev<-vector(mode="numeric")
  i<-1
  for(i in 1:length(sin[,1]))
  {
    fsonr[i]<-sum(sin[i,(2:length(sin[1]))]/PEMI[i])
    frrc[i]<-sum(sin[i,])/PEMI[i]
  }
  remove(i)
  i<-1
  for(i in 1:length(sin[1,]))
  {
        fdev[i]<-sum(sin[,i:length(sin[1,])])/sum(sin)
  }
  res<-list("Factores de Siniestralidad RRC"=frrc,"Factores de Siniestralidad SONR"=fsonr,"Factores de Devengamiento"=fdev)
  return(res)
}

# Ejecutamos la funci�n

Ind.Sin(PEMI,SIN)

# Si conocemos los elementos que regresa una funci�n podemos indicar que nos devuelva solo 
# el elemento que es de inter�s.

Ind.Sin(PEMI,SIN)$`Factores de Siniestralidad SONR`

## Ahora creemos un programa para simular muchas veces y obtener los factores

## Creamos la carpeta de Salidas para guardar las simulaciones

Ind.BEL<-function(nsim=100,PEMI,SIN)  ## Podemos asiganar valores por default en los argumentos
{
  FSRRC<-vector(mode="numeric")
  FSSONR<-vector(mode="numeric")
  FDEV<-matrix(,nrow=nsim,ncol=length(SIN[1,]))
  fdev<-vector(mode="numeric")            
  i<-1             
  for(i in 1:nsim)
  {
    a<-Ind.Sin(PEMI,SIN)
    FSRRC<-c(FSRRC,a$`Factores de Siniestralidad RRC`)
    FSSONR<-c(FSSONR,a$`Factores de Siniestralidad SONR`)
    FDEV[i,]<-a$`Factores de Devengamiento`
  }  
  j<-1
  for(j in 1:length(FDEV[1,]))
  {
    fdev[j]<-mean(FDEV[,j])
  }         
  write.csv(FSRRC,"Salidas/FSrrc.csv")
  write.csv(FSSONR,"Salidas/FSsonr.csv")
  write.csv(FDEV,"Salidas/Fdev.csv")             
  res<-list("FBELRRC"=mean(FSRRC),"FBELSONR"=mean(FSSONR),"Factores de Devengamiento"=fdev)
  return(res)
}

# Ejecutamos la funci�n con 1000 simulaciones

inic<-proc.time()
Prueba<-Ind.BEL(1000,PEMI,SIN)
proc.time()-inic

# Analizemos graficamente los resultados

FSRRC<-read.table("Salidas/FSrrc.csv",sep=",",header=T)
FSSONR<-read.table("Salidas/FSsonr.csv",sep=",",header=T)

# Para graficar lso factores simulados usamos plot
plot(FSRRC$x)
# Agragamos color
plot(FSRRC$x,col="blue3")
# Se tienen 8000 observaciones ya que cada sim regresa el factor para los 8 peri�dos de an�lisis
# Agrgemos un color para los factores de cada a�o
plot(FSRRC$x,col=c("black","grey15","grey20","blue4","skyblue","chartreuse4","coral4","cyan4"))
# Se percibe que para algunos periodos el factor es muy estable, y lo deber�a de ser, los factores del 
# a�o mas antiguo siempre son los mismos
# Para ver este efecto utilizaresmos la funci�n %% que nos regeresa el m�dulo de un n�mero respecto a otro
# Y la funci�n which que nos regresara los valores que cumplen una condici�n
# Usaremos la fuci�n summary para no desplegar todos los valores,
# summary nos regresa el valor max, min y los cuartiles
summary(FSRRC$x[which(FSRRC$X%%8==1)])
hist(FSRRC$x[which(FSRRC$X%%8==1)])
# En el ejemplo estamos obteniendo todos los factores derivados de la prima del periodo m�s antiguo (2008)
plot(FSRRC$x[which(FSRRC$X%%8==1)],ylim=c(.3,.45))
# Tanto en el resumen como en la gr�fica confirmamos que el valor no cambia para ese grupo de factores
# Veamos ahora el del resto de los a�os, con points gr�ficamo sobre la ventana activa
points(FSRRC$x[which(FSRRC$X%%8==2)],col="grey15")
points(FSRRC$x[which(FSRRC$X%%8==3)],col="grey20")
points(FSRRC$x[which(FSRRC$X%%8==4)],col="blue4")         # 2011
points(FSRRC$x[which(FSRRC$X%%8==5)],col="skyblue")       # 2012
points(FSRRC$x[which(FSRRC$X%%8==6)],col="chartreuse4")
points(FSRRC$x[which(FSRRC$X%%8==7)],col="coral3")
points(FSRRC$x[which(FSRRC$X%%8==0)],col="cyan4")         # 2015
# Definamos los colores usados en un vector
mysetcol<-c("black","grey15","grey20","blue4","skyblue","chartreuse4","coral4","cyan4")
# Podemos modificar las caracter�sticas de la gr�fica con los par�metros de la funci�n plot
plot(FSRRC$x,col=mysetcol,ylim=c(.3,.6), xlab="Simulaciones",ylab="Factores de Siniestralidad",main="Simulaci�n con muestreo simple")
legend(1850,.64,PE$A�o.P�liza,bty="n",col=mysetcol,lwd=2,lty=rep(1,8),ncol=3)
# Para ver rapidamente la convergencia de los factores graficamos el promedio actualizando en cada simulaci�n
belsimrrc<-vector(mode="numeric")
belsimsonr<-vector(mode="numeric")
j<-1
for(j in 1:1000)
{
  belsimrrc[j]<-mean(FSRRC$x[1:(j*8)])
  belsimsonr[j]<-mean(FSSONR$x[1:(j*8)])
}
remove(j)
# Grafiquemos los factores seleccionados y la convergencia de los promedios
# Cremos una ventana con 4 gr�ficas
par(mfrow=c(2,2))

plot(FSRRC$x,col="mediumturquoise",main="Factores de Siniestralidad RRC",xlab="Simulaci�n",ylab="Factor de Sinietralidad")
plot(belsimrrc,col="blue4",main="BEL RRC",xlab="Simulaci�n",ylab="Factor de Sinietralidad")
plot(FSSONR$x,col="olivedrab3",main="Factores de Siniestralidad SONR",xlab="Simulaci�n",ylab="Factor de Sinietralidad")
plot(belsimsonr,col="limegreen",main="BEL SONR",xlab="Simulaci�n",ylab="Factor de Sinietralidad")




