## Indicamos el directorio en el que vamos a trabajar

## setwd (set work direction)  cambia el directio al indicado
setwd("C:/Users/cody8/Desktop/Documentos/Escuela/9no/Teoria")

## Los insumos para el cálculo del BEL para el modelo estaturio son:

# Prima emitida de los últimos 8 años.
# Triángulo de Siniestros por año póliza y retraso

PE<-read.table("Insumos/Ejemplo/PE_GM_ej.csv",sep=",",header=T)
TS<-read.table("Insumos/Ejemplo/TS_GM_ej.csv",sep=",",header=T)

# La función read.table guarda un arreglo como un objeto de clase data.frame
# Los data frame son matrices arreglados como tablas, cada columna de la matriz 
# corresponde a una variable y las variables estan asociadas por renglón
class(PE)
# PE[1] Corresponde al año
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
# R aplica la misma división entrada por entrada para cada columna

## Completar el triángulo con Muestreo Simple (Bootstrap)

fsinc<-cbind(fsin[,1],fsin[,2],fsin[,3],fsin[,4],fsin[,5],fsin[,6],fsin[,7],fsin[,8])
class(fsinc)
fsin[1:8]
class(fsin[,1:8])
# "fsinc" es una matriz a diferencia de tomar los datos fsin que se conserva como 
# data frame

# Para completar el triángulo haremos muestreo de los factores
# Ejemplo de Muestreo: Indicamos una secuencia
1:10
# Si queremos un número entre 1:10 usamos sample y pedimos una observación
sample(1:10,1)

# Ahora veremos la importancia de utilizar la clase adecuada de los objetos en R
# Hagamos una muestra sobre fsin
sample(fsin,1)
# Como fsin es un data.frame tiene como elementos las variables de la tabla por lo que al
# ejecutar una muestra de tamaño 1 nos devuelve toda la variable
sample(fsinc,1)
# fsinc es de clase matriz por lo que al muestrear sobre este objeto si obtenemos 1 factor
# Para garantizar que se puedan replicar los valores podemos establecer una semilla 
set.seed(26)
sample(fsinc,1)

#Completamos la matriz
for (i in 2:length(fsinc[1,]))
{
 j<-2
 for(j in 2:i)
{
fsinc[(8-(i-j)),i]<-sample(fsinc[(1:(8-(i-j))),1],1)
}
}
#Borramos los índices
remove(i,j)

fsinc*PE$Prima..emitida
sin<-fsinc*PE$Prima..emitida

sum(sin-TS[,2:9])

sum(sin-TS[,2:9],na.rm=T)


fsonr<-vector(mode="numeric")
frrc

i<-1

for(i in 1:length(sin[,1]))
{
fsonr[i]<-sum(sin[i,(2:length(sin
frrc[i]<-
}

remove(i)

mean(frrc)
mean(fsonr)
}

#apoyo en materias de seguros y fianzas jeje

