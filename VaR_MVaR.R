library(dplyr)
library(plyr)

Acciones<-list()
df<-read.csv("C:/Users/jose.carlos.del.val1/Downloads/Datos históricos GAPB.csv",header=T)
df2<-read.csv("C:/Users/jose.carlos.del.val1/Downloads/Datos históricos BIMBOA.csv",header=T)
df3<-read.csv("C:/Users/jose.carlos.del.val1/Downloads/Datos históricos AMX.csv",header=T)

#días al pasado #lo hago en caso de que no haya fechas ordenadas en el cruce de tablas
df<-cbind(df,Dpast=seq(0,nrow(df)-1,1))

df<-df[1:(nrow(df)-2),]
df2<-df2[1:(nrow(df2)-2),]
df3<-df3[1:(nrow(df3)-2),]

Acciones[[1]]<-df
Acciones[[2]]<-df2
Acciones[[3]]<-df3

#View(df1)
Vol_Hoy<-c()
for (w in 1:length(Acciones)){
df1<-Acciones[[w]]
#Eliminar el -2, depende de como vengan los datos
df1<-df1[1:(nrow(df1)),]
#Viendo sólo un año
df1<-df1[1:260,]
names(df1)[7]<-"Porcent"

cat("Acción: ",w,"Media: ", mean(as.numeric(df1$Porcent)),"\n")
cat("Acción: ",w,"Var: ",var((df1$Porcent)),"\n")
#Cuidado al tomarlos, siempre ver el tipo
ifelse (class(df1$Cierre) == "factor",An<-as.numeric(as.character(df1$Cierre)),An<-as.numeric(df1$Cierre))

cambio_precioAn<-c()
for (i in 1:(length(An)-1)){
cambio_precioAn[i]<-log(An[i+1]/An[i])
}

Volatilidad<-c()#Mensual
for (i in 1:length(cambio_precioAn)){
Volatilidad[i]<-sd(cambio_precioAn[seq(i:i+22)])*sqrt(260)
}
#viendo 22 días (un mes) la volatilidad y 260 al anualizarlo

#Volatilidad
dfVol<-data.frame(Fecha=df1$ï..Fecha[1:length(Volatilidad)],Volatilidad=Volatilidad)

print(dfVol[1:8,])

Vol_Hoy[w]<-dfVol$Volatilidad[1]

K<-1
An<-An[seq(1+(K-1)*(260):(K)*(260))]

#Var
Prec<-c()
for (i in 2:length(An)){
Prec[i-1]<-(An[i-1]-An[i])/An[i]*100
}

hist(Prec)
hist(df1$Porcent)

VaR<-data.frame()
for (i in 1:8){
VaR<-rbind(VaR,data.frame(Dia_Fecha_VaR=as.character(df1$ï..Fecha[i]),
Dif_df=as.numeric(quantile(na.omit(Prec[i:260]),probs=.05)),
Dif_num=as.numeric(quantile(na.omit(df1$Porcent[i:260]),probs=.05)))
)
}
print(VaR)

}
#Covar & Correl
cru<-merge(df,df2,by=names(df)[1])
cru<-merge(cru,df3,by=names(df)[1])
#Habilitar si deseo ver una matriz de montos
#cru<-data.frame(A1=as.numeric(cru$Cierre.x),
# A2=as.numeric(cru$Cierre.y),
# A3=as.numeric(cru$Cierre))#Fecha=cru$ï..Fecha

cru<-data.frame(A1=as.numeric(cru$X..var..x),
A2=as.numeric(cru$X..var..y),
A3=as.numeric(cru$X..var.))#Fecha=cru$ï..Fecha

cov(cru)
cor(cru)

View(cru)

#Calcular MVar Óptimo

#Montos iniciales

MA1<-538568384 #GAP 
MA2<-998944128 #AMX
MA3<-962224064 ##BIMBO

#Volatillidades por Acción

VA1<-Vol_Hoy[1]
VA2<-Vol_Hoy[2]
VA3<-Vol_Hoy[3]

VA2
#Matriz de correlacion

MCorrel<-as.matrix.data.frame(cov(cru))

##----------------------------------------------------
##Ej Clase
set.seed(68)
z95<-quantile(rnorm(1000000),0.95)

V<-matrix(c(0.06^2,0,0,.14^2),2,2)
w<-matrix(c(4,2),1,2)
wt<-t(w)

sigmp2<-w%*%V%*%wt

MVaR<-z95*V%*%wt/sqrt(as.numeric(sigmp2))

#Cambio mis proporciones a 5 y 1, entonces
CompVaR<-as.numeric(matrix(c(1,-1),1,2)%*%MVaR)

#Mi objetivo es que sean iguales todas las entradas en la matriz MVaR
V<-matrix(c(0.06^2,0,0,.14^2),2,2)
w<-matrix(c(5,1),1,2)
wt<-t(w)
sigmp2<-w%*%V%*%wt
MVaR2<-z95*V%*%wt/sqrt(as.numeric(sigmp2))
MVaR2

##Hora de optimizar (método para 2 activos)
V<-matrix(c(0.06^2,0,0,.14^2),2,2)

for (i in seq(0.5,5.5,length.out = 100)){
q<-6-i
w<-matrix(c(i,q),1,2)
wt<-t(w)
sigmp2<-w%*%V%*%wt
MVaR2<-z95*V%*%wt/sqrt(as.numeric(sigmp2))
if(abs(MVaR2[1,1]-MVaR2[2,1])<.03){
cat(MVaR2[1,1]-MVaR2[2,1], " ",i," ",q,"\n")
}
}
#Segundo nivel de aproximación
for (i in seq(5.045455,5.09596,length.out = 100)){
q<-6-i
w<-matrix(c(i,q),1,2)
wt<-t(w)
sigmp2<-w%*%V%*%wt
MVaR2<-z95*V%*%wt/sqrt(as.numeric(sigmp2))
if(abs(MVaR2[1,1]-MVaR2[2,1])<.0005){
cat(MVaR2[1,1]-MVaR2[2,1], " ",i," ",q,"\n")
}
}

#Nuevos montos Óptimos: 5.068922 y 0.931078

V<-matrix(c(0.06^2,0,0,.14^2),2,2)
w<-matrix(c(5.068922,0.931078),1,2)
wt<-t(w)
sigmp2<-w%*%V%*%wt
MVaR2<-z95*V%*%wt/sqrt(as.numeric(sigmp2))
MVaR2

w<-matrix(c(5.068922,0.931078,4,5),1,4)
i<-5.5
###-----------------------------------------------------------------
#Para una atriz de n*n
#w es la matriz de montos
#V es la matriz de var/covar

MVaR_f<-function(w,V){
ifelse(class(w)!="matrix" | class(V)!="matrix",c(print("Debes ingresar matrices"),break()),"")
wt<-t(w)
sigmp2<-w%*%V%*%wt
MVaR2<-z95*V%*%wt/sqrt(as.numeric(sigmp2))

##Optimización del proceso 
#Criterio: el máximo y el mínimo valor en MVaR debe ser igual por al menos .05
#la desviación de X debe ser <.1
#...
suma<-sum(w)
for (i in seq(0,suma,length.out = 100)){
#necesito n-1 variables que vayan "corriendo"
w_vector<-c()
#desventaja: os valores intermedios son aleatorios, no toma en cuenta la cov
#ventaja: para dimensiones grandes de w es muy rápido
#entre más dimensiones se recomineda usar un length.out muy grande
for (k in 1:length(w)){
ifelse(k==length(w),w_vector[k]<-suma-sum(w_vector),
ifelse(k==1,w_vector[k]<-i,
w_vector[k]<-runif(1,0,suma-sum(w_vector))))
}

w<-matrix(w_vector,1,length(w_vector))
wt<-t(w)
sigmp2<-w%*%V%*%wt
MVaR2<-z95*V%*%wt/sqrt(as.numeric(sigmp2))

if(abs(max(MVaR2)-min(MVaR2))<.05){
cat(MVaR2[1,1]-MVaR2[2,1], " ",i," ",q,"\n")
}
}
return(list(as.numeric(sigmp2),MVaR2))
}

MVaR_f(w,V)
