
#¿Como construir la función de riesgo? Metodo Paramétrico 
par(mfrow=c(1,1))
hweibull=function(alfa,lambda,x)
{
alfa*lambda*(x^(alfa-1))
}
alfa=0.5
lambda=0.263282
alfa1=1
lambda1=0.1
alfa2=3.5
lambda2=0.00208
curve(hweibull(alfa,lambda,x),0,8,col="blue",
xlab="tiempo",ylab="Función de riesgo",ylim=c(0,0.3),lwd=2,
main=expression(paste("h(x)=",alpha,sep=" ",lambda,sep=" ",x^{alpha-1}))) 
curve(hweibull(alfa1,lambda1,x),0,8,add=TRUE,col="black",lwd=2)
curve(hweibull(alfa2,lambda2,x),0,8,add=TRUE,col="magenta",lwd=2)

sweibull=function(alfa,lambda,x)
{
exp(-lambda*x^alfa)
}
alfa=0.5
lambda=0.263282
alfa1=1
lambda1=0.1
alfa2=3.5
lambda2=0.00208
curve(sweibull(alfa,lambda,x),0,8,col="blue",
xlab="tiempo",ylab="Función de riesgo",ylim=c(0,0.3),lwd=2,
main=expression(paste("h(x)=",alpha,sep=" ",lambda,sep=" ",x^{alpha-1})),xlim=c(0,1)) 
curve(hweibull(alfa1,lambda1,x),0,8,add=TRUE,col="black",lwd=2)
curve(hweibull(alfa2,lambda2,x),0,8,add=TRUE,col="magenta",lwd=2)

title(sub="Función de riesgo")

leg.txt=c(expression(paste(alpha,"=",0.5,";",sep=" ",lambda,"=",0.26)),
expression(paste(alpha,"=",1,";",sep=" ",lambda,"=",0.01)),
expression(paste(alpha,"=",3.5,";",sep=" ",lambda,"=",0.002))) 
legend(5,0.3,leg.txt,col=c("blue","black","magenta"),
lty=c(1,1,1),bty='o',cex=0.8,lwd=2)      #bty='n' sin rectangulo

#¿Como se construye la función de supervivencia bajo KM?
library(splines)
library(survival)
data(package="survival")

datos=leukemia
help(leukemia)
# 0 : vivo (censurado)
# 1 : muerto (no censurado)
str(datos)
attach(datos)

#Estimador de KM
t=Surv(time,status) # crea un objeto de supervivencia
fsup=survfit(t~1) 
summary(fsup)
plot(fsup,lty=1:3,col=1:3,main=paste("Función de Supervivencia",sep="\n","kaplan-Meier"),ylab="Estimación KM",
xlab="tiempo",mark.time=TRUE,col.main="darkblue",col.lab="darkblue",col.axis="darkblue")
#mtext("kaplan",col="darkblue",font=4,cex=1.1)

#Grafica función de riesgo acumulado
plot(fsup,lty=c(1,2,2),col=c(2,3,3),fun="cumhaz",xlab="meses",ylab="Riesgo acumulado",mark.time=TRUE)

#Comparación de estimadores

KM=survfit(t~1,conf.int=F)
Nelson=survfit(t~1,type="fh2",conf.int=F) #estimador de Nelson-Aalen
#Nelson2=survfit(t~1,type="aalen",conf.int=F) #estimador de Nelson-Aalen
plot(KM,lty=1:3,col=1:3,main="Comparación de la estimación de Supervivencia",ylab="Estimación función de supervivencia",
xlab="tiempo",mark.time=TRUE)
lines(Nelson,lty=2,col="blue")
leg.txt=c("KM","Nelson-Aalen") 
legend("bottomleft",leg.txt,col=c("black","blue")
,bty='o',cex=0.8,lwd=2) 
# Ubicacion especifica de la leyenda
#bottom, bottomright, bottomleft, left, topleft, top, topright, right, center

#Intervalos de confianza (nivel de confianza)

Sup.int=survfit(t~1,conf.int=0.75)
plot(Sup.int,lty=c(1,2,2),col=c(2,3,3),main="Intervalos de Confianza",ylab="Estimación Func. Supervivencia",
xlab="tiempo",mark.time=TRUE)
lines(fsup,lty=c(1,3,3),col=c(2,4,4))

#Tipos de Intervalos

Sup.loglog=survfit(t~1,conf.int=0.90,conf.type="log-log")
Sup.normal=survfit(t~1,conf.int=0.90,conf.type="plain")

win.graph()
plot(survfit(t~1,conf.int=0.90,conf.type="plain"),lty=c(1,2,2),col=c(2,3,3),main="Tipos de intervalos",ylab="Estimación fun. supervivencia",
xlab="tiempo") #Intervalos verdes
lines(fsup,lty=c(1,3,3),col=c(2,4,4)) #Intervalos azules
lines(Sup.loglog,lty=c(1,4,4),col=c(2,6,6)) #Intervalos magenta

#Comparación de funciones de supervivencia
fsup2=survfit(t~x,data=datos) 
summary(fsup2)
plot(fsup2,col=c("blue","black"),mark.time=TRUE)
logrank=survdiff(t~x,data=datos,rho=0)
wilcoxon=survdiff(t~x,data=datos,rho=1)

#Tabla de vida
library(KMsurv)

tis=c(0,12, 24, 36, 48, 60,96,96)#Intervalos
nsubs=c(48,28,14,13,9,5,0)#Individuos en riesgo (nj)
nlost=c(4,4,0,1,2,1,0)#Individuos perdidos (cesurados cj)
nevent=c(16,10,1,3,2,4,0) #individuos q experimentaron el evento (dj)

tablavida=lifetab(tis, nsubs[1], nlost, nevent)
attributes(tablavida)
sup.tv=tablavida$surv
win.graph()
plot(sup.tv,type="s")
riesgo.tv=tablavida$hazard
plot(riesgo.tv,type="s")
riesgo.tv

x=c(0,tis)
y=c(sup.tv,0)
plot(x[1:8],y,type="s")

win.graph()
r=tis[1:length(tis)-1]
s=c(riesgo.tv[1:length(riesgo.tv)-1],riesgo.tv[length(riesgo.tv)-1])
plot(r,s,type="s")

-------------------------------------------------------------------
#Codigo para realizar la tabla de vida
----------------------------------------------------------------
datos=retinopathy

intervalo=seq(0,max(datos[,"futime"]),6)
expuestos=length(datos[,1])
fallas=subset(datos,datos[,"status"]==1)
f=0
for(i in 1:(length(intervalo)-1))
 { 
  f[i]=length(subset(fallas,fallas[,"futime"]<=intervalo[i+1]&fallas[,"futime"]>intervalo[i])[,1])
 } 
censuras=subset(datos,datos[,"status"]==0)
c=0
for(i in 1:(length(intervalo)-1))
 { 
  c[i]=length(subset(censuras,censuras[,"futime"]<=intervalo[i+1]&censuras[,"futime"]>intervalo[i])[,1])
 } 
library(KMsurv)

lifetab(intervalo,expuestos,c,f)












