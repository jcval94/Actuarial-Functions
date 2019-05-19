#Diferentes tipos de error, queda pendiente para que el usuario elija unar los quantiles o no
error<-function(X,MA,quantile=FALSE){
  if(quantile){
  qqX<-quantile(X,seq(0,100,by=1)/100)
  qqMA<-quantile(MA,seq(0,100,by=1)/100)
    }else{
    qqX<-X
    qqMA<-MA
  }
  #0 es mayor igualdad
  U<-sqrt(sum((qqX-qqMA)^2)/length(qqX))/(sqrt(sum(qqX^2)/length(qqX))+sqrt(sum(qqMA^2)/length(qqMA)))
  #0 es mayor igualdad en medias
  Um<-(mean(qqX)-mean(qqMA))/(sqrt(sum(qqX^2)/length(qqX))+sqrt(sum(qqMA^2)/length(qqMA)))
  #Nivel de correlación

  Us<-(sd(qqX)/sqrt(length(qqX))-sd(qqMA)/sqrt(length(qqMA)))/(sqrt(sum(qqX^2)/length(qqX))+sqrt(sum(qqMA^2)/length(qqMA)))

  #Nivel de correlaci?n
  Cor<-1-cor(qqMA,qqX)

  return(c(U,Um,Us,Cor))
}
