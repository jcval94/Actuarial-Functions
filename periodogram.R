periodicidad<-function(df,place=10){
  #df es una serie de tiempo ordenada
  #Hacer el proceso máx 3 veces
  ddT<-data.frame(freq=c(),spec=c(),orden=c())
  ords<-floor(length(df)*.3):length(df)
  for(i in 1:2){
    for(lu in ords){
      if(i==1){
        #Se identifica estacionalidad semanal si la hay
        p<-TSA::periodogram(df[1:lu],plot=F)
      }
      else if (i==2){
        p<-TSA::periodogram(df[(lu+1-min(ords)):length(df)],plot=F)
      }
      #spectrum(ts.anual,log="no")
      dds<- data.frame(freq=1/p$freq, spec=p$spec,orden=1:length(p$spec))
      dds<-head(dds[order(-dds$spec),],place)
      #si es la primera la omitimos porque es evidente la estacionalidad del total
      #Aqu? descartamos aquellas frecuencias que no alcanan el 6.5% de la freq m?xima
      ddT<-rbind(ddT,dds)
      #Regresa los datos agrupados conforme a las primeras 5 frecuencias
    }
  }
  
  ddT<-ddT[order(-ddT$spec),]
  Maxi<-max(ddT$spec)
  #Una periodicidad menor a 3 obs es sospechosa para un periodo anual
  ddT<-ddT[ddT$orden>2,]
  ddT<-head(ddT,20)
  ddT$Freq_Orden<-paste0(ddT$freq,"_",ddT$orden)
  ddT<-suppressWarnings(dcast(ddT,Freq_Orden~.,max,value.var="spec"))
  ddT$.<-ddT$./Maxi
  ddT<-ddT[order(-ddT$.),]
  
  return(list(unique(as.numeric(do.call("rbind",strsplit(ddT$Freq_Orden,"_"))[,1])),ddT))
}
