VA_Comp<-function(fn_freq,fn_Siniest,simulaciones=10000){

  Gen_RFUN<-function(fn_freq){
    Cte<-do.call("rbind",strsplit(fn_freq,"[(]"))
    fn_f<-get(paste0("r",as.character(Cte[,1])))
    prms_f<-as.numeric(do.call("c",strsplit(gsub("[)]","",Cte[,2:ncol(Cte)]),"[,]")))
    #Reformamos los par?metros de la fn de frecuencia
    Nuevos_param_f<-as.list(c(simulaciones,prms_f))
    #Si no son n?meros warning
    if(!class(c(simulaciones,prms_f)) %in% "numeric"){
      warning("no son n?meros los par?metros ingresados")
    }

    for (i in 1:length(Nuevos_param_f)){
      formals(fn_f)[i]<-Nuevos_param_f[i]
    }
    FF<-try(fn_f(),silent=T)
    if(assertthat::is.error(FF)){
      warning("par?metros mal definidos");break()
    }
    return(list(FF,fn_f))
  }

  #Generaci?n de va's
  Freq<-Gen_RFUN(fn_freq)[[1]]
  Sev<-Gen_RFUN(fn_Siniest)[[2]]

  #Simulaci?n
  Suma_S<-purrr::map_dbl(Freq,~sum((Sev(n=.x))))
  #Resultados con truncamiento
  data.frame(Num_incidentes=Freq,Monto_Reclamos=ifelse(Suma_S<0,0,Suma_S))
}

freq<-"pois(972)"
#Si
siniest<-"weibull(138,2)"

DD<-VA_Comp(freq,siniest,500)
