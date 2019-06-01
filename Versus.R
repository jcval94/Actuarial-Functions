####Competir variables (discreta vs cont?nua)

Versus<-function(df,var1="",var2="",
                 minmax=NULL,plot=F,disc=T){

  #Si dsc == T entonces v<-table()... en lugar de numeros
  #Si cualquier variable tiene un solo valor pierde el entido y brea

  ifelse(length(unique(df[,var1]))>20,break(),"")
  #si es un factor volverla n?mero
  #ifelse(class(df[,var1]) | length(unique(df[,var1]))>50,,)
  #minmax=c(0,1)
  a<-df[,var2]
  ifelse(is.null(minmax),c(minimo<-min(a),maximo<-max(a))
         ,c(minimo<-minmax[1],maximo<-minmax[2]))

  if(length(unique(df[,var2]))>500){
    v<-as.data.frame(table(cut(df[,var2],seq(minimo, maximo, length.out=100),include.lowest = T),
                           df[,var1]))
  }
  else{
    v<-as.data.frame(table(df[,var2],df[,var1]))
  }

  if(length(unique(v$Var1))<=length(unique(v$Var2))){
    xt<-unique(v$Var1);var<-"Var1",vir<-"Var2"
  }else{
  xt<-unique(v$Var2);var<-"Var2",vir<-"Var1")
  }
  
  list<-list()
  #var = la variable con mens valores
  for (i in 1:length(xt)) {
    list[[i]]<-v[v[,var]==xt[i],]
  }
  #by = la variable con m?s valores
  J<-list[[1]]
  for (i in 1:(length(list)-1)){
    J<-left_join(J,list[[i+1]],by = vir)
  }
  sumas<-rowSums(J[,startsWith(names(J),"Freq")])
  porcent<-J
  proporciones<-J[,(startsWith(names(J),"Freq"))]/sumas
  for (y in 1:ncol(proporciones)){
    names(proporciones)[y] <- paste(var1,"=", unique(v$Var2)[y])
  }
  porcent<-cbind(porcent,proporciones)
  names(porcent)[1] <- paste("Intervalo",var2,"")


  if(plot==T){
    for (k in (ncol(porcent)-length(list)+1):ncol(porcent)) {
      win.graph()
      plot(porcent[,k],main = paste(var1,"vs",var2),ylab = names(porcent)[k],xlab = var2,col=floor(runif(1,10,30)))
      lines(porcent[,k])
    }
  }
  return(porcent)
}

data(iris)

Versus(iris,"Species","Sepal.Length")
