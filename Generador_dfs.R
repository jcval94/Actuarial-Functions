#' Generador de data frames dummies
#'
#' @param df tabla de datos
#' @param records numero de registros esperado
#'
#' @return
#' @export
#' 
#' @importFrom purrr map_df
#'
#' @examples
#' 
#' 
#' df<-data.frame(Flag=1,
#' Table="PDT",
#' Var_Nm="Sesion_Id",
#' Var_Type="Char",
#' Var_Subtype="",
#' Desc="Identificador de la sesión")
#' 
#' df<-read.table("C:/Users/Robotics3/Documents/tabla1.txt",
#'                sep = "\t",
#'                header=T)
#' 
#' Generador(df)
#' 
#' 
Generador<-function(df,records=5000){
  nomb<-names(df)
  fact_as_string <- function(df) {
    f_a_s <- function(X) {
      if (class(X) %in% c("factor")) {
        X <- as.character(X)
      }
      X
    }
    purrr::map_df(df,f_a_s)
  }
  df<-fact_as_string(df)
  formato<-as.character(df[["Var_Type"]])
  subformato<-as.character(df[["Var_Subtype"]])
  campo<-as.character(df[["Var_Nm"]])
  has_id<-grepl("ID",toupper(campo))
  campo_id<-gsub("ID","",
                 gsub("id","",
                      gsub("Id","",
                           gsub("iD","",campo))))
  df_r<-data.frame()
  
  #Generamos las Columnas ID según el tipo
  Col<-data.frame(1:records)[,-1]
  for(i in 1:nrow(df)){
    if(toupper(formato[i])%in% c("CHAR","STRING","STR")){
      if(has_id[i]){
        Col[[campo[i]]]<-paste0(campo_id[i],1:records)
      }else{
        pl<-records/length(LETTERS)
        if(pl-floor(pl)==0){
          ext<-c()
        }else{
          ext<-LETTERS[1:((pl-floor(pl))*length(LETTERS))]
        }
        Col[[campo[i]]]<-c(rep(LETTERS,floor(pl)),ext)
      }
    }
    if(toupper(formato[i])%in% c("NUMBER","NUM","NUMERIC")){
      if(toupper(subformato[i]) %in% c("DATE","DATETIME")){
        Col[[campo[i]]]<-sample(as.Date(as.numeric(Sys.Date()):as.numeric(Sys.Date()-10),origin = "1970-01-01"),records,T)
      }
      if(toupper(subformato[i]) %in% c("INT","INTEGER","LONG","") | is.null(subformato[i]) 
          | is.na(subformato[i])| is.nan(subformato[i])){
        Col[[campo[i]]]<-sample(1:(records/10),records,replace=T)
      }
      if(toupper(subformato[i]) %in% c("DOUBLE","FLOAT","DOUBLE","LONG","DOUBLE")){
        Col[[campo[i]]]<-rnorm(records)
      }
    }
  }
  return(Col)
}
