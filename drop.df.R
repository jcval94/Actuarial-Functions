drop.df<-function(df){
  df[,purrr::map_lgl(df,~length(unique(.x))>1)]
}
