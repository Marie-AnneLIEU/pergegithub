#' @title Vérifier si une fin de période (de fonctionnement) est une vraie fin de période, retraiter les périodes de fonctionnement
#' @usage manage_period(list.data,list_period,seuil_delete,seuil_cut)
#' @param list.data liste de jeux de données à analyser
#' @param list_period liste de périodes de fonctionnement détectées par la fonction \code{list.period} ou \code{list.period.optipellet}
#' @param seuil_cut seuil de l'écart de temps entre 2 dernières données consécutives d'une période, pour un écart > ce seuil, soit on supprime la période, soit on enlève la dernière donnée.
#' @param seuil_delete nombre entier, période dont le nombre de données est inférieur à ce seuil sera supprimée
#' @return une liste d'objets:
#' @return \code{list_period} retraité
#' @return \code{ind_f_rm_l} liste des numéros de périodes dont la fin de période n'est pas une vraie fin de période.
#' @export



manage_period <- function(list.data,list_period,seuil_delete=2,seuil_cut=21){
  ind_f_rm_l <- c()
  for(i in 1:length(list.data)){
    period <- list_period[[i]]
    data <- list.data[[i]]
    ind_rm_fin <- rep(0,nrow(period))
    for (j in 1:nrow(period)){
      if(class(data$heure[period$on_end[j]])[1]!="POSIXct"){
      dif1 <- abs(as.numeric(difftime(as.POSIXct(data$heure[period$on_end[j]]),as.POSIXct(data$heure[(period$on_end[j]-1)]),units = "secs")))
      if(period$on_end[j]<nrow(data)){
      dif2 <- abs(as.numeric(difftime(as.POSIXct(data$heure[period$on_end[j]]),as.POSIXct(data$heure[(period$on_end[j]+1)]),units = "secs")))}
      else{dif2 <- 0}}
      else {dif1 <- abs(as.numeric(difftime(data$heure[period$on_end[j]],data$heure[(period$on_end[j]-1)],units = "secs")))
      if(period$on_end[j]<nrow(data)){
      dif2 <- abs(as.numeric(difftime(data$heure[period$on_end[j]],data$heure[(period$on_end[j]+1)],units = "secs")))}
      else{dif2 <- 0}}
      if(dif1>seuil_cut || dif2>seuil_cut){
        if((period$on_end[j]-period$on_start[j])<seuil_delete){
          ind_rm_fin[j] <- 2}
        else{
          period$on_end[j]<-period$on_end[j]-1
          ind_rm_fin[j]<- 1
        }
        }
    }
    list_period[[i]]<-period
    ind_f_rm_l[[i]]<-ind_rm_fin
    rm <- which(ind_rm_fin==2)
    if(length(rm)>0){
    list_period[[i]]<-period[-rm,]
    ind_f_rm_l[[i]]<-ind_rm_fin[-rm]}}
  return(list(list_period=list_period,ind_f_rm_l=ind_f_rm_l))
}
