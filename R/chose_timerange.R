#' Choisir un intervalle de temps pour analyser
#' @usage time_range_analysis(list_res,range1,range2,fichier)
#' @param list_res sortie de la fonction \code{detect_anoma_chambre} ou de la fonction \code{detec_anoma_chambre_sy}
#' @param range1 heure de départ de l'intervalle
#' @param range2 heure de fin de l'intervalle
#' @param fichier numéro du jeu de données concerné
#' @return \code{list_res} avec les tableaux de conclusions (tab_ad et tab_res) sans les périodes hors de l'intervalle de temps choisi
#' @export

time_range_analysis <- function(list_res,range1,range2,fichier){
  require(lubridate)
  for(i in 1:length(list_res)){
    if(length(list_res[[i]]$tab_res)>(fichier-1)){
    if(!is.null(list_res[[i]]$tab_res[[fichier]])){
      time_col_s<- list_res[[i]]$tab_res[[fichier]]$`Heure de début`
      time_col_e <- list_res[[i]]$tab_res[[fichier]]$`Heure de fin`
      if(class(time_col_s)[1]!="POSIXct"){
      time_col_s <- as.POSIXct(list_res[[i]]$tab_res[[fichier]]$`Heure de début`)
      time_col_e <- as.POSIXct(list_res[[i]]$tab_res[[fichier]]$`Heure de fin`)}
      s <- min(which(time_col_s>=range1))
      e <- max(which(time_col_e<=range2))
      if(!is.infinite(s)){
        if(!is.infinite(e)){
          list_res[[i]]$tab_res[[fichier]] <- list_res[[i]]$tab_res[[fichier]][s:e,]
          list_res[[i]]$tab_ad[[fichier]] <- list_res[[i]]$tab_ad[[fichier]][s:e,]
        }
        else{
          list_res[[i]]$tab_res[[fichier]] <- NULL
          list_res[[i]]$tab_ad[[fichier]] <- NULL
        }
      }
      else{
        list_res[[i]]$tab_res[[fichier]] <- NULL
        list_res[[i]]$tab_ad[[fichier]] <- NULL
      }
      }}

  }
  return(list_res)
}
