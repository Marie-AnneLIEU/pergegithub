#' @title Calculer l'écart de temps entre 2 prélèvements consécutifs
#' @description Fonction qui ajoute/recalcule une colonne Ecart indiquant l'écart de temps entre 2 prélèvements consécutifsdans chaque jeu de données.
#' @param list.data une liste de jeux de données
#' @import lubridate
#' @return \code{list.data}: une liste de jeux de données, pour chaque jeu de données, il y a une colonne Ecart nouvellement calculée/ajoutée
#' @examples list.data <- Ecart(list.data)
#' @export

Ecart <- function(list.data){
  require(lubridate)
  for (i in 1:length(list.data)){
    if ("Ecart" %notin% colnames(list.data[[i]])){
      heure_1 <- list.data[[i]]$heure[-length(list.data[[i]]$heure)]
      heure_2 <- list.data[[i]]$heure[-1]
      Ecart <- c(NA,as.numeric(difftime(heure_2,heure_1,units = "secs")))
      list.data[[i]] <- data.frame(Ecart,list.data[[i]])
    }
    else{
      list.data[[i]]$Ecart <- as.numeric(second(list.data[[i]]$Ecart))
    }
  }
  return(list.data)
}
