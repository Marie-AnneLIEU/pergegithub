#' @title détection de périodes de démarrage
#' @usage detect_first_wper(list.data,periode_ambscons,list_period)
#' dfw1(i,list.data,periode_ambscons,list_period)
#' dfw2(j,per,per_ac)
#' @param list.data liste de jeux de données à analyser
#' @param period_ambscons périodes où la température de l'ambiance est au-dessus de la température de consigne
#' @param list_period périodes de fonctionnement
#' @description  \code{dfw1} et \code{dfw2} sont les fonctions auxiliaires de la fonction \code{detect_first_wper}
#' @return \code{list_period_appendix} colonne indiquant si une période est une période de démarrage
#' @export


detect_first_wper <- function(list.data,periode_ambscons,list_period){
  list_period_appendix <- c()
  for (i in 1:length(list.data)){
    list_period_appendix[[i]] <- dfw1(i,list.data,periode_ambscons,list_period)
  }
  return(list_period_appendix)
}

#' @inherit detect_first_wper
#' @export
dfw1 <- function(i,list.data,periode_ambscons,list_period){
  data <- list.data[[i]]
  per_ac <- periode_ambscons[[i]]
  per <- list_period[[i]]
  ind <- rep(NA,nrow(per))
  for (j in 1:nrow(per)){
    ind[j]<-dfw2(j,per,per_ac)
  }
  ind2 <- rep(NA,nrow(per))
  for (k in 1:nrow(per_ac)){
    ind2[ind==k] <- 1:length(ind[ind==k])
  }
  res <- data.frame(ind,ind2)
  return(res)
}

#' @inherit detect_first_wper
#' @export
dfw2 <- function(j,per,per_ac){
  p <- per$on_start[j]
  num <- min(which(per_ac$end>p))
  return(num)
}
