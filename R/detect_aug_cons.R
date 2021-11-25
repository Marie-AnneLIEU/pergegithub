#' @title Détection les périodes où la témpérature de l'ambiance est au-dessus de la consigne
#' @usage detect_amb_s_cons(list.data,num_chambre)
#' cond_ambscons_f(i,list.data,num_chambre)
#' @param list.data liste des jeux de données à analyser
#' @param num_chambre numéro de la chambre/zone à chauffer
#' @return une liste d'objets:
#' @return \code{list.data} est \code{list.data} au départ avec une colonne cond_ambscons(valeur entre 0 et 1, 1 si la température de l'ambiannce est au-dessus de la consigne, 0 sinon) ajoutée à chaque jeu de données
#' @return \code{periode_ambscons}: data.frame des indices de commencement et de fin des périodes où la température de l'ambiance est au-dessous de la consigne
#' @export



# periodes ou la temp amb est sous la cons
detect_amb_s_cons <- function(list.data,num_chambre){
  periode_ambscons <- c()
 for (i in 1:length(list.data)){
    list.data[[i]] <- cond_ambscons_f(i,list.data,num_chambre)
    data <- list.data[[i]]
    start <- c()
    end <- c()
    if (data$cond_ambscons[1] == 0){
      start <- c(start,1)
    }
    for (j in 2:(length(data$cond_ambscons)-1)){
      if (data$cond_ambscons[j]!=data$cond_ambscons[j-1]){
        if (length(start) > length(end)){
          end <- c(end,j)}
        else {
          start <- c(start,j-1)
        }
      }
    }
    if(data$cond_ambscons[length(data$cond_ambscons)]==data$cond_ambscons[length(data$cond_ambscons)-1]){
      if (length(start)>length(end)){
        end <- c(end,length(data$cond_ambscons))
      }
    }
    periode_ambscons[[i]] <- data.frame(start,end)
 }
  return(list(periode_ambscons = periode_ambscons,list.data = list.data))
}

#' @inherit  detect_amb_s_cons
#' @export
cond_ambscons_f <- function(i,list.data,num_chambre){
  data <- list.data[[i]]
  cond_ambscons <- rep(0,nrow(data))
  cond_ambscons[data[[paste0("E_SAMB",num_chambre)]]>data[[paste0("V_AMB",num_chambre,"_HYST")]]] <- 1
  data <- cbind(data,cond_ambscons)
  return(data)
}


