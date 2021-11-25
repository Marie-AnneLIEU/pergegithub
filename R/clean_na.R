#' @title Supprimer les lignes où il y a des NA (on ne prend en compte que certaines colonnes)
#' @usage clean_na_all(list.data,num_chambre)
#' clean_na_gen(list.data)
#' clean_na_chambre(list.data,num_chambre)
#' clean_na_all_optipellet(list.data,num_chambre)
#' clean_na_gen_optipellet(list.data)
#' @param list.data liste de jeux de données
#' @param num_chambre numéro de la chambre/zone à chauffer à analyser
#' @description \code{clean_na_gen} vérifie dans les colonnes non chambre-spécifiques s'il y a des NAs, puis supprimer des lignes où il y a des NAs, de même pour la version optipellet \code{clean_na_gen_optipellet}
#' @description \code{clean_na_chambre} vérifie dans les colonnes chambre-spécifiques s'il y a des NAs, puis supprimer des lignes où il y a des NAs
#' @description \code{clean_na_all} est une fonction chambre-spécifique qui vérifie dans les colonnes non chambre-spécifiques et chambre-spécifiques s'il y a des NAs, puis supprimer des lignes où il y a des NAs, de même pour la version optipellet \code{clean_na_all_optipellet}
#' @return \code{list.data} avec les lignes contenant les NA supprimées
#' @export


clean_na_all <- function(list.data,num_chambre){
  columns <- c("date","heure","S_BRUFI","E_SDEP","C_LECHA_HYST","E_SEXT"
               ,paste0("V_DEFAUT_SAMB_",num_chambre),paste0("E_SAMB",num_chambre),
               paste0("S_PCHA",num_chambre),paste0("V_AMB",num_chambre,"_HYST"))
  delete <- c()
  for(i in 1:length(list.data)){
    for (j in columns){
      data <- list.data[[i]][[j]]
      list.data[[i]]<- list.data[[i]][!is.na(data),]
    }
    if (nrow(list.data[[i]])==0){
      delete <- c(delete,i)}
  }

  ## enlever les listes vides
  if(!is.null(delete)){
    list.data <- list.data[-delete]
  }

  return(list(list.data=list.data,delete = delete))
}

#' @inherit clean_na_all
#' @export
clean_na_gen <- function(list.data){
  columns <- c("date","heure","S_BRUFI","E_SDEP","C_LECHA_HYST","E_SEXT")
  delete <- c()
  for(i in 1:length(list.data)){
   for (j in columns){
     data <- list.data[[i]][[j]]
     list.data[[i]]<- list.data[[i]][!is.na(data),]
   }
  if (nrow(list.data[[i]])==0){
      delete <- c(delete,i)}
  }

  ## enlever les listes vides
  if(!is.null(delete)){
    list.data <- list.data[-delete]
  }

  return(list.data)
}

#' @inherit clean_na_all
#' @export
clean_na_chambre <- function(list.data,num_chambre){
  columns <- c(paste0("V_DEFAUT_SAMB_",num_chambre),paste0("E_SAMB",num_chambre),
               paste0("S_PCHA",num_chambre),paste0("V_AMB",num_chambre,"_HYST"))
  delete <- c()
  for(i in 1:length(list.data)){
    for (j in columns){
      data <- list.data[[i]][[j]]
      list.data[[i]]<- list.data[[i]][!is.na(data),]
    }
    if (nrow(list.data[[i]])==0){
      delete <- c(delete,i)}
  }

  ## enlever les listes vides
  if(!is.null(delete)){
    list.data <- list.data[-delete]
  }
  return(list.data)
}

#' @inherit clean_na_all
#' @export
clean_na_all_optipellet <- function(list.data,num_chambre){
  columns <- c("date","heure","E_SDEP","C_LECHA_HYST","E_SEXT","V_DEBUG_AUTOMATE_GR_BRULEUR"
               ,paste0("V_DEFAUT_SAMB_",num_chambre),paste0("E_SAMB",num_chambre),
               paste0("S_PCHA",num_chambre),paste0("V_AMB",num_chambre,"_HYST"))
  delete <- c()
  for(i in 1:length(list.data)){
    for (j in columns){
      data <- list.data[[i]][[j]]
      list.data[[i]]<- list.data[[i]][!is.na(data),]
    }
    if (nrow(list.data[[i]])==0){
      delete <- c(delete,i)}
  }

  ## enlever les listes vides
  if(!is.null(delete)){
    list.data <- list.data[-delete]
  }

  return(list(list.data=list.data,delete = delete))
}

#' @inherit clean_na_all
#' @export
clean_na_gen_optipellet <- function(list.data){
  columns <- c("date","heure","E_SDEP","C_LECHA_HYST","E_SEXT","V_DEBUG_AUTOMATE_GR_BRULEUR")
  delete <- c()
  for(i in 1:length(list.data)){
    for (j in columns){
      data <- list.data[[i]][[j]]
      list.data[[i]]<- list.data[[i]][!is.na(data),]
    }
    if (nrow(list.data[[i]])==0){
      delete <- c(delete,i)}
  }

  ## enlever les listes vides
  if(!is.null(delete)){
    list.data <- list.data[-delete]
  }

  return(list.data)
}
