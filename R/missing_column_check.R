#' Vérifier les colonnes indispensables à l'analyse sont présentes
#' @usage missing_column_check(list.data,num_chambre)
#' missing_column_gen(list.data)
#' missing_column_chambre(list.data,num_chambre)
#' missing_column_check_optipellet(list.data,num_chambre)
#' missing_column_gen_optipellet(list.data)
#' @description \code{missing_column_check},\code{missing_column_gen},\code{missing_column_chambre},\code{missing_column_check_optipellet},\code{missing_column_gen_optipellet} détectent les jeux de données dont certaines colonnes indispensables à l'analyse sont manquantes. La liste des colonnes à vérifier sont différentes pour chaque fonction.
#' @param list.data liste de jeux de données à analyser
#' @param num_chambre numéro de la chambre/zone à chauffer à analyser
#' @return \code{missing_column} liste des numéros des jeux de données avec les colonnes manquantes
#' @export


missing_column_check <- function(list.data,num_chambre){
  ## detecter les colonnes manquants

  columns <- c("date","heure","S_BRUFI","E_SDEP","C_LECHA_HYST","E_SEXT",paste0("V_DEFAUT_SAMB_",num_chambre),paste0("E_SAMB",num_chambre),
               paste0("S_PCHA",num_chambre),paste0("V_AMB",num_chambre,"_HYST"))
  missing_column <- c()
  for (i in 1:length(list.data)){
    if(length(columns[columns %in% colnames(list.data[[i]])])!=length(columns)){
      missing_column <- c(missing_column,i)
    }
  }
  return(missing_column)
}

#' @inherit missing_column_check
#' @export
missing_column_gen <- function(list.data){
  ## detecter les colonnes manquants

  columns <- c("date","heure","S_BRUFI","E_SDEP","C_LECHA_HYST","E_SEXT")
  missing_column <- c()
  for (i in 1:length(list.data)){
    if(length(columns[columns %in% colnames(list.data[[i]])])!=length(columns)){
      missing_column <- c(missing_column,i)
    }
  }
  return(missing_column)
}

#' @inherit missing_column_check
#' @export
missing_column_chambre <- function(list.data,num_chambre){
  columns <- c(paste0("V_DEFAUT_SAMB_",num_chambre),paste0("E_SAMB",num_chambre),
               paste0("S_PCHA",num_chambre),paste0("V_AMB",num_chambre,"_HYST"))
  missing_column <- c()
  for (i in 1:length(list.data)){
    if(length(columns[columns %in% colnames(list.data[[i]])])!=length(columns)){
      missing_column <- c(missing_column,i)
    }
  }
  return(missing_column)
}

## optipellet
#' @inherit missing_column_check
#' @export
missing_column_check_optipellet <- function(list.data,num_chambre){
  ## detecter les colonnes manquants

  columns <- c("date","heure","E_SDEP","C_LECHA_HYST","V_DEBUG_AUTOMATE_GR_BRULEUR"
               ,"E_SEXT",paste0("V_DEFAUT_SAMB_",num_chambre),paste0("E_SAMB",num_chambre),
               paste0("S_PCHA",num_chambre),paste0("V_AMB",num_chambre,"_HYST"))
  missing_column <- c()
  for (i in 1:length(list.data)){
    if(length(columns[columns %in% colnames(list.data[[i]])])!=length(columns)){
      missing_column <- c(missing_column,i)
    }
  }
  return(missing_column)
}

#' @inherit missing_column_check
#' @export
missing_column_gen_optipellet <- function(list.data){
  ## detecter les colonnes manquants

  columns <- c("date","heure","E_SDEP","C_LECHA_HYST","E_SEXT","V_DEBUG_AUTOMATE_GR_BRULEUR")
  missing_column <- c()
  for (i in 1:length(list.data)){
    if(length(columns[columns %in% colnames(list.data[[i]])])!=length(columns)){
      missing_column <- c(missing_column,i)
    }
  }
  return(missing_column)
}


