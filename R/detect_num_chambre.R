#' Détecter le nombre de chambres/zones à chauffer déclarées
#' @usage detect_nombre_chambre(list.data)
#' @param list.data liste de jeux de données à analyser
#' @return \code{list_num_chambre} liste de vecteurs contenant le numéro des chambres/zone à chauffer déclarées
#' @export

detect_nombre_chambre<-function(list.data){
  list_num_chambre <- c()
  for(i in 1:length(list.data)){
    data<-list.data[[i]]
    num <- c()
    if(length(which(data$V_DEFAUT_SAMB_1 ==1)) < length(data$V_DEFAUT_SAMB_1)){
      num<-c(num,1)
    }
    if(length(which(data$V_DEFAUT_SAMB_2 ==1)) < length(data$V_DEFAUT_SAMB_2)){
      num<- c(num,2)
    }
    if(length(which(data$V_DEFAUT_SAMB_3 ==1)) < length(data$V_DEFAUT_SAMB_3)){
      num<-c(num,3)
    }
    list_num_chambre[[i]]<-num
  }
  names(list_num_chambre)<- na
  return(list_num_chambre)
}

#' Nommer les tableaux de conclusions puis retourne un tableau indiquant les chambres/zones à chauffer déclarées
#' @usage tableau_cham_name_list_res(na,list_res)
#' @param na liste des noms des jeux de données à analyser
#' @param list_res liste des tableaux des conclusions, sortie de la fonction \code{detect_anoma_chambre} + version pour les optipellets
#' @return \code{list_res} nommé
#' @return data.frame contenant la liste de nom des jeux de données analysés, une colonne "Chambre 1" qui prend des valeurs 0 et 1 , 1 si la chambre 1/zone à chauffer 1 est déclarée, 0 sinon. De même pour les colonnes "Chambre 2", "Chambre 3"
#' @export
#'
tableau_cham_name_list_res <- function(na,list_res){
  excluded <- c()
  for (i in 1:length(list_res)) {
    excluded[[i]] <- c(list_res[[i]]$missing_column, list_res[[i]]$deleted_room_names,list_res[[i]]$deleted_period_names, list_res[[i]]$na_col_names,NA)
  }
  for (i in 1:length(list_res)) {
    if (!is.null(list_res[[i]]$tab_res)) {
      if (length(list_res[[i]]$tab_res) > 0) {
        names(list_res[[i]]$tab_res) <- na[na %notin% excluded[[i]]][1:length(list_res[[i]]$tab_res)]
        names(list_res[[i]]$tab_ad)<-names(list_res[[i]]$tab_res)
      }
    }
  }
  chambre <- rep(0,length(na))
  dataframe <- data.frame(na,chambre,chambre,chambre)
  colnames(dataframe)<-c("Fichier","Chambre 1","Chambre 2","Chambre 3")
  for(i in 1:length(na)){
    if(!is.null(names(list_res[[1]]$tab_res))){
      dataframe[i,2]<- ifelse(na[i] %in% names(list_res[[1]]$tab_res) ,1,0)
    }
    if(!is.null(names(list_res[[2]]$tab_res))){
      dataframe[i,3]<- ifelse(na[i] %in% names(list_res[[2]]$tab_res) ,1,0)
    }
    if(!is.null(names(list_res[[3]]$tab_res))){
      dataframe[i,4]<- ifelse(na[i] %in% names(list_res[[3]]$tab_res) ,1,0)
    }
  }
  return(list(dataframe=dataframe,list_res=list_res))
}

#' @title ajouter des intervalles de temps
#' @description Ajouter une colonne dans le tableau de sortie de la fonction \code{tableau_cham_name_list_res} contenant les intervalles de temps des segments de fonctionnement
#' @import lubridate
#' @param na liste des noms des jeux de données à analyser
#' @param list_list_res_dtf sortie de la fonction \code{tableau_cham_name_list_res}
#' @return tableau de sortie de la fonction \code{tableau_cham_name_list_res} avec une colonne ajoutée
#' @export

add_interval <- function(na,list_list_res_dtf){
  require(lubridate)
  #solo
  list_res <- list_list_res_dtf$list_res
  tab_r_l <- c()
    for(i in 1:length(na)){
      le <- which(list_list_res_dtf$dataframe[i,-1]>0)
      tab_r <- c()
      if(length(le)>0){
        for(j in le){
            if(na[i] %in% names(list_res[[j]]$tab_res)){
              tab_r[[j]]<- list_res[[j]]$tab_res[[na[i]]]
              tab_r[[j]]$`Heure de début` <- as.POSIXct(tab_r[[j]]$`Heure de début`)
              tab_r[[j]]$`Heure de fin` <- as.POSIXct(tab_r[[j]]$`Heure de fin`)
              intv <- interval(tab_r[[j]]$`Heure de début`,tab_r[[j]]$`Heure de fin`)
              cnames <- colnames(tab_r[[j]])
              tab_r[[j]] <- data.frame(tab_r[[j]],intv)
              colnames(tab_r[[j]])<-c(cnames,"Intervalle")
            }
      }
      }
      tab_r_l[[i]]<-tab_r
    }
  if(!is.null(tab_r_l)){names(tab_r_l)<-na}
  return(tab_r_l)
}

# #intervalle seulement
# interval_only <- function(na,list_list_res_dtf){
#   #solo
#   list_res <- list_list_res_dtf$list_res
#   tab_r_l <- c()
#   for(i in 1:length(na)){
#     le <- which(list_list_res_dtf$dataframe[i,-1]>0)
#     tab_r <- c()
#     if(length(le)>0){
#       for(j in le){
#         if(na[i] %in% names(list_res[[j]]$tab_res)){
#           tab_r[[j]]<- list_res[[j]]$tab_res[[na[i]]]
#           tab_r[[j]]$`Heure de début` <- as.POSIXct(tab_r[[j]]$`Heure de début`)
#           tab_r[[j]]$`Heure de fin` <- as.POSIXct(tab_r[[j]]$`Heure de fin`)
#           intv <- interval(tab_r[[j]]$`Heure de début`,tab_r[[j]]$`Heure de fin`)
#           tab_r[[j]] <- intv
#         }
#       }
#     }
#     tab_r_l[[i]]<-tab_r
#   }
#   if(!is.null(tab_r_l)){names(tab_r_l)<-na}
#   return(tab_r_l)
# }

#' @title fusionner des tableaux de sortie de la fonction \code{add_interval}
#' @description Pour un jeu de données, fusionner toutes les tableaux de sortie de la fonction \code{add_interval}, sachant qu'on a un tableau par chambre/zone à chauffer
#' @param interval sortie de la fonction \code{add_interval}
#' @return tableaux , un tableau par jeu de données
#' @import lubridate
#' @export
colle_list_res <- function(interval){
  require(lubridate)
  intv_colle <- c()
  for(i in 1:length(interval)){
    tab_inv <- interval[[i]]
    colle <- c()
    if(length(tab_inv)>0){
      tab_colle <- interval(as.POSIXct("2021-01-01 00:00:01"),as.POSIXct("2021-01-01 01:00:01"))
      index <- 0
      debut <- as.POSIXct("2021-01-01 00:00:01")
      fin <- as.POSIXct("2021-01-01 00:00:01")
      for(j in 1:length(tab_inv)){
        if(!is.null(tab_inv[[j]])){
          tab_colle <- c(tab_colle,tab_inv[[j]]$Intervalle)
          index <- c(index,rep(j,length(tab_inv[[j]]$Intervalle)))
          debut <- c(debut,tab_inv[[j]]$`Heure de début`)
          fin <- c(fin,tab_inv[[j]]$`Heure de fin`)
        }
      }
      colle <- data.frame(index,tab_colle,debut,fin)
      colle <- colle[-1,]
      colnames(colle)<- c("Chambre","Intervalle","Heure de début","Heure de fin")
    }
  intv_colle[[i]]<-colle}
  if(length(intv_colle)>0){
    names(intv_colle)<-names(interval)
  }
  return(intv_colle)
}

#' @inherit clean_period_nbr_chbr
#' @export
clean_period_nbr_chbr_choice1<-function(colle){ #t_c_n_l_r$dataframe
  if(length(colle)>0){
  for(i in 1:length(colle)){
    if(!is.null(colle[[i]])){
      ind_del<- rep(0,nrow(colle[[i]]))
      avai_c <- unique(colle[[i]]$Chambre)
      if(length(avai_c)>1){
      for(j in avai_c){
        ind <- which(colle[[i]]$Chambre==j)
        ind2 <- which(colle[[i]]$Chambre!=j)
        for(k in ind){
          for(o in ind2){
            if(colle[[i]]$`Heure de début`[k] %within% colle[[i]]$Intervalle[o]){
              ind_del[k]<-1
            }
            if(colle[[i]]$`Heure de fin`[k] %within% colle[[i]]$Intervalle[o]){
              ind_del[k]<-1
            }
          }
        }
        }
      }
      colle[[i]]<- data.frame(colle[[i]],ind_del)
      colnames(colle[[i]])<- c("Chambre","Intervalle","Heure de début","Heure de fin","ind_del")
    }}}
  return(colle)}

#' @inherit clean_period_nbr_chbr
#' @export
clean_period_nbr_chbr_choice2<-function(colle){ #t_c_n_l_r$dataframe, choice 2 = all chambers avai
  if(length(colle)>0){
    for(i in 1:length(colle)){
      if(!is.null(colle[[i]])){
        ind_del<- rep(0,nrow(colle[[i]]))
        avai_c <- unique(colle[[i]]$Chambre)
        if(length(avai_c)>1){
          ind_del<- rep(1,nrow(colle[[i]]))
          for(j in avai_c){
            ind <- which(colle[[i]]$Chambre==j)
            ind2 <- which(colle[[i]]$Chambre!=j)
            for(k in ind){
              for(o in ind2){
                if(colle[[i]]$`Heure de début`[k] %within% colle[[i]]$Intervalle[o]){
                if(colle[[i]]$`Heure de fin`[k] %within% colle[[i]]$Intervalle[o]){
                  ind_del[k]<-0
                }}
              }
            }
          }
        }
        colle[[i]]<- data.frame(colle[[i]],ind_del)
        colnames(colle[[i]])<- c("Chambre","Intervalle","Heure de début","Heure de fin","ind_del")
      }}}
  return(colle)}

#' @inherit clean_period_nbr_chbr
#' @export
clean_period_nbr_chbr_choice3<-function(colle,l_n_c){ #t_c_n_l_r$dataframe, all declared chambers
  if(length(colle)>0){
    for(i in 1:length(colle)){
      if(!is.null(colle[[i]])){
        avai_c <- unique(colle[[i]]$Chambre)
        iind <- which(names(l_n_c)==names(colle)[i])
        ind2 <- l_n_c[[iind]]
        if(length(ind2)!=length(avai_c)){
          ind_del<- rep(1,nrow(colle[[i]]))
        }
        else{
          ind_del<- rep(0,nrow(colle[[i]]))
        if(length(avai_c)>1){
          ind_del<- rep(1,nrow(colle[[i]]))
          for(j in avai_c){
            ind <- which(colle[[i]]$Chambre==j)
            ind3 <- which(colle[[i]]$Chambre!=j)
            for(k in ind){
              for(o in ind3){
                if(colle[[i]]$`Heure de début`[k] %within% colle[[i]]$Intervalle[o]){
                if(colle[[i]]$`Heure de fin`[k] %within% colle[[i]]$Intervalle[o]){
                  ind_del[k]<-0
                }}}
              }
            }
          }
        }
        colle[[i]]<- data.frame(colle[[i]],ind_del)
        colnames(colle[[i]])<- c("Chambre","Intervalle","Heure de début","Heure de fin","ind_del")
      }}}
  return(colle)}

#' @title Types de prise en compte des segments de fonctionnement
#' @description Supprimer les périodes dont on ne veut pas prendre en compte dans les calcules, il y a 3 types de prise en compte:
#' @description Choice 1: Pour chaque zone à chauffer/chambre, \code{clean_period_nbr_chbr_choice1} supprimer les segments dont l'intervalle coïncide aux ceux des autres chambres/zone à chauffer
#' @description Choice 2: Pour chaque zone à chauffer/chambre, \code{clean_period_nbr_chbr_choice2} supprimer les segments dont l'intervalle ne coïncide pas aux ceux des autres chambres/zone à chauffer (chambres/zones dont le nombre de segments de fonctionnement n'est pas nul).
#' @description Choice 3: Pour chaque zone à chauffer/chambre, \code{clean_period_nbr_chbr_choice3} supprimer les segments dont l'intervalle ne coïncide pas aux ceux des autres chambres/zone à chauffer (chambres/zones déclarées).
#' @param choice numéro du choix
#' @param colle sortie de la fonction \code{colle_list_res}
#' @param l_n_c sortie $dataframe de la fonction \code{tableau_cham_name_list_res}
#' @return  sortie de la fonction \code{colle_list_res} avec une colonne ind_del ajoutée qui prend valeurs entre 0 et 1, 1 si le segment est à supprimer, 0 sinon
#' @export

clean_period_nbr_chbr <- function(choice,colle,l_n_c){
  if(choice==1){
    res <- clean_period_nbr_chbr_choice1(colle)
  }
  else if(choice == 2){
    res <- clean_period_nbr_chbr_choice2(colle)
  }
  else{
    res <- clean_period_nbr_chbr_choice3(colle,l_n_c)
  }
  return(res)
}

#' @title list_res retraité
#' @description retourner list_res avec segments de fonctionnement supprimés selon le choix
#' @param res sortie de la fonction \code{clean_period_nbr_chbr}
#' @param list_res_n list_res, sortie de la fonction \code{detect_anoma_chambre}/\code{detect_anoma_chambre_optipellet}/\code{detect_anoma_chambre_sy}
#' @return list_res avec segments de fonctionnement supprimés selon le choix
#' @export
new_list_res <- function(res,list_res_n){
  for (i in 1:length(list_res_n)){
    if(length(list_res_n[[i]]$tab_res)>0){
      for(j in 1:length(list_res_n[[i]]$tab_res)){
        data <- res[[names(list_res_n[[i]]$tab_res)[j]]]
        if(!is.null(data)){
        data2 <- data[data$Chambre==i,]
        if(length(which(data2$ind_del==1))==nrow(data2)){
          list_res_n[[i]]$tab_res[[j]] <- NULL
          list_res_n[[i]]$tab_ad[[j]] <- NULL
          list_res_n[[i]]$list_data[[j]] <- NULL
          list_res_n[[i]]$deleted_period_names <- c(list_res_n[[i]]$deleted_period_names,names(list_res_n[[i]]$tab_res)[j])
        }
        else{
          list_res_n[[i]]$tab_res[[j]] <- list_res_n[[i]]$tab_res[[j]][-which(data2$ind_del==1),]
          list_res_n[[i]]$tab_ad[[j]] <- list_res_n[[i]]$tab_ad[[j]][-which(data2$ind_del==1),]
        }}
      }
    }
  }
  return(list_res_n)
}

