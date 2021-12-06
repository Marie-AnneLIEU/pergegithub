#' @title Fonctions qui extrait la loi d'eau actuelle et retourne une nouvelle loi d'eau
#' @description \code{extract_t_text} extrait la loi d'eau et les températures de l'extérieur et les C_MAXAMB correspondantes puis corriger la loi d'eau selon les cas, pour le cas "loi d'eau est insuffisante", on additionne un certain nombre de °C à la loi d'eau, pour les autres cas on en soustrait.
#' @description \code{extract1}, \code{extract2}, \code{extract3} sont les fonctions auxiliaires de \code{extract_t_text}
#' @description \code{tab_sort} pour chaque jeu de données, pour une plage de température de l'extérieur, cette fonction extrait les valeurs de la loi d'eau (corrigée, sans compensation de l'ambiance) correspondantes ainsi que les opérations faites sur ces valeurs (addition de combien et/ou soustraction de combien).
#' @description \code{tab_sort_all} est \code{tab_sort} sur toute la liste de jeux de données
#' @description \code{tab_sort_all_new} et \code{tab_sort_new} sont \code{tab_sort_all} et \code{tab_sort} mais sa sortie \code{tab_ind} est pour valeur 0 et 1 (1 si loi d'eau insuffisante, 0 sinon)
#' @description \code{max_narm} fonction \code{max} avec l'argument \code{na.rm} = TRUE
#' @description \code{mix} mettre en commun les sorties de la fonction \code{tab_sort_all} (cas loi d'eau insuffisante, cas loi d'eau suffisante)
#' @param list_res sortie de la fonction \code{detect_anoma_chambre}/\code{detect_anoma_chambre_sy}/\code{detect_anoma_chambre_optipellet}
#' @param non_insuf FALSE pour le cas "loi d'eau insuffisante", TRUE sinon
#' @param seuilcorr température de correction pour l'addition ou soustraction
#' @param j indice d'avancement du boucle for
#' @param k indice d'avancement du boucle for
#' @param lres \code{list_res} d'une chambre/zone à chauffer donnée
#' @param data un jeu de données
#' @param tab_add \code{tab_ad} d'une chambre/zone à chauffer donnée
#' @param tab_insuf sortie de la fonction \code{tab_sort_all} cas "loi d'eau insuffisante"
#' @param tab_non_insuf sortie de la fonction \code{tab_sort_all} cas "loi d'eau suffisante"
#' @param x vecteur numérique
#' @param insuf TRUE pour le cas "loi d'eau insuffisante", FALSE sinon
#' @param seuil_text pas de la plage de température de l'extérieur
#' @export


# # extract_t_ext <- function(list_res,path,seuilcorr=1,P_SEUIL,P_KCOMP,non_insuf=FALSE){
# extract_t_ext <- function(list_res,path,seuilcorr=1,non_insuf=FALSE){
#   extract_all_cham <- c()
#   for (i in 1:length(list_res)){
#     if (!is.null(list_res[[i]]$tab_ad)){
#       # res <- extract1(i,list_res,P_SEUIL,P_KCOMP,non_insuf=non_insuf)
#       res <- extract1(i,list_res,non_insuf=non_insuf)
#       if(!is.null(res)){
#         res <- res[!duplicated(res),]
#         if(non_insuf){
#           leaucorr <- res$leau}
#         else{leaucorr <- res$leau+seuilcorr}
#         extract_all_cham[[i]]<- data.frame(res,leaucorr)
#         for (v in 1:ncol(extract_all_cham[[i]])){
#           extract_all_cham[[i]][,v] <- round(extract_all_cham[[i]][,v],digits = 2)
#         }
#         colnames(extract_all_cham[[i]]) <- c("T_EXT","LECHA","C_MAXAMB","LECHA_CORRIGEE")
#         extract_all_cham[[i]] <- extract_all_cham[[i]][order(extract_all_cham[[i]]$C_MAXAMB),]
#         rownames(extract_all_cham[[i]]) <- NULL}
#       else{extract_all_cham[[i]]<-NA}}
#     else{extract_all_cham[[i]]<- NA}}
#   return(extract_all_cham)
# }
#
# # extract1 <- function(i,list_res,P_SEUIL,P_KCOMP,non_insuf){
# extract1 <- function(i,list_res,non_insuf){
#     lres <- list_res[[i]]
#     extractl <- extract2(lres,non_insuf)
#   return(extract)
# }
#
# # extract2 <- function(j,lres,P_SEUIL,P_KCOMP,non_insuf){
# extract2 <- function(lres,non_insuf){
#   data <- lres$data
#   tab_add <- lres$tab_ad
#   if(non_insuf){
#     ind <- which(tab_add$conclusion != "Loi d'eau insuffisante")}
#   else{ind <- which(tab_add$conclusion == "Loi d'eau insuffisante")}
#   if (length(ind)>0){
#     tab1 <- data.frame()
#     for (k in ind){
#       # all <- extract3(k,tab_add,data,P_SEUIL,P_KCOMP)
#       all <- extract3(k,tab_add,data)
#       if(!is.null(all)){
#         tab1 <- rbind(tab1,all)
#       }}
#     if(nrow(tab1)==0){tab1 <- NULL}
#   }
#   else{ tab1 <- NULL}
#   return(tab1)
# }
#
# # extract3 <- function(k,tab_add,data,P_SEUIL,P_KCOMP){
# extract3 <- function(k,tab_add,data){
#   if (tab_add$pertinence[k]==1 && tab_add$seg_demar[k]==0){
#     start <- tab_add$indice_s[k]
#     end <- tab_add$indice_e[k]
#     text <- as.numeric(data$E_SEXT[start:end])
#     leau <- as.numeric(data$C_LECHA_HYST[start:end])
#     vamb1 <- as.numeric(data$V_AMB1[start:end])
#     vamb2 <- as.numeric(data$V_AMB2[start:end])
#     vamb3 <- as.numeric(data$V_AMB3[start:end])
#     cmaxam <- c()
#     cham_non_decla <- c()
#     if(length(which(data$V_DEFAUT_SAMB_1[start:end]>0))==length(data$V_DEFAUT_SAMB_1[start:end])){
#       amb1 <- rep(NA,length(vamb1))
#       cham_non_decla <- c(cham_non_decla,1)
#     }
#     else{ amb1 <- as.numeric(data$E_SAMB1[start:end])}
#     if(length(which(data$V_DEFAUT_SAMB_2[start:end]>0))==length(data$V_DEFAUT_SAMB_2[start:end])){
#       amb2 <- rep(NA,length(vamb2))
#       cham_non_decla <- c(cham_non_decla,2)
#     }
#     else{ amb2 <- as.numeric(data$E_SAMB2[start:end])}
#     if(length(which(data$V_DEFAUT_SAMB_3[start:end]>0))==length(data$V_DEFAUT_SAMB_3[start:end])){
#       amb3 <- rep(NA,length(vamb3))
#       cham_non_decla <- c(cham_non_decla,3)
#     }
#     else{ amb3 <- as.numeric(data$E_SAMB3[start:end])}
#     com1 <- vamb1 - amb1
#     com2 <- vamb2 - amb2
#     com3 <- vamb3 - amb3
#     for(l in 1:length(com1)){
#       cmaxam[l] <- max(com1[l],com2[l],com3[l],na.rm = TRUE)
#     }
#     all <- data.frame(text,leau,cmaxam)
#     nomb_cham <- c(1,2,3)
#     if(length(cham_non_decla)>0){
#       rest <- nomb_cham[nomb_cham %notin% cham_non_decla]
#       if (length(cham_non_decla)>0){
#         ind_del <- c()
#         for(m in rest){
#           ind_del <- c(ind_del,which(data[[paste0("V_DEFAUT_SAMB_",m)]][start:end]>0))
#         }
#         ind_del <- unique(ind_del)
#         if(length(ind_del)>0){
#           all <- all[-ind_del,]}}
#       else{ all <- NULL}
#     }
#     else{
#       for(m in nomb_cham){
#         ind_del <- c(ind_del,which(data[[paste0("V_DEFAUT_SAMB_",m)]][start:end]>0))
#       }
#       ind_del <- unique(ind_del)
#       all <- all[-ind_del,]
#     }
#     if(nrow(all)==0){ all <- NULL}
#   }
#   else{ all <- NULL}
#   return(all)
# }
#
#
# ## tableau
#
# tab_sort_all <- function(data,insuf=TRUE){
#   tab_sort_all <- c()
#   for(i in 1:length(data)){
#     tab_sort_all[[i]] <- tab_sort(data[[i]],insuf = insuf)
#   }
#   return(tab_sort_all)
# }
#
# tab_sort <- function(data,insuf=TRUE,seuil_cmax=0.25,seuil_text=0.25){
#   plage_text <- seq(-20,25,seuil_text)
#   plage_cmaxamb <- c(0,seq(1+seuil_cmax,5,seuil_cmax))
#   tab_text_cmaxamb <- matrix(data = NA,nrow = length(plage_cmaxamb),length(plage_text))
#   tab_text_cmaxamb <- as.data.frame(tab_text_cmaxamb)
#   tab_text_cmaxamb <- data.frame(plage_cmaxamb,tab_text_cmaxamb)
#   colnames(tab_text_cmaxamb) <- c("C_MAXAMB",plage_text)
#   tab_ind <- tab_text_cmaxamb
#   if(is.data.frame(data)){
#     for(i in 2:length(plage_cmaxamb)){
#       temp1 <- abs(data$C_MAXAMB - plage_cmaxamb[i])
#       ind <- which(temp1<seuil_cmax)
#       if (length(ind)>0){
#         data2 <- data[ind,]
#         for(j in 1:length(plage_text)){
#           temp2 <- abs(data2$T_EXT - plage_text[j])
#           ind2 <- which(temp2 < seuil_text)
#           if (length(ind2)>0){
#             tab_text_cmaxamb[i,j+1] <- max(data2[ind2,]$LECHA_CORRIGEE)
#             tab_ind[i,j] <- ifelse(insuf,1,0)
#           }}
#       }
#     }
#     ind0 <- which((data$C_MAXAMB<=1) & (data$C_MAXAMB>=-1))
#     if(length(ind0)>0){
#       data2 <- data[ind0,]
#       for(j in 1:length(plage_text)){
#         temp2 <- abs(data2$T_EXT - plage_text[j])
#         ind2 <- which(temp2 < seuil_text)
#         if (length(ind2)>0){
#           tab_text_cmaxamb[1,j+1] <- max(data2[ind2,]$LECHA_CORRIGEE)
#           tab_ind[1,j] <- ifelse(insuf,1,0)
#         }}}}
#   return(list(tab_text_cmaxamb=tab_text_cmaxamb,tab_ind=tab_ind))}
#
# max_narm <- function(x){
#   max(x,na.rm = TRUE)
# }
#
# ## mix chambre
#
# mix_chambre <- function(data_list){
#   list_fin <- c()
#   for(k in 1:2){
#
#     for(i in 1:21){
#       temp1 <- rbind(data_list[[1]][[k]][i,-1],data_list[[2]][[k]][i,-1],data_list[[3]][[k]][i,-1])
#       new1 <- as.vector(apply(temp1, 2, max_narm))
#       new1[new1==-Inf] <- NA
#       data_list[[1]][[k]][i,] <- c(data_list[[1]][[k]][i,1],new1)
#     }
#     list_fin[[k]]<-data_list[[1]][[k]]}
#   return(list(tab_text_cmaxamb = list_fin[[1]],tab_ind = list_fin[[2]]))
# }
#
# ## mix insuf, non insuf
#
#
# mix <- function(tab_insuf,tab_non_insuf){
#   for(i in 1:21){
#     temp1 <- rbind(tab_insuf$tab_text_cmaxamb[i,-1],tab_non_insuf$tab_text_cmaxamb[i,-1])
#     temp2 <- rbind(tab_insuf$tab_ind[i,-1],tab_non_insuf$tab_ind[i,-1])
#     new1 <- as.vector(apply(temp1, 2, max_narm))
#     new1[new1==-Inf] <- NA
#     new2 <- as.vector(apply(temp2, 2, max_narm))
#     new2[new2==-Inf] <- NA
#     tab_insuf$tab_text_cmaxamb[i,] <- c(tab_insuf$tab_text_cmaxamb[i,1],new1)
#     tab_insuf$tab_ind[i,] <- c(tab_insuf$tab_ind[i,1],new2)
#   }
#   return(list(tab_text_cmaxamb_mix=tab_insuf$tab_text_cmaxamb,tab_ind_mix=tab_insuf$tab_ind))
# }
#
# # mix old_new full table
#
# mix_old_new <- function(old,new){
#   f_temp <- function(x){
#     x[x==1] <- 0
#     return(x)
#   }
#   old[[2]]<- apply(old[[2]],2,f_temp)
#   for (i in 1:2){
#     for(j in 1:21){
#       temp1 <- rbind(old[[i]][j,-1],new[[i]][j,-1])
#       new1 <- as.vector(apply(temp1, 2, max_narm))
#       new1[new1==-Inf] <- NA
#       old[[i]][j,] <- c(old[[i]][j,1],new1)
#     }
#   }
#   return(old)}




# all files version ----


# C_DAMBcal <- function(P_SEUIL,C_MAXAMB){
#   f <- function(x){
#     if (x>P_SEUIL){ res <- x - P_SEUIL }
#     else if (x < -P_SEUIL) {res <- x + P_SEUIL}
#     else{ res <- 0}
#     return(res)
#   }
#   l <- sapply(C_MAXAMB, f)
#   return(l)
# }


# extract_t_ext <- function(list_res,path,seuilcorr=1,P_SEUIL,P_KCOMP,non_insuf=FALSE){
extract_t_ext <- function(list_res,seuilcorr=1,non_insuf=FALSE){
  extract_all_cham <- c()
  if(length(list_res)>0){
    for (i in 1:length(list_res)){
      if (!is.null(list_res[[i]]$tab_ad)){
        # res <- extract1(i,list_res,P_SEUIL,P_KCOMP,non_insuf=non_insuf)
        res <- extract1(i,list_res,non_insuf=non_insuf)
        if(!is.null(res)){
          res <- res[!duplicated(res),]
          if(non_insuf){
            leaucorr <- res$leau-seuilcorr} #or unchanged, depend
          else{leaucorr <- res$leau+seuilcorr}
          extract_all_cham[[i]]<- data.frame(res,leaucorr)
          for (v in 1:ncol(extract_all_cham[[i]])){
            extract_all_cham[[i]][,v] <- round(extract_all_cham[[i]][,v],digits = 2)
          }
          # colnames(extract_all_cham[[i]]) <- c("T_EXT","LECHA","C_MAXAMB","C_DAMB","C_COMP","LECHA_CORRIGEE")
          colnames(extract_all_cham[[i]]) <- c("T_EXT","LECHA","C_MAXAMB","LECHA_CORRIGEE")
          # extract_all_cham[[i]] <- extract_all_cham[[i]][order(extract_all_cham[[i]]$C_COMP),]
          extract_all_cham[[i]] <- extract_all_cham[[i]][order(extract_all_cham[[i]]$C_MAXAMB),]
          rownames(extract_all_cham[[i]]) <- NULL}
        else{extract_all_cham[[i]]<-NA}}
      else{extract_all_cham[[i]]<- NA}}}
  else{for(k in 1:3){extract_all_cham[[k]]<-NA}}
  return(extract_all_cham)
}

#' @inherit extract_t_ext
#' @export
# extract1 <- function(i,list_res,P_SEUIL,P_KCOMP,non_insuf){
extract1 <- function(i,list_res,non_insuf){
  lres <- list_res[[i]]
  if (length(lres$list_data)>0){
    extractl <- c()
    for (j in 1:length(lres$list_data)){
      # tab1 <- extract2(j,lres,P_SEUIL,P_KCOMP,non_insuf)
      tab1 <- extract2(j,lres,non_insuf=non_insuf)
      extractl[[j]]<- tab1
    }
    extract <- data.frame()
    if(length(extractl)>0){
      for (p in 1:length(extractl)){
        if(!is.null(extractl[[p]])){
          extract <- rbind(extract,extractl[[p]])
        }
      }}
    if(nrow(extract)==0){extract <- NULL}}
  else{extract <- NULL}
  return(extract)
}

#' @inherit extract_t_ext
#' @export
# extract2 <- function(j,lres,P_SEUIL,P_KCOMP,non_insuf){
extract2 <- function(j,lres,non_insuf){
  if(length(lres$list_data)>=j & length(lres$tab_ad)>=j){
    data <- lres$list_data[[j]]
    tab_add <- lres$tab_ad[[j]]
    if(!is.null(data)){
      if(non_insuf){
        ind <- which(tab_add$conclusion != "Loi d'eau insuffisante")}
      else{ind <- which(tab_add$conclusion == "Loi d'eau insuffisante")}
      if (length(ind)>0){
        tab1 <- data.frame()
        for (k in ind){
          # all <- extract3(k,tab_add,data,P_SEUIL,P_KCOMP)
          all <- extract3(k,tab_add,data)
          if(!is.null(all)){
            tab1 <- rbind(tab1,all)
          }}
        if(nrow(tab1)==0){tab1 <- NULL}
      }
      else{ tab1 <- NULL}}
    else{tab1 <- NULL}}
  else{tab1 <- NULL}
  return(tab1)
}

#' @inherit extract_t_ext
#' @export
# extract3 <- function(k,tab_add,data,P_SEUIL,P_KCOMP){
extract3 <- function(k,tab_add,data){
  if (tab_add$pertinence[k]==1 && tab_add$seg_demar[k]==0){
    start <- tab_add$indice_s[k]
    end <- tab_add$indice_e[k]
    text <- as.numeric(data$E_SEXT[start:end])
    leau <- as.numeric(data$C_LECHA_HYST[start:end])
    vamb1 <- as.numeric(data$V_AMB1[start:end])
    vamb2 <- as.numeric(data$V_AMB2[start:end])
    vamb3 <- as.numeric(data$V_AMB3[start:end])
    cmaxam <- c()
    cham_non_decla <- c()
    if(length(which(data$V_DEFAUT_SAMB_1!=0))==nrow(data)){
      amb1 <- rep(NA,length(vamb1))
      cham_non_decla <- c(cham_non_decla,1)
    }
    else{ amb1 <- as.numeric(data$E_SAMB1[start:end])}
    if(length(which(data$V_DEFAUT_SAMB_2!=0))==nrow(data)){
      amb2 <- rep(NA,length(vamb2))
      cham_non_decla <- c(cham_non_decla,2)
    }
    else{ amb2 <- as.numeric(data$E_SAMB2[start:end])}
    if(length(which(data$V_DEFAUT_SAMB_3!=0))==nrow(data)){
      amb3 <- rep(NA,length(vamb3))
      cham_non_decla <- c(cham_non_decla,3)
    }
    else{ amb3 <- as.numeric(data$E_SAMB3[start:end])}
    com1 <- vamb1 - amb1
    com2 <- vamb2 - amb2
    com3 <- vamb3 - amb3
    for(l in 1:length(com1)){
      cmaxam[l] <- max(com1[l],com2[l],com3[l],na.rm = TRUE)
    }
    # C_DAMB <- C_DAMBcal(P_SEUIL,cmaxam)
    # C_COMP <- 1 + (P_KCOMP*(C_DAMB/100))
    # all <- data.frame(text,leau,cmaxam,C_DAMB,C_COMP)
    all <- data.frame(text,leau,cmaxam)
    nomb_cham <- c(1,2,3)
    if(length(cham_non_decla)>0){
      rest <- nomb_cham[nomb_cham %notin% cham_non_decla]
      # if (length(cham_non_decla)>0){
      ind_del <- c()
      for(m in rest){
        ind_del <- c(ind_del,which(data[[paste0("V_DEFAUT_SAMB_",m)]][start:end]>0))
      }
      ind_del <- unique(ind_del)
      if(length(ind_del)>0){
        all <- all[-ind_del,]}
    }
    else{
      ind_del <- c()
      for(m in nomb_cham){
        ind_del <- c(ind_del,which(data[[paste0("V_DEFAUT_SAMB_",m)]][start:end]>0))
      }
      ind_del <- unique(ind_del)
      if(length(ind_del)>0){
        all <- all[-ind_del,]}
    }
    if(nrow(all)==0){ all <- NULL}
  }
  else{ all <- NULL}
  return(all)
}

# create_tab_comb_all <- function(insuf,non_insuf){
#   newtab <- c()
#   list_ccomp <- c()
#   restab <- c()
#   for (j in 1:3){
#     dt1 <- insuf[[j]]
#     if(!is.na(dt1)){
#       dt2 <- non_insuf[[j]]
#       if(!is.na(dt2)){
#         u1 <- unique(dt1$C_COMP)
#         u2 <- unique(dt2$C_COMP)
#         indu <- u2[u2 %in% u1]
#         for(i in indu){
#           dt1t <- dt1[dt1$C_COMP==i,]
#           dt2t <- dt2[dt2$C_COMP==i,]
#           utext1 <- unique(dt1t$T_TEXT)
#           utext2 <- unique(dt2t$T_TEXT)
#           indu2 <- utext2[utext2 %in% utext1]
#           del <- which((dt2$C_COMP==i) & (dt2$T_EXT %in% indu2))
#           dt2 <- dt2[-del,]
#         }
#         if(nrow(dt2)>0){
#           dt <- rbind(dt1,dt2)}
#         else{dt <- dt1}
#       }
#       else{dt <- dt1}
#       dt <- data.frame(dt)
#       dt <- dt[!duplicated(dt),]
#       dt <- dt[order(dt$C_COMP),]
#       unique_ccomp <- unique(dt$C_COMP)
#       lecha_ord <- rep(NA,length(unique_ccomp))
#       lecha_coef <- lecha_ord
#       lecha_cor_ord <- lecha_ord
#       lecha_cor_coef <- lecha_ord
#       u_text_l <- c()
#       u_ccomp_l <- c()
#       lecha_l <- c()
#       lecha_cor_l <- c()
#       for(k in 1:length(unique_ccomp)){
#         dtf <- dt[dt$C_COMP==unique_ccomp[k],]
#         u_text <- sort(unique(dtf$T_EXT))
#         lecha <- rep(NA,length(u_text))
#         lecha_cor <- lecha
#         for(l in 1:length(u_text)){
#           lecha[l] <- mean(dtf$LECHA[dtf$T_EXT==u_text[l]])
#           lecha_cor[l] <- mean(dtf$LECHA_CORRIGEE[dtf$T_EXT==u_text[l]])
#         }
#         mod1 <- lm(lecha~u_text)
#         mod2 <- lm(lecha_cor~u_text)
#         ord <- mean(lecha)
#         ord_cor <- mean(lecha_cor)
#         coef <- 0
#         coef_cor <- 0
#         if(!is.na(mod1$coefficients[2])){
#           ord <- mod1$coefficients[1]
#           coef <- mod1$coefficients[2]
#         }
#         if(!is.na(mod2$coefficients[2])){
#           ord_cor <- mod2$coefficients[1]
#           coef_cor <- mod2$coefficients[2]
#         }
#         lecha_ord[k] <- ord
#         lecha_coef[k] <- coef
#         lecha_cor_ord[k] <- ord_cor
#         lecha_cor_coef[k] <- coef_cor
#         u_text_l <- c(u_text_l,u_text)
#         u_ccomp_l <- c(u_ccomp_l,rep(unique_ccomp[k],length(u_text)))
#         lecha_l <- c(lecha_l,lecha)
#         lecha_cor_l <- c(lecha_cor_l,lecha_cor)
#       }
#       datares <- data.frame(unique_ccomp,lecha_ord,lecha_coef,lecha_cor_ord,lecha_cor_coef)
#       restab[[j]] <- datares
#       list_ccomp[[j]] <- unique_ccomp
#       newtab[[j]] <- data.frame(u_ccomp_l,u_text_l,lecha_l,lecha_cor_l)
#     }
#     else{
#       restab[[j]]<-NA
#       list_ccomp[[j]] <- NA
#       newtab[[j]]<-NA
#     }
#   }
#   result <- list(restab = restab,list_ccomp = list_ccomp,newtab=newtab)
#   return(result)}

## tableau

#' @inherit extract_t_ext
#' @export
tab_sort_all <- function(data,insuf=TRUE,seuil_text=0.25,seuilcorr=1){
  tab_sort_all <- c()
  for(i in 1:length(data)){
    tab_sort_all[[i]] <- tab_sort(data[[i]],insuf = insuf,seuil_text=seuil_text,seuilcorr=seuilcorr)
  }
  return(tab_sort_all)
}

#' @inherit extract_t_ext
#' @export
tab_sort <- function(data,insuf=TRUE,seuil_text,seuilcorr){
  plage_text <- seq(-20,25,seuil_text)
  # plage_cmaxamb <- c(0,seq(1+seuil_cmax,5,seuil_cmax))
  # tab_text_cmaxamb <- matrix(data = NA,nrow = length(plage_cmaxamb),length(plage_text))
  tab_text_cmaxamb <- c(0,rep(NA,length(plage_text)))
  tab_text_cmaxamb <- as.data.frame(t(tab_text_cmaxamb))
  # tab_text_cmaxamb <- data.frame(plage_cmaxamb,tab_text_cmaxamb)
  colnames(tab_text_cmaxamb) <- c("C_MAXAMB",plage_text)
  tab_ind <- tab_text_cmaxamb
  if(is.data.frame(data)){
    # for(i in 2:length(plage_cmaxamb)){
    #   temp1 <- abs(data$C_MAXAMB - plage_cmaxamb[i])
    #   ind <- which(temp1<seuil_cmax)
    #   if (length(ind)>0){
    #     data2 <- data[ind,]
    #     for(j in 1:length(plage_text)){
    #       temp2 <- abs(data2$T_EXT - plage_text[j])
    #       ind2 <- which(temp2 < seuil_text)
    #       if (length(ind2)>0){
    #         tab_text_cmaxamb[i,j+1] <- max(data2[ind2,]$LECHA_CORRIGEE)
    #         tab_ind[i,j] <- ifelse(insuf,seuilcorr,-seuilcorr)
    #       }}
    #   }
    # }
    ind0 <- which((data$C_MAXAMB<=1) & (data$C_MAXAMB>=-1))
    if(length(ind0)>0){
      data2 <- data[ind0,]
      for(j in 1:length(plage_text)){
        temp2 <- abs(data2$T_EXT - plage_text[j])
        ind2 <- which(temp2 < seuil_text)
        if (length(ind2)>0){
          tab_text_cmaxamb[1,j+1] <- max(data2[ind2,]$LECHA_CORRIGEE)
          tab_ind[1,j+1] <- ifelse(insuf,seuilcorr,-seuilcorr)
        }}}}
  return(list(tab_text_cmaxamb=tab_text_cmaxamb,tab_ind=tab_ind))}

#' @inherit extract_t_ext
#' @export
max_narm <- function(x){
  max(x,na.rm = TRUE)
}

#' @inherit extract_t_ext
#' @export
tab_sort_all_new <- function(data,insuf=TRUE,seuil_text=0.25){
  tab_sort_all <- c()
  for(i in 1:length(data)){
    tab_sort_all[[i]] <- tab_sort_new(data[[i]],insuf = insuf,seuil_text=seuil_text)
  }
  return(tab_sort_all)
}

#' @inherit extract_t_ext
#' @export
tab_sort_new <- function(data,insuf=TRUE,seuil_text){
  plage_text <- seq(-20,25,seuil_text)
  # plage_cmaxamb <- c(0,seq(1+seuil_cmax,5,seuil_cmax))
  # tab_text_cmaxamb <- matrix(data = NA,nrow = length(plage_cmaxamb),length(plage_text))
  tab_text_cmaxamb <- c(0,rep(NA,length(plage_text)))
  tab_text_cmaxamb <- as.data.frame(t(tab_text_cmaxamb))
  # tab_text_cmaxamb <- data.frame(plage_cmaxamb,tab_text_cmaxamb)
  colnames(tab_text_cmaxamb) <- c("C_MAXAMB",plage_text)
  tab_ind <- tab_text_cmaxamb
  if(is.data.frame(data)){
    # for(i in 2:length(plage_cmaxamb)){
    #   temp1 <- abs(data$C_MAXAMB - plage_cmaxamb[i])
    #   ind <- which(temp1<seuil_cmax)
    #   if (length(ind)>0){
    #     data2 <- data[ind,]
    #     for(j in 1:length(plage_text)){
    #       temp2 <- abs(data2$T_EXT - plage_text[j])
    #       ind2 <- which(temp2 < seuil_text)
    #       if (length(ind2)>0){
    #         tab_text_cmaxamb[i,j+1] <- max(data2[ind2,]$LECHA_CORRIGEE)
    #         tab_ind[i,j] <- ifelse(insuf,seuilcorr,-seuilcorr)
    #       }}
    #   }
    # }
    ind0 <- which((data$C_MAXAMB<=1) & (data$C_MAXAMB>=-1))
    if(length(ind0)>0){
      data2 <- data[ind0,]
      for(j in 1:length(plage_text)){
        temp2 <- abs(data2$T_EXT - plage_text[j])
        ind2 <- which(temp2 < seuil_text)
        if (length(ind2)>0){
          tab_text_cmaxamb[1,j+1] <- max(data2[ind2,]$LECHA_CORRIGEE)
          tab_ind[1,j+1] <- ifelse(insuf,1,0)
        }}}}
  return(list(tab_text_cmaxamb=tab_text_cmaxamb,tab_ind=tab_ind))}
## mix chambre

# mix_chambre <- function(data_list){
#   list_fin <- c()
#   for(k in 1:2){
#
#     for(i in 1:21){
#       temp1 <- rbind(data_list[[1]][[k]][i,-1],data_list[[2]][[k]][i,-1],data_list[[3]][[k]][i,-1])
#       new1 <- as.vector(apply(temp1, 2, max_narm))
#       new1[new1==-Inf] <- NA
#       data_list[[1]][[k]][i,] <- c(data_list[[1]][[k]][i,1],new1)
#     }
#     list_fin[[k]]<-data_list[[1]][[k]]}
#   return(list(tab_text_cmaxamb = list_fin[[1]],tab_ind = list_fin[[2]]))
# }

### à voir -------
#' @inherit extract_t_ext
#' @export
mix_chambre <- function(data_list){
  list_fin <- c()
  for(k in 1:2){
      temp1 <- rbind(data_list[[1]][[k]][1,-1],data_list[[2]][[k]][1,-1],data_list[[3]][[k]][1,-1])
      new1 <- as.vector(apply(temp1, 2, max_narm))
      new1[new1==-Inf] <- NA
      data_list[[1]][[k]][1,] <- c(data_list[[1]][[k]][1,1],new1)
    list_fin[[k]]<-data_list[[1]][[k]]}
  return(list(tab_text_cmaxamb = list_fin[[1]],tab_ind = list_fin[[2]]))
}

## mix gen (fichiers)


# mix <- function(tab_insuf,tab_non_insuf){
#   for(i in 1:21){
#     temp1 <- rbind(tab_insuf$tab_text_cmaxamb[i,-1],tab_non_insuf$tab_text_cmaxamb[i,-1])
#     temp2 <- rbind(tab_insuf$tab_ind[i,-1],tab_non_insuf$tab_ind[i,-1])
#     new1 <- as.vector(apply(temp1, 2, max_narm))
#     new1[new1==-Inf] <- NA
#     new2 <- as.vector(apply(temp2, 2, max_narm))
#     new2[new2==-Inf] <- NA
#     tab_insuf$tab_text_cmaxamb[i,] <- c(tab_insuf$tab_text_cmaxamb[i,1],new1)
#     tab_insuf$tab_ind[i,] <- c(tab_insuf$tab_ind[i,1],new2)
#   }
#   return(list(tab_text_cmaxamb_mix=tab_insuf$tab_text_cmaxamb,tab_ind_mix=tab_insuf$tab_ind))
# }

#' @inherit extract_t_ext
#' @export
mix <- function(tab_insuf,tab_non_insuf){
  temp1 <- rbind(tab_insuf$tab_text_cmaxamb[1,-1],tab_non_insuf$tab_text_cmaxamb[1,-1])
  temp2 <- rbind(tab_insuf$tab_ind[1,-1],tab_non_insuf$tab_ind[1,-1])
  new1 <- as.vector(apply(temp1, 2, max_narm))
  new1[new1==-Inf] <- NA
  new2 <- as.vector(apply(temp2, 2, max_narm))
  new2[new2==-Inf] <- NA
  tab_insuf$tab_text_cmaxamb[1,] <- c(tab_insuf$tab_text_cmaxamb[1,1],new1)
  tab_insuf$tab_ind[1,] <- c(tab_insuf$tab_ind[1,1],new2)
  return(list(tab_text_cmaxamb_mix=tab_insuf$tab_text_cmaxamb,tab_ind_mix=tab_insuf$tab_ind))
}

# mix old_new replacementt

# mix_old_new <- function(old,new,line,seuilcorr){
#   # f_temp <- function(x){
#   #   x[x %in% c(seuilcorr,-seuilcorr)] <- 0
#   #   return(x)
#   # }
#   # old[[2]][line,]<- lapply(old[[2]][line,],f_temp)
#   old[[2]][line,][old[[2]][line,] %in% c(seuilcorr,-seuilcorr)]<- 0
#   for (i in 1:2){
#       temp1 <- rbind(old[[i]][line,-c(1,2)],new[[i]][line,-1])
#       new1 <- as.vector(apply(temp1, 2, max_narm))
#       new1[new1==-Inf] <- NA
#       old[[i]][line,] <- c(old[[i]][line,1:2],new1)
#   }
#   return(old)}

# mix_old_new <- function(old,new,seuilcorr){
#   old[[2]][old[[2]] %in% c(seuilcorr,-seuilcorr)]<- 0
#   # p <- which(new$tab_ind_mix==seuilcorr)
#   # m <- which(new$tab_ind_mix==-seuilcorr)
#   #ind
#   temp1 <- rbind(old[[2]][,-c(1,2)],new[[2]][,-1])
#   new1 <- as.vector(apply(temp1, 2, max_narm))
#   new1[new1==-Inf] <- NA
#   old[[2]] <- c(old[[2]][,1:2],new1)
#   #lecha
#   temp2 <- rbind(old[[1]][,-c(1,2)],new[[1]][,-1],new[[2]][,-1])
#   check <- function(x){
#     if (!is.na(x[1])){
#
#     }
#     else{
#
#     }
#   }
#   new2 <- as.vector(apply(temp2, 2, check))
#   new2[is.infinite(new2)] <- NA
#   old[[1]] <- c(old[[1]][,1:2],new2)
#   return(old)}

# mix old new with addition

# sum_narm <- function(x){
#   val <- NA
#   if(length(which(is.na(x)))<length(x)){
#     val <- sum(x,na.rm = TRUE)
#   }
#   return(val)
# }

# mix_old_new <- function(old,new){
#   for (i in 1:2){
#     for(j in 1:21){
#       temp1 <- rbind(old[[i]][j,-1],new[[i]][j,-1])
#       if(i==2){
#       new1 <- as.vector(apply(temp1, 2, sum_narm))}
#       else{
#         new1 <- as.vector(apply(temp1, 2, max_narm))
#         new1[new1==-Inf] <- NA
#       }
#       old[[i]][j,] <- c(old[[i]][j,1],new1)
#     }
#   }
#   return(old)}

# mix_old_new without fixed values

# mix_old_new_no_fixed_values <- function(old,new,fixed_table,line){
#   f_pt <- which(fixed_table==1)
#   if(length(f_pt)>0){
#     new$tab_text_cmaxamb_mix[f_pt-1] <- NA
#     new$tab_ind_mix[f_pt-1] <- NA
#   }
#   mixed_data <- mix_old_new(old,new)
#   return(mixed_data)
# }
#
# mix_old_new_no_fixed_values <- function(old,new,fixed,seuilcorr=1){
#   f_pt <- which(fixed==1)
#   if(length(f_pt)>0){
#     new$tab_text_cmaxamb_mix[f_pt-1] <- NA
#     new$tab_ind_mix[f_pt-1] <- NA
#   }
#   mixed_data <- mix_old_new(old,new,seuilcorr = seuilcorr)
#   return(mixed_data)
# }


# original values

# org_val <- function(fin){
#   y_org <- fin$tab_text_cmaxamb_mix[-1]-fin$tab_ind_mix[-1]
#   return(y_org)
# }
