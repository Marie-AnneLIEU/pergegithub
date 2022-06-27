detect_anoma_in_tunnel_sy<- function(list.data,num_chambre,seuil_length=12,seuil_tdep=0.003,seuil_cut=21,duree_pertinence=60,seuil_delete=5){
  require(lubridate)

  ## detecter les colonnes manquantes----
  missing_column <- missing_column_check(list.data,num_chambre)

  ## enlever les fichiers ou il ya des colonnes manquantes
  if(!is.null(missing_column)){
    list.data <- list.data[-missing_column]
  }

  if(length(list.data)>0){

    res_na <- clean_na_all(list.data,num_chambre)
    list.data <- res_na$list.data

    if(length(list.data)>0){

      ## convert
      for(i in 1:length(list.data)){
        data <- list.data[[i]]
        if(class(data$heure)=="character"){
          data$heure <- dmy_hms(paste(data$date[1],data$heure,sep = " "))
          data$date <- dmy(data$date)}
        else{
          h <- paste(hour(data$heure),minute(data$heure),second(data$heure),sep = ":")
          d <- as.character(data$date[1])
          h2 <- paste(d,h)
          data$heure <- ymd_hms(h2)
        }
        list.data[[i]]<-data}

      ## Ecart------
      list.data <- Ecart(list.data)

      ## enlever les defauts de transmission + lissage---

      # list.data <- smooth_list(list.data,span = span,num_chambre = num_chambre)
      list.data <- smooth_list(list.data,num_chambre = num_chambre)

      ## enlever les chambres non declares ----
      delete_data_null <- c()
      for (i in 1:length(list.data)){
        data <- list.data[[i]]
        if(is.null(data$data_smooth)){
          delete_data_null <- c(delete_data_null,i)
        }
      }

      if (!is.null(delete_data_null)){
        list.data <- list.data[-delete_data_null]
      }


      if(length(list.data)>0){

        ## periodes ou la chaudiere marche----

        res <- list.period(list.data,num_chambre,seuil_tdep,seuil_length)
        list.data <- res$list.data
        list_period <- res$list_period

        # enlever les jours ou la chaudiere ne marche pas ----
        delete <- c()
        for (i in 1:length(list.data)){
          period <- list_period[[i]]
          if (is.null(dim(period))){
            delete <- c(delete,i)
          }
          else if (nrow(period)==0){
            delete <- c(delete,i)
          }
        }

        if (!is.null(delete)){
          list.data <- list.data[-delete]
          list_period <- list_period[-delete]}

        if(length(list_period)>0){

          #period_check
          manage_per <- manage_period(list.data,list_period,seuil_delete=seuil_delete,seuil_cut)
          list_period <- manage_per$list_period
          ind_f_rm_l <- manage_per$ind_f_rm_l

          # enlever les fichiers ou la chaudiere ne marche pas ----
          delete <- c()
          for (i in 1:length(list.data)){
            period <- list_period[[i]]
            if (nrow(period)==0){
              delete <- c(delete,i)
            }
          }

          if (!is.null(delete)){
            list.data <- list.data[-delete]
            list_period <- list_period[-delete]}

          if(length(list_period)>0){
            tab_ad <- c()
            tab_res <- c()
            for(i in 1:length(list.data)){
              data <- list.data[[i]]
              period <- list_period[[i]]
              conclusion <- rep("RAS(tunnel)",nrow(period))
              pertinence <- rep(1,nrow(period))
              seg_demar <- rep(0,nrow(period))
              tab_add <- data.frame(period$on_start,period$on_end,data$heure[period$on_start],data$heure[period$on_end],conclusion)
              colnames(tab_add)<-c("indice_s","indice_e","heure_start..1.","heure_end..1.","conclusion")
              #duree, pertinence
              duree <- difftime(tab_add$heure_end..1.,tab_add$heure_start..1.,units = "mins")
              pertinence[duree<duree_pertinence] <- 0
              tab_add <- cbind(tab_add,duree,pertinence)
              #tab_fin
              tab_fin <- tab_add[,-c(1,2)]
              colnames(tab_fin)<- c("Heure de début","Heure de fin","Conclusion","Durée","Pertinence")
              #in tunnel test
              res_in_tunnel<- in_tunnel(data,tab_add,num_chambre = num_chambre)
              per_in <- res_in_tunnel$per_in
              data <- res_in_tunnel$data
              tab_add$conclusion[per_in==-1]<-"Souschauffe"
              tab_add$conclusion[per_in==3]<-"Fortes fluctuations"
              tab_add$conclusion[per_in==2]<- "Surchauffe"
              tab_fin$Conclusion <- tab_add$conclusion
              tab_ad[[i]]<-tab_add
              tab_res[[i]]<-tab_fin
              list.data[[i]]<- data
            }
          }else{tab_ad <- NULL
          tab_res <- NULL}
        }else{tab_ad <- NULL
        tab_res <- NULL}
      }else{tab_ad <- NULL
      tab_res <- NULL}
    }else{tab_ad <- NULL
    tab_res <- NULL}
  }else{tab_ad <- NULL
  tab_res <- NULL}
  return(list(list_data =list.data,tab_ad = tab_ad, tab_res = tab_res ))
}


detect_anoma_in_tunnel_optipellet_sy<- function(list.data,num_chambre,seuil_cut=21,duree_pertinence=60,seuil_delete=5){
  require(lubridate)

  ## detecter les colonnes manquantes----
  missing_column <- missing_column_check(list.data,num_chambre)

  ## enlever les fichiers ou il ya des colonnes manquantes
  if(!is.null(missing_column)){
    list.data <- list.data[-missing_column]
  }

  if(length(list.data)>0){

    res_na <- clean_na_all(list.data,num_chambre)
    list.data <- res_na$list.data

    if(length(list.data)>0){

      ## convert
      for(i in 1:length(list.data)){
        data <- list.data[[i]]
        if(class(data$heure)=="character"){
          data$heure <- dmy_hms(paste(data$date[1],data$heure,sep = " "))
          data$date <- dmy(data$date)}
        else{
          h <- paste(hour(data$heure),minute(data$heure),second(data$heure),sep = ":")
          d <- as.character(data$date[1])
          h2 <- paste(d,h)
          data$heure <- ymd_hms(h2)
        }
        list.data[[i]]<-data}

      ## Ecart------
      list.data <- Ecart(list.data)

      ## enlever les defauts de transmission + lissage---

      # list.data <- smooth_list(list.data,span = span,num_chambre = num_chambre)
      list.data <- smooth_list(list.data,num_chambre = num_chambre)

      ## enlever les chambres non declares ----
      delete_data_null <- c()
      for (i in 1:length(list.data)){
        data <- list.data[[i]]
        if(is.null(data$data_smooth)){
          delete_data_null <- c(delete_data_null,i)
        }
      }

      if (!is.null(delete_data_null)){
        list.data <- list.data[-delete_data_null]
      }


      if(length(list.data)>0){

        ## periodes ou la chaudiere marche----

        res <- list.period.optipellet(list.data,num_chambre)
        list.data <- res$list.data
        list_period <- res$list_period

        # enlever les jours ou la chaudiere ne marche pas ----
        delete <- c()
        for (i in 1:length(list.data)){
          period <- list_period[[i]]
          if (is.null(dim(period))){
            delete <- c(delete,i)
          }
          else if (nrow(period)==0){
            delete <- c(delete,i)
          }
        }

        if (!is.null(delete)){
          list.data <- list.data[-delete]
          list_period <- list_period[-delete]}

        if(length(list_period)>0){

          #period_check
          manage_per <- manage_period(list.data,list_period,seuil_delete=seuil_delete,seuil_cut)
          list_period <- manage_per$list_period
          ind_f_rm_l <- manage_per$ind_f_rm_l

          # enlever les fichiers ou la chaudiere ne marche pas ----
          delete <- c()
          for (i in 1:length(list.data)){
            period <- list_period[[i]]
            if (nrow(period)==0){
              delete <- c(delete,i)
            }
          }

          if (!is.null(delete)){
            list.data <- list.data[-delete]
            list_period <- list_period[-delete]}

          if(length(list_period)>0){
            tab_ad <- c()
            tab_res <- c()
            for(i in 1:length(list.data)){
              data <- list.data[[i]]
              period <- list_period[[i]]
              conclusion <- rep("RAS(tunnel)",nrow(period))
              pertinence <- rep(1,nrow(period))
              seg_demar <- rep(0,nrow(period))
              tab_add <- data.frame(period$on_start,period$on_end,data$heure[period$on_start],data$heure[period$on_end],conclusion)
              colnames(tab_add)<-c("indice_s","indice_e","heure_start..1.","heure_end..1.","conclusion")
              #duree, pertinence
              duree <- difftime(tab_add$heure_end..1.,tab_add$heure_start..1.,units = "mins")
              pertinence[duree<duree_pertinence] <- 0
              tab_add <- cbind(tab_add,duree,pertinence)
              #tab_fin
              tab_fin <- tab_add[,-c(1,2)]
              colnames(tab_fin)<- c("Heure de début","Heure de fin","Conclusion","Durée","Pertinence")
              #in tunnel test
              res_in_tunnel<- in_tunnel(data,tab_add,num_chambre = num_chambre)
              per_in <- res_in_tunnel$per_in
              data <- res_in_tunnel$data
              tab_add$conclusion[per_in==-1]<-"Souschauffe"
              tab_add$conclusion[per_in==3]<-"Fortes fluctuations"
              tab_add$conclusion[per_in==2]<- "Surchauffe"
              tab_fin$Conclusion <- tab_add$conclusion
              tab_ad[[i]]<-tab_add
              tab_res[[i]]<-tab_fin
              list.data[[i]]<- data
            }
          }else{tab_ad <- NULL
          tab_res <- NULL}
        }else{tab_ad <- NULL
        tab_res <- NULL}
      }else{tab_ad <- NULL
      tab_res <- NULL}
    }else{tab_ad <- NULL
    tab_res <- NULL}
  }else{tab_ad <- NULL
  tab_res <- NULL}
  return(list(list_data =list.data,tab_ad = tab_ad, tab_res = tab_res ))
}
