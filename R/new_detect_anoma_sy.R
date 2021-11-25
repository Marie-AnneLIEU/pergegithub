#' @inherit detect_anoma_chambre
#' @description version pour l'application Shiny
#' @export



# detect_anoma_chambre <- function(list.data,num_chambre,seuil_data = 12,k_pas = 6,s = 0.0061,span = 1/30,seuil_delete=5,seuil_cut=30
#                                  ,seuil_stat=5e-5,s_d = 0.1,t_eau_maxi=90,t_eau_hyst = 2,seuil_tdep=0.001,seuil_length=12){
detect_anoma_chambre_sy <- function(list.data,num_chambre,seuil_data = 12,k_pas = 6,s = 0.0061,seuil_delete=5,seuil_cut=21
                                    ,seuil_stat=5e-5,s_d = 0.1,t_eau_maxi=90,seuil_tdep=0.003,seuil_length=12){

  require(lubridate)

  ## detecter les colonnes manquantes----
  list_names <- names(list.data)
  missing_column <- missing_column_check(list.data,num_chambre)

  ## enlever les fichiers ou il ya des colonnes manquantes
  if(!is.null(missing_column)){
    list.data <- list.data[-missing_column]
  }

  missing_column <- list_names[missing_column]
  list_names <- names(list.data)

  if(length(list.data)>0){

    res_na <- clean_na_all(list.data,num_chambre)
    list.data <- res_na$list.data
    na_col_names <- list_names[res_na$delete]

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

      delete_data_null_name <- list_names[delete_data_null]

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

        delete_names <- names(list.data)[delete]

        if (!is.null(delete)){
          list.data <- list.data[-delete]
          list_period <- list_period[-delete]}

        if(length(list_period)>0){

          #period_check
          manage_per <- manage_period(list.data,list_period,seuil_delete,seuil_cut)
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

          delete_names <- c(delete_names,names(list.data)[delete])

          if (!is.null(delete)){
            list.data <- list.data[-delete]
            list_period <- list_period[-delete]}

          if(length(list_period)>0){

            ## x des periodes

            x_ax_period <- c()
            for (i in 1:length(list.data)){
              data <- list.data[[i]]
              period <- list_period[[i]]
              x <- c()
              for (j in 1:nrow(period)){
                x[[j]] <- x_axis(data,period$on_start[j],period$on_end[j])
              }
              names(x) <- seq(1,nrow(period))
              x_ax_period[[i]]<-x
            }

            ## regression glissante, analyse de tendance -----

            seuil_data <- seuil_data # nmbr de vals prises au depart
            k_pas <- k_pas #nombre de pas
            s <- s
            list_reg <- c()
            for (i in 1:length(list.data)){
              data <- list.data[[i]]
              period <- list_period[[i]]
              reg_per <- c()
              for (l in 1:dim(period)[1]){
                data2 <- data[period$on_start[l]:period$on_end[l],]
                dat <- data2$data_smooth
                modl <- c()
                coef <- c()
                ord <- c()
                pente <- c()
                period_start <- 1
                period_end <- seuil_data
                dif <- 0
                #angle <- c()
                if (length(dat)>=seuil_data){
                  dat2 <- dat[1:seuil_data]
                  x <- x_axis(data2,1,seuil_data)
                  coef1 <- NA
                  ord1 <- mean(dat2)
                  pente1 <- 0
                  modl[[1]] <- NA
                  mod1 <- lm(dat2~x)
                  if(!is.na(mod1$coefficients[2])){
                    coef1 <- mod1$coefficients[2]
                    ord1 <- mod1$coefficients[1]
                    # pente1 <- coef1*x
                    pente1 <- coef1*10*seuil_data
                    modl[[1]] <- mod1}
                  coef <- c(coef,coef1)
                  ord <- c(ord,ord1)
                  pente <- c(pente,pente1)
                  pente_start <- pente1
                  j <- 2
                  start <- 1+k_pas
                  end <- seuil_data+k_pas
                  period_start <- c(period_start,start)
                  period_end <- c(period_end,end)
                  while(length(which(is.na(dat[start:end])))==0){
                    x2 <- x_axis(data2,start,end)
                    dat2 <- dat[start:end]
                    modl[[j]] <- NA
                    coef2 <- NA
                    ord2 <- mean(dat2)
                    pente2 <- 0
                    mod2 <- lm(dat2 ~ x2)
                    if(!is.na(mod2$coefficients[2])){
                      coef2 <- mod2$coefficients[2]
                      ord2 <- mod2$coefficients[1]
                      # pente2 <- coef2*x2[length(x2)]
                      pente2 <- coef2*10*seuil_data
                      modl[[j]] <- mod2}
                    coef <- c(coef,coef2)
                    ord <- c(ord,ord2)
                    pente <- c(pente,pente2)
                    dif <- c(dif,abs(pente2 - pente_start))
                    if(dif[j]>s){pente_start <- pente2}
                    j <- j +1
                    start <- start + k_pas
                    end <- end + k_pas
                    period_start <- c(period_start,start)
                    period_end <- c(period_end,end)
                  }
                  period_start <- period_start[-length(period_start)]
                  period_end <- period_end[-length(period_end)]
                  if(period_end[length(period_end)]<length(dat)){
                    start <- period_start[length(period_start)]+k_pas
                    end <- length(dat)
                    period_start <- c(period_start,start)
                    period_end <- c(period_end,end)
                    x3 <- x_axis(data2,start,end)
                    dat3 <- dat[start:end]
                    modl[[j]] <- NA
                    coef3 <- NA
                    ord3<- mean(dat3)
                    pente3 <- 0
                    mod3 <- lm(dat3 ~ x3)
                    if(!is.na(mod3$coefficients[2])){
                      coef3 <- mod3$coefficients[2]
                      ord3 <- mod3$coefficients[1]
                      # pente3 <- coef3*(x3[length(x3)]+(10*(seuil_data-(end-start+1))))
                      pente3 <- coef3*10*seuil_data
                      modl[[j]] <- mod3}
                    coef <- c(coef,coef3)
                    ord <- c(ord,ord3)
                    pente <- c(pente,pente3)
                    dif <- c(dif,abs(pente3 - pente_start))
                  }
                  tab <- data.frame(period_start,period_end,coef,ord,pente,dif)
                  reg_per[[l]] <- list(tab = tab,mod = modl)
                }
                else{
                  period_start <- 1
                  period_end <- length(dat)
                  coef <- NA
                  ord <- mean(dat)
                  pente <- 0
                  dif <- 0
                  x <- x_axis(data2,1,length(dat))
                  mod <- lm(dat~x)
                  if(!is.na(mod$coefficients[2])){
                    coef <- mod$coefficients[2]
                    ord <- mod$coefficients[1]
                    # pente <- x[length(x)]*coef}
                    pente <- 10*seuil_data*coef}
                  tab <- data.frame(period_start,period_end,coef,ord,pente,dif)
                  reg_per[[l]]<-list(tab=tab,mod=mod)
                }
              }
              names(reg_per)<-seq(1,dim(period)[1])
              list_reg[[i]] <- reg_per
            }

            ## tendance -----

            tendance <- c()
            for (i in 1:length(list.data)){
              regression <- list_reg[[i]]
              data <- list.data[[i]]
              period <- list_period[[i]]
              x_per <- x_ax_period[[i]]
              trend <- c()
              for (k in 1:length(regression)){
                tab <- regression[[k]]$tab
                pdc <- which(tab$dif>s)
                vpdc <- tab$period_end[pdc]-k_pas
                #vpdc <- tab$period_start[pdc]
                n <- (period$on_end[k] - period$on_start[k])+1
                if (length(vpdc)>0){
                  if(n>vpdc[length(vpdc)]){
                    seg_start <- c(1,vpdc)
                    seg_end <- c(vpdc-1,n)}
                  else{
                    seg_start <- c(1,vpdc[-length(vpdc)])
                    seg_end <- c(vpdc[-length(vpdc)]-1,n)
                  }}
                else{
                  seg_start <- 1
                  seg_end <- n
                }
                ord_seg <- c()
                coef_seg <- c()
                # pente_seg <- c()
                for(j in 1:length(seg_start)){
                  dat <- data$data_smooth[period$on_start[k]:period$on_end[k]][seg_start[j]:seg_end[j]]
                  x <- x_per[[as.character(k)]][seg_start[j]:seg_end[j]]
                  mod <- lm(dat ~ x)
                  if(!is.na(mod$coefficients[2])){
                    ord_seg <- c(ord_seg,mod$coefficients[1])
                    coef_seg <- c(coef_seg,mod$coefficients[2])
                    # pente_seg <- c(pente_seg,x[length(x)]*mod$coefficients[2])
                  }
                  else{
                    ord_seg <- c(ord_seg,mean(dat))
                    coef_seg <- c(coef_seg,0)
                    # pente_seg <- c(pente_seg,0)
                  }
                }
                periode <- rep(k,length(seg_start))
                # trend[[k]]<-data.frame(periode,seg_start,seg_end,ord_seg,coef_seg,pente_seg)
                trend[[k]]<-data.frame(periode,seg_start,seg_end,ord_seg,coef_seg)
                rownames(trend[[k]])<-NULL
              }
              names(trend) <- c(1:nrow(period))
              tendance[[i]] <- trend
            }

            #tab indiquant qu'une periode est une premiere periode apres demarrage ou pas -----
            res_ac <- detect_amb_s_cons(list.data,num_chambre)
            list.data <- res_ac$list.data
            periode_ambscons <- res_ac$periode_ambscons
            appendix <- detect_first_wper(list.data,periode_ambscons,list_period)

            # tableau des resultats----

            tab_res <- c()
            seg_res <- c()
            tab_ad <- c() #temp
            for (i in 1:length(list.data)){
              data <- list.data[[i]]
              span <- round(nrow(data)/280,digits = 0)
              ext_smooth <- lowess(data$heure,data$E_SEXT,f=1/span)$y
              data <- cbind(ext_smooth,data)
              list.data[[i]] <- data
              period <- list_period[[i]]
              tend <- tendance[[i]]
              heure_start <- list.data[[i]]$heure[1] # dummy
              heure_end <- list.data[[i]]$heure[2] #dummy
              table <- c()
              fin <- c()
              loi_att <- c()
              brul_on_off <- c()
              ecs <- c()
              t_cons <- c()
              indice_s <- c()
              indice_e <- c()
              coupe_par_ctn5 <- c()
              t_max_a <- c()
              for (k in 1:length(tend)){
                indice_sk <- period$on_start[k]
                indice_ek <- period$on_end[k]
                data2 <- data[indice_sk:indice_ek,]
                heure_per <- data2$heure
                tendk <- tend[[k]]
                for (j in 1:length(tendk$seg_start)){
                  datak <- data2[tendk$seg_start[j]:tendk$seg_end[j],]
                  loi_att <- c(loi_att,ifelse(length(which(datak$E_SDEP>=datak$C_LECHA_HYST))>0,1,0))
                  brul_on_off <- c(brul_on_off,ifelse(length(which(datak$S_BRUFI==1))>0,1,0))
                  coupe_par_ctn5 <- c(coupe_par_ctn5,ifelse(length(which(datak$cp_col==1))>0,1,0))
                  t_max_a <- c(t_max_a,ifelse(length(which(datak$C_LECHA_HYST>=t_eau_maxi))>0,1,0))
                  if((!is.null(datak$S_PCECS)) & (!is.null(datak$S_VZECS))){
                    ecs <- c(ecs,ifelse((length(which(datak$S_PCECS == 1))>0) |(length(which(datak$S_VZECS == 1))>0),1,0))}
                  else{ecs <- c(ecs,0)}
                  t_cons <- c(t_cons,ifelse(length(which(datak[[paste0("E_SAMB",num_chambre)]] >= datak[[paste0("V_AMB",num_chambre,"_HYST")]]))>0,1,0))
                }
                heure_start <- c(heure_start,heure_per[tendk$seg_start])
                heure_end <- c(heure_end,heure_per[tendk$seg_end])
                indice_s <- c(indice_s,tendk$seg_start+indice_sk-1)
                indice_e <- c(indice_e,tendk$seg_end+indice_sk-1)
                fin <- c(fin,c(rep(0,nrow(tendk)-1),1))
                table <- rbind(table,tendk)
              }
              date <- rep(data$date[1],length(heure_start)-1)

              #tendance
              signe <- rep(0,length(date))
              signe[table$coef_seg>seuil_stat] <- 1
              signe[table$coef_seg<(-seuil_stat)] <- (-1)

              tab_cons<-data.frame(date,indice_s,indice_e,heure_start[-1],heure_end[-1],fin,t_cons,ecs,table,signe,loi_att
                                   ,brul_on_off,coupe_par_ctn5,t_max_a)

              #derniere periode est fin ou pas
              tab_cons$fin[nrow(tab_cons)] <-ifelse(data$cond[period$on_end[nrow(period)]]==0,1,0)

              #seq appartient ou pas a une period de demarrage
              seg_demar <- rep(0,length(date))
              per_demar <- which(appendix[[i]]$ind2==1)
              for (m in per_demar){
                ind_dem <- which(tab_cons$periode==m)
                if( length(tab_cons$periode[tab_cons$periode==m])>1){
                  if (tab_cons$signe[ind_dem[2]]<1){
                    seg_demar[ind_dem[1:2]] <- 1
                  }
                  else{
                    seg_demar[ind_dem[1]] <- 1}
                }
                else if(length(tab_cons$periode[tab_cons$periode==m])==1){
                  seg_demar[ind_dem] <- 1
                }
              }

              # segments'names
              dup <- unique(tab_cons$periode[duplicated(tab_cons$periode)])
              seg_period <- tab_cons$periode
              if (length(dup)>0){
                for (p in 1:length(dup)){
                  l <- length(tab_cons$periode[tab_cons$periode==dup[p]])
                  f_name <- function(x){paste(dup[p],x,sep = ".")}
                  seg_period[seg_period==dup[p]]<-sapply(1:l,f_name)
                }}

              tab_cons <- cbind(seg_period,seg_demar,tab_cons)

              per_f_rm <- which(ind_f_rm_l[[i]] == 1)
              rm_f <- which(tab_cons$periode %in% per_f_rm)
              if(length(rm_f)>0){
                tab_cons$fin[rm_f]<-0
              }

              #conclusion
              conclusion <- rep("Impossible-Problème de composant(s)",nrow(tab_cons))
              conclusion[tab_cons$signe==1] <- "RAS" #montant

              si <- which(tab_cons$signe!=1) #descendant ou stationnaire
              f <- which(tab_cons$fin==1) # fin de periode
              a <- which(tab_cons$loi_att==1) # loi d'eau atteinte
              e <- which(tab_cons$ecs==1) # ecs on
              br <- which(tab_cons$brul_on_off==1) #bruleur on
              c <- which(tab_cons$t_cons == 1) # consigne atteinte
              cp_ctn <- which(tab_cons$coupe_par_ctn5 == 1) #coupe par ctn5
              t_maxi <- which(tab_cons$t_max_a == 1) #t_maxi atteinte

              #descendant + fin de periode + consigne atteinte
              # cond1 <- si[si %in% f]
              cond1 <- si[si %in% c]
              conclusion[cond1]<-"RAS"

              #descendant + fin de periode + ecs on
              # cond11 <- cond1[cond1 %notin% c]
              # cond1bis <- cond11[cond11 %in% e]
              cond12 <- si[si %in% e]
              conclusion[cond12]<-"RAS"

              #descendant + fin de periode + loi d'eau atteinte
              # cond1bis <- cond11[cond11 %notin% e]
              cond13 <- si[si %in% a]
              # t_maxi non atteinte
              cond13bis <- cond13[cond13 %notin% t_maxi]
              conclusion[cond13bis] <- "Loi d'eau insuffisante"
              #t_maxi atteinte
              cond13tris <- cond13[cond13 %in% t_maxi]
              conclusion[cond13tris] <- "Problème d'installation"

              #descendant + fin de periode + coupe par ctn5
              cond14 <- si[si %in% cp_ctn]
              conclusion[cond14] <- "Coupé par CTN5 - Démarrer le brûleur"

              # descendant + non fin de periode
              cond1 <- si[si %notin% f]

              # <- +bruleur on
              cond15 <- cond1[cond1 %in% br]
              conclusion[cond15] <- "Pièce(s) défectueuse(s)/puissance insuffisante"
              # <- bruleur off
              cond15bis <- cond1[cond1 %notin% br]
              conclusion[cond15bis] <- "Bivalence trop basse"

              # Pertinence
              pertinence <- rep(0,nrow(tab_cons))
              duree <- difftime(tab_cons$heure_end..1. , tab_cons$heure_start..1.,units = "secs" )
              pertinence[duree>40] <- 1

              tabtemp <- data.frame(tab_cons, conclusion, pertinence)
              biv <- rep(NA, nrow(tabtemp))
              ind3 <- which(tabtemp$conclusion == "Bivalence trop basse")
              if (length(ind3) > 0) {
                for (y in ind3) {
                  start <- tabtemp$indice_s[y]
                  end <- tabtemp$indice_e[y]
                  biv[y] <- max(data$E_SEXT[start:end])
                }
              }

              tab_add <- data.frame(tab_cons, conclusion,pertinence, biv)
              loi_eau_insuf <- which(tab_add$conclusion == "Loi d'eau insuffisante")

              T_ext_stat <- rep(NA, nrow(tab_add))
              min_text <- rep(NA, nrow(tab_add))
              max_text <- rep(NA, nrow(tab_add))
              mean_text <- rep(NA, nrow(tab_add))
              if (length(loi_eau_insuf) > 0) {
                for (t in loi_eau_insuf) {
                  T_ext_per <- as.numeric(data$E_SEXT[tab_add$indice_s[t]:tab_add$indice_e[t]])
                  a <- as.vector(summary(T_ext_per))
                  min_text[t] <- a[1]
                  max_text[t] <- a[6]
                  mean_text[t] <- a[4]
                  T_ext_stat[t] <- paste0(round(a[4], digits = 2),"[",round(a[1], digits = 2),";",round(a[6], digits = 2),"]")
                }
              }

              tab_add <- data.frame(tab_add, min_text, mean_text,max_text, T_ext_stat)
              # tab_test <- tab_add[tab_add$pertinence == 1,]
              # tab_test <- tab_test[tab_test$seg_demar == 0, ]
              # if (nrow(tab_test) > 0) {
              #   if (length(which(tab_test$conclusion == "Loi d'eau insuffisante")) ==nrow(tab_test)) {
              #     tab_add$conclusion[tab_add$conclusion == "Loi d'eau insuffisante"] <- "Loi d'eau insuffisante - Augmenter le décalage parallèle"
              #   }
              #   else {
              #     tab_add$conclusion[tab_add$conclusion == "Loi d'eau insuffisante"] <- "Loi d'eau insuffisante - Augmenter le décalage parallèle"
              #   }
              # }

              tab_fin <- subset(tab_add, select = -c(seg_start,seg_end, periode, indice_s, indice_e, min_text,
                                                     max_text, mean_text))

              tab_fin$coef_seg <- tab_fin$coef_seg * 3600
              tab_ad[[i]] <- tab_add
              tab_res[[i]] <- tab_fin

              # colnames(tab_res[[i]]) <- c("Période","Segment de départ","date","Heure de début","Heure de fin","Fin de période"
              #                             ,"Consigne atteinte","Pompe/Vanne de zone ECS","Ordonnée à l'origine",
              #                             "Coefficient","Ecart maximal","Tendance","Loi d'eau atteitne","Brûleur est ON"
              #                             ,"Conclusion","Pertinence","Bivalence conseillée")
              colnames(tab_res[[i]]) <- c("Période","Segment de départ","date","Heure de début","Heure de fin","Fin de période"
                                          ,"Consigne atteinte","Pompe/Vanne de zone ECS","Ordonnée à l'origine",
                                          "Variation de T°ambiance dans 1h","Tendance","Loi d'eau atteitne","Brûleur est ON","Coupure par CTN5"
                                          ,"T°maxi(loi d'eau) atteinte","Conclusion","Pertinence","Bivalence conseillée","T°extérieure")
            }
          }else{tab_res <- NULL
          tab_ad <- NULL}}  else{tab_res <- NULL
          tab_ad <- NULL}
      }
      else{
        delete_names <- NULL
        tab_res <- NULL
        tab_ad <- NULL}}
    else{
      delete_data_null_name <- NULL
      delete_names <- NULL
      tab_res <- NULL
      tab_ad <- NULL
    }}
  else{
    na_col_names <- NULL
    delete_data_null_name <- NULL
    delete_names <- NULL
    tab_res <- NULL
    tab_ad <- NULL}

  return(list(missing_column = missing_column,na_col_names = na_col_names ,deleted_room_names = delete_data_null_name
              ,deleted_period_names = delete_names, tab_res = tab_res ,tab_ad = tab_ad,list_data = list.data))
}
