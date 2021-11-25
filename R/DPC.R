#' @title Détection de coupures par CNT5
#' @description Ces fonctions détectent théoriquement les coupures par CNT5
#' @usage coef_segm(data,seuil_data,k_pas)
#' phase_desc(tab,seuil_length)
#' add_coef_col(data,data_desc)
#' extra_cond_dpc(data_desc_new,seuil)
#' CPD(data_phase_desc,data)
#' s_dep_cpd_one_file(data,seuil,seuil_length)
#' s_dep_cpd_list_file(list.data,seuil,seuil_length)
#' @param data un jeu de données
#' @param seuil_data nombre de points sur lesquels on fait une régression, 12 par défault
#' @param k_pas nombre de points par déplacement, 6 par défault
#' @param tab data.frame à la sortie de la fonction \code{coef_segm}
#' @param seuil_length longueur d'une phase descendante, au dessous de laquelle on ne prend plus en compte de cette phase, 12 par défault
#' @param data_desc data.frame à la sortie de la fonction phase_desc
#' @param data_desc_new data.frame à la sortie de la fonction add_coef_col
#' @param seuil valeur seuil de la pente d'une phase de variation, au dessous de laquelle on ne prend plus en compte de cette phase, 0.003 par défault
#' @description Le but de cette séries de fonctions est d'identifier les périodes où la témpérature de l'eau de départ descend (on les appelle ici les phases descendantes), puis ajouter une colonne au jeu de données de départ qui prend pour valeurs 0 et 1, 1 quand il y a une coupure par CNT5, 0 sinon. Pour chaque phase descendante on considère qu'il y a une coupure par CNT5 à partir du 2/3 de la phase jusqu'à la fin de la phase.
#' @description La fonction \code{coef_segm} fait une régression glissante (voir le document sur l'analyse) sur les données, puis retourne un tableau avec les coefficients
#' @description La fonction \code{phase_desc} identifie les périodes/phases où la température de départ descend et les coupures par CNT5
#' @description La fonction \code{add_coef_col} fait une régression pour chaque période descendante et ajoute une colonne des valeurs de la pente au tableau de sortie de la fonction phase_desc
#' @description La fonction \code{extra_cond_dpc} supprime des périodes descendantes où la valeur de la pente ne dépasse pas un certain seuil
#' @description La fonction \code{CPD} comprend \code{coef_segm}, \code{phase_desc}, \code{add_coef_col}, \code{extra_cond_dpc}
#' @description La fonction \code{s_dep_cpd_one_file} prend pour argument un jeu de données puis ajoute une colonne cp_col indiquant l'état de fonctionnement du CTN5
#' @description La fonction \code{s_dep_cpd_list_file} prend pour argument une liste de jeux de données puis ajoute une colonne cp_col indiquant l'état de fonctionnement du CTN5 pour chaque jeu de données
#' @export



# data <- list.data[[1]]
# data <- Ecart_sy(data)
# # x <- x_axis(data,1,nrow(data))
# # y <- as.numeric(data$E_SDEP)
# # data2 <- data.frame(x,y)
# # Save an object to a file
# saveRDS(data2, file = "test_data.rds")
# # Restore the object
# data <- readRDS(file = "test_data.rds")

# # fonction qui retourne RMSE,MRE, MAE
# calc_err <- function(simulated,actual){
#   # MSE
#   library(Metrics)
#   mse <- mse(actual,simulated)
#
#   # mean absolute error
#   mae <- mean(abs(actual - simulated))
#
#   # # mean relative error
#   # mre <- mean(abs((actual - simulated)/simulated))
#   total <- mse+mae
#   results <- data.frame(mse,mae,total)
#   colnames(results) <- c("Erreur quadratique moyenne", "Erreur absolue moyenne","Total")
#
#   return(results)
# }

coef_segm <- function(data,seuil_data=12,k_pas=6){
  # x <- data$x
  # y <- data$y
  x <- x_axis(data,1,nrow(data))
  y <- as.numeric(data$E_SDEP)
  modl <- c()
  coef <- c()
  ord <- c()
  pente <- c()
  s_start <- 1
  s_end <- seuil_data
  # dif <- 0
  if (length(y) >= seuil_data) {
    y2 <- y[1:seuil_data]
    x2 <- x[1:seuil_data]
    coef1 <- NA
    ord1 <- mean(y2)
    # pente1 <- 0
    modl[[1]] <- NA
    mod1 <- lm(y2 ~ x2)
    if(!is.na(mod1$coefficients[2])){
      coef1 <- mod1$coefficients[2]
      ord1 <- mod1$coefficients[1]
      # pente1 <- coef1*10*seuil_data
      modl[[1]]<- mod1
    }
    coef <- c(coef,coef1)
    ord <- c(ord,ord1)
    # pente <- c(pente,pente1)
    # pente_start <- pente1
    j <- 2
    start <- 1+k_pas
    end <- seuil_data+k_pas
    s_start <- c(s_start,start)
    s_end <- c(s_end,end)
    while(length(which(is.na(y[start:end])))==0){
      x2 <- x[start:end]
      x2 <- x2 - x2[1]+1
      y2 <- y[start:end]
      modl[[j]] <- NA
      coef2 <- NA
      ord2 <- mean(y2)
      # pente2 <- 0
      mod2 <- lm(y2 ~ x2)
      if(!is.na(mod2$coefficients[2])){
        coef2 <- mod2$coefficients[2]
        ord2 <- mod2$coefficients[1]
        # pente2 <- coef2*10*seuil_data
        modl[[j]] <- mod2}
      coef <- c(coef,coef2)
      ord <- c(ord,ord2)
      # pente <- c(pente,pente2)
      # dif <- c(dif,abs(pente2 - pente_start))
      # if(dif[j]>s){pente_start <- pente2}
      j <- j +1
      start <- start + k_pas
      end <- end + k_pas
      s_start <- c(s_start,start)
      s_end <- c(s_end,end)
    }
    s_start <- s_start[-length(s_start)]
    s_end <- s_end[-length(s_end)]
    if(s_end[length(s_end)]<length(y)){
      start <- s_start[length(s_start)]+k_pas
      end <- length(y)
      s_start <- c(s_start,start)
      s_end <- c(s_end,end)
      x3 <- x[start:end]
      x3 <- x3 - x3[1]+1
      y3 <- y[start:end]
      modl[[j]] <- NA
      coef3 <- NA
      ord3<- mean(y3)
      # pente3 <- 0
      mod3 <- lm(y3 ~ x3)
      if(!is.na(mod3$coefficients[2])){
        coef3 <- mod3$coefficients[2]
        ord3 <- mod3$coefficients[1]
        # pente3 <- coef3*10*seuil_data
        modl[[j]] <- mod3}
      coef <- c(coef,coef3)
      ord <- c(ord,ord3)
      # pente <- c(pente,pente3)
      # dif <- c(dif,abs(pente3 - pente_start))
    }
    # tab <- data.frame(s_start,s_end,coef,ord,pente,dif)
    # tab <- data.frame(s_start,s_end,coef,ord,pente)
    tab <- data.frame(s_start,s_end,coef,ord)

  }
  else{
    coef <- NA
    ord <- mean(y)
    # pente <- 0
    # dif <- 0
    mod <- lm(y~x)
    s_start <- 1
    s_end <- length(y)
    if(!is.na(mod$coefficients[2])){
      coef <- mod$coefficients[2]
      ord <- mod$coefficients[1]}
      # pente <- seuil_data*10*coef}
    # tab <- data.frame(s_start,s_end,coef,ord,pente,dif)
    # tab <- data.frame(s_start,s_end,coef,ord,pente)
      tab <- data.frame(s_start,s_end,coef,ord)
      }
    return(tab)
}


## detect changepoint (directional)

# CPD <- function(tab,data){
#   cp <- rep(0,nrow(data))
#   # s_dep <- sign(tab$coef[1])
#   for(i in 1:nrow(tab)){
#     if(sign(tab$coef[i])>0){
#       cp[tab$s_start[i]:tab$s_end[i]] <- 1
#     }
#   }
#   return(cp)
# }

#' @inherit coef_segm
#' @export
phase_desc <- function(tab,seuil_length){

  desc <- rep(0,nrow(tab))
  desc[tab$coef < 0] <- 1

  #descendant
  start <- c()
  end <- c()
  if (length(desc[desc == 1]) > 4) {
    if (desc[1] == 1) {
      start <- c(start, 1)
    }
    for (i in 2:(length(desc) - 1)) {
      if (desc[i] != desc[i - 1]) {
        if (length(start) > length(end)) {
          end <- c(end, i-1)
        }
        else {
          start <- c(start, i)
        }
      }
    }
    if (length(start) > length(end)) {
        end <- c(end, length(desc))
    }
    phase_desc <- data.frame(cbind(start, end))
    data_phase_desc_start <- tab$s_start[phase_desc$start]
    data_phase_desc_end <- tab$s_end[phase_desc$end]
    length_phase <- data_phase_desc_end - data_phase_desc_start +1
    ind_att <- which(length_phase<seuil_length)
    if(length(ind_att)>0){
    data_phase_desc_start <- data_phase_desc_start[-ind_att]
    data_phase_desc_end <-  data_phase_desc_end[-ind_att]
    length_phase <- length_phase[-ind_att]}
    data_phase_desc_two_third <- data_phase_desc_start +  round(length_phase/3) - 1
    data_phase_desc <- data.frame(data_phase_desc_start,data_phase_desc_end,data_phase_desc_two_third)
    colnames(data_phase_desc)<-c("start","end","two_third")
  }
  else {
    data_phase_desc <- NULL
  }

  return(data_phase_desc)
}

#' @inherit coef_segm
#' @export
add_coef_col <- function(data,data_desc){
  coef <- rep(NA,nrow(data_desc))
  if(!is.null(data_desc)){
    for(i in 1:nrow(data_desc)){
      y <- as.numeric(data$E_SDEP[data_desc$start[i]:data_desc$end[i]])
      x <- x_axis(data,data_desc$start[i],data_desc$end[i])
      mod <- lm(y~x)
      coef[i]<-mod$coefficients[2]}}
  coef[is.na(coef)]<-0
  return(data.frame(data_desc,coef))
}

extra_cond_dpc <- function(data_desc_new,seuil){
  if(!is.null(data_desc_new)){
    new <- data_desc_new[which(abs(data_desc_new$coef)>seuil),]
  }
  else{new <- data_desc_new}
  return(new)
}

#' @inherit coef_segm
#' @export
CPD <- function(data_phase_desc,data){
  cp <- rep(0,nrow(data))
  if(!is.null(data_phase_desc)){
  # s_dep <- sign(tab$coef[1])
  # for(i in 1:nrow(tab)){
  #   if(tab$coef[i]>0){
  #     cp[tab$s_start[i]:tab$s_end[i]] <- 0
  #   }
  # }
  if(nrow(data_phase_desc)>0){
  for(j in 1:nrow(data_phase_desc)){
    cp[data_phase_desc$start[j]:data_phase_desc$two_third[j]] <- 1
  }}}
  return(cp)
}


# plot(x_axis(data,1,nrow(data)),data$E_SDEP,type = "l")
# lines(x_axis(data,1,nrow(data)),25*cp_col,col=2)

#' @inherit coef_segm
#' @export
s_dep_cpd_one_file <- function(data,seuil=0.003,seuil_length=12){
  tab <- coef_segm(data)
  phase <- phase_desc(tab,seuil_length = seuil_length)
  new_phase <- add_coef_col(data,phase)
  nnew_phase <- extra_cond_dpc(new_phase,seuil = seuil)
  cp_col <- CPD(nnew_phase,data)
  #temp, pas sur
  ind <- which(data$S_BRUFI==1)
  cp_col[ind]<-0
  #
  data <- cbind(data,cp_col)
  return(data)
}

#' @inherit coef_segm
#' @export
s_dep_cpd_list_file <- function(list.data,seuil=0.003,seuil_length=12){
  for(i in 1:length(list.data)){
    list.data[[i]]<-s_dep_cpd_one_file(list.data[[i]],seuil = seuil,seuil_length = seuil_length)
  }
  return(list.data)
}













## might not need

# phases <- function(tab){
#
#   mont <- rep(0,nrow(tab))
#   mont[tab$coef > 0] <- 1
#
#   #montantes
#   start <- c()
#   end <- c()
#   if (length(mont[mont == 1]) > 4) {
#     if (mont[1] == 1) {
#       start <- c(start, 1)
#     }
#     for (i in 2:(length(mont) - 1)) {
#       if (mont[i] != mont[i - 1]) {
#         if (length(start) > length(end)) {
#           end <- c(end, i-1)
#         }
#         else {
#           start <- c(start, i)
#         }
#       }
#     }
#     if (mont[length(mont)] == mont[length(mont) - 1]) {
#       if (length(start) > length(end)) {
#         end <- c(end, length(mont))
#       }
#     }
#     phase_mont <- data.frame(cbind(start, end))
#   }
#   else {
#     phase_mont <- NULL
#   }
#
#   # descendantes
#   if (is.null(dim(phase_mont))) {
#     if (length(mont[mont == 1]) >= 1) {
#       ind <- which(mont == 1)
#       d_start <- c(1, ind + 1)
#       d_end <- c(ind - 1, nrow(tab))
#     }
#     else {
#       d_start <- 1
#       d_end <- nrow(tab)
#     }
#   }
#   else if (nrow(phase_mont) == 1) {
#     if ((phase_mont$start[1] == 1) && (phase_mont$end[1] ==
#                                       nrow(tab))) {
#       d_start <- NA
#       d_end <- NA
#     }
#     else {
#       d_start <- c(1, phase_mont$end + 1)
#       d_end <- c(phase_mont$start - 1, nrow(tab))
#     }
#   }
#   else if (nrow(phase_mont) == 0) {
#     if (length(mont[mont == 1]) >= 1) {
#       ind <- which(mont == 1)
#       d_start <- c(1, ind + 1)
#       d-end <- c(ind - 1, nrow(tab))
#     }
#     else {
#       d_start <- 1
#       d_end <- nrow(tab)
#     }
#   }
#   else {
#     d_start <- c(1, phase_mont$end + 1)
#     d_end <- c(phase_mont$start - 1, nrow(tab))
#   }
#   rest <- data.frame(d_start,d_end)
#   if (length(which(d_start >= d_end)) > 0) {
#     rest <- rest[-which(d_start >= d_end), ]
#   }
#   phase_des <- rest
#
#
#   return(list(phase_mont = phase_mont, phase_des = phase_des))
# }
#
#
# tab <- coef_segm(data,seuil_data = 10,1)
# phase <- phases(tab)
#
# phasemont1 <- data[tab$s_start[phase$phase_mont$start[1]]:tab$s_end[phase$phase_mont$end[1]],]
# phasedesc1 <- data[tab$s_start[phase$phase_des$d_start[1]]:tab$s_end[phase$phase_des$d_end[1]],]
#
#
# fit_log <- function(phase){
#   list_fit_log <- c()
#   newx <- phase$x - phase$x[1]
#   newy <- phase$y - phase$y[1]
#   b <- 1
#   a <- newy[-1]/log(newx[-1]+1)
#   for (i in 1:length(a)){
#     list_fit_log[[i]] <- function(x) a[i]*log(x+b)
#   }
#   return(list_fit_log)
# }
#
# pol_3 <- function(phase){
#   newx <- phase$x - phase$x[1]
#   newy <- phase$y - phase$y[1]
#   d <- 0
#   list_func <- c()
#   tab_all <- data.frame()
#   repeat{
#       ind <- sample.int(length(newy)-1,3)
#       xneed <- newx[ind]
#       yneed <- newy[ind]
#       cons <- yneed[3] - ((yneed[2]-(yneed[1]*xneed[2]/xneed[1]))*xneed[3]^2)/(xneed[2]^2-xneed[2]*xneed[1])
#       - xneed[3]*yneed[1]/xneed[1] + (xneed[1]*xneed[3]*(yneed[2]-yneed[1]*xneed[2]/xneed[1]))/(xneed[2]^2-xneed[1]*xneed[2])
#       a <- cons/(xneed[3]^3 - (((xneed[2]^3-xneed[1]*xneed[2]^2)*xneed[3]^2)/(xneed[2]^2-xneed[1]*xneed[2]))
#                  -xneed[1]^2*xneed[3] -  (((xneed[2]^3-xneed[1]*xneed[2]^2)*xneed[3]*xneed[1])/(xneed[2]^2-xneed[1]*xneed[2])))
#       b <- (yneed[2]-yneed[1]*xneed[2]/xneed[1] - a*(xneed[2]^3 - xneed[1]*xneed[2]^2))/(xneed[2]^2-xneed[1]*xneed[2])
#       c <- yneed[1]/xneed[1] - a*xneed[1]^2 - b*xneed[1]
#       all <- c(a,b,c,d)
#       if((length(which(is.infinite(all)))==0) && (length(which(is.nan(all)))==0)){
#         tab_all <- rbind(tab_all,all)
#       }
#       # f <- function(x){a*x^3 + b*x^2 + c*x +d}
#       # list_func[[t]] <- f
#       t <- nrow(tab_all)
#       if (t  == 6){
#         break
#       }
#   }
#   colnames(tab_all) <- c("a","b","c","d")
#
#   for(i in 1:nrow(tab_all)){
#     list_func[[i]] <- function(x){tab_all$a[i]*x^3+tab_all$b[i]*x^2+tab_all$c[i]*x+tab_all$d[i]}
#   }
#   return(list(list_func=list_func,tab_all=tab_all))
# }
#
# pol_2 <- function(phase){
#   newx <- phase$x - phase$x[1]
#   newy <- phase$y - phase$y[1]
#   c <- 0
#   list_func <- c()
#   tab_all <- data.frame()
#   repeat{
#     ind <- sample.int(length(newy)-1,2)
#     xneed <- newx[ind]
#     yneed <- newy[ind]
#     a <- (yneed[2]/xneed[2] - yneed[1]/xneed[1])/(xneed[2]-xneed[1])
#     b <- yneed[1]/xneed[1]-a*xneed[1]
#     all <- c(a,b,c)
#     if((length(which(is.infinite(all)))==0) && (length(which(is.nan(all)))==0)){
#       tab_all <- rbind(tab_all,all)
#     }
#     # f <- function(x){a*x^3 + b*x^2 + c*x +d}
#     # list_func[[t]] <- f
#     t <- nrow(tab_all)
#     if (t  == 6){
#       break
#     }
#   }
#   colnames(tab_all) <- c("a","b","c")
#
#   for(i in 1:nrow(tab_all)){
#     list_func[[i]] <- function(x){tab_all$a[i]*x^2+tab_all$b[i]*x+tab_all$c[i]}
#   }
#   return(list(list_func=list_func,tab_all=tab_all))
# }
#
# lin <- function(phase){
#   newx <- phase$x - phase$x[1]
#   newy <- phase$y - phase$y[1]
#   mod <- lm(newy~newx)
#   if(!is.na(mod$coefficients[2])){
#     f <- function(x) mod$coefficients[2]*x+mod$coefficients[1]
#   }
#   else{
#     f <- function(x) {mean(newy)}
#   }
#   return(f)
# }
#
# exp_as <- function(phase){
#   newx <- phase$x - phase$x[1]
#   newy <- phase$y - phase$y[1]+1
#   a <- 1
#   b <- log(newy)/newx
#   b <- b[!is.nan(b)]
#   list_func <- c()
#   for (i in 1:length(b)){
#     f <- function(x) a*exp(x*b[i])
#     list_func[[i]] <- f
#   }
#   return(list_func)
# }
#
# exp_desc <- function(phase){
#   newx <- phase$x - phase$x[1]
#   newy <- phase$y - min(phase$y[1])
#   a <- newy[1]
#   b <- log(newy/a)/newx
#   b <- b[!is.nan(b)]
#   list_func <- c()
#   for (i in 1:length(b)){
#     f <- function(x) a*exp(x*b[i])
#     list_func[[i]] <- f
#   }
#   return(list_func)
# }
#
# fit_test <- function(phase,desc=FALSE){
#   newx <- phase$x - phase$x[1]
#   newy <- phase$y - phase$y[1]
#   newy_exp <- ifelse(desc,phase$y - min(phase$y[1]),phase$y - phase$y[1]+1)
#
#   log_f <- fit_log(phase)
#   exp_f <- ifelse(desc,exp_desc(phase),exp_as(phase))
#   pol_3_f <- pol_3(phase)$list_func
#   pol_2_f <- pol_2(phase)$list_func
#   lin_f <- lin(phase)
#
#   func_l <- log_f
#   func_l_all <- log_f
#
#   log_tab <- matrix(nrow = length(newx),ncol = length(log_f))
#   for (i in 1:length(log_f)){
#     log_tab[,i] <- log_f[[i]](newx)
#   }
#   la <- length(func_l_all)
#   exp_tab <- matrix(nrow = length(newx),ncol = length(exp_f))
#   for (i in 1:length(exp_f)){
#     exp_tab[,i] <- exp_f[[i]](newx)
#     func_l_all[[la+i]] <- exp_f[[i]]
#   }
#   l <- length(func_l)
#   la <- length(func_l_all)
#   pol3_tab <- matrix(nrow = length(newx),ncol = length(pol_3_f))
#   for (i in 1:length(pol_3_f)){
#     pol3_tab[,i] <- pol_3_f[[i]](newx)
#     func_l[[l+i]] <- pol_3_f[[i]]
#     func_l_all[[la+i]] <- pol_3_f[[i]]
#   }
#   l <- length(func_l)
#   la <- length(func_l_all)
#   pol2_tab <- matrix(nrow = length(newx),ncol = length(pol_2_f))
#   for (i in 1:length(pol_2_f)){
#     pol2_tab[,i] <- pol_2_f[[i]](newx)
#     func_l[[l+i]] <- pol_2_f[[i]]
#     func_l_all[[la+i]] <- pol_2_f[[i]]
#   }
#   lin_y <- lin_f(newx)
#   l <- length(func_l)
#   la <- length(func_l_all)
#   func_l[[l+1]] <- lin_f
#   func_l_all[[la+1]] <- lin_f
#
#
#   frame <- data.frame()
#   name <- c()
#   func_l_ind <- c()
#   for(i in 1:ncol(log_tab)){
#     frame <- rbind(frame,calc_err(log_tab[,i],newy))
#     name <- c(name,paste0("Log",i))
#     func_l_ind <- c(func_l_ind,i)
#   }
#   for(i in 1:ncol(exp_tab)){
#     frame <- rbind(frame,calc_err(exp_tab[,i],newy_exp))
#     name <- c(name,paste0("Exp",i))
#     func_l_ind <- c(func_l_ind,i)
#   }
#   for(i in 1:ncol(pol3_tab)){
#     frame <- rbind(frame,calc_err(pol3_tab[,i],newy))
#     name <- c(name,paste0("Pol3_",i))
#   }
#   for(i in 1:ncol(pol2_tab)){
#     frame <- rbind(frame,calc_err(pol2_tab[,i],newy))
#     name <- c(name,paste0("Pol2_",i))
#   }
#   frame <- rbind(frame,calc_err(lin_y,newy))
#   name <- c(name,"Lin")
#   dframe <- cbind(name,frame)
#
#   return(list(dframe=dframe,func_l_all = func_l_all,func_l = func_l,exp_f = exp_f))
# }
#
# best_fit <- function(phase,desc=FALSE){
#   res <- fit_test(phase,desc = desc)
#   dframe <- res$dframe
#   func_l <- res$func_l
#   exp_f <- res$exp_f
#   func_l_all <- res$func_l_all
#
#   best_f_ind <- which(dframe$Total == min(dframe$Total))
#   best_f_ind <- best_f_name[1]
#
#   return(func_l_all[[best_f_ind]])
# }
