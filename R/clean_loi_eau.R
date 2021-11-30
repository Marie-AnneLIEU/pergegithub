#' @title Tracer la loi d'eau théorique, enlever les données dont l'écart positif entre la loi d'eau réelle et la loi d'eau théorique est supérieur à un seuil donné
#' @description \code{clean_loi_eau} recalcule la loi d'eau puis enlever les données dont l'écart positif entre la loi d'eau réelle et la loi d'eau théorique est supérieur à un seuil donné
#' @description \code{loi_eau_graphe} trace un plotly graphe de la loi d'eau théorique et de la loi d'eau réelle
#' @description \code{C_DAMBcal} cacule le Delta de température retenu dans le calcul de la compensation
#' @description \code{calc_loi_eau} recalcule la loi d'eau à partir des paramètres et les données d'après la formule théorique
#' @param list.data liste de jeux de données
#' @param P_PENTE pente de la loi d'eau
#' @param P_DECPA décalage parallèle de la loi d'eau
#' @param P_TEXTMAX température de l'extérieur maximale, au-delà de laquelle le chauffage est coupé (pompes et brûleur)
#' @param P_DTCHAUD delta chaudière
#' @param P_TMAX température maximale de la loi d'eau
#' @param P_KCOMP Coefficient de sensibilité de la correction
#' @param P_SEUIL Seuil de déclenchement de la compensation d'ambiance
#' @param seuil_ecart seuil de l'écart
#' @param fichier numéro du jeu de données
#' @param data un jeu de données
#' @param HYST hystérésis (loi d'eau)
#' @return \code{list.data} avec données enlevées (\code{clean_loi_eau}) et la colonne C_LECHA ajoutée, plotly graphe (loi_eau_graphe), C_DAMB (\code{C_DAMBcal}), loi d'eau théorique(\code{calc_loi_eau})
#' @export

C_DAMBcal <- function(P_SEUIL,C_MAXAMB){
  f <- function(x){
    if (x>P_SEUIL){ res <- x - P_SEUIL }
    else if (x < -P_SEUIL) {res <- x + P_SEUIL}
    else{ res <- 0}
    return(res)
  }
  l <- sapply(C_MAXAMB, f)
  return(l)
}

#' @inherit C_DAMBcal
#' @export
calc_loi_eau <- function(data,P_PENTE,P_DECPA,P_TEXTMAX,P_DTCHAUD,P_TMAX,seuil_ecart,P_SEUIL,P_KCOMP){
  vamb1 <- as.numeric(data$V_AMB1)
  vamb2 <- as.numeric(data$V_AMB2)
  vamb3 <- as.numeric(data$V_AMB3)
  cmaxam <- c()
  if(length(which(data$V_DEFAUT_SAMB_1!=0))==nrow(data)){
    amb1 <- rep(NA,length(vamb1))
  }
  else{ amb1 <- as.numeric(data$E_SAMB1)}
  if(length(which(data$V_DEFAUT_SAMB_2!=0))==nrow(data)){
    amb2 <- rep(NA,length(vamb2))
  }
  else{ amb2 <- as.numeric(data$E_SAMB2)}
  if(length(which(data$V_DEFAUT_SAMB_3!=0))==nrow(data)){
    amb3 <- rep(NA,length(vamb3))
  }
  else{ amb3 <- as.numeric(data$E_SAMB3)}
  com1 <- vamb1 - amb1
  com2 <- vamb2 - amb2
  com3 <- vamb3 - amb3
  for(l in 1:length(com1)){
    cmaxam[l] <- max(com1[l],com2[l],com3[l],na.rm = TRUE)
  }
  C_DAMB <- C_DAMBcal(P_SEUIL,cmaxam)
  C_COMP <- 1 + (P_KCOMP*(C_DAMB/100))
  C_LECHA_t <- (-P_PENTE*(as.numeric(data$E_SEXT)-P_TEXTMAX)+P_TEXTMAX+P_DTCHAUD+P_DECPA)*C_COMP
  C_LECHA_t2 <- rep(NA,length(C_LECHA_t))
  for (j in 1:length(C_LECHA_t)){
    C_LECHA_t2[j] <- min(C_LECHA_t[j],P_TMAX)
  }
  C_LECHA <- rep(NA,length(C_LECHA_t2))
  for(k in 1:length(C_LECHA_t2)){
    C_LECHA[k] <- max(C_LECHA_t2[k],P_TEXTMAX+P_DTCHAUD)
  }
  return(C_LECHA)
}

#' @inherit C_DAMBcal
#' @export
clean_loi_eau <- function(list.data,P_PENTE,P_DECPA,P_TEXTMAX,P_DTCHAUD,P_TMAX,seuil_ecart,P_SEUIL,P_KCOMP,HYST){
  # loi_eau_sans_hyst_theorique <- c()
  for(i in 1:length(list.data)){
    data <- list.data[[i]]
    C_LECHA <- calc_loi_eau(data,P_PENTE=P_PENTE,P_DECPA=P_DECPA,P_TEXTMAX=P_TEXTMAX,P_DTCHAUD=P_DTCHAUD
                            ,P_TMAX=P_TMAX,seuil_ecart=seuil_ecart,P_SEUIL=P_SEUIL,P_KCOMP=P_KCOMP)
    # vamb1 <- as.numeric(data$V_AMB1)
    # vamb2 <- as.numeric(data$V_AMB2)
    # vamb3 <- as.numeric(data$V_AMB3)
    # cmaxam <- c()
    # if(length(which(data$V_DEFAUT_SAMB_1!=0))==nrow(data)){
    #   amb1 <- rep(NA,length(vamb1))
    # }
    # else{ amb1 <- as.numeric(data$E_SAMB1)}
    # if(length(which(data$V_DEFAUT_SAMB_2!=0))==nrow(data)){
    #   amb2 <- rep(NA,length(vamb2))
    # }
    # else{ amb2 <- as.numeric(data$E_SAMB2)}
    # if(length(which(data$V_DEFAUT_SAMB_3!=0))==nrow(data)){
    #   amb3 <- rep(NA,length(vamb3))
    # }
    # else{ amb3 <- as.numeric(data$E_SAMB3)}
    # com1 <- vamb1 - amb1
    # com2 <- vamb2 - amb2
    # com3 <- vamb3 - amb3
    # for(l in 1:length(com1)){
    #   cmaxam[l] <- max(com1[l],com2[l],com3[l],na.rm = TRUE)
    # }
    # C_DAMB <- C_DAMBcal(P_SEUIL,cmaxam)
    # C_COMP <- 1 + (P_KCOMP*(C_DAMB/100))
    # C_LECHA_t <- (-P_PENTE*(as.numeric(data$E_SEXT)-P_TEXTMAX)+P_TEXTMAX+P_DTCHAUD+P_DECPA)*C_COMP
    # C_LECHA_t2 <- rep(NA,length(C_LECHA_t))
    # for (j in 1:length(C_LECHA_t)){
    #   C_LECHA_t2[j] <- min(C_LECHA_t[j],P_TMAX)
    # }
    # C_LECHA <- rep(NA,length(C_LECHA_t2))
    # for(k in 1:length(C_LECHA_t2)){
    #   C_LECHA[k] <- max(C_LECHA_t2[k],P_TEXTMAX+P_DTCHAUD)
    # }
    data <- data.frame(data,C_LECHA)
    ecart <- as.numeric(data$C_LECHA_HYST)-C_LECHA
    ind_del <- which(ecart > seuil_ecart | ecart< -(HYST+seuil_ecart))
    if(length(ind_del)>0){
      list.data[[i]] <- data[-ind_del,]}
    else{list.data[[i]]<-data}}
  # loi_eau_sans_hyst_theorique[[i]]<-C_LECHA}
  return(list.data)
}

#' @inherit C_DAMBcal
#' @export
loi_eau_graphe <- function(list.data,fichier){
  require(plotly)
  data <- list.data[[fichier]]
  if (class(data$heure[1]) == "character") {
    x_heure <- dmy_hms(paste(data$date[1], data$heure, sep = " "))
  }
  else if (class(data$heure[1])[1] != "character") {
    h <- paste(hour(data$heure), minute(data$heure), second(data$heure),
               sep = ":")
    d <- as.character(data$date[1])
    h2 <- paste(d, h)
    x_heure <- ymd_hms(h2)
  }
  f <- plot_ly(x = ~x_heure)
  f <- f %>% add_trace(y = ~data$C_LECHA_HYST, mode = "lines",
                       name = "Loi d'eau réelle")
  f <- f %>% add_trace(y = ~data$C_LECHA, mode = "lines",
                       name = "Loi d'eau théorique")
  f <- layout(f, title = paste0("Chaudière: ", data$chaudiere[1],
                                " , Date: ", data$date[1]), xaxis = list(title = "Temps"),
              yaxis = list(title = "Température (°C)"))
  return(f)
}
