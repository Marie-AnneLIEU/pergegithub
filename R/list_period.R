#' Fonction qui détecte les périodes de fonctionnement
#' @usage list.period(list.data,num_chambre,seuil_tdep,seuil_length)
#' list.period.optipellet(list.data,num_chambre)
#' @param list.data liste de jeux de données
#' @param num_chambre numéro de la chambre/zone à chauffer
#' @param seuil_tdep seuil de variation de température de départ
#' @param seuil_length longueur d'une phase descendante, au dessous de laquelle on ne prend plus en compte de cette phase
#' @return tableau contenant les indices de départ et de fin des périodes.
#' @export

list.period <- function(list.data,num_chambre,seuil_tdep,seuil_length){
  list_period <- c()
  for (j in 1:length(list.data)){
    data <- list.data[[j]]
    ## ON/OFF
    on_start <- c()
    on_end <- c()

    #column indicating if the condition where PAC/bru & circulateur work is satisfied
    cond <- rep(0,length(data$heure))
    ind <- which((data$S_PAC==1 | data$S_BRUFI==1) & data[[paste0("S_PCHA",num_chambre)]]==1 )
    cond[ind] <- 1
    if(!is.null(data$S_VZECS)){cond[data$S_VZECS==1]<-0}
    if(!is.null(data$S_PCECS)){cond[data$S_PCECS==1]<-0}
    data <- s_dep_cpd_one_file(data,seuil = seuil_tdep,seuil_length=seuil_length) #added, remove when new data is available
    cond[data$cp_col==1]<-0 #added, remove when new data is available
    data <- cbind(data,cond)
    list.data[[j]] <- data

    if(length(data$cond[data$cond ==1])>4){
      #loop
      if (data$cond[1] == 1){
        on_start <- c(on_start,1)
      }
      for (i in 2:(length(data$heure)-1)){
        if (data$cond[i]!=data$cond[i-1]){
          if (length(on_start) > length(on_end)){
            on_end <- c(on_end,i)}
          else {
            on_start <- c(on_start,i)
          }
        }
      }
      # if(data$cond[length(data$cond)]==data$cond[length(data$cond)-1]){
        if (length(on_start)>length(on_end)){
          on_end <- c(on_end,length(data$cond))
        }
      # }
      list_period[[j]] <- data.frame(cbind(on_start,on_end))
      if (length(which((on_end-on_start)<1))>0){
        list.period[[j]] <- list.period[[j]][-which((on_end-on_start)<1),]}
    }
    else{
      list_period[[j]] <- NA
    }}
  return(list(list_period = list_period, list.data = list.data))
}

#' @inherit list.period
#' @export
list.period.optipellet <- function(list.data,num_chambre){
  list_period <- c()
  for (j in 1:length(list.data)){
    data <- list.data[[j]]
    ## ON/OFF
    on_start <- c()
    on_end <- c()

    #column indicating if the condition where PAC/bru & circulateur work is satisfied
    cond <- rep(0,length(data$heure))
    ind <- which((data$V_DEBUG_AUTOMATE_GR_BRULEUR %in% 7:17) & data[[paste0("S_PCHA",num_chambre)]]==1 )
    cond[ind] <- 1
    if(!is.null(data$S_VZECS)){cond[data$S_VZECS==1]<-0}
    if(!is.null(data$S_PCECS)){cond[data$S_PCECS==1]<-0}
    data <- cbind(data,cond)
    list.data[[j]] <- data

    if(length(data$cond[data$cond ==1])>4){
      #loop
      if (data$cond[1] == 1){
        on_start <- c(on_start,1)
      }
      for (i in 2:(length(data$heure)-1)){
        if (data$cond[i]!=data$cond[i-1]){
          if (length(on_start) > length(on_end)){
            on_end <- c(on_end,i)}
          else {
            on_start <- c(on_start,i)
          }
        }
      }
      # if(data$cond[length(data$cond)]==data$cond[length(data$cond)-1]){
      if (length(on_start)>length(on_end)){
        on_end <- c(on_end,length(data$cond))
      }
      # }
      list_period[[j]] <- data.frame(cbind(on_start,on_end))
      if (length(which((on_end-on_start)<1))>0){
        list.period[[j]] <- list.period[[j]][-which((on_end-on_start)<1),]}
    }
    else{
      list_period[[j]] <- NA
    }}
  return(list(list_period = list_period, list.data = list.data))
}
