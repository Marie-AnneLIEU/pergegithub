#' @title Tunnels de satisfaction
#' @param list.data liste de jeux de données à analyser
#' @param seuil_satis seuil de satisfaction, = 0.5 par défault
#' @return list.data avec colonnes ajoutées
#' @export

add_tunnel <- function(list.data,seuil_satis=0.5){
  for(i in 1:length(list.data)){
    data <- list.data[[i]]
    tunnel_upper1 <- data$V_AMB1_HYST+seuil_satis
    tunnel_upper2 <- data$V_AMB2_HYST+seuil_satis
    tunnel_upper3 <- data$V_AMB3_HYST+seuil_satis
    tunnel_lower1 <- data$V_AMB1_HYST-seuil_satis
    tunnel_lower2 <- data$V_AMB2_HYST-seuil_satis
    tunnel_lower3 <- data$V_AMB3_HYST-seuil_satis
    list.data[[i]]<-data.frame(data,tunnel_lower1,tunnel_lower2,tunnel_lower3,tunnel_upper1,tunnel_upper2,tunnel_upper3)
  }
  return(list.data)
}

#' @title détection des périodes dont la T° de l'ambiance se trouve dans le tunnel de satisfaction
#' @param num_chambre numéro de la chambre/zone à chauffer à analyser
#' @param list.data liste de jeux de données à analyser
#' @param list_period liste des périodes de fonctionnement
#' @return \code{per_tunnel} périodes dont la T° de l'ambiance se trouve dans le tunnel de satisfaction
#' @return \code{per_ind} indices des données se trouvant dans tunnel
#' @export

# in_tunnel <- function(list.data,list_period,num_chambre){
#   per_tunnel <- c()
#   ind_tunnel <- c()
#   for(i in 1:length(list.data)){
#     data <- list.data[[i]]
#     period <- list_period[[i]]
#     ind_low <- which(data$data_smooth>=data[[paste0("tunnel_lower",num_chambre)]])
#     ind_upper <- which(data$data_smooth<=data[[paste0("tunnel_upper",num_chambre)]])
#     u <- unique(c(ind_low,ind_upper))
#     per_in <- rep(0,nrow(period))
#     if(length(u)>0){
#         for(j in 1:nrow(period)){
#           ind_in <- which(dplyr::between(u,period$on_start[j],period$on_end[j])==TRUE)
#           if(length(ind_in)>0){
#             per_in[j]<-1
#           }
#           }
#     }
#     per_tunnel[[i]]<-per_in
#     ind_tunnel[[i]]<-u
#   }
#   return(list(per_tunnel=per_tunnel,ind_tunnel=ind_tunnel))
# }

in_tunnel <- function(data,tab_add,num_chambre){
  n <- nrow(data)
  ind_in <- rep(1,n)
  ind_in[data$data_smooth<data[[paste0("tunnel_lower",num_chambre)]]] <- 0
  ind_in[data$data_smooth>data[[paste0("tunnel_upper",num_chambre)]]] <- 2
  per_in <- rep(0,nrow(tab_add))
  for(j in 1:nrow(tab_add)){
    ind_in_per <- ind_in[tab_add$indice_s[j]:tab_add$indice_e[j]]
    if(length(which(ind_in_per!=1))==0){
        per_in[j]<-1
      }
  }
  ind_n2 <- which(per_in==0)
  n2 <- length(which(per_in==0))
  if(n2>0){
    for(k in ind_n2){
      ind_notin_per <- ind_in[tab_add$indice_s[k]:tab_add$indice_e[k]]
      if(length(which(ind_in_per==0))>0){
        per_in[k]<- -1
        if(length(which(ind_in_per==2))>0){
          per_in[k]<- 3
        }
      }
      else{
        per_in[k]<-2}
    }
  }
  return(list(per_in=per_in,data = cbind(data,ind_in)))
}
