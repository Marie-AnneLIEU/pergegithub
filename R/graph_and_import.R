#' @title importer les fichiers
#' @param inputfiles fichiers importés par fileInput
#' @export
import.multiple.files<-function(inputfiles,...)
{
  require(stringr)
  require(utils)
  require(readr)
  tmp.list.1<-inputfiles$name
  paths <- inputfiles$datapath

  # choose .csv or .xlsx
  l1 <-  tmp.list.1[str_detect(tmp.list.1,".csv")]
  l2 <-  tmp.list.1[str_detect(tmp.list.1,".xlsx")]

  # creer une liste vide
  tmp.list.2 <- list()

  # les paths correspondant
  l1path <- paths[inputfiles$name %in% l1]
  l2path <- paths[inputfiles$name %in% l2]

  #import
  if((length(l2)>0) && (length(l1)>0)){
    for (j in 1:length(l1)){tmp.list.2[[j]]<-read_delim(l1path[j],";", escape_double = FALSE
                                                        , col_types = cols(date = col_character()
                                                                           ,heure = col_character(),date_heure = col_character(),
                                                                           dt = col_character(),chaudiere = col_character()),trim_ws = TRUE)}
    for (i in 1:length(l2)){tmp.list.2[[i+length(l1)]]<-readxl::read_xlsx(l2path[i],...)}
    names(tmp.list.2)<-c(l1,l2)}

  else if(length(l2)==0){
    for (j in 1:length(l1)){tmp.list.2[[j]]<-read_delim(l1path[j],";", escape_double = FALSE
                                                        , col_types = cols(date = col_character()
                                                                           ,heure = col_character(),date_heure = col_character(),
                                                                           dt = col_character(),chaudiere = col_character()),trim_ws = TRUE)}
    names(tmp.list.2)<-l1
  }
  else if (length(l1)==0){
    for (i in 1:length(l2)){tmp.list.2[[i]]<-readxl::read_xlsx(l2path[i],...)}
    names(tmp.list.2)<-l2
  }
  tmp.list.2
}


# graphic func
#' @title Tracer un graphe plotly pour toutes les chambres/zones à chauffer
#' @param data jeu de données
#' @param coefc1 coefficient à multiplier aux données du circulateur 1
#' @param coefc2 coefficient à multiplier aux données du circulateur 2
#' @param coefc3 coefficient à multiplier aux données du circulateur 3
#' @param brul coefficient à multiplier aux données du brûleur
#' @param pac coefficient à multiplier aux données de la PAC
#' @param pecs coefficient à multiplier aux données de la pompe ECS
#' @param vecs coefficient à multiplier aux données de la vanne de zone ECS
#' @export

makegraph <- function(data,coefc1,coefc2,coefc3,brul,pac,pecs,vecs){
  if(class(data$heure[1])=="character"){
    x_heure <- dmy_hms(paste(data$date[1],data$heure,sep = " "))}
  else if (class(data$heure[1])[1]!="character"){
    h <- paste(hour(data$heure),minute(data$heure),second(data$heure),sep = ":")
    d <- as.character(data$date[1])
    h2 <- paste(d,h)
    x_heure <- ymd_hms(h2)
  }
  f <- plot_ly(x=~x_heure)
  f <-f %>% add_trace( y = ~as.numeric(data$tunnel_upper1), mode = 'lines', line = list(color = 'transparent')
                       , name = 'Tunnel de satisfaction 1 (haut)')
  f <- f %>% add_trace(y = ~as.numeric(data$tunnel_lower1), mode = 'lines',
                       fill = 'tonexty',  line = list(color = 'transparent'), name = 'Tunnel de satisfaction 1 (bas)')
  f <-f %>% add_trace( y = ~as.numeric(data$tunnel_upper2), mode = 'lines', line = list(color = 'transparent')
                       , name = 'Tunnel de satisfaction 2 (haut)')
  f <- f %>% add_trace(y = ~as.numeric(data$tunnel_lower2), mode = 'lines',
                       fill = 'tonexty', line = list(color = 'transparent'), name = 'Tunnel de satisfaction 2 (bas)')
  f <-f %>% add_trace( y = ~as.numeric(data$tunnel_upper3), mode = 'lines', line = list(color = 'transparent')
                       , name = 'Tunnel de satisfaction 3 (haut)')
  f <- f %>% add_trace(y = ~as.numeric(data$tunnel_lower3), mode = 'lines',
                       fill = 'tonexty', line = list(color = 'transparent'), name = 'Tunnel de satisfaction 3 (bas)')
  f <- f %>% add_trace(y=~coefc1*data$S_PCHA1,mode="lines",name="Circulateur 1")
  f <- f %>% add_trace(y=~coefc2*data$S_PCHA2,mode="lines",name="Circulateur 2")
  f <- f %>% add_trace(y=~coefc3*data$S_PCHA3,mode="lines",name="Circulateur 3")
  f <- f %>% add_trace(y=~brul*data$S_BRUFI,mode="lines",name="Brûleur")
  f <- f %>% add_trace(y=~pac*data$S_PAC,mode="lines",name="PAC")
  f <- f %>% add_trace(y=~pecs*data$S_PCECS,mode="lines",name="Pompe de charge ECS")
  f <- f %>% add_trace(y=~vecs*data$S_VZECS,mode="lines",name="Vanne de zone ecs")
  f <- f %>% add_trace(y=~as.numeric(data$E_SAMB1),mode ="lines",name="T°ambiance 1")
  f <- f %>% add_trace(y=~as.numeric(data$E_SAMB2),mode ="lines",name="T°ambiance 2")
  f <- f %>% add_trace(y=~as.numeric(data$E_SAMB3),mode ="lines",name="T°ambiance 3")
  f <- f %>% add_trace(y=~as.numeric(data$E_SDEP),mode ="lines",name = "T°depart")
  f <- f %>% add_trace(y=~as.numeric(data$C_LECHA_HYST),mode ="lines",name="Loi d'eau")
  f <- f %>% add_trace(y=~as.numeric(data$V_AMB1_HYST),mode ="lines",name="Consigne 1")
  f <- f %>% add_trace(y=~as.numeric(data$V_AMB2_HYST),mode ="lines",name="Consigne 2")
  f <- f %>% add_trace(y=~as.numeric(data$V_AMB3_HYST),mode ="lines",name="Consigne 3")
  f <- f %>% add_trace(y=~as.numeric(data$E_SPCHA),mode ="lines",name="T°primaire")
  f <- f %>% add_trace(y=~as.numeric(data$E_SECS),mode ="lines",name="T°ecs")
  f <- f %>% add_trace(y=~as.numeric(data$E_SEXT),mode ="lines",name="T°exterieur")
  f <- layout(f,title = paste0("Chaudière: ",data$chaudiere[1]," , Date: " ,data$date[1]),
              xaxis = list(title = "Temps"),
              yaxis = list(title = "Température (°C)"))
  config(f,modeBarButtonsToAdd="toggleSpikelines")
  # return(f)
}

# graphic func chambre

# vline <- function(x = 0, color = "red") {
#   list(
#     type = "line",
#     y0 = 0,
#     y1 = 1,
#     yref = "paper",
#     x0 = x,
#     x1 = x,
#     line = list(color = color),
#     dash = 'dash'
#   )
# }

# graphic func
#' @title Tracer un graphe plotly pour une chambre/zone à chauffer donnée
#' @param list_res sortie de la fonction \code{detect_anoma_chambre}/\code{detect_anoma_chambre_sy}/\code{detect_anoma_chambre_optipellet}
#' @param coefc1 coefficient à multiplier aux données du circulateur 1
#' @param coefc2 coefficient à multiplier aux données du circulateur 2
#' @param coefc3 coefficient à multiplier aux données du circulateur 3
#' @param brul coefficient à multiplier aux données du brûleur
#' @param pac coefficient à multiplier aux données de la PAC
#' @param pecs coefficient à multiplier aux données de la pompe ECS
#' @param vecs coefficient à multiplier aux données de la vanne de zone ECS
#' @param ind numéro du jeu de données dans la liste des jeux de données \code{list.data}
#' @param chambre numéro de la chambre/zone à chauffer
#' @export

makegraphchambre <- function(list_res,chambre,coefc1,coefc2,coefc3,brul,pac,pecs,vecs,ind){
  data <- list_res[[chambre]]$list_data[[ind]]
  coef <- c(coefc1,coefc2,coefc3)[chambre]
  # seg <- list_res[[chambre]]$tab_ad$heure_end..1.
  f <- plot_ly(x=~data$heure)
  # if(length(seg)>0){
  #   vlist <- c()
  #   for (j in 1:length(seg)){
  #   vlist[[j]] <- vline(x = seg[j])
  # }}
  f <- f %>% add_trace(y=~coef*data[[paste0("S_PCHA",chambre)]],mode="lines",name=paste("Circulateur",chambre,sep = " "))
  f <- f %>% add_trace(y=~brul*data$S_BRUFI,mode="lines",name="Brûleur")
  f <- f %>% add_trace(y=~pac*data$S_PAC,mode="lines",name="PAC")
  f <- f %>% add_trace(y=~pecs*data$S_PCECS,mode="lines",name="Pompe de charge ECS")
  f <- f %>% add_trace(y=~vecs*data$S_VZECS,mode="lines",name="Vanne de zone ecs")
  f <- f %>% add_trace(y=~data$data_smooth,mode="lines",name=paste("T°ambiance",chambre,"lissée",sep = " "))
  f <- f %>% add_trace(y=~data$ext_smooth,mode="lines",name="T°extérieure lissée")
  f <- f %>% add_trace(y=~as.numeric(data[[paste0("E_SAMB",chambre)]]),mode ="lines",name=paste("T°ambiance",chambre,sep = " "))
  f <- f %>% add_trace(y=~as.numeric(data$E_SDEP),mode ="lines",name = "T°depart")
  f <- f %>% add_trace(y=~as.numeric(data$C_LECHA_HYST),mode ="lines",name="Loi d'eau")
  f <- f %>% add_trace(y=~as.numeric(data[[paste0("V_AMB",chambre,"_HYST")]]),mode ="lines",name=paste("Consigne",chambre,sep = " "))
  f <- f %>% add_trace(y=~as.numeric(data$E_SPCHA),mode ="lines",name="T°primaire")
  f <- f %>% add_trace(y=~as.numeric(data$E_SECS),mode ="lines",name="T°ecs")
  f <- f %>% add_trace(y=~as.numeric(data$E_SEXT),mode ="lines",name="T°extérieure")
  f <- f %>% add_trace(y=~18*data$cp_col,mode ="lines",name="CNT5")
  f <- layout(f,title = paste0("Chaudière: ",data$chaudiere[1]," , Date: " ,data$date[1]),
              xaxis = list(title = "Temps"),
              yaxis = list(title = "Température (°C)"))
  config(f,modeBarButtonsToAdd="toggleSpikelines")
  # return(f)
}

#' @title Tracer le graphe selon le choix de l'utilisateur
#' @inherit makegraph
#' @inherit makegraphchambre
#' @export
showgraph <- function(fig,list_res,val,ind){
  if (val == 0){
    makegraph(fig$list.data[[ind]],fig$coefc1,fig$coefc2,fig$coefc3,fig$brul,
              fig$pac,fig$pecs,fig$vecs)}
  else{
    makegraphchambre(list_res,val,fig$coefc1,fig$coefc2,fig$coefc3,fig$brul,
                     fig$pac,fig$pecs,fig$vecs,ind)
  }
}


# graphic func
#' @inherit makegraph
#' @description  Tracer le graphe des données, version pour les optipellets
#' @export
makegraph_optipellet <- function(data,coefc1,coefc2,coefc3,pecs,vecs){
  if(class(data$heure[1])=="character"){
    x_heure <- dmy_hms(paste(data$date[1],data$heure,sep = " "))}
  else if (class(data$heure[1])[1]!="character"){
    h <- paste(hour(data$heure),minute(data$heure),second(data$heure),sep = ":")
    d <- as.character(data$date[1])
    h2 <- paste(d,h)
    x_heure <- ymd_hms(h2)
  }
  period <- rep(0,length(data$V_DEBUG_AUTOMATE_GR_BRULEUR))
  period[which(data$V_DEBUG_AUTOMATE_GR_BRULEUR %in% 11:15)] <- 1
  f <- plot_ly(x=~x_heure)
  f <-f %>% add_trace( y = ~as.numeric(data$tunnel_upper1), mode = 'lines', line = list(color = 'transparent')
                       , name = 'Tunnel de satisfaction 1 (haut)')
  f <- f %>% add_trace(y = ~as.numeric(data$tunnel_lower1), mode = 'lines',
                       fill = 'tonexty',  line = list(color = 'transparent'), name = 'Tunnel de satisfaction 1 (bas)')
  f <-f %>% add_trace( y = ~as.numeric(data$tunnel_upper2), mode = 'lines', line = list(color = 'transparent')
                       , name = 'Tunnel de satisfaction 2 (haut)')
  f <- f %>% add_trace(y = ~as.numeric(data$tunnel_lower2), mode = 'lines',
                       fill = 'tonexty', line = list(color = 'transparent'), name = 'Tunnel de satisfaction 2 (bas)')
  f <-f %>% add_trace( y = ~as.numeric(data$tunnel_upper3), mode = 'lines', line = list(color = 'transparent')
                       , name = 'Tunnel de satisfaction 3 (haut)')
  f <- f %>% add_trace(y = ~as.numeric(data$tunnel_lower3), mode = 'lines',
                       fill = 'tonexty', line = list(color = 'transparent'), name = 'Tunnel de satisfaction 3 (bas)')
  f <- f %>% add_trace(y=~coefc1*data$S_PCHA1,mode="lines",name="Circulateur 1")
  f <- f %>% add_trace(y=~coefc2*data$S_PCHA2,mode="lines",name="Circulateur 2")
  f <- f %>% add_trace(y=~coefc3*data$S_PCHA3,mode="lines",name="Circulateur 3")
  f <- f %>% add_trace(y=~pecs*data$S_PCECS,mode="lines",name="Pompe de charge ECS")
  f <- f %>% add_trace(y=~vecs*data$S_VZECS,mode="lines",name="Vanne de zone ecs")
  f <- f %>% add_trace(y=~as.numeric(data$E_SAMB1),mode ="lines",name="T°ambiance 1")
  f <- f %>% add_trace(y=~as.numeric(data$E_SAMB2),mode ="lines",name="T°ambiance 2")
  f <- f %>% add_trace(y=~as.numeric(data$E_SAMB3),mode ="lines",name="T°ambiance 3")
  f <- f %>% add_trace(y=~as.numeric(data$E_SDEP),mode ="lines",name = "T°depart")
  f <- f %>% add_trace(y=~as.numeric(data$C_LECHA_HYST),mode ="lines",name="Loi d'eau")
  f <- f %>% add_trace(y=~as.numeric(data$V_AMB1_HYST),mode ="lines",name="Consigne 1")
  f <- f %>% add_trace(y=~as.numeric(data$V_AMB2_HYST),mode ="lines",name="Consigne 2")
  f <- f %>% add_trace(y=~as.numeric(data$V_AMB3_HYST),mode ="lines",name="Consigne 3")
  f <- f %>% add_trace(y=~as.numeric(data$E_SPCHA),mode ="lines",name="T°primaire")
  f <- f %>% add_trace(y=~as.numeric(data$E_SECS),mode ="lines",name="T°ecs")
  f <- f %>% add_trace(y=~as.numeric(data$E_SEXT),mode ="lines",name="T°exterieur")
  f <- f %>% add_trace(y=~14*period,mode="lines",name="Automate GR")
  f <- layout(f,title = paste0("Chaudière: ",data$chaudiere[1]," , Date: " ,data$date[1]),
              xaxis = list(title = "Temps"),
              yaxis = list(title = "Température (°C)"))
  config(f,modeBarButtonsToAdd="toggleSpikelines")
  # return(f)
}

#' @inherit makegraphchambre
#' @description  Tracer le graphe des données, version pour les optipellets
#' @export
makegraphchambre_optipellet <- function(list_res,chambre,coefc1,coefc2,coefc3,pecs,vecs,ind){
  data <- list_res[[chambre]]$list_data[[ind]]
  coef <- c(coefc1,coefc2,coefc3)[chambre]
  # seg <- list_res[[chambre]]$tab_ad$heure_end..1.
  f <- plot_ly(x=~data$heure)
  # if(length(seg)>0){
  #   vlist <- c()
  #   for (j in 1:length(seg)){
  #   vlist[[j]] <- vline(x = seg[j])
  # }}
  period <- rep(0,length(data$V_DEBUG_AUTOMATE_GR_BRULEUR))
  period[which(data$V_DEBUG_AUTOMATE_GR_BRULEUR %in% 11:15)] <- 1
  f <- f %>% add_trace(y=~coef*data[[paste0("S_PCHA",chambre)]],mode="lines",name=paste("Circulateur",chambre,sep = " "))
  f <- f %>% add_trace(y=~pecs*data$S_PCECS,mode="lines",name="Pompe de charge ECS")
  f <- f %>% add_trace(y=~vecs*data$S_VZECS,mode="lines",name="Vanne de zone ecs")
  f <- f %>% add_trace(y=~data$data_smooth,mode="lines",name=paste("T°ambiance",chambre,"lissée",sep = " "))
  f <- f %>% add_trace(y=~data$ext_smooth,mode="lines",name="T°extérieure lissée")
  f <- f %>% add_trace(y=~as.numeric(data[[paste0("E_SAMB",chambre)]]),mode ="lines",name=paste("T°ambiance",chambre,sep = " "))
  f <- f %>% add_trace(y=~as.numeric(data$E_SDEP),mode ="lines",name = "T°depart")
  f <- f %>% add_trace(y=~as.numeric(data$C_LECHA_HYST),mode ="lines",name="Loi d'eau")
  f <- f %>% add_trace(y=~as.numeric(data[[paste0("V_AMB",chambre,"_HYST")]]),mode ="lines",name=paste("Consigne",chambre,sep = " "))
  f <- f %>% add_trace(y=~as.numeric(data$E_SPCHA),mode ="lines",name="T°primaire")
  f <- f %>% add_trace(y=~as.numeric(data$E_SECS),mode ="lines",name="T°ecs")
  f <- f %>% add_trace(y=~as.numeric(data$E_SEXT),mode ="lines",name="T°extérieure")
  f <- f %>% add_trace(y=~14*period,mode="lines",name="Automate GR")
  f <- layout(f,title = paste0("Chaudière: ",data$chaudiere[1]," , Date: " ,data$date[1]),
              xaxis = list(title = "Temps"),
              yaxis = list(title = "Température (°C)"))
  config(f,modeBarButtonsToAdd="toggleSpikelines")
  # return(f)
}

#' @inherit showgraph
#' @description \code{showgraph} version optipellet
#' @export
showgraph_optipellet <- function(fig,list_res,val,ind){
  if (val == 0){
    makegraph_optipellet(fig$list.data[[ind]],fig$coefc1,fig$coefc2,fig$coefc3,fig$pecs,fig$vecs)}
  else{
    makegraphchambre_optipellet(list_res,val,fig$coefc1,fig$coefc2,fig$coefc3,fig$pecs,fig$vecs,ind)
  }
}
