#' Fonction qui trace un plotly graphe puis l'enregister sous format html
#' @import htmlwidgets
#' @import plotly
#' @import stringr
#' @usage graphe(list.data,mypath)
#' graphe_optipellet(list.data,mypath)
#' @param list.data une liste de jeux de données
#' @param mypath chemin d'accès au répertoire de savegarde
#' @return un fichier html contenant le plotly graphe
#' @export

graphe <- function(list.data,mypath){
  require(htmlwidgets)
  require(plotly)
  require(stringr)
  for (k in 1:length(list.data)){

    fig <- plot_ly(x=~list.data[[k]]$heure)
    fig <- fig %>% add_trace(y=~12*list.data[[k]]$S_PCHA1,mode="lines",name="Circulateur 1")
    fig <- fig %>% add_trace(y=~10*list.data[[k]]$S_PCHA2,mode="lines",name="Circulateur 2")
    fig <- fig %>% add_trace(y=~8*list.data[[k]]$S_PCHA3,mode="lines",name="Circulateur 3")
    # fig <- fig %>% add_trace(y=~list.data[[k]]$data_smooth,mode="lines",name="lowess")
    fig <- fig %>% add_trace(y=~as.numeric(list.data[[k]]$E_SAMB1),mode ="lines",name="T°ambiance 1")
    fig <- fig %>% add_trace(y=~as.numeric(list.data[[k]]$E_SAMB2),mode ="lines",name="T°ambiance 2")
    fig <- fig %>% add_trace(y=~as.numeric(list.data[[k]]$E_SAMB3),mode ="lines",name="T°ambiance 3")
    fig <- fig %>% add_trace(y=~as.numeric(list.data[[k]]$E_SDEP),mode ="lines",name = "T°depart")
    fig <- fig %>% add_trace(y=~as.numeric(list.data[[k]]$C_LECHA_HYST),mode ="lines",name="loi d'eau")
    fig <- fig %>% add_trace(y=~14*as.numeric(list.data[[k]]$S_BRUFI),mode ="lines",name="Brûleur")
    fig <- fig %>% add_trace(y=~16*as.numeric(list.data[[k]]$S_PAC),mode ="lines",name="PAC")
    fig <- fig %>% add_trace(y=~as.numeric(list.data[[k]]$V_AMB1_HYST),mode ="lines",name="consigne 1")
    fig <- fig %>% add_trace(y=~as.numeric(list.data[[k]]$V_AMB2_HYST),mode ="lines",name="consigne 2")
    fig <- fig %>% add_trace(y=~as.numeric(list.data[[k]]$V_AMB3_HYST),mode ="lines",name="consigne 3")
    fig <- fig %>% add_trace(y=~6*as.numeric(list.data[[k]]$S_PCECS),mode ="lines",name="Pompe de charge ECS")
    fig <- fig %>% add_trace(y=~4*as.numeric(list.data[[k]]$S_VZECS),mode ="lines",name="Vanne de zone ecs")
    fig <- fig %>% add_trace(y=~as.numeric(list.data[[k]]$E_SPCHA),mode ="lines",name="T°primaire")
    fig <- fig %>% add_trace(y=~as.numeric(list.data[[k]]$E_SECS),mode ="lines",name="T°ecs")
    fig <- fig %>% add_trace(y=~as.numeric(list.data[[k]]$E_SEXT),mode ="lines",name="T°exterieur")
    # fig <- fig %>% add_trace(y=~as.numeric(list.data[[k]]$add_smooth),mode ="lines",name="lowess")
    # fig <- fig %>% add_trace(y=~as.numeric(list.data[[k]]$cp_col),mode ="lines",name="coupure par ctn5")

    fig

    name <- names(list.data)[k]
    for (i in c(".csv",".html",".xlsx")){name <- str_remove(name,i)}
    name <- paste0(mypath,name,".html")

    saveWidget(fig, name, selfcontained = F, libdir = "lib")}
}

#' @inherit graphe
#' @export
graphe_optipellet <- function(list.data,mypath){
  require(htmlwidgets)
  require(plotly)
  require(stringr)
  for (k in 1:length(list.data)){
    period <- rep(0,length(list.data[[k]]$V_DEBUG_AUTOMATE_GR_BRULEUR))
    period[which(list.data[[k]]$V_DEBUG_AUTOMATE_GR_BRULEUR %in% 7:17)] <- 1
    fig <- plot_ly(x=~list.data[[k]]$heure)
    fig <- fig %>% add_trace(y=~12*list.data[[k]]$S_PCHA1,mode="lines",name="Circulateur 1")
    fig <- fig %>% add_trace(y=~10*list.data[[k]]$S_PCHA2,mode="lines",name="Circulateur 2")
    fig <- fig %>% add_trace(y=~8*list.data[[k]]$S_PCHA3,mode="lines",name="Circulateur 3")
    # fig <- fig %>% add_trace(y=~list.data[[k]]$data_smooth,mode="lines",name="lowess")
    fig <- fig %>% add_trace(y=~as.numeric(list.data[[k]]$E_SAMB1),mode ="lines",name="T°ambiance 1")
    fig <- fig %>% add_trace(y=~as.numeric(list.data[[k]]$E_SAMB2),mode ="lines",name="T°ambiance 2")
    fig <- fig %>% add_trace(y=~as.numeric(list.data[[k]]$E_SAMB3),mode ="lines",name="T°ambiance 3")
    fig <- fig %>% add_trace(y=~as.numeric(list.data[[k]]$E_SDEP),mode ="lines",name = "T°depart")
    fig <- fig %>% add_trace(y=~as.numeric(list.data[[k]]$C_LECHA_HYST),mode ="lines",name="loi d'eau")
    fig <- fig %>% add_trace(y=~as.numeric(list.data[[k]]$V_AMB1_HYST),mode ="lines",name="consigne 1")
    fig <- fig %>% add_trace(y=~as.numeric(list.data[[k]]$V_AMB2_HYST),mode ="lines",name="consigne 2")
    fig <- fig %>% add_trace(y=~as.numeric(list.data[[k]]$V_AMB3_HYST),mode ="lines",name="consigne 3")
    fig <- fig %>% add_trace(y=~6*as.numeric(list.data[[k]]$S_PCECS),mode ="lines",name="Pompe de charge ECS")
    fig <- fig %>% add_trace(y=~4*as.numeric(list.data[[k]]$S_VZECS),mode ="lines",name="Vanne de zone ecs")
    fig <- fig %>% add_trace(y=~as.numeric(list.data[[k]]$E_SPCHA),mode ="lines",name="T°primaire")
    fig <- fig %>% add_trace(y=~as.numeric(list.data[[k]]$E_SECS),mode ="lines",name="T°ecs")
    fig <- fig %>% add_trace(y=~as.numeric(list.data[[k]]$E_SEXT),mode ="lines",name="T°exterieur")
    fig <- fig %>% add_trace(y=~14*period,mode="lines",name="Automate GR")
    # fig <- fig %>% add_trace(y=~as.numeric(list.data[[k]]$add_smooth),mode ="lines",name="lowess")
    # fig <- fig %>% add_trace(y=~as.numeric(list.data[[k]]$cp_col),mode ="lines",name="coupure par ctn5")
    fig

    name <- names(list.data)[k]
    for (i in c(".csv",".html",".xlsx")){name <- str_remove(name,i)}
    name <- paste0(mypath,name,".html")

    saveWidget(fig, name, selfcontained = F, libdir = "lib")}
}

