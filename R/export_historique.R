#' @title exporter le tableau de l'historique des modifications (pour l'appli Shiny)
#' @usage historique_export(results,p_a_d,tableselect1,tableselect2,P_TMAX)
#' @param results sortie de la fonction \code{new_law_chambre}
#' @param p_a_d sortie de la fonction \code{mix}
#' @param tableselect1 premier fichier choisi
#' @param tableselect2 2ème fichier choisi
#' @param P_TMAX température de l'eau maximale
#' @export

historique_export <- function(results,p_a_d,tableselect1,tableselect2,P_TMAX){
  require(lubridate)
  for(i in 1:3){
    wb <- createWorkbook()
    sheet1 = createSheet(wb, sheetName = "Fichiers analysés")
    sheet2 = createSheet(wb, sheetName = "Historique")
    sheet3 = createSheet(wb, sheetName = "Paramètres")
    sheet4 = createSheet(wb, sheetName = "Loi")
    options(xlsx.datetime.format = "yyyy-mm-dd hh:mm:ss")

    loi <- data.frame(Sys.time(),p_a_d[[i]]$tab_text_cmaxamb_mix)
    colnames(loi)<-c("Date",colnames(p_a_d[[i]]$tab_text_cmaxamb_mix))
    loi$Date <- force_tz(loi$Date,tzone = "GMT")
    d_l <- which(loi[1,-c(1,2)]>= P_TMAX) + 2
    loi[1,d_l] <- NA

    if(length(results)>=i){
    if(!is.null(results[[i]]$list_param)){
    param <- data.frame(Sys.time(),results[[i]]$list_param)
    colnames(param)<-c("Date",colnames(results[[i]]$list_param))
    param$Date <- force_tz(param$Date,tzone = "GMT")}
    else{param <- NULL}}
    else{param <- NULL}

    hist <- data.frame(Sys.time(),p_a_d[[i]]$tab_ind_mix)
    colnames(hist)<-colnames(loi)
    hist$Date <- force_tz(hist$Date,tzone = "GMT")
    hist[1,d_l] <- NA

    l_data_a <- data.frame(rep(Sys.time(),2),c(tableselect1,tableselect2))
    colnames(l_data_a)<-c("Date","Fichiers analysés")
    l_data_a$Date <- force_tz(l_data_a$Date,tzone = "GMT")

    addDataFrame(l_data_a,sheet = sheet1,row.names = FALSE)
    addDataFrame(hist,sheet = sheet2,row.names = FALSE)
    if(!is.null(param)){
    addDataFrame(param,sheet = sheet3,row.names = FALSE)}
    addDataFrame(loi,sheet = sheet4,row.names = FALSE)

    autoSizeColumn(sheet1,1)
    autoSizeColumn(sheet2,1)
    if(!is.null(param)){
    autoSizeColumn(sheet3,1)}
    autoSizeColumn(sheet4,1)

    saveWorkbook(wb,paste0("Chambre",i,".xlsx"))
  }
}

#' @title combiner les fichiers de l'historique
#' @usage combine_import_export(files,coltype)
#' @param files fichiers à importer
#' @param coltype format des colonnes
#' @return liste des tableaux importés
#' @export
combine_import_export <- function(files,coltype){
  require(readxl)
  list <- c()
  if(!is.null(files)){
  for(k in 1:4){
  list_d <- c()
  for(i in 1:nrow(files)){
    list_d[[i]]<-read_excel(files$datapath[i], col_types = coltype[[k]],sheet = k)}
  ind <- c()
  for(l in 1:length(list_d)){
    if(nrow(list_d[[l]])>0){
      ind <- c(ind,l)
    }
  }
  if(length(ind)>0){
    f <- list_d[[ind[1]]]
    if(length(ind)>1){
    for(j in ind[-1]){
      f <- rbind(f,list_d[[j]])
    }}
    f <- f[order(f$Date),]
    rownames(f)<-NULL
  }
  else{f<-NULL}
  list[[k]]<-f}}
  return(list)
}

#' @title exporter le fichier combiné
#' @description exporter les tableaux dans le fichier de l'historique d'une chambre/zonne à chauffer donnée
#' @usage export_combine(list,chambre)
#' @param list sortie de la fonction \code{combine_import_export}
#' @param chambre numéro de la chambre/zone à chauffer
#' @return fichier .xlsx
#' @export
export_combine <- function(list,chambre){
  require(xlsx)
  wb <- createWorkbook()
  sheets <- c("Fichiers analysés","Historique","Paramètres","Loi")
  options(xlsx.datetime.format = "yyyy-mm-dd hh:mm:ss")
  for(i in 1:4){
    sheet = createSheet(wb, sheetName = sheets[i])
    if(!is.null(list[[i]])){
    if(nrow(list[[i]])>0){
    addDataFrame(as.data.frame(list[[i]]),row.names = FALSE,sheet = sheet)}}
  }
  saveWorkbook(wb,paste0("Chambre",chambre,".xlsx"))
}



