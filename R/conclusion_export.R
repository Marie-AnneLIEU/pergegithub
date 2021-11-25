#' @title Exporter le tableau des conclusions de l'analyse
#' @usage conclusion_export(na,list_res,path,len)
#' conclusion_export_syl(na,list_res,file,len)
#' @param list.res sortie de la fonction \code{detect_anoma_chambre}
#' @param na une liste des noms des jeux de données
#' @param path chemin d'accès au répertoire de sauvegarde
#' @param len nombre de colonnes par chambre/zone à chauffer
#' @return un fichier excel, format .xlsx
#' @description  \code{conclusion_export} et \code{conclusion_export_syl} sont similaires mais \code{conclusion_export_syl} est faite pour pouvoir être incorporée dans l'application Shiny
#' @export
#' @import xlsx


conclusion_export <- function(na,list_res,path,len=19){
  require(xlsx)

  excluded <- c()
  for(i in 1:length(list_res)){
    excluded[[i]] <- c(list_res[[i]]$missing_column,list_res[[i]]$deleted_room_names,list_res[[i]]$deleted_period_names,list_res[[i]]$na_col_names,NA)
  }

  for (i in 1:length(list_res)){
    if(!is.null(list_res[[i]]$tab_res)){
      if(length(list_res[[i]]$tab_res)>0){
        names(list_res[[i]]$tab_res) <- na[na %notin% excluded[[i]]][1:length(list_res[[i]]$tab_res)]
      }}}

    wb = createWorkbook()

    for (i in 1:length(na)){
      #oldOpt <- options()
      options(xlsx.datetime.format="h:mm:ss;@")
      sheet = createSheet(wb, na[i])
      rows  <- createRow(sheet, rowIndex=1)
      cell.1 <- createCell(rows, colIndex=1)[[1,1]]
      cell.2 <- createCell(rows, colIndex=2+len)[[1,1]]
      cell.3 <- createCell(rows, colIndex=3+2*len)[[1,1]]

      setCellValue(cell.1, "Chambre 1")
      setCellValue(cell.2, "Chambre 2")
      setCellValue(cell.3, "Chambre 3")

      cellStyle1 <- CellStyle(wb) +
        Fill(backgroundColor="orange", foregroundColor="orange",
             pattern="SOLID_FOREGROUND") +
        Alignment(h="ALIGN_CENTER")

      setCellStyle(cell.1, cellStyle1)
      setCellStyle(cell.2, cellStyle1)
      setCellStyle(cell.3, cellStyle1)

      addMergedRegion(sheet, 1, 1, 1, len)
      addMergedRegion(sheet, 1, 1, 2+len,1+2*len)
      addMergedRegion(sheet, 1, 1, 3+2*len,2+3*len)

      col_num <- 2
      for (k in 1:length(list_res)){
      if(na[i] %in% names(list_res[[k]]$tab_res)){
      addDataFrame(subset(list_res[[k]]$tab_res[[na[i]]],select = -c(date))
                   , sheet=sheet, startColumn=col_num, row.names=FALSE,startRow=2)}
      col_num <- col_num+len+1}

      options(xlsx.datetime.format="yyyy-mm-dd")
      if(na[i] %in% names(list_res[[1]]$tab_res)){
        date1 <- data.frame(list_res[[1]]$tab_res[[na[i]]]$date)
        colnames(date1) <- "date"
        addDataFrame(date1,sheet=sheet, startColumn=1, row.names=FALSE,startRow = 2)}


      if(na[i] %in% names(list_res[[2]]$tab_res)){
        date2 <- data.frame(list_res[[2]]$tab_res[[na[i]]]$date)
        colnames(date2) <- "date"
        addDataFrame(date2,sheet=sheet, startColumn=2+len, row.names=FALSE,startRow = 2)


      }
      if(na[i] %in% names(list_res[[3]]$tab_res)){
        date3 <- data.frame(list_res[[3]]$tab_res[[na[i]]]$date)
        colnames(date3) <- "date"
        addDataFrame(date3,sheet=sheet, startColumn=3+2*len, row.names=FALSE,startRow = 2)

      }

      for (l in 1:(2+3*len)){
        autoSizeColumn(sheet, l)}



    }

    saveWorkbook(wb, paste0(mypath,"Conclusion.xlsx"))
}

#' @inherit conclusion_export
#' @export
conclusion_export_syl <- function(na,list_res,file,len=20){
  require(xlsx)

  excluded <- c()
  for(i in 1:length(list_res)){
    excluded[[i]] <- c(list_res[[i]]$missing_column,list_res[[i]]$deleted_room_names,list_res[[i]]$deleted_period_names,list_res[[i]]$na_col_names,NA)
  }

  for (i in 1:length(list_res)){
    if(!is.null(list_res[[i]]$tab_res)){
      if(length(list_res[[i]]$tab_res)>0){
      names(list_res[[i]]$tab_res) <- na[na %notin% excluded[[i]]][1:length(list_res[[i]]$tab_res)]
    }}}

  wb = createWorkbook()

  for (i in 1:length(na)){
    #oldOpt <- options()
    options(xlsx.datetime.format="h:mm:ss;@")
    sheet = createSheet(wb, na[i])
    rows  <- createRow(sheet, rowIndex=1)
    cell.1 <- createCell(rows, colIndex=1)[[1,1]]
    cell.2 <- createCell(rows, colIndex=2+len)[[1,1]]
    cell.3 <- createCell(rows, colIndex=3+2*len)[[1,1]]

    setCellValue(cell.1, "Chambre 1")
    setCellValue(cell.2, "Chambre 2")
    setCellValue(cell.3, "Chambre 3")

    cellStyle1 <- CellStyle(wb) +
      Fill(backgroundColor="orange", foregroundColor="orange",
           pattern="SOLID_FOREGROUND") +
      Alignment(h="ALIGN_CENTER")

    setCellStyle(cell.1, cellStyle1)
    setCellStyle(cell.2, cellStyle1)
    setCellStyle(cell.3, cellStyle1)

    addMergedRegion(sheet, 1, 1, 1, len)
    addMergedRegion(sheet, 1, 1, 2+len,1+2*len)
    addMergedRegion(sheet, 1, 1, 3+2*len,2+3*len)

    col_num <- 2
    for (k in 1:length(list_res)){
      if(na[i] %in% names(list_res[[k]]$tab_res)){
        addDataFrame(subset(list_res[[k]]$tab_res[[na[i]]],select = -c(date))
                     , sheet=sheet, startColumn=col_num, row.names=FALSE,startRow=2)}
      col_num <- col_num+len+1}

    options(xlsx.datetime.format="yyyy-mm-dd")
    if(na[i] %in% names(list_res[[1]]$tab_res)){
      date1 <- data.frame(list_res[[1]]$tab_res[[na[i]]]$date)
      colnames(date1) <- "date"
      addDataFrame(date1,sheet=sheet, startColumn=1, row.names=FALSE,startRow = 2)}


    if(na[i] %in% names(list_res[[2]]$tab_res)){
      date2 <- data.frame(list_res[[2]]$tab_res[[na[i]]]$date)
      colnames(date2) <- "date"
      addDataFrame(date2,sheet=sheet, startColumn=2+len, row.names=FALSE,startRow = 2)


    }
    if(na[i] %in% names(list_res[[3]]$tab_res)){
      date3 <- data.frame(list_res[[3]]$tab_res[[na[i]]]$date)
      colnames(date3) <- "date"
      addDataFrame(date3,sheet=sheet, startColumn=3+2*len, row.names=FALSE,startRow = 2)

    }

    for (l in 1:(2+3*len)){
      autoSizeColumn(sheet, l)}



  }

  saveWorkbook(wb, file)
}

