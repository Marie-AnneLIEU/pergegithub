#'
#'
#' ## create------
#' create_t_amb <- function(chambre_list,mypath){
#'   require(xlsx)
#'
#'   wb = createWorkbook()
#'   for (i in 1:length(chambre_list)){
#'   sheet = createSheet(wb, sheetName = paste0("Sheet",i))
#'   name <- names(chambre_list[[i]])
#'   for (j in 1:length(name)){
#'     val <- data.frame(chambre_list[[i]][[j]])
#'     colnames(val)<-paste0("Temp Ext = ",name[j])
#'     addDataFrame(val,sheet = sheet, startColumn=j, row.names=FALSE,startRow = 1)}}
#'
#'   saveWorkbook(wb, paste0(mypath,"DeltaTchambre.xlsx"))
#' }
#'
#' ## add -----
#' add_t_amb <- function(chambre_list,mypath,file){
#'   require(xlsx)
#'   require(readxl)
#'
#'   path <- file
#'   sheets <- excel_sheets(path = path)
#'
#'   import <- c()
#'   for (i in 1:length(chambre_list)){
#'     import[[i]]<-read_excel(path, sheet = sheets[i])
#'   }
#'
#'   wb = createWorkbook()
#'   for (i in 1:length(chambre_list)){
#'     sheet = createSheet(wb, sheetName = paste0("Sheet",i))
#'     name <- names(chambre_list[[i]])
#'     for (j in 1:length(name)){
#'       val <- data.frame(c(import[[i]][,j][!is.na(import[[i]][,j])],chambre_list[[i]][[j]]))
#'       colnames(val)<-paste0("Temp Ext = ",name[j])
#'       addDataFrame(val,sheet = sheet, startColumn=j, row.names=FALSE,startRow = 1)}}
#'
#'   saveWorkbook(wb, paste0(mypath,"DeltaTchambre.xlsx"))
#'
#' }
#'
#' choice <- function(file,chambre_list,mypath){
#'   if(!file.exists(file)){
#'     create_t_amb(chambre_list,mypath)
#'   }
#'   else{
#'     add_t_amb(chambre_list,mypath,file)
#'   }
#' }


