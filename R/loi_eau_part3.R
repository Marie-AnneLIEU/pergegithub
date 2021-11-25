# ## create------
# create_text_cmaxamb<- function(fin,mypath){
#   require(xlsx)
#   wb = createWorkbook()
#   name <- c("Loi d'eau","Indice de renouvellement")
#   for (i in 1:length(fin)){
#     sheet = createSheet(wb, sheetName = name[i])
#     dataframe <- fin[[i]]
#     # if(is.na(dataframe)){dataframe <- NULL}
#     addDataFrame(dataframe,sheet = sheet, startColumn=1, row.names=FALSE,startRow = 1)}
#   filename <- "/Loi_d_eau.xlsx"
#   saveWorkbook(wb, paste0(mypath,filename))
# }
#
# ## add -----
# add_text_cmaxamb <- function(fin,mypath,file,fixed_table){
#   require(xlsx)
#   require(readxl)
#
#   path <- file
#   sheets <- excel_sheets(path = path)
#
#   import <- c()
#   for (i in 1:length(sheets)){
#     data <- read_xlsx(path, sheet = sheets[i])
#     data<-lapply(data,as.numeric)
#     import[[i]]<- as.data.frame(data)
#   }
#
#   wb = createWorkbook()
#   dataframe <- mix_old_new_no_fixed_values(import,fin,fixed_table)
#   for (i in 1:length(fin)){
#     sheet = createSheet(wb, sheetName = sheets[i])
#     addDataFrame(dataframe[[i]],sheet = sheet, startColumn=1, row.names=FALSE,startRow = 1)}
#   filename <-  "/Loi_d_eau.xlsx"
#   saveWorkbook(wb, paste0(mypath,filename))
#
# }
#
# choice_text_cmaxamb <- function(file,mypath,fin,fixed_table){
#   if(!file.exists(file)){
#     create_text_cmaxamb(fin,mypath)
#   }
#   else{
#     add_text_cmaxamb(fin,mypath)
#   }
# }
#
# # mix old_new without export
#
# # mix_old_new_with_import <- function(fin,path){
# #   #import old
# #   path <- file
# #   sheets <- excel_sheets(path = path)
# #   import <- c()
# #   for (i in 1:length(sheets)){
# #     data <- read_xlsx(path, sheet = sheets[i])
# #     data<-lapply(data,as.numeric)
# #     import[[i]]<- as.data.frame(data)
# #   }
# #   new <- mix_old_new(import,fin)
# #   return(new)
# # }
#
#
#
#
