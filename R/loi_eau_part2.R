# ## create------
# create_text<- function(extract_text,mypath,non_insuf){
#   require(xlsx)
#   wb = createWorkbook()
#   for (i in 1:length(extract_text)){
#   sheet = createSheet(wb, sheetName = paste0("Sheet",i))
#   dataframe <- extract_text[[i]]
#   # if(is.na(dataframe)){dataframe <- NULL}
#   addDataFrame(dataframe,sheet = sheet, startColumn=1, row.names=FALSE,startRow = 1)}
#   filename <- ifelse(non_insuf,"loi_eau_non_insuf.xlsx","loi_eau_insuf.xlsx")
#   saveWorkbook(wb, paste0(mypath,filename))
# }

# ## add -----
# add_text <- function(extract_text,mypath,file,non_insuf){
# require(xlsx)
#   require(readxl)
#
#   path <- file
#   sheets <- excel_sheets(path = path)
#
#   import <- c()
#   for (i in 1:length(extract_text)){
#     data <- read_xlsx(path, sheet = sheets[i])
#     data<-lapply(data,as.numeric)
#     import[[i]]<- as.data.frame(data)
#   }
#
#
#   wb = createWorkbook()
#   for (i in 1:length(extract_text)){
#     #sheet = createSheet(wb, sheetName = paste0("Sheet",i))
#     if(!is.na(extract_text[[i]])){
#       if(!is.null(import[[i]]$T_EXT)){
#       dataframe <- rbind(import[[i]],extract_text[[i]])
#       dataframe <- dataframe[!duplicated(dataframe),]
#       dataframe <- dataframe[order(dataframe$C_MAXAMB),]
#       rownames(dataframe) <- NULL}
#       else{dataframe <-extract_text[[i]]}}
#     else{
#       if(!is.null(import[[i]]$T_EXT)){
#       dataframe <- import[[i]]}
#       else{dataframe <- NA}}}
#     #addDataFrame(dataframe,sheet = sheet, startColumn=1, row.names=FALSE,startRow = 1)}
#   filename <- ifelse(non_insuf,"loi_eau_non_insuf.xlsx","loi_eau_insuf.xlsx")
#   saveWorkbook(wb, paste0(mypath,filename))
#
# }
#
# choice_text <- function(file,mypath,extract_text,non_insuf){
#   if(!file.exists(file)){
#     create_text(extract_text,mypath,non_insuf = non_insuf)
#   }
#   else{
#     add_text(extract_text,mypath,file,non_insuf = non_insuf)
#   }
# }


