#' @title Fonctions de bases utilisées
#' @usage import.multiple.xlsx.files(mypath,...)
#' x_axis(data,start,end)
#' y_lin(x,ord_orig,coeff)
#' is.error(x)
#' x_axis_ecart(ecart,start,end)
#' smooth_data(data,span,num_chambre)
#' smooth_list(list.data,num_chambre)
#' ext_smooth_list(list.data)
#' import.multiple.files.fileslist(mypath,files_name,...)
#' @description \code{import.multiple.xlsx.files} importe une liste de jeux de données
#' @description \code{x_axis} convertit la colonne "heure" format "dttm" en numérique (en seconds)
#' @description \code{y_lin} calcule y à partir d'un modèle linéaire
#' @description \code{is.error} TRUE s'il y a un "try-error", FALSE sinon
#' @description \code{x_axis_ecart} fait même chose que \code{\link{x_axis}} avec un argument différent
#' @description \code{smooth_data} lisse les données de température de l'ambiance d'un jeu de données, pour une chambre/zone à chauffer donnée
#' @description \code{smooth_list} lisse les données de température de l'ambiance d'une liste de jeux de données, pour une chambre/zone à chauffer donnée
#' @description \code{ext_smooth_list} liste les données de température de l'extérieur d'une liste de jeux de données
#' @description \code{import.multiple.files.fileslist} importe une liste de jeux de données, utilisée par l'appli Shiny
#' @param list.data liste de jeux de données à analyser
#' @param mypath chemin d'accès au répertoire
#' @param start indice de commencement
#' @param end indice de fin
#' @param x numérique
#' @param ord_orig valeur de l'ordonnée à l'origine d'une fonction linéaire
#' @param coeff valeur de la pente d'une fonction linéaire
#' @param ecart colonne Ecart du jeu de données
#' @param data un jeu de données
#' @param span 1/span est l'argument f de la fonction \code{lowess}
#' @param num_chambre numéro de la chambre/zone à chauffer à analyser
#' @param files_name noms des fichiers à importer
#' @export


import.multiple.xlsx.files<-function(mypath,...)
{
  require(stringr)
  require(utils)
  require(readr)
  tmp.list.1<-list.files(path = mypath)

  # choose .csv or .xlsx
  l1 <-  tmp.list.1[str_detect(tmp.list.1,".csv")]
  l2 <-  tmp.list.1[str_detect(tmp.list.1,".xlsx")]

  # creer une liste vide
  tmp.list.2 <- list()

  #import
  if((length(l2)>0) && (length(l1)>0)){
  for (j in 1:length(l1)){tmp.list.2[[j]]<-read_delim(paste0(mypath,l1[j]),";", escape_double = FALSE
                                                      , col_types = cols(date = col_date(format = "%d/%m/%Y")
                                                                         ,heure = col_time(format = "%H:%M:%S")),trim_ws = TRUE)}
  for (i in 1:length(l2)){tmp.list.2[[i+length(l1)]]<-readxl::read_xlsx(paste0(mypath,l2[i]),...)}
  names(tmp.list.2)<-c(l1,l2)}

  else if(length(l2)==0){
    for (j in 1:length(l1)){tmp.list.2[[j]]<-read_delim(paste0(mypath,l1[j]),";", escape_double = FALSE
                                                        , col_types = cols(date = col_date(format = "%d/%m/%Y")
                                                                           ,heure = col_time(format = "%H:%M:%S")),trim_ws = TRUE)}
    names(tmp.list.2)<-l1
  }
  else if (length(l1)==0){
    for (i in 1:length(l2)){tmp.list.2[[i]]<-readxl::read_xlsx(paste0(mypath,l2[i]),...)}
    names(tmp.list.2)<-l2
  }
  tmp.list.2
}

# use it like this
#csv.import<-import.multiple.csv.files("~/R/projects/tutorials/import_multiple_data_to_R/",".csv$",sep=",")
# note: with ... we enable the function to refine the import with parameters from read.csv.
# here we define the separator of entries in the csv files to be comma.

# save it to the folder with your custom functions
#save(import.multiple.csv.files,file="~/R/functions/import.multiple.csv.files.RData")

# load it like this whenever you need it in another script with
#load("~/R/functions/import.multiple.csv.files.RData")

# end

## interval

#' @inherit import.multiple.xlsx.files
#' @export
x_axis <- function(data,start,end){
  x <- 1
  for (i in start:(end-1)){
    a <- x[length(x)]
    x <- c(x,a+data$Ecart[i+1])
  }
  return(x)
}

#' @inherit import.multiple.xlsx.files
#' @export
y_lin <- function(x,ord_orig,coeff){
  ord_orig+coeff*x
}

#' @export %notin%
`%notin%` <- Negate(`%in%`)

#' @inherit import.multiple.xlsx.files
#' @export
is.error <- function(x) inherits(x, "try-error")

#' @inherit import.multiple.xlsx.files
#' @export
x_axis_ecart <- function(ecart,start,end){
  x <- 1
  for (i in start:(end-1)){
    a <- x[length(x)]
    x <- c(x,a+ecart[i+1])
  }
  return(x)
}
#' @inherit import.multiple.xlsx.files
#' @export
smooth_data <- function(data,span,num_chambre){
  span <- round(nrow(data)/280,digits = 0)
  lowess(data$heure,data[[paste0("E_SAMB",num_chambre)]],f=1/span)$y
}
#' @inherit import.multiple.xlsx.files
#' @export
smooth_list <- function(list.data,num_chambre){
for (i in 1:length(list.data)){
  list.data[[i]] <- list.data[[i]][list.data[[i]][[paste0("V_DEFAUT_SAMB_",num_chambre)]]==0,]
  if(nrow(list.data[[i]])>0){
    data_smooth <- smooth_data(list.data[[i]],num_chambre=num_chambre)
    list.data[[i]] <- cbind(list.data[[i]],data_smooth)}}
  return(list.data)
}
#' @inherit import.multiple.xlsx.files
#' @export
ext_smooth_list <- function(list.data){
  for (i in 1:length(list.data)){
    list.data[[i]] <- list.data[[i]][list.data[[i]]$V_DEFAUT_SEXT==0,]
    if(nrow(list.data[[i]])>0){
      span <- round(nrow(list.data[[i]])/280,digits = 0)
      ext_smooth <- lowess(list.data[[i]]$heure,list.data[[i]]$E_SEXT,f=1/span)$y
      list.data[[i]] <- cbind(list.data[[i]],ext_smooth)}}
  return(list.data)
}

#' @inherit import.multiple.xlsx.files
#' @export
import.multiple.files.fileslist<-function(mypath,files_name,...)
{
  require(stringr)
  require(utils)
  require(readr)
  tmp.list.1<-files_name

  # choose .csv or .xlsx
  l1 <-  tmp.list.1[str_detect(tmp.list.1,".csv")]
  l2 <-  tmp.list.1[str_detect(tmp.list.1,".xlsx")]

  # creer une liste vide
  tmp.list.2 <- list()

  #import
  if((length(l2)>0) && (length(l1)>0)){
    for (j in 1:length(l1)){tmp.list.2[[j]]<-read_delim(paste0(mypath,l1[j]),";", escape_double = FALSE
                                                        , col_types = cols(date = col_date(format = "%d/%m/%Y")
                                                                           ,heure = col_time(format = "%H:%M:%S")),trim_ws = TRUE)}
    for (i in 1:length(l2)){tmp.list.2[[i+length(l1)]]<-readxl::read_xlsx(paste0(mypath,l2[i]),...)}
    names(tmp.list.2)<-c(l1,l2)}

  else if(length(l2)==0){
    for (j in 1:length(l1)){tmp.list.2[[j]]<-read_delim(paste0(mypath,l1[j]),";", escape_double = FALSE
                                                        , col_types = cols(date = col_date(format = "%d/%m/%Y")
                                                                           ,heure = col_time(format = "%H:%M:%S")),trim_ws = TRUE)}
    names(tmp.list.2)<-l1
  }
  else if (length(l1)==0){
    for (i in 1:length(l2)){tmp.list.2[[i]]<-readxl::read_xlsx(paste0(mypath,l2[i]),...)}
    names(tmp.list.2)<-l2
  }
  tmp.list.2
}
